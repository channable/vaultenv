#!/usr/bin/env python3.6
import logging
import os
import re
import sys

from collections import defaultdict
from pathlib import Path
from textwrap import dedent
from typing import Dict, Optional

from channabuild.chroot import execute_in_chroot
from channabuild.config import ConfigLevel, get_config
from channabuild.exceptions import ChannaBuildUserError
from channabuild.logging import get_logger
from channabuild.main import initialize
from channabuild.namespaces import execute_in_new_namespace
from channabuild.packaging import build_deb, ship_deb_to_freight
from channabuild.repository import get_changelog, get_tag
from channabuild.subprocess import execute, execute_capture
from channabuild.system import create_file


@execute_in_new_namespace
def extract_image() -> None:
    execute(['rm', '-rf', 'chroot/'])
    execute(['mkdir', 'chroot/'])
    execute(['tar', '--checkpoint=10000', '--anchored', '--exclude=./dev/*',
             '-xpf', 'chroot.tar', '-C', 'chroot/'])


@execute_in_chroot(
    chroot_dir='chroot',
    binds={
        '.': '/root/build/repo'
    }
)
def build_in_chroot() -> None:
    os.chdir('/root/build/repo')

    for line in get_changelog().splitlines():
        logger.info(line)

    execute(['stack', 'build'])
    execute(['stack', 'install'])


@execute_in_chroot(
    chroot_dir='chroot',
    binds={
        '.': '/root/build/repo'
    }
)
def test_in_chroot() -> None:
    os.chdir('/root/build/repo')
    execute(['./integration-test.sh'])


def build_package() -> None:
    config = get_config()

    # Install vaultenv into ./pkg
    execute(['rm', '-rf', 'pkg/'])

    Path('pkg/usr/bin').mkdir(parents=True, exist_ok=True)
    Path('pkg/etc/secrets.d').mkdir(parents=True, exist_ok=True)

    create_file('pkg/DEBIAN/control', dedent(f"""\
        Package: vaultenv
        Version: {config.VERSION}
        Priority: optional
        Architecture: amd64
        Maintainer: Laurens Duijvesteijn <laurens@channable.com>
        Description: Run processes with secrets from HashiCorp Vault
        Depends: libc6,
                 libgmp10,
                 libgcc1,
                 zlib1g,
                 netbase
        """))

    stack_path = execute_capture(['stack', 'path', '--local-install-root']).strip().decode()
    execute(['cp', f'{stack_path}/bin/vaultenv', 'pkg/usr/bin/'])

    # Package it as deb file
    build_deb('pkg', f'vaultenv-{config.VERSION}.deb')


def ship_package() -> None:
    config = get_config()

    # Ship package if tagged with production tag:
    if config.VERSION and re.fullmatch('[0-9]+', config.VERSION):
        ship_deb_to_freight(f'chroot/root/build/repo/channabuild-{config.VERSION}.deb')


def cleanup() -> None:
    # We need to clear the chroot from within a namespace due to permission errors.
    execute_in_new_namespace(lambda: execute(['rm', '-rf', 'chroot']))()

    execute(['rm', '-rf', 'pkg/'])


def build() -> None:
    extract_image()
    build_in_chroot()
    test_in_chroot()
    build_package()
    ship_package()
    cleanup()


def dynamic_config() -> Dict[ConfigLevel, Dict[str, Optional[str]]]:
    config: Dict[ConfigLevel, Dict[str, Optional[str]]] = defaultdict(lambda: {})

    # Figure out the version.
    stack_version = execute_capture(
        ['stack', 'query', 'locals', 'vaultenv', 'version']).strip().decode('utf-8')
    stack_version = stack_version.strip("'")

    git_version = get_tag()

    if stack_version == git_version:
        config[ConfigLevel.repository]['VERSION'] = stack_version
    else:
        config[ConfigLevel.repository]['VERSION'] = f'{stack_version}.dev'

    return config


if __name__ == '__main__':
    initialize(config=dynamic_config, log_level=logging.DEBUG)
    logger = get_logger()

    try:
        if sys.argv[1] == 'build':
            build()
        else:
            print('Specify a valid command')
            sys.exit(1)
    except ChannaBuildUserError:
        logger.exception('Build failed:')
        sys.exit(1)
    except Exception:
        logger.exception('Build failed:')
        sys.exit(1)
