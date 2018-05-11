#!/usr/bin/env python3.6
import logging
import os
import re
import sys

from collections import defaultdict
from pathlib import Path
from textwrap import dedent
from typing import Dict, Optional

import click

from channabuild.config import ConfigLevel
from channabuild.caching import BuildStep
from channabuild.exceptions import ChannaBuildUserError
from channabuild.main import ChannaBuild

cb = ChannaBuild(logging_level=logging.DEBUG)

BINDS = { '.': '/root/build/repo',
          'cache/stack-work': '/root/build/repo/.stack-work',
          'cache/stack': '/root/.stack'
        }

class SetupCache(BuildStep):
    dependencies = []

    @classmethod
    def run(cls):
        cb.ensure_directories(['cache/stack', 'cache/stack-work'])


class DownloadRootFs(BuildStep):
    dependencies = [SetupCache]

    @classmethod
    def run(cls):
        cb.download_image_from_lxc(filename='rootfs.tar.xz')
        cb.execute(['rm', '-rf', 'cache/chroot'])
        cb.execute(['mkdir', '-p', 'cache/chroot'])
        cb.execute(['tar', '--exclude=./dev/*', '-xpJf',
                    'rootfs.tar.xz', '-C', 'cache/chroot/'])


class SetupRootFs(BuildStep):
    dependencies = [DownloadRootFs]

    @classmethod
    def run(cls):
        if cls.dependencies_changed:
            cls.run_changed()
        cls.run_always()

    @classmethod
    @cb.execute_in_new_namespace
    def run_always(cls):
        cb.execute(['rm', '-rf', 'chroot/'])
        cb.execute(['cp', '-r', 'cache/chroot', 'chroot/'])

    @classmethod
    @cb.execute_in_chroot('cache/chroot')
    def run_changed(cls):
        cb.install_apt_packages(['curl', 'wget', 'unzip'])
        cb.execute(['bash', '-c', 'curl -sSL https://get.haskellstack.org/ | sh'])

        Path('/tmp/vault').mkdir()
        os.chdir('/tmp/vault')
        vault_url = 'https://releases.hashicorp.com/vault/0.9.6/vault_0.9.6_linux_amd64.zip'
        cb.execute(['wget', vault_url])
        cb.execute(['unzip', 'vault_0.9.6_linux_amd64.zip'])
        cb.execute(['cp', 'vault', '/usr/local/bin/vault'])
        os.chdir('/')
        cb.execute(['rm', '-rf', '/tmp/vault'])


class Build(BuildStep):
    dependencies = [SetupRootFs]

    @classmethod
    @cb.execute_in_chroot('chroot', binds=BINDS,
            working_dir='/root/build/repo', environment_overrides={'HOME':'/root'}
            )
    def run(cls):
        cb.execute(['stack', 'build'])
        cb.execute(['stack', 'install'])


class Test(BuildStep):
    dependencies = [Build]

    @classmethod
    @cb.execute_in_chroot('chroot', binds=BINDS, working_dir='/root/build/repo',
        environment_overrides={'HOME':'/root'})
    def run(cls):
        cb.execute(['./integration-test.sh'])


class InitializeVersion(BuildStep):
    dependencies = [SetupRootFs]
    always_run = True

    @classmethod
    def run(cls):
        repository_config = cb.get_config_layer(ConfigLevel.repository)

        # Figure out the version.
        stack_version = cb.execute_capture(
            ['stack', 'query', 'locals', 'vaultenv', 'version']).strip().decode('utf-8')
        stack_version = stack_version.strip("'")

        git_version = cb.get_tag()

        if stack_version == git_version:
            repository_config.VERSION = stack_version
        else:
            repository_config.VERSION = f'{stack_version}.dev'


class BuildPackageTree(BuildStep):
    dependencies = [Test, InitializeVersion]

    @classmethod
    @cb.execute_in_chroot('chroot', binds=BINDS,
            environment_overrides={'HOME':'/root'},working_dir='/root/build/repo')
    def run(cls):
        config = cb.get_config()
        print(config.VERSION)

        # Install vaultenv into ./pkg
        cb.execute(['rm', '-rf', 'pkg/'])

        Path('pkg/usr/bin').mkdir(parents=True, exist_ok=True)
        Path('pkg/etc/secrets.d').mkdir(parents=True, exist_ok=True)

        cb.create_file('pkg/DEBIAN/control', dedent(f"""\
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

        stack_path = cb.execute_capture(['stack', 'path', '--local-install-root']).strip().decode()
        cb.execute(['cp', f'{stack_path}/bin/vaultenv', 'pkg/usr/bin/'])


class Package(BuildStep):
    dependencies = [BuildPackageTree, InitializeVersion]

    @classmethod
    def run(cls):
        config = cb.get_config()
        cb.build_deb('pkg', f'vaultenv-{config.VERSION}.deb')


class Ship(BuildStep):
    dependencies = [Package]

    @classmethod
    def run(cls):
        config = get_config()

        # Ship package if tagged with production tag:
        if config.VERSION and re.fullmatch('[0-9]+', config.VERSION):
            ship_deb_to_freight(f'channabuild-{config.VERSION}.deb')


class CleanArtifacts(BuildStep):
    pass


class CleanCache(BuildStep):
    pass


BUILD_GRAPH = BuildStep.__subclasses__()
CACHE_FILE = 'channabuild.cache'


@cb.cli.command(name='build')
@click.option('--ship-package/--no-ship-package', default=False, help='Ship package?')
def build_cmd(ship_package):
    """Build everything"""
    target = Ship if ship_package else Package
    cb.get_and_run_build_plan(target, BUILD_GRAPH, CACHE_FILE)


if __name__ == '__main__':
    try:
        cb.cli()
    except ChannaBuildUserError:
        cb.error('Build failed')
        sys.exit(1)
    except Exception:
        cb.exception('Build failed:')
        sys.exit(1)
