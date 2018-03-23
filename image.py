#!/usr/bin/env python3.6
import logging
import os
import sys

from pathlib import Path

from channabuild.chroot import execute_in_chroot
from channabuild.exceptions import ChannaBuildUserError
from channabuild.image import download_image_from_lxc
from channabuild.logging import get_logger
from channabuild.main import initialize
from channabuild.namespaces import execute_in_new_namespace
from channabuild.subprocess import execute


def clear_environment():
    """ Temporary fix until https://github.com/channable/channabuild/issues/15 is resolved. """
    for key in list(os.environ.keys()):
        if key.startswith('LC_'):
            del os.environ[key]
        elif key == 'LANGUAGE':
            del os.environ[key]
        elif key == 'LANG':
            del os.environ[key]

    os.environ['LANG'] = 'en_US.utf-8'


def download_image():
    download_image_from_lxc(filename='rootfs.tar.xz')


@execute_in_new_namespace
def extract_image() -> None:
    execute(['rm', '-rf', 'chroot/'])
    execute(['mkdir', 'chroot/'])
    # Copied from /usr/share/lxc/templates/lxc-download
    execute(['tar', '--checkpoint=10000', '--anchored', '--exclude=./dev/*',
             '-xpJf', 'rootfs.tar.xz', '-C', 'chroot/'])


@execute_in_chroot('chroot')
def setup_image() -> None:
    clear_environment()
    execute(['apt-get', '-qqy', 'update'])
    execute(['apt-get', '-qqy', 'upgrade'])

    execute(['apt-get', '-qqy', 'install', 'curl', 'wget', 'unzip'])

    execute(['bash', '-c', 'curl -sSL https://get.haskellstack.org/ | sh'])

    Path('/tmp/vault').mkdir()
    os.chdir('/tmp/vault')
    vault_url = 'https://releases.hashicorp.com/vault/0.9.6/vault_0.9.6_linux_amd64.zip'
    execute(['wget', vault_url])
    execute(['unzip', 'vault_0.9.6_linux_amd64.zip'])
    execute(['cp', 'vault', '/usr/local/bin/vault'])
    os.chdir('/')
    execute(['rm', '-rf', '/tmp/vault'])


@execute_in_new_namespace
def package_image() -> None:
    execute(['tar', '-cf', 'chroot.tar',
             '-C', 'chroot', '.',
             '--numeric-owner', '--selinux', '--checkpoint=1000'])

    # TODO: Use `xz` compression when we move the images to a repository.
    # execute(['tar', '-Jcf', 'chroot.tar.xz',
    #          '-C', 'chroot', '.',
    #          '--numeric-owner', '--selinux', '--checkpoint=1000'])


def cleanup() -> None:
    # We need to clear the chroot from within a namespace due to permission errors.
    execute_in_new_namespace(lambda: execute(['rm', '-rf', 'chroot']))()

    execute(['rm', 'rootfs.tar.xz'])


def build() -> None:
    download_image()
    extract_image()
    setup_image()
    package_image()
    cleanup()


if __name__ == '__main__':
    initialize(log_level=logging.DEBUG)
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
