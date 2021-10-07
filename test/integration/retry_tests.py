#!/usr/bin/python3.7
"""
Tests whether the retry mechanism in Vaultenv works correctly by starting Vaultenv
before the Vault server is ready. Runs after the rest of the tests in
integration_test.sh, and requires that there is no Vault server running.

Requires these environment variables (should all be set by integration_test.sh):
  - VAULT_ADDR, VAULT_HOST, VAULT_PORT, VAULT_TOKEN: Credentials to connect to development Vault
  - VAULT_SEEDS, VAULT_SEEDS_V2: Paths to a V1 and V2 secrets file
"""

import os
import signal
import subprocess
from pathlib import Path
from time import sleep
from typing import Set

import tap


def main() -> None:
    """
    Test whether the retry behaviour in Vaultenv works as expected.

    We do this by:
      - Starting two Vaultenv processes (one for each version of the secrets file
        format) and letting them run for 5 seconds (during which they should retry)
      - Suspending the Vaultenv processes with the SIGSTOP signal
      - Setting up a Vault server process with the appropriate secrets
      - Resuming the Vaultenv processes with the SIGCONT signal
      - Checking whether the Vaultenv processes exited with code 0 and got the right
        secrets (by having them run /usr/bin/env)
      - Killing the Vault server

    We do this twice, once for version 1 of Vault's KV secret store API and once for
    version 2 of the API.

    The SIGSTOP/SIGCONT signals are necessary to prevent the Vaultenv processes from
    contacting the Vault server while it is up, but does not yet contain the right
    secrets (which causes a non-retryable error by design).
    """
    v1_secrets_file = Path(os.environ["VAULT_SEEDS"])
    v2_secrets_file = Path(os.environ["VAULT_SEEDS_V2"])

    tap.plan(4)

    tap.diagnose("Starting Vaultenv processes")
    v1_handle = run_vaultenv(v1_secrets_file)
    v2_handle = run_vaultenv(v2_secrets_file)

    sleep(5)
    tap.diagnose("Pausing Vaultenv processes")
    v1_handle.send_signal(signal.SIGSTOP)
    v2_handle.send_signal(signal.SIGSTOP)

    tap.diagnose("Starting Vault server")
    vault_server = run_vault_server()
    sleep(5)

    tap.diagnose("Resuming Vaultenv processes")
    v1_handle.send_signal(signal.SIGCONT)
    v2_handle.send_signal(signal.SIGCONT)

    check_vaultenv_result(v1_handle, secrets_version=1, api_version=1)
    check_vaultenv_result(v2_handle, secrets_version=2, api_version=1)

    # We need to kill and restart the server or the kernel will accept the TCP
    # connections while the Vault server is paused.
    vault_server.terminate()
    vault_server.wait(timeout=5)

    tap.diagnose("Starting Vaultenv processes")
    v1_handle = run_vaultenv(v1_secrets_file)
    v2_handle = run_vaultenv(v2_secrets_file)
    sleep(5)

    tap.diagnose("Pausing Vaultenv processes")
    v1_handle.send_signal(signal.SIGSTOP)
    v2_handle.send_signal(signal.SIGSTOP)

    tap.diagnose("Starting Vault server")
    vault_server = run_vault_server()
    sleep(5)

    tap.diagnose("Resuming Vaultenv processes")
    v1_handle.send_signal(signal.SIGCONT)
    v2_handle.send_signal(signal.SIGCONT)

    check_vaultenv_result(v1_handle, secrets_version=1, api_version=2)
    check_vaultenv_result(v2_handle, secrets_version=2, api_version=2)

    tap.diagnose("Killing Vault server")
    vault_server.terminate()
    vault_server.wait(timeout=10)

def run_vaultenv(secrets_file: Path) -> subprocess.Popen:
    """
    Run Vaultenv with the secrets file from the given path.

    Return a subprocess.Popen-handle to the running Vaultenv process.
    """
    return subprocess.Popen(
        [
            "stack",
            "run",
            "--no-nix-pure",
            "--",
            "vaultenv",
            "--no-connect-tls",
            "--host",
            os.environ["VAULT_HOST"],
            "--port",
            os.environ["VAULT_PORT"],
            "--secrets-file",
            str(secrets_file),
            "/usr/bin/env",
        ],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE,
        stderr=None,  # Inherit calling process's stderr
        text=True,
    )


def check_vaultenv_result(
    process_handle: subprocess.Popen, secrets_version: int, api_version: int
) -> None:
    """
    Check the return code and output of the Vaultenv process under the given
    process_handle.

    Emit an appropriate TAP message for the result of the test.
    """

    description = f"v{secrets_version} secrets/v{api_version} API"

    return_code = process_handle.wait()
    if return_code != 0:
        tap.not_ok(f"{description} returned with code {return_code}")
        return

    expected_env: Set[str]
    if secrets_version == 1:
        expected_env = {
            "TESTING_KEY=testing42",
            "TESTING_OTHERKEY=testing8",
            "TESTING2_FOO=val1",
            "TESTING2_BAR=val2",
            "TEST_TEST=testing42",
            "TEST__TEST=testing42",
            "_TEST__TEST=testing42",
        }
    elif secrets_version == 2:
        expected_env = {
            "SECRET_TESTING_KEY=testing42",
            "SECRET_TESTING_OTHERKEY=testing8",
            "SECRET_TESTING2_FOO=val1",
            "SECRET_TESTING2_BAR=val2",
        }

    actual_env = set(line.strip() for line in process_handle.stdout.readlines())

    if not expected_env <= actual_env:
        missing = ", ".join(expected_env - actual_env)
        tap.not_ok(f"{description} missing vars {missing}")
        return

    tap.ok(description)


def run_vault_server() -> subprocess.Popen:
    """
    Run the Vault dev server and seed it with the same set of secrets as in
    test_integration.sh.

    Return a handle to the Vault server process.
    """
    env = {
        "VAULT_TOKEN": "integration",
        "VAULT_HOST": "localhost",
        "VAULT_PORT": "8200",
        "VAULT_ADDR": "http://localhost:8200",
    }

    vault_server = subprocess.Popen(
        ["vault", "server", "-dev", "-dev-root-token-id=integration"],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    sleep(1)  # As in integration_test.sh

    subprocess.run(["vault", "secrets", "disable", "secret"], check=True, env=env)
    subprocess.run(
        ["vault", "secrets", "enable", "-path=secret", "-version=1", "kv"],
        check=True,
        env=env,
    )
    subprocess.run(
        ["vault", "kv", "put", "secret/testing", "key=testing42", "otherkey=testing8"],
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        env=env,
    )
    subprocess.run(
        ["vault", "kv", "put", "secret/testing2", "foo=val1", "bar=val2"],
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return vault_server


if __name__ == "__main__":
    main()
