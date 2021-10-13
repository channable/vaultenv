#!/usr/bin/env python3

"""
Verify Vault Kubernetes authentication.

WARNING
This script expects a (local) Kubernetes cluster to be running (tested with
minikube) and it runs arbitrary `kubectl` commands. Do not run this with kubectl
pointed at a production cluster!

Before you can run this test, you need to build a container with Vaultenv and
put it in Minikube's registry, as follows:

    nix build --file kubernetes_auth_container.nix --out-link container.tar.gz
    minikube image load container.tar.gz
"""

import os
import signal
import base64
import subprocess
import textwrap
import time

from typing import Any, Set
from pathlib import Path

import tap


def kubectl(*args: str, **kwargs: Any) -> subprocess.CompletedProcess:
    return subprocess.run(['kubectl', *args], **kwargs)


def vault(*args: str, **kwargs: Any) -> subprocess.CompletedProcess:
    return subprocess.run(['vault', *args], **kwargs)


def main() -> None:
    """
    """
    tap.plan(4)

    tap.diagnose("Setting up Kubernetes service account ...")
    kubectl(
        '--namespace', 'default', 'create', 'serviceaccount', 'test-vault-auth',
        check=False,
    )
    kubectl(
        'apply', '--filename', '-',
        input=textwrap.dedent(
            """\
            ---
            apiVersion: rbac.authorization.k8s.io/v1
            kind: ClusterRoleBinding
            metadata:
              name: role-tokenreview-binding
              namespace: default
            roleRef:
              apiGroup: rbac.authorization.k8s.io
              kind: ClusterRole
              name: system:auth-delegator
            subjects:
            - kind: ServiceAccount
              name: test-vault-auth
              namespace: default
            """),
        encoding='utf-8',
        check=True,
    )

    tap.diagnose("Starting Vault server ...")
    vault_server = run_vault_server()

    # Ensure that all of the `vault` calls below connect over http to the dev
    # server.
    os.environ["VAULT_ADDR"] = "http://127.0.0.1:8200"

    tap.diagnose("Adding policy and test value to Vault ...")
    vault(
        "policy", "write", "testapp-kv-ro", "-",
        input=textwrap.dedent(
            """\
            path "secret/data/testapp/*" {
                capabilities = ["read", "list"]
            }
            path "secret/metadata/testapp/*" {
                capabilities = ["list"]
            }
            """
        ),
        encoding="utf-8",
        check=True,
    )
    vault(
        "kv",
        "put",
        "secret/testapp/config",
        "username=vaultenvtestuser",
        "password=hunter2",
        check=True,
    )

    # Get the name of the service account and export it where Vault can find it.
    vault_service_account_name = kubectl(
        "get", "sa", "test-vault-auth", "--output", "jsonpath={.secrets[*]['name']}",
        capture_output=True,
        check=True,
        encoding='utf-8',
    ).stdout
    os.environ["VAULT_SA_NAME"] = vault_service_account_name
    if vault_service_account_name.startswith('test-vault-auth'):
        tap.ok(f"Vault service account created: {vault_service_account_name}")
    else:
        tap.not_ok("No valid Vault service account.")

    service_account_jwt_b64 = kubectl(
        "get", "secret", vault_service_account_name, "--output", "go-template={{ .data.token }}",
        capture_output=True,
        check=True,
        encoding='utf-8',
    ).stdout
    service_account_jwt = base64.b64decode(service_account_jwt_b64).decode('utf-8')

    kubernetes_ca_cert_b64 = kubectl(
        "config", "view", "--raw", "--minify", "--flatten", "--output", "jsonpath={.clusters[].cluster.certificate-authority-data}",
        capture_output=True,
        check=True,
        encoding='utf-8',
    ).stdout
    kubernetes_ca_cert = base64.b64decode(kubernetes_ca_cert_b64).decode('utf-8')

    kubernetes_host = kubectl(
        "config", "view", "--raw", "--minify", "--flatten", "--output",
        "jsonpath={.clusters[].cluster.server}'",
        capture_output=True,
        check=True,
        encoding='utf-8',
    ).stdout

    vault("auth", "enable", "kubernetes", check=True)
    vault(
        "write",
        "auth/kubernetes/config",
        f"token_reviewer_jwt={service_account_jwt}",
        f"kubernetes_host={kubernetes_host}",
        f"kubernetes_ca_cert={kubernetes_ca_cert}",
        "issuer=https://kubernetes.default.svc.cluster.local",
        check=True,
    )

    vault(
        "write",
        "auth/kubernetes/role/vaultenv-test-role",
        "bound_service_account_names=test-vault-auth",
        "bound_service_account_namespaces=default",
        "policies=testapp-kv-ro",
        "ttl=24h",
        check=True,
    )

    # Delete the pod, in case it was left over from a previous test run.
    kubectl("delete", "pod", "vaultenv-test-pod", check=False)

    kubectl(
        'apply', '--filename', '-',
        input=textwrap.dedent(
            """
            apiVersion: v1
            kind: Pod

            metadata:
              name: vaultenv-test-pod

            spec:
              containers:
              - name: vaultenv-test
                image: vaultenv/vaultenv:test
                # Image should already be loaded into the registry before we
                # start the test. It should be locally built, so we can't pull
                # it from anywhere anyway.
                imagePullPolicy: Never
                args: [
                  "/bin/vaultenv",
                  "--log-level", "info",
                  # Inside the container, Minikube adds an entry to /etc/hosts
                  # that resolves to the IP of the host's network interface.
                  "--addr", "http://host.minikube.internal:8200",
                  "--kubernetes-role", "vaultenv-test-role",
                  "--secrets-file", "/lib/test.secrets",
                  "--",
                  "/bin/env"
                ]

              # Allow host access, because the Vault server runs on the host network.
              hostNetwork: true
              dnsPolicy: Default
              serviceAccount: test-vault-auth
            """),
        encoding='utf-8',
        check=True,
    )

    print("awaiting")
    import sys
    sys.stdin.readline()

    # We need to kill and restart the server or the kernel will accept the TCP
    # connections while the Vault server is paused.
    vault_server.terminate()
    wait_seconds = 5
    vault_server.wait(timeout=wait_seconds)


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


def run_minikube() -> subprocess.Popen:
    """
    Run a local Kubernetes cluster with Minikube.
    """
    minikube = subprocess.Popen(
        [
            "minikube",
            "start",
            # The KVM2 driver is the only one that I have been able to get
            # running without root privileges. On Arch it requires the following
            # packages:
            # * libvirt
            # * qemu
            # * dnsmasq
            # * ebtables
            # Also, your user needs to be a member of the libvirt group, and the
            # libvirtd systemd service must be running.
            # Alternative backends that I could not get to work.
            # * Docker (when your user is not part of the "docker" group, which
            #   it should not be, because it is effectively root.
            # * Podman
            # * None
            "--driver",
            "kvm2",
        ],
        stdin=subprocess.DEVNULL,
        #stdout=subprocess.DEVNULL,
        #stderr=subprocess.DEVNULL,
    )


def run_vault_server() -> subprocess.Popen:
    """
    Run the Vault dev server with Kubernetes auth enabled.

    Return a handle to the Vault server process.
    """
    vault_server = subprocess.Popen(
        [
            "vault",
            "server",
            "-dev",
            "-dev-root-token-id=integration",
            # Listen on 0.0.0.0 so the birdged Minikube network can also reach
            # the server.
            "-dev-listen-address", "0.0.0.0:8200",
        ],
        stdin=subprocess.DEVNULL,
        # TODO: Enable after debug complete.
        #stdout=subprocess.DEVNULL,
        #stderr=subprocess.DEVNULL,
    )
    sleep_seconds = 1
    time.sleep(sleep_seconds)

    return vault_server


if __name__ == "__main__":
    main()
