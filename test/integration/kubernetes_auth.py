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
import json

from typing import Any, Set
from pathlib import Path
from urllib.request import urlopen

import tap


def kubectl(*args: str, **kwargs: Any) -> subprocess.CompletedProcess:
    return subprocess.run(['kubectl', *args], **kwargs)


def vault(*args: str, **kwargs: Any) -> subprocess.CompletedProcess:
    return subprocess.run(['vault', *args], **kwargs)


def tap_starts_with_success(status: str, haystack: str) -> None:
    if haystack.startswith('Success!'):
        tap.ok(status)
    else:
        tap.not_ok(status)


def main() -> None:
    """
    Start a Vault server, and enable Kubernetes authentication on it. Then start
    a pod with the test image that runs Vaultenv to retrieve some secrets and
    then runs "env". Then check the logs for that pod to see if we really
    retrieved the secrets.
    """
    tap.plan(7)

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
    output = vault(
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
        stdout=subprocess.PIPE,
        check=True,
    ).stdout
    tap_starts_with_success('Created testapp-kv-ro policy', output)

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
        "jsonpath={.clusters[].cluster.server}",
        capture_output=True,
        check=True,
        encoding='utf-8',
    ).stdout

    # We need to tell Vault what the issuer of the JWT tokens is. But to figure
    # out what the issuer is, we first need to enable issuer discovery. Then we
    # can query the issuers through a "kubectl proxy" reverse proxy.
    kubectl(
        "create", "clusterrolebinding", "oidc-reviewer",
        "--clusterrole=system:service-account-issuer-discovery",
        "--group=system:unauthenticated",
    )
    kubectl_proxy = subprocess.Popen(
        ["kubectl", "proxy", "--port", "8001"],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
    )
    sleep_seconds = 0.1
    time.sleep(sleep_seconds)
    openid_config_bytes = urlopen("http://127.0.0.1:8001/.well-known/openid-configuration")
    openid_config_json = json.load(openid_config_bytes)
    issuer = openid_config_json['issuer']
    kubectl_proxy.terminate()

    output = vault("auth", "enable", "kubernetes", check=True, stdout=subprocess.PIPE, encoding='utf-8').stdout
    tap_starts_with_success('Enabled Kubernetes auth in Vault', output)

    output = vault(
        "write",
        "auth/kubernetes/config",
        f"token_reviewer_jwt={service_account_jwt}",
        f"kubernetes_host={kubernetes_host}",
        f"kubernetes_ca_cert={kubernetes_ca_cert}",
        f"issuer={issuer}",
        check=True,
        stdout=subprocess.PIPE,
        encoding='utf-8',
    ).stdout
    tap_starts_with_success('Set Kubernetes auth config', output)

    output = vault(
        "write",
        "auth/kubernetes/role/vaultenv-test-role",
        "bound_service_account_names=test-vault-auth",
        "bound_service_account_namespaces=default",
        "policies=testapp-kv-ro",
        "ttl=24h",
        check=True,
        stdout=subprocess.PIPE,
        encoding='utf-8',
    ).stdout
    tap_starts_with_success('Bind service account role', output)

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

    # Wait up to 10 seconds for the pod to become ready.
    print("# Waiting for pod to become ready ...")
    for _ in range(20):
        ready = kubectl(
            "get", "pod", "vaultenv-test-pod", "--output", "jsonpath={.status.containerStatuses[].ready}",
            stdout=subprocess.PIPE,
            check=True,
        ).stdout
        if ready == b'true':
            break
        else:
            sleep_seconds = 0.5
            time.sleep(sleep_seconds)

    # Then wait up to another 10 seconds for all logs to come in.
    print("# Waiting for pod logs ...")
    logs = []
    for _ in range(20):
        logs = kubectl(
            "logs", "vaultenv-test-pod",
            stdout=subprocess.PIPE,
            check=True,
            encoding="utf-8",
        ).stdout.splitlines()

        # Vaultenv in info mode prints 16 lines of config at the start. The next
        # line will be an error or a result, so only after that line, we can
        # continue.
        if len(logs) > 16:
            break
        else:
            sleep_seconds = 0.5
            time.sleep(sleep_seconds)

    expected_lines = [
        "DATA_TESTAPP_CONFIG_USERNAME=vaultenvtestuser",
        "DATA_TESTAPP_CONFIG_PASSWORD=hunter2",
    ]
    for line in expected_lines:
        if line in logs:
            tap.ok(f'{line} was retrieved')
        else:
            tap.not_ok(f'{line} was not retrieved')

    # Clean up the pod now that the test is done.
    kubectl("delete", "pod", "vaultenv-test-pod", check=False)

    # We need to kill and restart the server or the kernel will accept the TCP
    # connections while the Vault server is paused.
    vault_server.terminate()
    wait_seconds = 5
    vault_server.wait(timeout=wait_seconds)


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
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    sleep_seconds = 1
    time.sleep(sleep_seconds)

    return vault_server


if __name__ == "__main__":
    main()
