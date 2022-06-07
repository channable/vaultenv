#!/bin/bash

export VAULT_TOKEN="integration"
export VAULT_HOST="127.0.0.1"
export VAULT_PORT="8200"
export VAULT_ADDR="http://${VAULT_HOST}:${VAULT_PORT}"

set -e

# Check the vault command exists:
if ! which vault > /dev/null; then
  echo "vault: command not found"
  exit 1
fi

if [[ $(vault -version) != "Vault v1"* ]]; then
  echo "Wrong vault version installed. Integration tests require Vault >= 1.0"
  echo "Get it here: https://www.vaultproject.io/downloads.html"
  exit 1
fi

# Start the Vault server we're going to use for our integration tests
vault server -dev -dev-root-token-id=${VAULT_TOKEN} &> /dev/null &

# Brief timeout hack to avoid race condition where we write secrets but
# vault hasn't booted yet.
sleep 1

# Seed Vault and some secret files
export VAULT_SEEDS="$(mktemp)"
cat > ${VAULT_SEEDS} << EOF
testing#key
testing#otherkey
testing2#foo
testing2#bar
TEST_TEST=testing#key
TEST__TEST=testing#key
_TEST__TEST=testing#key
EOF

export VAULT_SEEDS_V2="$(mktemp)"
cat > ${VAULT_SEEDS_V2} <<EOF
VERSION 2

MOUNT secret
testing#key
testing#otherkey

MOUNT secret
testing2#foo
testing2#bar
EOF

# First unmount the default `secret/` mount and remount as V1 so we can test
# with that API version first.
echo "Running tests for Key/Value mount V1"
vault secrets disable secret
vault secrets enable -path=secret -version=1 kv

vault kv put secret/testing key=testing42 otherkey=testing8 &> /dev/null
vault kv put secret/testing2 foo=val1 bar=val2 &> /dev/null

# Run all tests if we didn't get any arguments.
if [ $# -eq 0 ]; then
  echo "Running all tests"
  echo "NOTE: You might see errors in the log. That's part of what we test."
  echo "Look at the test summary report to see if there's anything wrong."
  prove $(find integration -type f -iname '*.sh' ! -name '_*')
else
  echo "Running only $1"
  $1
fi

# Upgrade the default API.
echo "Running tests for Key/Value mount V2"
vault kv enable-versioning secret

# Run all tests if we didn't get any arguments.
if [ $# -eq 0 ]; then
  echo "Running all tests"
  echo "NOTE: You might see errors in the log. That's part of what we test."
  echo "Look at the test summary report to see if there's anything wrong."
  prove $(find integration -type f -iname '*.sh' ! -name '_*')
else
  echo "Running only $1"
  $1
fi

# Cleanup the vault dev server
kill %%

# TODO: fix and enable the kubernetes tests.
# Run the kubernetes auth tests
# This tests run their own Vault
# nix build --file integration/kubernetes_auth_container.nix --out-link container.tar.gz
# minikube start
# minikube image load container.tar.gz
# prove --comments integration/kubernetes_auth.py
# minikube stop

# The retry tests work locally, but not in CI.
# This is due to the SIGSTOP signal to the vaultenv processes not coming through,
# which is probably due to implementation specifics of Semaphore CI.
if [[ ${CI:-"false"} == "false" ]];
then
  PYTHONUNBUFFERED=1 prove --comments integration/retry_tests.py
fi
