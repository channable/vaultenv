#!/bin/bash

set -e

echo "1..2"

export TEST_VAR="willbeset"
stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  /usr/bin/env \
  | grep "TEST_VAR"

echo "ok 1 - vaultenv passed through test var"

stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  --no-inherit-env \
  /usr/bin/env \
  | grep -v "TEST_VAR"

echo "ok 2 - vaultenv didn't pass through test var"
