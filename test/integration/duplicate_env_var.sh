#!/bin/bash

echo "1..1"

export TESTING_KEY=testing666
stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} -- \
  /bin/echo "not ok 1 - vaultenv didn't error with duplicate env var"

echo "ok 1 - vaultenv didn't complete"
unset TESTING_KEY
