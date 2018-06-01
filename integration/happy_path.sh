#!/bin/bash

set -e

echo "1..3"

if [ $VAULT_TOKEN = "integration" ]; then
  echo "ok 1 - vault token set"
else
  echo "not ok 1 - vault token not set"
fi

stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} -- \
  /bin/echo "ok 2 - happy path"

stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS_V2} -- \
  /bin/echo "ok 3 - happy path v2"
