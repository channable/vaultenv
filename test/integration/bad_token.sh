#!/bin/bash

echo "1..1"

export VAULTENV_RETRY_ATTEMPTS=1
stack exec -- vaultenv \
  --no-connect-tls \
  --token thiswillfail \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  /bin/echo "not ok 1 - this should never print"

echo "ok 1 - vault didn't fetch any secrets"
