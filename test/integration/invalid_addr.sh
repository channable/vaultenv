#!/bin/bash

echo "1..5"

stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --addr http://${VAULT_HOST}:${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} -- \
  /bin/echo "ok 1 - vaultenv correctly recognized the addr"

if [ $? -ne 0 ]; then
  echo "not ok 1 - vaultenv didn't complete"
fi

stack exec -- vaultenv \
  --connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  --addr http://${VAULT_HOST}:${VAULT_PORT} \
  /bin/echo "not ok 2 - this should never print"

echo "ok 2 - vault rejected the scheme"

stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  --addr https://${VAULT_HOST}:${VAULT_PORT} \
  /bin/echo "not ok 3 - this should never print"

echo "ok 3 - vault rejected the scheme"

stack exec -- vaultenv \
  --no-connect-tls \
  --host invalidhost \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  --addr http://${VAULT_HOST}:${VAULT_PORT} \
  /bin/echo "not ok 4 - this should never print"

echo "ok 4 - vault rejected the host"

stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port 4321 \
  --secrets-file ${VAULT_SEEDS} \
  --addr http://${VAULT_HOST}:${VAULT_PORT} \
  /bin/echo "not ok 5 - this should never print"

echo "ok 5 - vault rejected the port"
