#!/bin/bash

echo "1..5"

#test if the addr is accepted when the same as the host, port and scheme (no tls)
stack exec --no-nix-pure -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --addr http://${VAULT_HOST}:${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} -- \
  /bin/echo "ok 1 - vaultenv correctly recognized the addr"

if [ $? -ne 0 ]; then
  echo "not ok 1 - vaultenv didn't complete"
fi

#test if the addr is rejected when the same as the host, port, but different form the scheme (use tls)
stack exec --no-nix-pure -- vaultenv \
  --connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  --addr http://${VAULT_HOST}:${VAULT_PORT} \
  /bin/echo "not ok 2 - this should never print"

echo "ok 2 - vault rejected the scheme"

#test if rejected with a different scheme (https vs no-tls)
stack exec --no-nix-pure -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  --addr https://${VAULT_HOST}:${VAULT_PORT} \
  /bin/echo "not ok 3 - this should never print"

echo "ok 3 - vault rejected the scheme"

#test if rejected with an invalid host
stack exec --no-nix-pure -- vaultenv \
  --no-connect-tls \
  --host invalidhost \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  --addr http://${VAULT_HOST}:${VAULT_PORT} \
  /bin/echo "not ok 4 - this should never print"

echo "ok 4 - vault rejected the host"

#test if rejected with an invalid port
stack exec --no-nix-pure -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port 4321 \
  --secrets-file ${VAULT_SEEDS} \
  --addr http://${VAULT_HOST}:${VAULT_PORT} \
  /bin/echo "not ok 5 - this should never print"

echo "ok 5 - vault rejected the port"
