#!/bin/bash

echo "1..4"

export TESTING_KEY=testing666
stack exec --no-nix-pure -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} -- \
  /bin/echo "not ok 1 - vaultenv didn't error with duplicate env var"

echo "ok 1 - vaultenv didn't complete"

stack exec --no-nix-pure -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --duplicate-variable-behavior error \
  --secrets-file ${VAULT_SEEDS} -- \
  /bin/echo "not ok 2 - vaultenv didn't error with duplicate env var"

echo "ok 2 - vaultenv didn't complete"


stack exec --no-nix-pure -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --duplicate-variable-behavior keep \
  --secrets-file ${VAULT_SEEDS} -- \
  /usr/bin/env \
  | grep "TESTING_KEY=testing666"

if [ $? -eq 0 ]; then
  echo "ok 3 - vaultenv passed through test var"
else
  echo "not ok 3 - vaultenv didn´t pass through test var"
fi

stack exec --no-nix-pure -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --duplicate-variable-behavior overwrite \
  --secrets-file ${VAULT_SEEDS} -- \
  /usr/bin/env \
  | grep "TESTING_KEY=testing42"

if [ $? -eq 0 ]; then
  echo "ok 4 - vaultenv overwrote test var"
else
  echo "not ok 4 - vaultenv didn´t overwrite test var"
fi

unset TESTING_KEY
