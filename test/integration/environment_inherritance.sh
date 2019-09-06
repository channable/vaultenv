#!/bin/bash

echo "1..2"

export TEST_VAR="willbeset"
stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  --inherit-env \
  /usr/bin/env \
  | grep "TEST_VAR"

if [ $? -eq 0 ]; then
  echo "ok 1 - vaultenv passed through test var"
else
  echo "not ok 1 - vaultenv didn´t pass through test var"
fi

stack exec -- vaultenv \
  --no-connect-tls \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ${VAULT_SEEDS} \
  --no-inherit-env \
  /usr/bin/env \
  | grep -v "TEST_VAR"

if [ $? -eq 0 ]; then
  echo "ok 2 - vaultenv didn't pass through test var"
else
  echo "not ok 2 - vaultenv passed through test var"
fi

