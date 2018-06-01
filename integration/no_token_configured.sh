#!/bin/bash

echo "1..1"

unset VAULT_TOKEN
stack exec -- vaultenv \
           --no-connect-tls \
           --host ${VAULT_HOST} \
           --port ${VAULT_PORT} \
           --secrets-file ${VAULT_SEEDS} \
           /bin/echo "not ok 1 - completed, but no token is configured"

echo "ok 1 - vaultenv errors when token isn't configured"
