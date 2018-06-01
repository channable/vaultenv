#!/bin/bash

export VAULT_TOKEN="integration"
export VAULT_HOST="localhost"
export VAULT_PORT="8200"
export VAULT_ADDR="http://${VAULT_HOST}:${VAULT_PORT}"

# Start the Vault server we're going to use for our integration tests
vault server -dev -dev-root-token-id=${VAULT_TOKEN} &> /dev/null &

# Seed Vault and some secret files
export VAULT_SEEDS="$(mktemp)"
cat > ${VAULT_SEEDS} << EOF
testing#key
testing#otherkey
testing2#foo
testing2#bar
EOF

vault write secret/testing key=testing42 otherkey=testing8 &> /dev/null
vault write secret/testing2 foo=val1 bar=val2 &> /dev/null

prove --verbose $(find integration -type f -iname '*.sh' ! -name '_*')

# Cleanup the vault dev server
kill %%
