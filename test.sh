#!/bin/bash

export VAULT_TOKEN="integration"
export VAULT_HOST="localhost"
export VAULT_PORT="8200"
export VAULT_ADDR="http://${VAULT_HOST}:${VAULT_PORT}"

# Start the Vault server we're going to use for our integration tests
vault server -dev -dev-root-token-id=${VAULT_TOKEN} &> /dev/null &

# Brief timeout hack to avoid race condition where we write secrets but
# vault hasn't booted yet.
sleep 1

# Seed Vault and some secret files
export VAULT_SEEDS="$(mktemp)"
cat > ${VAULT_SEEDS} << EOF
testing#key
testing#otherkey
testing2#foo
testing2#bar
EOF

export VAULT_SEEDS_V2="$(mktemp)"
cat > ${VAULT_SEEDS_V2} <<EOF
VERSION 2

MOUNT secret
testing#key
testing#otherkey

MOUNT secret
testing2#foo
testing2#bar
EOF

vault write secret/testing key=testing42 otherkey=testing8 &> /dev/null
vault write secret/testing2 foo=val1 bar=val2 &> /dev/null

# Run all tests if we didn't get any arguments.
if [ $# -eq 0 ]; then
  echo "Running all tests"
  echo "NOTE: You might see errors in the log. That's part of what we test."
  echo "Look at the test summary report to see if there's anything wrong."
  prove $(find integration -type f -iname '*.sh' ! -name '_*')
else
  echo "Running only $1"
  $1
fi

# Cleanup the vault dev server
kill %%
