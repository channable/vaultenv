#!/bin/bash

# Integration testing Vault token
VAULT_HOST="127.0.0.1"
VAULT_PORT="8200"

export VAULT_TOKEN="somethingsecret"
export VAULT_ADDR="http://127.0.0.1:8200"

echo "Building latest changes"
stack build

# Run vault while we run the tests
echo "Starting vault..."
vault server -dev -dev-root-token-id=${VAULT_TOKEN} &> /dev/null &

# Write a secrets file to use:
cat > integration.secrets << EOF
testing#key
testing#otherkey
EOF

cat > not-found.secrets << EOF
notinvault#key
EOF

cat > bad-key.secrets << EOF
testing#notinvault
EOF

sleep 1

echo "Writing test secrets..."
vault write secret/testing key=testing42 otherkey=testing8

echo "[TEST] Happy path"
stack exec -- vaultenv \
  --token ${VAULT_TOKEN} \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ./integration.secrets \
  /usr/bin/env \
  | grep "TESTING_KEY"

echo "[TEST] Unknown secret, passes if vaultenv errors with secret not found"
stack exec -- vaultenv \
  --token ${VAULT_TOKEN} \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ./not-found.secrets \
  echo "[FAIL] This should never print"

echo "[TEST] Unknown secret key, passes if vaultenv errors with key not found in secret"
stack exec -- vaultenv \
  --token ${VAULT_TOKEN} \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ./bad-key.secrets \
  echo "[FAIL] This should never print"

echo "[TEST] Bad token, vaultenv should fail with forbidden"
stack exec -- vaultenv \
  --token notthevaulttoken \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ./integration.secrets \
  echo "[FAIL] This should never print"

# Kill the vault dev server so we can test the "unavailable" error messages
# that we get when vault is e.g. sealed. We now start up vault in normal mode,
# so it is sealed by default
kill %%

# Write a small default configuration for Vault, so we can start it outside of
# dev mode.
cat > integration-config.hcl << EOF
storage "file" {
  path = "/tmp/vaultenv-integration"
}
listener "tcp" {
  address = "127.0.0.1:8200"
  tls_disable = "true"
}
EOF

vault server -config integration-config.hcl &

echo "[TEST] Vault unavailable. Should error with a corresponding message"
stack exec -- vaultenv \
  --token notthevaulttoken \
  --host ${VAULT_HOST} \
  --port ${VAULT_PORT} \
  --secrets-file ./integration.secrets \
  echo "[FAIL] This should never print"


# Cleanup: kill background vault process and remove integration test secrets
# file.
kill %%
echo "Cleaning up..."
rm integration.secrets bad-key.secrets not-found.secrets integration-config.hcl
