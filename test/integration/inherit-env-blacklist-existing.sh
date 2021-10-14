#!/bin/bash

set -euo pipefail

echo "1..6"

TEMP_RUN_DIRECTORY=$(mktemp -d)
vault_token_present() {
    stack exec --no-nix-pure --cwd $TEMP_RUN_DIRECTORY -- vaultenv \
        --no-connect-tls \
        --host ${VAULT_HOST} \
        --port ${VAULT_PORT} \
        --secrets-file ${VAULT_SEEDS} \
        "$@" \
        /usr/bin/env | grep --count "VAULT_TOKEN=integration"
}

## Test blacklisting a variable from the outer environment (VAULT_TOKEN).
# Test that the variable is present when no blacklist is used.
if [[ $(vault_token_present) -eq 1 ]]; then
    echo "ok 1 - VAULT_TOKEN present when not blacklisted"
else
    echo "not ok 1 - VAULT_TOKEN absent while not blacklisted"
fi

# Test that blacklisting secrets via the `--inherit-env-blacklist` argument works
if [[ $(vault_token_present --inherit-env-blacklist VAULT_TOKEN) -eq 0 ]]; then
    echo "ok 2 - VAULT_TOKEN absent when blacklisted via argument"
else
    echo "not ok 2 - VAULT_TOKEN present while blacklisted via argument"
fi

# # Test that blacklisting via the environment variable works.
export VAULTENV_INHERIT_ENV_BLACKLIST=VAULT_TOKEN
if [[ $(vault_token_present) -eq 0 ]]; then
    echo "ok 3 - VAULT_TOKEN absent when blacklisted via environment"
else
    echo "not ok 3 - VAULT_TOKEN present while blacklisted via environment"
fi
unset VAULTENV_INHERIT_ENV_BLACKLIST

# Test that blacklisting via the .env config file works.
echo "VAULTENV_INHERIT_ENV_BLACKLIST=VAULT_TOKEN" > "${TEMP_RUN_DIRECTORY}/.env"
if [[ $(vault_token_present) -eq 0 ]]; then
    echo "ok 4 - VAULT_TOKEN absent when blacklisted via config file"
else
    echo "not ok 4 - VAULT_TOKEN present while blacklisted via config file"
fi

# Test that the blacklist from config can be disabled with the CLI
vault_token_present_with_disabled_blacklist() {
    stack exec --no-nix-pure --cwd $TEMP_RUN_DIRECTORY -- vaultenv \
        --no-connect-tls \
        --host ${VAULT_HOST} \
        --port ${VAULT_PORT} \
        --secrets-file ${VAULT_SEEDS} \
        --inherit-env-blacklist '' \
        "$@" \
        /usr/bin/env | grep --count "VAULT_TOKEN=integration"
}

if [[ $(vault_token_present_with_disabled_blacklist) -eq 1 ]]; then
    echo "ok 5 - VAULT_TOKEN present when blacklisted via config but blacklist disabled with CLI"
else
    echo "not ok 5 - VAULT_TOKEN absent when blacklisted via config but blacklist disabled with CLI"
fi

# Test that the blacklist from config can be disabled with the environment
export VAULTENV_INHERIT_ENV_BLACKLIST=
if [[ $(vault_token_present) -eq 1 ]]; then
    echo "ok 6 - VAULT_TOKEN present when blacklisted via config but blacklist disabled with environment"
else
    echo "not ok 6 - VAULT_TOKEN absent when blacklisted via config but blacklist disabled with environment"
fi
unset VAULTENV_INHERIT_ENV_BLACKLIST

rm "${TEMP_RUN_DIRECTORY}/.env"
