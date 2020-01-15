#!/bin/bash

set -euo pipefail

echo "1..4"

TEMP_RUN_DIRECTORY=$(mktemp -d)
testing_key_present() {
    stack exec --cwd $TEMP_RUN_DIRECTORY -- vaultenv \
        --no-connect-tls \
        --host ${VAULT_HOST} \
        --port ${VAULT_PORT} \
        --secrets-file ${VAULT_SEEDS} \
        "$@" \
        /usr/bin/env | grep --count "TESTING_KEY=testing42"
}

## Test blacklisting a variable that would normally be set by Vaultenv itself (TESTING_KEY).
# Test that the variable is present when no blacklist is used.
if [[ $(testing_key_present) -eq 1 ]]; then
    echo "ok 1 - TESTING_KEY present when not blacklisted"
else
    echo "not ok 1 - TESTING_KEY absent while not blacklisted"
fi

# Test that blacklisting secrets via the `--inherit-env-blacklist` argument works
if [[ $(testing_key_present --inherit-env-blacklist TESTING_KEY) -eq 0 ]]; then
    echo "ok 2 - TESTING_KEY absent when blacklisted via argument"
else
    echo "not ok 2 - TESTING_KEY present while blacklisted via argument"
fi

# # Test that blacklisting via the environment variable works.
export VAULTENV_INHERIT_ENV_BLACKLIST=TESTING_KEY
if [[ $(testing_key_present) -eq 0 ]]; then
    echo "ok 3 - TESTING_KEY absent when blacklisted via environment"
else
    echo "not ok 3 - TESTING_KEY present while blacklisted via environment"
fi
unset VAULTENV_INHERIT_ENV_BLACKLIST

# Test that blacklisting via the .env config file works.
echo "VAULTENV_INHERIT_ENV_BLACKLIST=TESTING_KEY" > "${TEMP_RUN_DIRECTORY}/.env"
if [[ $(testing_key_present) -eq 0 ]]; then
    echo "ok 4 - TESTING_KEY absent when blacklisted via config file"
else
    echo "not ok 4 - TESTING_KEY present while blacklisted via config file"
fi
rm "${TEMP_RUN_DIRECTORY}/.env"
