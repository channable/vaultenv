#!/bin/bash

set -euo pipefail

echo "1..4"

testing_keys_present() {
    stack exec --no-nix-pure -- vaultenv \
        --no-connect-tls \
        --host ${VAULT_HOST} \
        --port ${VAULT_PORT} \
        --secrets-file ${VAULT_SEEDS} \
        "$@" \
        /usr/bin/env | grep --count -e "TESTING_KEY=testing42" -e "VAULT_TOKEN=integration"
}

# Test that the variables are present when no blacklist is used.
if [[ $(testing_keys_present) -eq 2 ]]; then
    echo "ok 1 - keys present when not blacklisted"
else
    echo "not ok 1 - keys absent while not blacklisted"
fi

# Test that blacklisting secrets via the `--inherit-env-blacklist` argument works
if [[ $(testing_keys_present --inherit-env-blacklist VAULT_TOKEN,TESTING_KEY) -eq 0 ]]; then
    echo "ok 2 - keys absent when blacklisted via argument"
else
    echo "not ok 2 - keys present while blacklisted via argument"
fi

# Test that blacklisting via the environment variable works.
export VAULTENV_INHERIT_ENV_BLACKLIST=VAULT_TOKEN,TESTING_KEY
if [[ $(testing_keys_present) -eq 0 ]]; then
    echo "ok 3 - keys absent when blacklisted via environment"
else
    echo "not ok 3 - keys present while blacklisted via environment"
fi
unset VAULTENV_INHERIT_ENV_BLACKLIST

# Test that blacklisting via the .env config file works.
echo "VAULTENV_INHERIT_ENV_BLACKLIST=VAULT_TOKEN,TESTING_KEY" > .env
if [[ $(testing_keys_present) -eq 0 ]]; then
    echo "ok 4 - keys absent when blacklisted via config file"
else
    echo "not ok 4 - keys present while blacklisted via config file"
fi
rm .env
