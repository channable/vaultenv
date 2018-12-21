#!/bin/bash

set -eufo pipefail

echo "1..6"

if [ $VAULT_TOKEN = "integration" ]; then
  echo "ok 1 - vault token set"
else
  echo "not ok 1 - vault token not set"
fi

test_env_contents() {
  # Arguments:
  #
  #  $1 - Test number
  #  $2 - Env var to check
  #  $3 - Contents of (first line of) env var
  #  $4 - Secrets file path

  OUTPUT=$(stack exec -- vaultenv \
    --no-connect-tls \
    --host ${VAULT_HOST} \
    --port ${VAULT_PORT} \
    --secrets-file $4 -- \
    /usr/bin/env)

  GREPPED=`grep "^$2" <<< $OUTPUT`
  grep $3 <<< $GREPPED

  echo "ok $1 - happy path"
}

test_env_contents 2 "TESTING_KEY" "testing42" ${VAULT_SEEDS}
test_env_contents 3 "TEST_TEST" "testing42" ${VAULT_SEEDS}
test_env_contents 4 "TEST__TEST" "testing42" ${VAULT_SEEDS}
test_env_contents 5 "_TEST__TEST" "testing42" ${VAULT_SEEDS}

test_env_contents 6 "SECRET_TESTING_KEY" "testing42" ${VAULT_SEEDS_V2}
