# vaultenv

Run processes with secrets from [HashiCorp Vault]. It:

 1. Reads a list of required secrets
 2. Fetches them from Vault
 4. Calls `exec` with the secrets in the process environment

There is nothing else going on.

## Comparison to alternatives

The only alternative to this tool that we are aware of is [envconsul], also by
HashiCorp. Unlike envconsul, `vaultenv` does not:

 - daemonize
 - spawn any child processes
 - manage the lifecycle of the process it provides the secrets for

`vaultenv` calls a syscall from the `exec` family after fetching secrets for
you. This means that `vaultenv` replaces it's own process with whatever you want.
After your service has started, `vaultenv` is not running anymore.

This approach does mean that we cannot automatically restart services if
secrets in Vault have changed.

## Terminology

A brief summary of Vault terminology:

A Vault **secret** consists of multiple **key/value pairs**, which are stored
under a **path** in a **backend**.

Let's use the Vault CLI to write a secret to see all the concepts in action:

```shell
$ vault write secret/production/third-party api-key=fecb0f6e97c5b37b3a814107682cf68416f072a8
              ^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             backend          path            key                   value
```

Note: this will fail without a running Vault instance. Use `vault server -dev`
to get one up and running locally.

## Tutorial

The following program depends on the secret that we stored in Vault in the
previous section:

```bash
#!/bin/bash

# Fail when referencing an unbound variable
set -u

# Mentally substitute the call to echo with something like:
# curl -H "Content-Type: application/json" -H "X-API-Key: ${PRODUCTION_THIRD_PARTY_API_KEY}" -X POST -d '{"my": "payload"}' https://example.com/
echo "${PRODUCTION_THIRD_PARTY_API_KEY}"
```

This program will fail without `PRODUCTION_THIRD_PARTY_API_KEY` in it's
environment:

```
$ ./tutorial.sh
./tutorial.sh: line 8: PRODUCTION_THIRD_PARTY_API_KEY: unbound variable
```

We can use `vaultenv` to fetch the required secret before running `tutorial.sh`.
Create a file `tutorial.secrets` with the following content:

```
production/third-party#api-key
```

And run `vaultenv` like so:

```
$ vaultenv --token <YOUR_VAULT_TOKEN_HERE> --secrets-file ./tutorial.secrets ./tutorial.sh
fecb0f6e97c5b37b3a814107682cf68416f072a8
```

This instructs `vaultenv` to fetch `secret/production/third-party` and load the
contents of `api-key` under `PRODUCTION_THIRD_PARTY_API_KEY` in the environment
of `tutorial.sh`.

## Usage

```
vaultenv - run programs with secrets from HashiCorp Vault

Usage: vaultenv [--host HOST] [--port PORT] --token TOKEN
                --secrets-file FILENAME CMD [ARGS...]

Available options:
  --host HOST              Vault host, either an IP address or DNS name,
                           defaults to localhost
  --port PORT              Vault port, defaults to 8200
  --token TOKEN            token to authenticate to Vault with
  --secrets-file FILENAME  config file specifying which secrets to request
  CMD                      command to run after fetching secrets
  ARGS...                  arguments to pass to CMD, defaults to nothing
  -h,--help                Show this help text
```

## Secret specification

EBNF of the `--secrets-file` format:

```
file = line, { line } ;
line = path , "#" , key , "\n" ;
path = char , { char } ;
key = char , { char } ;

char = letter | digit | "_" | "-" | "/" ;
```

Example:

```
production/third-party#api-key
production/another-third-party#refresh-token
```

A `file` consists of multiple `line`s, each specifying a secret to fetch. A
line specifies the secret `path` and the `key` to fetch, separated by a `#`.

## Environment variable names

The secret `path` and `key` determine the name of the environment variable
that the secret contents will be available under. `path` and `key` are
uppercased and concatenated by a `_`. All `/` and `-` characters are also
replaced by underscores.

Example: the contents of `production/third-party#api-key` will be available
as `PRODUCTION_THIRD_PARTY_API_KEY`.

## Future work

 - Support DNS `SRV` record lookups, so users only need to specify the host
   Vault runs on. This integrates `vaultenv` nicely with Vaults HA setup.

## License

3-clause BSD. See `LICENSE` for details.

  [HashiCorp Vault]: https://www.vaultproject.io/
  [envconsul]: https://github.com/hashicorp/envconsul
