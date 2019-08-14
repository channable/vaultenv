# vaultenv

Run processes with secrets from [HashiCorp Vault]. It:

 1. Reads a list of required secrets
 2. Fetches them from Vault
 4. Calls `exec` with the secrets in the process environment

There is nothing else going on.

`vaultenv` supports the Vault KV API. It supports both version 1 and version 2.
This support is automatic; but you do need a token which has read access on
the `/sys/mounts` endpoint.

## Comparison to alternatives

The only alternative to this tool that we are aware of is [envconsul], also by
HashiCorp. Unlike envconsul, `vaultenv` does not:

 - daemonize
 - spawn any child processes
 - manage the lifecycle of the process it provides the secrets for

All of the above should not be done by a secret fetching tool. This should be
left to a service manager, like systemd.

`vaultenv` calls a syscall from the `exec` family after fetching secrets for
you. This means that `vaultenv` replaces its own process with whatever you want.
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

Before we can start, [build vaultenv locally][build-vaultenv] or [download a
binary][download-vaultenv].

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

This program will fail without `PRODUCTION_THIRD_PARTY_API_KEY` in its
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
$ vaultenv --token <YOUR_VAULT_TOKEN_HERE> --no-connect-tls --secrets-file ./tutorial.secrets ./tutorial.sh
fecb0f6e97c5b37b3a814107682cf68416f072a8
```

This instructs `vaultenv` to fetch `secret/production/third-party` and load the
contents of `api-key` under `PRODUCTION_THIRD_PARTY_API_KEY` in the environment
of `tutorial.sh`.

## Usage

```
vaultenv - run programs with secrets from HashiCorp Vault

Usage: vaultenv [--host HOST] [--port PORT] --token TOKEN
                --secrets-file FILENAME CMD [ARGS...] ([--no-connect-tls] |
                [--connect-tls]) ([--no-validate-certs] | [--validate-certs])
                ([--no-inherit-env] | [--inherit-env])
                [--retry-base-delay-milliseconds MILLISECONDS]
                [--retry-attempts NUM] [--log-level error | info]

Available options:
  -h,--help                Show this help text
  --host HOST              Vault host, either an IP address or DNS name.
                           Defaults to localhost. Also configurable via
                           VAULT_HOST.
  --port PORT              Vault port. Defaults to 8200. Also configurable via
                           VAULT_PORT.
  --token TOKEN            Token to authenticate to Vault with. Also
                           configurable via VAULT_TOKEN.
  --secrets-file FILENAME  Config file specifying which secrets to request. Also
                           configurable via VAULTENV_SECRETS_FILE.
  CMD                      command to run after fetching secrets
  ARGS...                  Arguments to pass to CMD, defaults to nothing
  --no-connect-tls         Don't use TLS when connecting to Vault. Default: use
                           TLS. Also configurable via VAULTENV_CONNECT_TLS.
  --connect-tls            Always connect to Vault via TLS. Default: use TLS.
                           Can be used to override VAULTENV_CONNECT_TLS.
  --no-validate-certs      Don't validate TLS certificates when connecting to
                           Vault. Default: validate certs. Also configurable via
                           VAULTENV_VALIDATE_CERTS.
  --validate-certs         Always validate TLS certificates when connecting to
                           Vault. Default: validate certs. Can be used to
                           override VAULTENV_CONNECT_TLS.
  --no-inherit-env         Don't merge the parent environment with the secrets
                           file. Default: merge environments. Also configurable
                           via VAULTENV_INHERIT_ENV.
  --inherit-env            Always merge the parent environment with the secrets
                           file. Default: merge environments. Can be used to
                           override VAULTENV_INHERIT_ENV.
  --retry-base-delay-milliseconds MILLISECONDS
                           Base delay for vault connection retrying. Defaults to
                           40ms. Also configurable via
                           VAULTENV_RETRY_BASE_DELAY_MS.
  --retry-attempts NUM     Maximum number of vault connection retries. Defaults
                           to 9. Also configurable through
                           VAULTENV_RETRY_ATTEMPTS.
  --log-level error | info Log-level to run vaultenv under. Options: 'error' or
                           'info'. Defaults to 'error'. Also configurable via
                           VAULTENV_LOG_LEVEL

```

## Configuration

Vaultenv reads configuration from two types of files:

 - A specification of secrets to fetch.
 - Configuration options for `vaultenv` itself, mostly connection related.

Decoupling these is useful, because this allows for e.g. changing which secrets
are fetched on a per project basis, while the connection options stay the same.
Let's first discuss secrets files.

### Secret specification

There are two versions of the secret specification format. The first version shipped
with the initial version of Vaultenv, but doesn't allow users to specify custom
mountpoints for backends. Vaultenv would always fetch from the generic secret
backend mounted at `secret/`. Version 2 of the format supports custom mount
points.

Example (version 1, implicit `secret/` path prepended):

```
production/third-party#api-key
production/another-third-party#refresh-token
FOOBAR=production/third-party#foobar
```

Vaultenv will make the following environment variables available:

 - `PRODUCTION_THIRD_PARTY_API_KEY`: Contents of the `api-key` field of the
   secret at `secret/production/third-party`.
 - `PRODUCTION_ANOTHER_THIRD_PARTY_REFRESH_TOKEN`: Contents of the
   `refresh-token` field of the secret at
   `secret/production/another-third-party#refresh-token`.
 - `FOOBAR`: Contents of the `foobar` field in the secret at
   `secret/production/third-party`.

The `FOOBAR=` syntax means: make this secret available under the FOOBAR
environment variable.

Example (version 2, explicit mount paths):

```
VERSION 2

MOUNT secret
third-party#api-key

MOUNT production
third-party#refresh-token
FOOBAR=third-party#foobar
```

Vaultenv will make the following environment variables available:

 - `SECRET_THIRD_PARTY_API_KEY` with the contents of the `api-key` field of the
   secret at `secret/third-party`.
 - `PRODUCTION_THIRD_PARTY_REFRESH_TOKEN` with the contents of the
   `refresh-token` field from the secret at `production/third-party`
 - `FOOBAR` with the contents of the `foobar` field of the secret at
   `production/third-party`.

### Behavior configuration

Vaultenv supports loading behavior configuration from files (in addition to the
CLI flags and environment variable lookups). Vaultenv currently looks for these
files in the following places:

 - `/etc/vaultenv.conf`
 - `$HOME/.config/vaultenv/vaultenv.conf`
 - `$CWD/.env`

These config files support the exact same syntax as the environment variables
that you can use to configure Vaultenv. See the `--help` output for a list of
what's available.

These files follow the `.env` format (as popularized by [this Ruby
gem](https://github.com/bkeepers/dotenv)). An example:

```
# /etc/vaultenv.conf
# Also: comments are allowed if they start with `#`.
VAULT_TOKEN="your-vault-token"
VAULT_PORT="8200"
VAULTENV_INHERIT_ENV="yes"
```

This is mostly useful on development machines. It allows you to:

 - Set global connection options on a per-machine basis. Useful if you run a
   Vault instance in your VPN.
 - Set per-user tokens.
 - Set per-project secrets files.

All while running Vaultenv without any CLI args.

## Allowed characters in environment variables

We disallow the following in any path to keep the parser and format simple and
unambiguous:

 - Whitespace
 - The `#` and `=` characters
 - Control characters

Everything else is allowed.

**N.B.:** Be careful with special characters in path components. While
vault supports them, and vaultenv parses them from the secrets file just fine,
you MUST specify an environment variable to put them in, otherwise you may run
into unexpected behavior.

## Environment variable names

The secret `path` and `key` determine the name of the environment variable
that the secret contents will be available under. `path` and `key` are
uppercased and concatenated by a `_`. All `/` and `-` characters are also
replaced by underscores.

Example: the contents of `production/third-party#api-key` will be available
as `PRODUCTION_THIRD_PARTY_API_KEY`.

## Building

Vaultenv is written in Haskell and builds with [Stack][stack]:

    stack setup
    stack build

The binary can then be found at `$(stack path
--local-install-root)/bin/vaultenv`.  You can also run it directly
with `stack exec`:

```
stack exec vaultenv -- --token SECRET --secrets-file foo.env /usr/bin/env
```

It is possible to `stack build` with `--split-objs` to produce a smaller binary.
To take full advantage of this, the Stackage snapshot has to be rebuilt.

## Future work

 - Support DNS `SRV` record lookups, so users only need to specify the host
   Vault runs on. This integrates `vaultenv` nicely with Vaults HA setup.
 - Certificate pinning/validation

## License

3-clause BSD. See `LICENSE` for details.

  [HashiCorp Vault]: https://www.vaultproject.io/
  [envconsul]: https://github.com/hashicorp/envconsul
  [stack]: https://haskellstack.org
  [build-vaultenv]:#building
  [download-vaultenv]:https://github.com/channable/vaultenv/releases
