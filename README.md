# vaultenv

[![Build Status](https://travis-ci.com/channable/vaultenv.svg?branch=master)](https://travis-ci.com/channable/vaultenv)

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
vaultenv 0.13.2 - run programs with secrets from HashiCorp Vault

Usage: vaultenv [--version] [--host HOST] [--port PORT] [--addr ADDR]
                [--token TOKEN] [--secrets-file FILENAME] [CMD] [ARGS...]
                [--no-connect-tls | --connect-tls]
                [--no-validate-certs | --validate-certs]
                [--no-inherit-env | --inherit-env]
                [--inherit-env-blacklist COMMA_SEPARATED_NAMES]
                [--retry-base-delay-milliseconds MILLISECONDS]
                [--retry-attempts NUM] [--log-level error | info] [--use-path]
                [--max-concurrent-requests NUM]

Available options:
  -h,--help                Show this help text
  --version                Show version
  --host HOST              Vault host, either an IP address or DNS name.
                           Defaults to localhost. Also configurable via
                           VAULT_HOST.
  --port PORT              Vault port. Defaults to 8200. Also configurable via
                           VAULT_PORT.
  --addr ADDR              Vault address, the scheme, either http:// or
                           https://, the ip-address or DNS name, followed by the
                           port, separated with a ':'. Cannot be combined with
                           either VAULT_PORT or VAULT_HOST
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
  --inherit-env-blacklist COMMA_SEPARATED_NAMES
                           Comma-separated list of environment variable names to
                           remove from the environment before executing CMD.
                           Also configurable via VAULTENV_INHERIT_ENV_BLACKLIST.
                           Has no effect if no-inherit-env is set!
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
  --use-path               Use PATH for finding the executable that vaultenv
                           should call. Default: don't search PATH. Also
                           configurable via VAULTENV_USE_PATH.
  --max-concurrent-requests NUM
                           Maximum number of concurrent requests to vault.
                           Defaults to 8. Pass 0 to disable the limit. Also
                           configurable through
                           VAULTENV_MAX_CONCURRENT_REQUESTS.
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

All while running Vaultenv without any CLI args.

#### Different levels of configuration

It can happen that conflicting configuration values are send to Vaultenv. An example
would be a global secret file, which is overwritten by a project specific configuration file.
The order of precedence (from least precedence to most precedence) is as follows:
 - `/etc/vaultenv.conf`
 - `$HOME/.config/vaultenv/vaultenv.conf`
 - `$CWD/.env`
 - environment variables
 - command line options

This is useful on development machines. It allows you to:

 - Set global connection options on a per-machine basis. Useful if you run a
   Vault instance in your VPN.
 - Set per-user tokens.
 - Set per-project secrets files.

This means that any command line option that is present would overwrite any other configuration.
If an option is not specified, the default is used. The defaults are as follows:
```
VAULT_HOST:                     localhost
VAULT_PORT:                     8200
VAULT_ADDR:                     https://localhost:8200
VAULT_TOKEN:                    Unspecified
VAULTENV_SECRETS_FILE:          Unspecified
CMD:                            Unspecified
ARGS:                           []
VAULTENV_CONNECT_TLS:           True
VAULTENV_VALIDATE_CERTS:        True
VAULTENV_INHERIT_ENV:           True
VAULTENV_INHERIT_ENV_BLACKLIST: []
VAULTENV_RETRY_BASE_DELAY:      40
VAULTENV_RETRY_ATTEMPTS:        9
VAULTENV_LOG_LEVEL:             Error
VAULTENV_USE_PATH:              True
VAULTENV_MAX_CONCURRENT_REQUESTS: 8
```
In cases where no default nor any value is specified, which is possible for `Token`, `Secret file` and
`Command`, Vaultenv will give an error that it requires these values to operate.

#### Connection options

Vaultenv also supports the `VAULT_ADDR` configuration. In such a case, without specifying separate
parameters for host, port and whether to use TLS or not, one can specify these values in a single value.
The address always starts with either `http://` or `https://`, followed by a either a DNS name
or an ip-address. The port is specified at the end of the address, using a `:` to separate the host
and the port. For example: `https://example.com:42` would create a TLS enabled connection
to the host `example.com` on port `42`.

Other errors can happen with the address configuration. There are two ways of specifying
what the connection options are, either via the address of via a combination of
the port, the host and whether to use TLS. As there are two ways of specifying this,
it is also possible for these values to conflict. Consider the situation where
`VAULT_ADDR` is `http://example.com:8200` and `VAULT_PORT` is set to `42`. There
are two ways Vaultenv can resolve this. In the case of the address and the port
being specified in the same configuration, like the same file or both as command
line options, Vaultenv will not choose either way and will report an error.

In the case they are specified in different configuration levels, like the address in a file
and the port in the command line options, the higher precedence (as defined
[above](#Different-levels-of-configuration)) is used for that specific value.
In this case, this would result in a host of `example.com`, no usage of TLS,
due to the `http://` scheme, and a port of `42`.

Other errors than the mismatch address error that can happen during parsing are:
- A non-numeric port in the address, like `http://localhost:my_port`
- A non-supported scheme in the address, like `ftp://example.com:42`

By default, `vaultenv` will open at most 8 concurrent HTTP connections to the vault server.
This limit can be changed using the `VAULTENV_MAX_CONCURRENT_REQUESTS` setting, and it can
be disabled by choosing the limit `0`.

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

If you want a fully static executable without a runtime dependency on `libc`
and run GNU/Linux, you can install [Nix](https://nixos.org/nix/) and run:

```
$ $(nix-build --no-link -A full-build-script nix/vaultenv-static.nix)
```

This has not been tested on any other platform.

That will build vaultenv (and a bunch of dependencies). The final line of the
output should be a path in `/nix/store` which contains the final vaultenv
binary.

It is possible to speed up the compilation process by copying some dependencies
from a Nix cache, to not have to recompile all of Haskell and its dependencies.
To set up the cache, execute these commands once before building:

```console
$ nix run -c cachix use static-haskell-nix
$ nix run -c cachix use channable-public
```

Note that the build process via Nix is not (yet) reproducible, which means that
different builds of the same source code may result in different Nix derivation
hashes.

## Development

If you want a convenient way to gather the development dependencies of
`vaultenv`, you can use `nix`.

The repository contains a `default.nix` which will get you `stack` and `vault`.
You can then use this to get a shell with the tools in scope to work on and
test `vaultenv`.

Get this shell with:

```
$ nix run
```

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
