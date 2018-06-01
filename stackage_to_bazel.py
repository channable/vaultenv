#!/usr/bin/env python3

import yaml
import sys

build_plan = []

with open('/home/ruuda/.stack/build-plan/lts-10.5.yaml', 'r') as lts_file:
    build_plan = yaml.load(lts_file)

core_packages = set(build_plan['system-info']['core-packages'].keys())
packages = build_plan['packages']

roots = sys.argv[1:]

done = set(list(core_packages))
todo = roots

f = open('stackage.bzl', 'w')

while todo:
    name = todo.pop()

    if name in done:
        continue

    package = packages[name]

    deps = []
    for dep, props in package['description']['packages'].items():
        if 'library' in props['components']:
            deps.append(dep)

    missing_deps = [dep for dep in deps if not dep in done]

    if missing_deps:
        todo.append(name)
        todo.extend(missing_deps)
        continue

    done.add(name)

    version_name = name + '-' + package['version']
    target_name = name.replace('-', '_').replace('.', '_')
    repo_name = 'hackage_' + target_name
    sha256 = package['cabal-file-info']['hashes']['SHA256']

    prebuilt_deps = [f'\n    "{dep}",' for dep in deps if dep in core_packages]
    prebuilt_deps_str = ''.join(prebuilt_deps)

    print(version_name, '=>', '@' + repo_name)

    build_def = f'''
new_http_archive(
  name = "{repo_name}",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/{version_name}/{version_name}.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/{version_name}.tar.gz",
  ],
  # sha256 = "{sha256}",
  strip_prefix = "{version_name}",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "{target_name}",
  srcs = glob(["src/**/*.hs"]),
  prebuilt_dependencies = [{prebuilt_deps_str}
  ],
)
  """,
)'''
    print(build_def, file=f)
