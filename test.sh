#!/bin/bash

prove $(find integration -type f -iname '*.sh' ! -name '_*')
