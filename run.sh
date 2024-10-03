#!/bin/bash
cabal build -v0
cabal exec vpccs-exec -- "$@"