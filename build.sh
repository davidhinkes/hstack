#!/usr/bin/env bash
cabal configure --enable-tests || exit 1
cabal build || exit 1
cabal test
