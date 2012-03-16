#!/bin/bash
rm -rf cabal-dev
cabal-dev add-source ptrace
cabal-dev add-source trace
cabal-dev add-source .

