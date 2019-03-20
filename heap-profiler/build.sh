#!/bin/bash

stack --work-dir .stack-work-profile build --profile --no-library-profiling --ghc-options="-fprof-auto -rtsopts -auto-all -caf-all" cardano-ledger-heap-profiler
