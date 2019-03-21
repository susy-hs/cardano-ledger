#!/bin/bash

stack --work-dir .stack-work-profile build --profile --no-library-profiling --ghc-options="-fprof-auto -rtsopts -fprof-cafs" cardano-ledger-heap-profiler
