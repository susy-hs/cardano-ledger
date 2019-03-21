#!/bin/bash

BIN_DIR=`stack path --work-dir .stack-work-profile --local-install-root`/bin

${BIN_DIR}/heap-profiler +RTS -p
