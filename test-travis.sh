#!/bin/sh
#
# Very simple test script

set -ex

# About the simplest test we can do, ensure the file compiles without error
${EMACS} --version
${EMACS} -q --batch -l ert -l "chromebook-ert.el" -f ert-run-tests-batch-and-exit
