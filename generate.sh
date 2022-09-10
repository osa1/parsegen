#!/bin/bash

set -e
set -x

cargo run -- test_grammar
dot -Tpng lr0.dot > lr0.png
dot -Tpng lr1.dot > lr1.png
