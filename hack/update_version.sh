#!/bin/bash

if [ -z "$1" ]; then
    echo "missing version argument"
    exit 1
fi

DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. &> /dev/null && pwd)

sed -i "s/^;; Version: .*/;; Version: $1/" $DIR/cue-mode.el
