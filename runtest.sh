#!/bin/bash

if [ -z "$1" ]; then
    echo
    echo "Error: missing test filename argument" >&2
    echo "Files are searched in the lagtests directory in the root of the project" >&2
    echo "Usage: $0 <test-name>" >&2
    echo
    exit 1
fi

exec ./zig-out/bin/secret.exe "./langtests/$1"

