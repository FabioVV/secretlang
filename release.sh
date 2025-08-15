#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 d|f|s - where d=Debug, f=ReleaseFast and s=ReleaseSmall"
    echo "Defaulting to debug mode..."
    zig build -Doptimize=Debug
    exit 0
fi


case "$1" in
    d)
        echo "Building in Debug mode"
        zig build -Doptimize=Debug
        ;;
    f)
        echo "Building in ReleaseFast mode"
        zig build -Doptimize=ReleaseFast
        ;;
    s)
        echo "Building in ReleaseSmall mode"
        zig build -Doptimize=ReleaseSmall
        ;;
    *)
        echo "Error: Unknown build mode '$1'"
        echo "Usage: $0 d or f or s - where d=Debug, f=ReleaseFast and s=ReleaseSmall"
        echo "Defaulting to debug mode..."
        zig build -Doptimize=Debug
        exit 1
        ;;
esac
