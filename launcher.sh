#!/bin/sh
# Game launcher for Linux and OSX

ORIG_DIR=$(pwd)
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function finish {
    cd $ORIG_DIR
}
trap finish EXIT

cd $SCRIPT_DIR
if [[ "$(uname)" == "Darwin" ]]; then
    chmod +x fetch-quest-osx
    export DYLD_LIBRARY_PATH=$SCRIPT_DIR/
    exec -a fetch-quest ./fetch-quest-osx
else
    export LD_LIBRARY_PATH=$SCRIPT_DIR/
    chmod +x fetch-quest-linux
    exec -a fetch-quest ./fetch-quest-linux
fi
