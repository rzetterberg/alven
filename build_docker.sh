#!/bin/bash

set -e

script_path=${BASH_SOURCE[0]}
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $script_dir

function build_container {
    local tag=$1
    local docker_file=$2

    echo "> Building "$docker_file

    docker build -f config/docker/$docker_file -t $tag .
}

build_container "kael/builder" "builder.docker"
build_container "kael/db" "db.docker"
