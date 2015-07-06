# Starts all containers needed to build and test the project
# and drops the user into the builders shell
#
# All containers are removed upon exit

script_path=${BASH_SOURCE[0]}
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $script_dir

echo "> Starting tester"
docker run --rm -i -t --link alven-builder:builder --link alven-database:db -v $PWD:/opt/alven \
       alven/tester /bin/bash
