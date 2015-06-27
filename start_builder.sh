# Starts all containers needed to build and test the project
# and drops the user into the builders shell
#
# All containers are removed upon exit

script_path=${BASH_SOURCE[0]}
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $script_dir

echo "> Starting database container"
db_id=$(docker run -P -d --name kael-database kael/db)

echo "> All background containers started, starting builder"
docker run --rm -i -t -p 127.0.0.1:3000:3000 --link kael-database:db \
       --name kael-builder -v $PWD:/opt/kael kael/builder /bin/bash

echo "> Cleaning up background containers"
docker rm -f $db_id
