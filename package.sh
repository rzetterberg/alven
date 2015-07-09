set -e

script_path=${BASH_SOURCE[0]}
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $script_dir

cd src

cabal install --bindir=$script_dir/tmp --datasubdir=$script_dir/tmp

echo "> Packaging complete"
