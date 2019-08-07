#!/bin/bash

# set -e

# load our ignored files helper function
path=$(dirname "$0") # this file's path
. $path/get_sources.sh

exit_code=0
for f in ${srcs[@]}
do
    if ! isIgnored $f ; then
        # cmp -s <(stack exec -- stylish-haskell $f) <(cat $f) # compare stylish-haskell's output to the current file
        cmp -s <(stack exec -- brittany $f) <(cat $f) # compare brittany's output to the current file
        if [ $? -ne 0 ]
        then
          echo "Please format $f ❌"
          stack exec -- brittany --write-mode=inplace $f
          # stack exec -- stylish-haskell $f -i
          exit_code=1
        fi
        stack exec -- hlint $f
        if [ $? -ne 0 ]
        then
          stack exec -- hlint $f --refactor --refactor-options -i
          echo "Please lint $f ❌"
          exit_code=1
        fi
    fi
done
if [ $exit_code -ne 0 ]
then
  exit $exit_code
else
  echo "All code is formatted ✅"
fi
