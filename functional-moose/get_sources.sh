#!/bin/bash

path=$(dirname "$0") # this file's path

ignored_files=(`cat "$path/ignored_style_files.txt"`)

isIgnored () {
  for i in ${ignored_files[*]}; do
    if [[ $path/$i -ef $1 ]]
    then
      echo "$i ignored"
      return 0
    fi
  done
  return 1
}

src_dirs=($path/app $path/test $path/src $path/Setup.hs) # places src code can live
srcs=$(find ${src_dirs[*]} -type f -name "*.hs") # find all .hs files

