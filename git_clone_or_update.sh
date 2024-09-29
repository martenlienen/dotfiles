#!/bin/bash

path=$1
repo=$2

if [[ -d "$path" ]]; then
  (cd "$path" && git pull)
else
  git clone "$path" "$repo"
fi
