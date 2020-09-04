#!/usr/bin/env bash
echo $1
if test -f $1; then
  echo "node_modules/.bin/bsc -format $1 > /tmp/a.res && mv /tmp/a.res $1"
  node_modules/.bin/bsc -format $1 > /tmp/a.res && mv /tmp/a.res $1
else
  echo "$1 doesn't exist"
fi
