#!/bin/sh

set -xe

for out in {out.ast,}; do
  diff --color src/$out src/$out
done
