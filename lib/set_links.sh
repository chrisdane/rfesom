#!/bin/bash

echo
echo "******** run set_links.sh **********"
home=$(git rev-parse --show-toplevel)
ln -s $home/lib/.pre-commit $home/.git/hooks/
ln -s $home/lib/.post-commit $home/.git/hooks/
echo "******** finish set_links.sh **********"
echo
