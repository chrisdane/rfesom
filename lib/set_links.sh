#!/bin/bash

echo
echo "******** run set_links.sh **********"
home=$(git rev-parse --show-toplevel)
echo "ln -s $home/lib/.pre-commit $home/.git/hooks/pre-commit"
ln -s $home/lib/.pre-commit $home/.git/hooks/pre-commit
echo "ln -s $home/lib/.post-commit $home/.git/hooks/post-commit"
ln -s $home/lib/.post-commit $home/.git/hooks/post-commit
echo "******** finish set_links.sh **********"
echo
