#!/bin/bash

cd ~/dotfiles/

output=$(git status)

echo "Dotfiles Git Status ///// $(date)"
echo "---"
echo -e "$output"
