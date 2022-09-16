#!/usr/bin/env zsh

echo "Testing for" $@

if [[ -z $(brew list | grep $@) ]]; then
  echo "Installing" $@
  brew install $@
else
  echo $@ "already installed"
fi
