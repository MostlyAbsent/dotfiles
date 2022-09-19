#!/usr/bin/env zsh

# Dock Config
output=$(defaults read com.apple.dock persistent-apps 2>/dev/null)

if [[ -n $output ]]; then
  defaults delete com.apple.dock persistent-apps
  killall Dock
fi
