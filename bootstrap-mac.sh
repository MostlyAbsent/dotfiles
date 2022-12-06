#!/usr/bin/env zsh

# Install brew
if ! command -v brew &>/dev/null; then
  echo "Installing Brew"

  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# Install babashka
if ! command -v bb &>/dev/null; then
  echo "Installing Babashka"

  brew install borkdude/brew/babashka
fi

# Install ohmyzsh
if ! [[ -d ~/.oh-my-zsh ]]; then
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

# Link in zshrc
if ! [[ -h ~/.zshrc ]]; then
  rm ~/.zshrc
  ln -s ~/Documents/dotfiles/zshrc ~/.zshrc
fi

# Link in scripts
if ! [[ -h ~/.bin ]]; then
  ln -s ~/Documents/dotfiles/bin ~/.bin
fi

# Test for rectangle and install if not found

./bin/apptest.sh bettertouchtool
./bin/apptest.sh quicksilver
./bin/apptest.sh syntax-highlight
./bin/apptest.sh espanso
./bin/apptest.sh quicksilver
./bin/apptest.sh appcleaner
./bin/apptest.sh openjdk
./bin/apptest.sh terminal-notifier
./bin/apptest.sh thefuck
./bin/apptest.sh fd
./bin/apptest.sh leiningen
./bin/apptest.sh clojure
./bin/apptest.sh hunspell
