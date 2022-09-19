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
  ln -s ~/Documents/dotfiles/zshrc .zshrc
fi

# Test for rectangle and install if not found

./bin/apptest.sh rectangle
./bin/apptest.sh quicksilver
