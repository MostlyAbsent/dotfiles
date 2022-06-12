#!/usr/bin/env bash

# Set permissions
# There will need to be executable flags on the helper scripts

# Install Brew
if ! command -v brew &> /dev/null; then
  echo "Installing Brew"

  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

  cat config/brewrc.txt >> ~/.bashrc

  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

  sudo yum groupinstall 'Development Tools'

  brew install gcc
fi

# Exports
if [[ ! -f ~/.bashrc || $(grep -c "EDITOR" ~/.bashrc) -eq 0 ]]; then
  cat config/editor.txt >> ~/.bashrc
fi

# Install Bash Autocompletion
if [[ ! -f ~/.bashrc || $(grep -c "Homebrew autocompletion" ~/.bashrc) -eq 0 ]]; then
  echo "Installing Bash Autocompletion"

  cat config/bash-autocomplete.txt >> ~/.bashrc
fi

# Install vim
if ! command -v vim &>/dev/null; then
  echo "Installing vim"
  brew install vim
fi

# Configure vim
if [[ ! -f ~/.vimrc ]]; then
  echo "Configuring vim"

  cp config/vimrc.txt ~/.vimrc
fi

# Configure Emacs
if [[ ! -d ~/.emacs.d ]]; then
  echo "Configuring emacs"

  ln -s emacs.d $HOME/.emacs.d
fi

# Install babashka
if ! command -v bb &>/dev/null; then
  echo "Installing Babashka"

  brew install borkdude/brew/babashka
fi

