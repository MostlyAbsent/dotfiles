export ZSH="$HOME/.oh-my-zsh/"
ZSH_THEME="eastwood"
plugins=(git lein thefuck)
source $ZSH/oh-my-zsh.sh
export PATH="$HOME/.bin":$PATH
eval $(thefuck --alias)

alias ls="ls -alG"
export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"

source /opt/homebrew/Cellar/chruby/0.3.9/share/chruby/chruby.sh

eval `keychain --eval --agents ssh --inherit any id_ed25519`
