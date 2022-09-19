export ZSH="$HOME/.oh-my-zsh/"
ZSH_THEME="fox"
plugins=(git lein thefuck)
source $ZSH/oh-my-zsh.sh
export PATH="$HOME/.bin":$PATH
eval $(thefuck --alias)

alias ls="ls -al"
