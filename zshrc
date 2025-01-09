eval "$(/opt/homebrew/bin/brew shellenv)"

export ZSH="$HOME/.oh-my-zsh/"
export EDITOR="/opt/homebrew/bin/nvim"
ZSH_THEME="eastwood"
plugins=(aliases colored-man-pages git)
source $ZSH/oh-my-zsh.sh

export PATH="$HOME/.bin":$PATH
export PATH="/Applications/Emacs.app/Contents/MacOS/bin":$PATH
export PATH="$HOME/.config/emacs/bin:$PATH"

# java stuff
export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
eval "$(direnv hook zsh)"

# C build garbage
export LDFLAGS=-L/opt/homebrew/opt/llvm/lib
export CPPFLAGS=-I/opt/homebrew/opt/llvm/include
export CFLAGS=-I/opt/homebrew/opt/llvm/include
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
if [ -d "$(brew --prefix)/opt/grep/libexec/gnubin" ]; then
    PATH="$(brew --prefix)/opt/grep/libexec/gnubin:$PATH"
fi

# Aliases
alias cdd="cd ~/Documents/"
alias ls="ls -alG"
alias c="nvim ."
alias nv="nvim"
alias compile_epub="java -jar ~/Documents/compile-epub/target/compile-epub.jar" # TODO: this should be a bin
alias epub_replacements='bb ~/Documents/epub-replacements/src/epub_replacements/epub_replacements.clj' # TODO: this should be a bin

# pnpm
export PNPM_HOME="/Users/jacobdoran/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# TMUX
tmux_running=$(pgrep tmux)
if [[ -n $TMUX ]] && [[ -n $tmux_running ]]; then
  _tmux-margins c
fi

tmux-window-name() {
	($TMUX_PLUGIN_MANAGER_PATH/tmux-window-name/scripts/rename_session_windows.py &)
}

add-zsh-hook chpwd tmux-window-name

# keychain autoload
eval `keychain --eval --agents ssh --inherit any id_ed25519`

# FZF
export FZF_COMPLETION_TRIGGER='**'

# Options to fzf command
export FZF_COMPLETION_OPTS='--border --info=inline'

# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

# Advanced customization of fzf options via _fzf_comprun function
# - The first argument to the function is the name of the command.
# - You should make sure to pass the rest of the arguments to fzf.
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf --preview 'tree -C {} | head -200'   "$@" ;;
    export|unset) fzf --preview "eval 'echo \$'{}"         "$@" ;;
    ssh)          fzf --preview 'dig {}'                   "$@" ;;
    *)            fzf --preview 'bat -n --color=always {}' "$@" ;;
  esac
}
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


# Created by `pipx` on 2024-11-20 04:40:39
export PATH="$PATH:/Users/jacobdoran/.local/bin"
