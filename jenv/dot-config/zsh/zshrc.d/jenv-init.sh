#set jenv location
export JENV_ROOT=$XDG_DATA_HOME/jenv

# init jenv
eval "$(jenv init -)"

# enable JAVA_HOME export, but only on first launch of zsh
if [[ -z "$TMUX" ]] && [[ -z "$TMUX_INIT" ]]; then
  jenv enable-plugin export
fi
