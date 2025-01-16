if [[ -z "$TMUX" ]] && [[ -z "$TMUX_INIT" ]]; then
  export TMUX_INIT=TRUE
  tmux
fi

if [ -n "$TMUX" ]; then
  _tmux-margins c $TMUX_PANE
fi 
