if [[ -z "$TMUX" ]] && [[ -z "$TMUX_INIT" ]]; then
  export TMUX_INIT=TRUE
  tmux
fi

if [[ -n "$TMUX" ]] && [[ $(tmux list-panes | wc -l ) -lt 3 ]]; then
  _tmux-margins c $TMUX_PANE
fi 
