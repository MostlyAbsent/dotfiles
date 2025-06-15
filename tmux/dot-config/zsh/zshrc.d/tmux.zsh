if [[ -z "$TMUX" ]] && [[ -z "$TMUX_INIT" ]]; then
  export TMUX_INIT=TRUE
  tmux new-session -A -s base
fi

# If we're running inside tmux, export the session name
if [ -n "$TMUX" ]; then
  export TMUX_SESSION_NAME=$(tmux display-message -p '#S')
fi

# If we're in tmux, but with only one pane, marginise it
if [[ -n "$TMUX" ]] && [[ $(tmux list-panes | wc -l ) -lt 3 ]]; then
  _tmux-margins c $TMUX_PANE
fi 

cd () {
  if [ "$#" -eq 0 ] && [ -n "${ROOTDIR}" ]
  then
    builtin cd $ROOTDIR
  else
    builtin cd "$@"
  fi
}
