if [[ -z "$TMUX" ]] && [[ -z "$TMUX_INIT" ]]; then
  export TMUX_INIT=TRUE
  tmux new-session -A -s base
fi

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
