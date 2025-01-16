#!/usr/bin/env bash

# NOTE: This is from the config files
# # make sure it's executable with:
# # chmod +x ~/.config/sketchybar/plugins/aerospace.sh
#
# if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
#     sketchybar --set $NAME background.drawing=on
# else
#     sketchybar --set $NAME background.drawing=off
# fi

sketchybar --set workspace label=$(aerospace list-workspaces --focused)
