#!/bin/bash
export OCB_SESSION="opencrowbar"
if [[ ! $TMUX && $SESSION != $OCB_SESSION ]]; then
    if !which tmux &>/dev/null; then
        echo "Docker-slaves needs tmux installed!"
        exit 1
    fi
    export SESSION=$OCB_SESSION
    tmux new-session -d -n control -s "$SESSION" "/bin/bash -i"
    tmux set-option -t "$SESSION" set-remain-on-exit on
    tmux new-window -n "$SESSION:${0##*/}" "$0 $*"
    tmux attach-session -t "$SESSION"
    exit
fi

