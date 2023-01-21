#!/bin/bash

function run {
    if ! pgrep $1 ;
    then 
        $@&
    fi
}


run sxhkd -c ~/.config/sxhkd/sxhkdrc &
# run picom --config $HOME/.config/picom/picom.conf --vsync --experimental-backends &
nitrogen --restore &
