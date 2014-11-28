#!/bin/bash -eu

echo $1
#xrandr --output DVI-I-1 --mode 1024x768 --output DP-0 --mode 1024x768 --right-of DVI-I-1
#sleep 2

exp-eyelink/bin/expeyelink -expdb LexCompRI2.db -dbdriver sqlite -session $1

#xrandr --output DVI-I-1 --mode 1280x1024 --output DP-0 --mode 1280x1024 --right-of DVI-I-1
