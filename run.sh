#!/bin/bash -eu

echo $1
xrandr --output DVI-I-1 --mode 1024x768 --output DP-0 --off
sleep 2

exp-eyelink/bin/expeyelink -expdb LexCompRI3.db -dbdriver sqlite -session $1 -width 1024 -height 768

xrandr --output DVI-I-1 --auto --output DP-0 --auto --right-of DVI-I-1

