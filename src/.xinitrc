#!/bin/sh

# Load mod-key mappings
xmodmap ~/.Xmodmap

# Load X settings
xrdb -merge ~/.Xresources

exec xmonad
