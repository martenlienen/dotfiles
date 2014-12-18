#!/bin/sh

# Load Xresources
xrdb ~/.Xresources

# Load mod-key mappings
xmodmap ~/.Xmodmap

exec xmonad
