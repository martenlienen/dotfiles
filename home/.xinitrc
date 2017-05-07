#!/bin/sh

# LightDM only reads .xprofile, but others like startx or SLiM use .xinitrc
source ~/.xprofile

exec awesome
