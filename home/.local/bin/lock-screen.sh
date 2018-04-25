#!/usr/bin/env bash

# Taken from https://www.reddit.com/r/unixporn/comments/3358vu/i3lock_unixpornworthy_lock_screen/

tmp=$(mktemp --dry-run --tmpdir XXX.png)

maim --hidecursor | convert - -scale 7.5% -scale 1333.33% "$tmp"
i3lock --no-unlock-indicator --image="$tmp"

rm "$tmp"
