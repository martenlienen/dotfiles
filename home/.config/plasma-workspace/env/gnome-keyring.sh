#!/bin/sh

# Start gnome-keyring as an ssh-agent replacement
eval $(gnome-keyring-daemon --start --components=ssh)
export SSH_AUTH_SOCK
