#!/bin/sh

# This is read by greetd when starting a session, so these variables are know to sway.

# Initialize rustup
export PATH="$HOME/.cargo/bin:$PATH"

# Initialize pixi
export PATH="$HOME/.pixi/bin:$PATH"

# Make my utilities available on PATH
export PATH="$HOME/.local/bin:$PATH"

# Set editor
export EDITOR="vim"

# Register the ripgrep config
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep"

# Use ipdb as the default python debugger
export PYTHONBREAKPOINT="ipdb.set_trace"

# Enable swipe left/right gestures in firefox
export MOZ_ENABLE_WAYLAND="1"

# Use the ssh-agent started by systemd
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Tell XDG that we are running sway
export XDG_CURRENT_DESKTOP=sway

# Include .bashrc if the file is being read by bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi
