# Initialize rustup
export PATH="$HOME/.cargo/bin:$PATH"

# Initialize pixi
export PATH="$HOME/.pixi/bin:$PATH"

# Initialize direnv
if hash direnv 2> /dev/null; then
  # Cached output of `direnv hook zsh`
  _direnv_hook() {
    trap -- '' SIGINT
    eval "$(direnv export zsh)"
    trap - SIGINT
  }
  typeset -ag precmd_functions
  if (( ! ${precmd_functions[(I)_direnv_hook]} )); then
    precmd_functions=(_direnv_hook $precmd_functions)
  fi
  typeset -ag chpwd_functions
  if (( ! ${chpwd_functions[(I)_direnv_hook]} )); then
    chpwd_functions=(_direnv_hook $chpwd_functions)
  fi
fi

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

# Read system-local settings
if [[ -r ~/.local/zshenv ]]; then
  . ~/.local/zshenv
fi
