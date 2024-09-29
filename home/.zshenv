# Initialize rustup
export PATH="$HOME/.cargo/bin:$PATH"

# Initialize pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
. ~/.local/zsh/pyenv.zsh

# Initialize mamba
export MAMBA_ROOT="$HOME/.mambaforge"
if [[ -d $MAMBA_ROOT ]]; then
  . ~/.local/zsh/mamba.zsh
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
