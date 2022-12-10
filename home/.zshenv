# Initialize rustup
export PATH="$HOME/.cargo/bin:$PATH"

# Initialize pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
. ~/.local/zsh/pyenv.zsh

# Make my utilities available on PATH
export PATH="$HOME/.local/bin:$PATH"

# Set editor
export EDITOR="vim"

# Register the ripgrep config
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep"

# Use ipdb as the default python debugger
export PYTHONBREAKPOINT="ipdb.set_trace"

# Enable swipe left/right gestures in firefox in X.org
export MOZ_USE_XINPUT2="1"

# Read system-local settings
if [[ -r ~/.local/zshenv ]]; then
  . ~/.local/zshenv
fi
