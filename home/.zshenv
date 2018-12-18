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

# Use the system libstdc++ because the emulator won't start otherwise
export ANDROID_EMULATOR_USE_SYSTEM_LIBS=1

# Read system-local settings
if [[ -x ~/.local/zshenv ]]; then
  . ~/.local/zshenv
fi
