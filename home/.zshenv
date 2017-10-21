# Add installed haskell binaries to PATH
export PATH="$HOME/.cabal/bin:$PATH"

# Initialize rustup
export PATH="$HOME/.cargo/bin:$PATH"

# Initialize pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - --no-rehash)"
eval "$(pyenv virtualenv-init -)"

# Make my utilities available on PATH
export PATH="$HOME/.local/bin:$PATH"

# Set editor
export EDITOR="vim"
