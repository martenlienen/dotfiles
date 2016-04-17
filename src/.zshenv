# Add installed haskell binaries to PATH
export PATH="$HOME/.cabal/bin:$PATH"

# Initialize rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init - --no-rehash)"

# Initialize pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - --no-rehash)"
eval "$(pyenv virtualenv-init -)"

# Initialize cask
export PATH="$HOME/.cask/bin:$PATH"

# Make my utilities available on PATH
export PATH="$HOME/bin:$PATH"

# Android SDK
export PATH="/opt/android-sdk/tools:/opt/android-sdk/platform-tools:$PATH"

# Set editor
export EDITOR="vim"

# Use emacs for gems
export BUNDLER_EDITOR="emacs"

# Improve performance in ruby dev environment
export RUBY_GC_MALLOC_LIMIT=90000000
export RUBY_GC_HEAP_FREE_SLOTS=200000
