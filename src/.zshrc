. ~/.antigen/antigen.zsh

# omz should not update itself
DISABLE_AUTO_UPDATE="true"

antigen use oh-my-zsh

antigen bundle git
antigen bundle bundler
antigen bundle archlinux
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-history-substring-search

# Actually a theme
antigen bundle sindresorhus/pure

# Ensure all dependencies are installed
antigen apply

# Add installed haskell binaries to PATH
export PATH="$HOME/.cabal/bin:$PATH"

# Initialize rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init - --no-rehash)"

# Initialize pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - --no-rehash)"

# Initialize cask
export PATH="$HOME/.cask/bin:$PATH"

# Make my utilities available on PATH
export PATH="$HOME/bin:$PATH"

# Android SDK
export PATH=/opt/android-sdk/tools:/opt/android-sdk/platform-tools:$PATH

# Set editor
export EDITOR="vim"

# Use emacs for gems
export BUNDLER_EDITOR="emacs"

# Use emacs bindings on the command line
bindkey -e

# Do not get in my way, when I type
unsetopt correct

# Do not block on Ctrl-S
stty -ixon

# Improve performance in ruby dev environment
export RUBY_GC_MALLOC_LIMIT=90000000
export RUBY_GC_HEAP_FREE_SLOTS=200000

# Uniquify history
setopt HIST_IGNORE_ALL_DUPS

# Use substring search
bindkey -M emacs "^P" history-substring-search-up
bindkey -M emacs "^N" history-substring-search-down

# Aliases
alias grb="git rebase"
alias grbs="git rebase --skip"
alias t="bundle exec translations"
alias s="bundle exec spring"
