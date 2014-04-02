# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Enable 256 color support
TERM="xterm-256color"

# Set name of the theme to load. Look in ~/.oh-my-zsh/themes/ Optionally, if you set this to
# "random", it'll load a random theme each time that oh-my-zsh is loaded.
ZSH_THEME="cqql"

# omz should not update itself
DISABLE_AUTO_UPDATE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*) Custom
# plugins may be added to ~/.oh-my-zsh/custom/plugins/ Example format:
# plugins=(rails git textmate ruby lighthouse)
plugins=(git gitfast bundler archlinux)

# Initialize rbenv
PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init - --no-rehash)"

# Initialize cask
PATH="$HOME/.cask/bin:$PATH"

# Load omz after initializing PATH
source $ZSH/oh-my-zsh.sh

# Android SDK
export PATH=/opt/android-sdk/tools:/opt/android-sdk/platform-tools:$PATH

# Set editor
EDITOR="vim"

# Use emacs bindings on the command line
bindkey -e

# Do not get in my way, when I type
unsetopt correct

# Do not block on Ctrl-S
stty -ixon

# Improve performance in ruby dev environment
export RUBY_GC_MALLOC_LIMIT=90000000
export RUBY_GC_HEAP_FREE_SLOTS=200000

# Load SSH keys
eval `keychain --eval --quiet cqql`
