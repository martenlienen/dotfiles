# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Look in ~/.oh-my-zsh/themes/ Optionally, if you set this to 
# "random", it'll load a random theme each time that oh-my-zsh is loaded.
ZSH_THEME="cqql"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*) Custom 
# plugins may be added to ~/.oh-my-zsh/custom/plugins/ Example format:
# plugins=(rails git textmate ruby lighthouse)
plugins=(git gitfast bundler)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

# Initialize rbenv
PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# IntelliJ
alias idea=~/IDEA12.1/bin/idea.sh

# Android SDK
export PATH=/opt/android-sdk/tools:/opt/android-sdk/platform-tools:$PATH

# Set editor
EDITOR="vim"

# Use vi for command line editing
bindkey -v

# Do not get in my way, when I type
unsetopt correct

# Do not block on Ctrl-S
stty -ixon
