. ~/.antigen/antigen.zsh

# Add local completions. compinit is called by antigen already
fpath+=~/.local/zsh

# Install plugins
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-history-substring-search

# Ensure all dependencies are installed
antigen apply

# Load my theme
. ~/.local/zsh/cqql.zsh

# Use emacs bindings on the command line
bindkey -e

# Do not get in my way, when I type
unsetopt correct

# Do not block on Ctrl-S
stty -ixon

# Interpret directories as cd-ing into them
setopt AUTO_CD

# Quick cd-ing to parent directories
alias ...='../..'
alias ....='../../..'
alias .....='../../../..'
alias ......='../../../../..'

# Alternate cd to the last directory
alias -- -='cd -'

# Share history between terminals
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# Do not put the history command in the history
setopt HIST_NO_STORE

# Strip whitespace
setopt HIST_REDUCE_BLANKS

# Uniquify history
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_EXPIRE_DUPS_FIRST

# File and limits
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
HISTSIZE=50000
SAVEHIST=10000

# Autocomplete case-insensitive for lowercase patterns and partial matches
zstyle ":completion:*" matcher-list "m:{a-z}={A-Za-z}" "r:|=*" "l:|=* r:|=*"

# Use substring search
bindkey -M emacs "^P" history-substring-search-up
bindkey -M emacs "^N" history-substring-search-down

. ~/.zsh/aliases.zsh
