# Use the simplest possible terminal for TRAMP
[[ $TERM == "tramp" ]] && unsetopt zle && PS1="$ " && return

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

# Do not beep! Under any circumstances!
unsetopt BEEP

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

# Ignore commands starting with a space
setopt HIST_IGNORE_SPACE

# Enable extended pattern matching
setopt KSH_GLOB

# File and limits
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
HISTSIZE=50000
SAVEHIST=10000

# Autocomplete case-insensitive for lowercase patterns and partial matches
zstyle ":completion:*" matcher-list "m:{a-z}={A-Za-z}" "r:|=*" "l:|=* r:|=*"

# Accept / as a word separator
WORDCHARS=$(echo $WORDCHARS | sed -e 's:/::')

# Use substring search
bindkey -M emacs "^P" history-substring-search-up
bindkey -M emacs "^N" history-substring-search-down

. ~/.local/zsh/aliases.zsh

# Read system-local settings
if [[ -r ~/.local/zshrc ]]; then
  . ~/.local/zshrc
fi
