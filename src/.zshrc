. ~/.antigen/antigen.zsh

# omz should not update itself
DISABLE_AUTO_UPDATE="true"

antigen use oh-my-zsh

antigen bundle git
antigen bundle archlinux
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-history-substring-search

antigen theme steeef

# Ensure all dependencies are installed
antigen apply

# Use emacs bindings on the command line
bindkey -e

# Do not get in my way, when I type
unsetopt correct

# Do not block on Ctrl-S
stty -ixon

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
