# Initialize direnv
if hash direnv 2> /dev/null; then
  # Cached output of `direnv hook zsh`
  _direnv_hook() {
    trap -- '' SIGINT
    eval "$(direnv export zsh)"
    trap - SIGINT
  }
  typeset -ag precmd_functions
  if (( ! ${precmd_functions[(I)_direnv_hook]} )); then
    precmd_functions=(_direnv_hook $precmd_functions)
  fi
  typeset -ag chpwd_functions
  if (( ! ${chpwd_functions[(I)_direnv_hook]} )); then
    chpwd_functions=(_direnv_hook $chpwd_functions)
  fi
fi

. ~/.profile

# Read system-local settings
if [[ -r ~/.local/zshenv ]]; then
  . ~/.local/zshenv
fi
