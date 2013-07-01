function collapse_pwd {
  if [ $HOME = `pwd` ]; then
    echo "~"
  else
    # Remove trailing /
    normalized_home=$HOME
    
    if [ $HOME[${#HOME}] = "/" ]; then
      normalized_home=${HOME:0:${#HOME} - 1}
    fi

    echo $(pwd | sed -e "s,^$normalized_home,~,")
  fi
}

PROMPT='
%{$fg_bold[cyan]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%} in %{$fg_bold[green]%}$(collapse_pwd)%{$reset_color%} $(git_prompt_info)
$ '

ZSH_THEME_GIT_PROMPT_PREFIX="on %{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%} !%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%} ✓%{$reset_color%}"
