# My zsh theme adapted from zeta-theme [1]
#
# It is optimized for performance because losing 100ms just for prompt rendering
# is useless and wasteful. This means that function and program calls are
# minimized. So everything is in a single function and it uses the built-in
# print instead of echo.
#
# [1]: https://github.com/skylerlee/zeta-zsh-theme

# Load zsh color definitions
autoload -U colors
colors

local black=$fg[black]
local red=$fg[red]
local blue=$fg[blue]
local green=$fg[green]
local yellow=$fg[yellow]
local magenta=$fg[magenta]
local cyan=$fg[cyan]
local white=$fg[white]

local black_bold=$fg_bold[black]
local red_bold=$fg_bold[red]
local blue_bold=$fg_bold[blue]
local green_bold=$fg_bold[green]
local yellow_bold=$fg_bold[yellow]
local magenta_bold=$fg_bold[magenta]
local cyan_bold=$fg_bold[cyan]
local white_bold=$fg_bold[white]

local highlight_bg=$bg[red]

local zeta='ζ'

# The following two functions have been taken from oh-my-zsh so that I can avoid
# loading all the rest of it. Saves 0.05s on every shell start.
#
# Outputs current branch info in prompt format
function git_prompt_info() {
  local ref
  ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
  echo "%{$blue_bold%}${ref#refs/heads/}$(parse_git_dirty)%{$reset_color%}"
}

# Checks if working tree is dirty
function parse_git_dirty() {
  local STATUS=''
  local -a FLAGS=('--porcelain' '--ignore-submodules=dirty')

  STATUS=$(command git status ${FLAGS} 2> /dev/null | tail -n1)

  if [[ -n $STATUS ]]; then
    echo "%{$red_bold%} ✘"
  else
    echo "%{$green_bold%} ✔"
  fi
}

# Enable extended globbing for the #-pattern feature
setopt extended_glob

function get_prompt {
  # Store the exit code of the last command before overwriting it by running git
  local last_cmd_status=$?

  local user="%n"
  if [[ "$USER" == "root" ]]; then
    user="%{$highlight_bg%}%{$white_bold%}$user%{$reset_color%}"
  fi

  local git_info=""
  if [[ -n $(git rev-parse --is-inside-work-tree 2>/dev/null) ]]; then
    git_info=" <$(git_prompt_info)>"
  fi

  local directory="${PWD/#$HOME/~}"
  if [[ ${#directory} -ge 50 ]]; then
    # While the directory is too long and there are at least three parts to it
    while [[ (${#directory} -ge 50) && (${#${directory//[^\/]}} -ge 2) ]]; do
      # Remove the ... from a previous iteration
      directory="${directory/#?\/...\//${directory/%\/*/}/}"

      # Replace the part after the first slash with ...
      directory="${directory/#?\/[^\/]#\//${directory/%\/*/}/.../}"
    done
  fi

  local timestamp="%*"
  local info_line="\
%{$blue%}# \
%{$green_bold%}$user\
%{$blue%}@\
%{$cyan_bold%}$HOST: \
%{$yellow_bold%}$directory%{$reset_color%}\
$git_info %{$blue%}($timestamp)%{$reset_color%}"

  local prompt_char="%{$magenta_bold%}$zeta"
  if [[ $last_cmd_status -ne 0 ]]; then
    prompt_char="%{$red_bold%}!"
  fi

  print "$info_line\n$prompt_char%{$reset_color%} "
}

# Enable prompt substitution
setopt promptsubst

PROMPT='$(get_prompt)'
