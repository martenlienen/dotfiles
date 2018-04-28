# My zsh theme adapted from zeta-theme [1]
#
# It is optimized for performance because losing 100ms just for prompt rendering
# is useless and wasteful. This means that function and program calls are
# minimized. So everything is in a single function and it uses the built-in
# print instead of echo.
#
# [1]: https://github.com/skylerlee/zeta-zsh-theme

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

# Git info.
ZSH_THEME_GIT_PROMPT_PREFIX="%{$blue_bold%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$green_bold%} ✔"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$red_bold%} ✘"

function get_prompt {
  local user="%n"
  if [[ "$USER" == "root" ]]; then
    user="%{$highlight_bg%}%{$white_bold%}$user%{$reset_color%}"
  fi

  local git_info=""
  if [[ -n $(git rev-parse --is-inside-work-tree 2>/dev/null) ]]; then
    git_info=" <$(git_prompt_info)>"
  fi

  local directory="${PWD/#$HOME/~}"
  local timestamp="%*"
  local info_line="\
%{$blue%}# \
%{$green_bold%}$user\
%{$blue%}@\
%{$cyan_bold%}$HOST: \
%{$yellow_bold%}$directory%{$reset_color%}\
$git_info %{$blue%}($timestamp)%{$reset_color%}"

  local prompt_color="%{$magenta_bold%}"
  if [[ $? -ne 0 ]]; then
    prompt_color="%{$red_bold%}"
  fi

  print -P "$info_line\n$prompt_color$zeta %{$reset_color%}"
}

PROMPT='$(get_prompt)'
