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

local red=$fg[red]
local blue=$fg[blue]
local green=$fg[green]
local yellow=$fg[yellow]
local magenta=$fg[magenta]
local cyan=$fg[cyan]
local white=$fg[white]

local red_bold=$fg_bold[red]
local blue_bold=$fg_bold[blue]
local green_bold=$fg_bold[green]
local yellow_bold=$fg_bold[yellow]
local magenta_bold=$fg_bold[magenta]
local cyan_bold=$fg_bold[cyan]
local white_bold=$fg_bold[white]

local red_no_bold=$fg_no_bold[red]
local blue_no_bold=$fg_no_bold[blue]
local green_no_bold=$fg_no_bold[green]
local yellow_no_bold=$fg_no_bold[yellow]
local magenta_no_bold=$fg_no_bold[magenta]
local cyan_no_bold=$fg_no_bold[cyan]
local white_no_bold=$fg_no_bold[white]

local highlight_bg=$bg[red]

local prompt_symbol="λ"

# The following two functions have been taken from oh-my-zsh so that I can avoid
# loading all the rest of it. Saves 0.05s on every shell start.
#
# Outputs current branch info in prompt format
function git_prompt_info() {
  local ref
  ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
  echo "%{$blue_bold%}${ref#refs/heads/} $(parse_git_dirty)%{$reset_color%}"
}

# Checks if working tree is dirty
function parse_git_dirty() {
  local STATUS=''
  local -a FLAGS=('--porcelain' '--ignore-submodules=dirty')

  STATUS=$(command git status ${FLAGS} 2> /dev/null | tail -n1)

  if [[ -n $STATUS ]]; then
    echo "%{$red_bold%}≠"
  else
    echo "%{$green_bold%}="
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

  local pyenv_info=""
  if whence pyenv > /dev/null; then
    local pyenv_version="$(pyenv version-name)"
    if [[ "${pyenv_version}" != "$(pyenv global)" ]]; then
      pyenv_info=" %{$blue%}🐍%{$reset_color%} ${pyenv_version}"
    fi
  fi

  local conda_info=""
  if [[ -n $CONDA_DEFAULT_ENV ]]; then
    conda_info=" %{$green%}🐍%{$reset_color%} ${CONDA_DEFAULT_ENV}"
  fi

  local tmux_info=""
  if [[ -z $TMUX ]]; then
    if tmux has-session &> /dev/null; then
      local sessions=("${(@f)$(tmux ls -F "#{session_name}" 2> /dev/null)}")
      if [[ ${#sessions} -gt 0 ]]; then
        tmux_info=" 🦥 $(IFS=,; echo "${sessions[*]}")"
      fi
    fi
  fi

  local directory="${PWD/#$HOME/~}"
  if [[ ${#directory} -ge 50 ]]; then
    # While the directory is too long and there are at least three parts to it
    while [[ (${#directory} -ge 50) && (${#${directory//[^\/]}} -ge 2) ]]; do
      # Remove the ... from a previous iteration
      directory="${directory/#?(~)\/...\//${directory/%\/*/}/}"

      # Replace the part after the first slash with ...
      directory="${directory/#?(~)\/[^\/]#\//${directory/%\/*/}/.../}"
    done
  fi

  local timestamp="%*"
  local info_line="\
  %{$yellow_bold%}$user\
%{$white_bold%}@\
%{$white_no_bold%}$HOST: \
%{$yellow_bold%}$directory%{$reset_color%}\
$git_info$pyenv_info$conda_info$tmux_info %{$cyan%}($timestamp)%{$reset_color%}"

  local prompt_char="%{$white_bold%}$prompt_symbol"
  if [[ $last_cmd_status -ne 0 ]]; then
    prompt_char="%{$red_bold%}!"
  fi

  print "$info_line\n$prompt_char%{$reset_color%} "
}

# Enable prompt substitution
setopt promptsubst

case "$TERM" in
"dumb")
  # Use a very simple prompt for TRAMP
  PROMPT='> '
  ;;
*)
  PROMPT='$(get_prompt)'
  ;;
esac
