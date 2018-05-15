# The cached output of $(pyenv init - --no-rehash)
export PATH="$HOME/.pyenv/shims:${PATH}"
export PYENV_SHELL=zsh
source "$HOME/.pyenv/libexec/../completions/pyenv.zsh"
pyenv() {
  local command
  command="${1:-}"
  if [ "$#" -gt 0 ]; then
    shift
  fi

  case "$command" in
  activate|deactivate|rehash|shell)
    eval "$(pyenv "sh-$command" "$@")";;
  *)
    command pyenv "$command" "$@";;
  esac
}
