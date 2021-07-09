#!/bin/bash

# Upgrade all python executable symlinks in pipx venvs
#
# When pipx installs a package, it symlinks the current python interpreter into the
# application venv. If I then later upgrade my python version with pyenv, the linked
# interpreter disappears and the links need to be updated.

for f in $(find ~/.local/pipx/ -type l -name "python*"); do
  executable=$(basename "${f}")
  new=$(pyenv which "${executable}")
  found=$?
  rm $f
  if [[ $found -eq 0 ]]; then
    ln --symbolic --no-target-directory "${new}" "${f}"
  fi
done
