#!/usr/bin/env bash

if command -v chromium > /dev/null; then
  # Arch Linux
  chromium
else
  # Ubuntu
  chromium-browser
fi
