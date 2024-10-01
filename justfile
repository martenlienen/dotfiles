alias d := dotfiles
alias e := emacs
alias p := packages

default: dotfiles tools

dotfiles:
  rsync --keep-dirlinks --archive home/ "$HOME"
  fc-cache
  pkill -SIGUSR1 kitty || true

packages:
  #!/bin/bash
  packages=(
    # Tuxedo hardware control
    tuxedo-control-center-bin
    # Snapshots
    btrfs-assistant snapper-support
    # Applications
    firefox chromium thunderbird evince kitty
    # Terminal & command line tools
    zsh tmux htop just tree rsync
    # Network tools
    nmap tcpdump dnsutils nmon iftop
    networkmanager-openvpn
    # Cryptography
    openssh keepassxc
    # Programming. npm is required by emacs-copilot
    git vim emacs-wayland ttf-fira-code npm
    # Latex
    texlive
    # Application containers
    flatpak
    # pyenv dependencies to compile python
    base-devel openssl zlib xz tk
  )
  yay -S --needed ${packages[@]}

user-services:
  systemctl --user enable resticprofile-backup@profile-default.timer
  systemctl --user enable --now ssh-agent.service

emacs: dotfiles
  #!/bin/sh
  for f in $(find "home/.emacs.d" -name "*.org"); do
    emacs --quick --batch --eval \
      "(progn (require 'ob-tangle) (org-babel-tangle-file \"$HOME/${f:5}\"))"
  done

rustup:
  #!/bin/sh
  if ! command -v cargo 2>&1 > /dev/null; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  fi

rust: rustup
  cargo install exa hexyl fd-find bat ripgrep tokei gping viu

flatpak:
  flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  flatpak install -y flathub com.spotify.Client
  flatpak install -y flathub com.todoist.Todoist
  flatpak install -y flathub com.mattermost.Desktop
  flatpak install -y flathub us.zoom.Zoom
  flatpak install -y flathub md.obsidian.Obsidian
  flatpak install -y flathub org.zotero.Zotero

tools:
  ./git_clone_or_update.sh "$HOME/.pyenv" "https://github.com/yyuu/pyenv.git"
  ./git_clone_or_update.sh "$HOME/.pyenv/plugins/pyenv-virtualenv" "https://github.com/yyuu/pyenv-virtualenv.git"
  ./git_clone_or_update.sh "$HOME/.antigen" "https://github.com/zsh-users/antigen.git"

pipx:
  pipx install ruff asciinema yt-dlp python-lsp-server
  pipx inject python-lsp-server pylsp-rope python-lsp-ruff

pipx-upgrade:
  pipx upgrade-all --include-injected
