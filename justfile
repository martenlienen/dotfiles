alias d := dotfiles
alias e := emacs
alias p := packages

default: dotfiles tools

dotfiles:
  rsync --keep-dirlinks --archive home/ "$HOME"
  if hash fc-cache 2> /dev/null; then fc-cache; fi
  pkill -SIGUSR1 kitty || true

systemfiles:
  sudo rsync -av etc /

packages:
  #!/bin/bash
  packages=(
    # System
    earlyoom
    # Display management
    greetd greetd-tuigreet
    # Compositor
    sway swayidle chayang swaylock swaybg xorg-xwayland
    waybar otf-font-awesome ttf-jetbrains-mono-nerd
    autotiling-rs darkman fuzzel
    # Fonts
    otf-libertinus ttf-fira-code
    # Desktop portals to let flatpaks interact with other programs
    xdg-desktop-portal-wlr # Screenshots and screensharing
    xdg-desktop-portal-gtk # Everything else
    # Audio
    pipewire-audio pipewire-pulse pipewire-alsa wireplumber playerctl
    # Screen
    brightnessctl
    # External monitors
    shikane nwg-displays
    # Applets
    network-manager-applet
    # BlueTooth
    bluetui
    # Notifications
    dunst libnotify
    # Screenshots
    grim slurp satty wl-clipboard
    # Backups
    restic resticprofile
    # Power management
    upower poweralertd
    # Permissions GUI
    polkit-gnome
    # Redshift
    wlsunset
    # Tuxedo hardware control
    #tuxedo-control-center-bin
    # Snapshots
    btrfs-assistant snapper-support
    # Applications
    firefox chromium thunderbird evince kitty gimp
    # Terminal & command line tools
    zsh tmux htop just tree rsync jq pixi
    # Network tools
    nmap tcpdump dnsutils nmon iftop
    networkmanager-openvpn
    # Cryptography
    openssh keepassxc
    # Programming. npm is required by emacs-copilot
    git vim emacs-wayland npm
    # Latex
    texlive texlive-langenglish texlive-langgerman
    # Typst
    typst
    # Spell check
    aspell-en words
    # latexindent
    perl-yaml-tiny perl-file-homedir
    # Application containers
    flatpak
    # makepkg in pacman
    base-devel
  )
  if hash yay 2> /dev/null; then
    yay -S --needed ${packages[@]}
  fi
  sudo systemctl enable earlyoom.service
  sudo systemctl enable greetd.service
  pixi global install direnv ruff yt-dlp pipx exa fd-find bat ripgrep tokei viu hexyl watchexec

user-services:
  systemctl --user enable resticprofile-backup@profile-default.timer
  systemctl --user enable --now ssh-agent.service
  systemctl --user enable --now darkman.service

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
  rustup self update
  rustup update

rust: rustup
  cargo install gping

flatpak:
  flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  flatpak install -y flathub com.spotify.Client
  flatpak install -y flathub com.todoist.Todoist
  flatpak install -y flathub com.mattermost.Desktop
  flatpak install -y flathub us.zoom.Zoom
  flatpak install -y flathub md.obsidian.Obsidian
  flatpak install -y flathub org.zotero.Zotero

tools:
  ./git_clone_or_update.sh "$HOME/.antigen" "https://github.com/zsh-users/antigen.git"

pipx:
  pipx install python-lsp-server
  pipx inject python-lsp-server pylsp-rope python-lsp-ruff

upgrade: tools rust
  pipx upgrade-all --include-injected
  pixi global update
  flatpak update -y
  zsh -ic "antigen update"
