require "tmpdir"
require "find"

def manage_git_repo(path, repo)
  if File.directory?(path + "/.git")
    sh "cd #{path} && git pull"
  else
    if File.directory?(path)
      # Allow cloning into existing directories
      Dir.mktmpdir do |dir|
        sh "git clone #{repo} #{dir}"
        sh "cp --recursive #{dir}/. #{path}"
      end
    else
      sh "git clone #{repo} #{path}"
    end
  end
end

def is_backup?(f)
  file = File.basename(f)
  file.start_with? "#" or file.start_with? ".#"
end

FILES = Find.find("home")
ORG_FILES = FileList.new(FILES.select { |f| f.end_with? ".org" and not is_backup?(f)})
ORG_FILES.gsub!(/^home/, Dir.home)

task :default => [:dotfiles, :tools, :packages]

task :dotfiles do
  sh "rsync --keep-dirlinks --archive home/ #{Dir.home}"

  # Update the font cache
  sh "fc-cache"

  # Reload the kitty configuration
  sh "pkill -SIGUSR1 kitty || true"
end

multitask :tools => [:pyenv, :pyenv_virtualenv, :antigen]

task :pyenv do
  manage_git_repo "#{Dir.home}/.pyenv", "https://github.com/yyuu/pyenv.git"
end

task :pyenv_virtualenv => :pyenv do
  manage_git_repo "#{Dir.home}/.pyenv/plugins/pyenv-virtualenv", "https://github.com/yyuu/pyenv-virtualenv.git"
end

task :antigen do
  manage_git_repo "#{Dir.home}/.antigen", "https://github.com/zsh-users/antigen.git"
end

multitask :packages => [:emacs]

task :emacs => :dotfiles do
  ORG_FILES.each do |f|
    sh <<END
emacs --quick --batch --eval \
      "(progn (require 'ob-tangle) (org-babel-tangle-file \\"#{f}\\"))"
END
  end
end

task :rustup do
  sh <<END
if ! type cargo > /dev/null; then
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi
END
end

task :rust => :rustup do
  sh <<END
cargo install exa hexyl fd-find bat ripgrep tokei gping viu
END
end

task :system => [:system_packages, :system_conf]

task :system_packages => [:system_conf] do
  sh <<END
# Desktop environment
de="i3-wm python3-i3ipc picom rofi nitrogen redshift-gtk autorandr xdotool libnotify-bin"
# libinput-gestures"

# Python compilation requirements
pyenv="build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl libncursesw5-dev xz-utils tk-dev libxml2-dev libffi-dev liblzma-dev"
# The pyenv wiki says that the following is also required but I cannot install it on debian bookworm and it seems to work without it
# "libxmlsec1-dev"

# Terminal and shell
shell="bash-completion zsh tmux"

# Cryptography
crypto="openssh-client"

# Utilities
utils="htop tree rsync"

# Network utilities
netutils="nmap tcpdump dnsutils"

# Programming tools
programming="git vim"

# Web
web="firefox chromium thunderbird evince"

latex="texlive texlive-latex-extra texlive-science"

# Applications
apps="flatpak"

sudo apt-get install $de $shell $utils $netutils $programming $web $apps $pyenv $cryptography $latex

# Install a recent emacs
sudo apt-get install -t bookworm-backports emacs
END
end

task :flathub => [:system_packages] do
  sh "flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo"
  sh "flatpak install flathub com.spotify.Client"
  sh "flatpak install flathub com.todoist.Todoist"
  sh "flatpak install flathub com.mattermost.Desktop"
  sh "flatpak install flathub us.zoom.Zoom"
  sh "flatpak install flathub md.obsidian.Obsidian"
  sh "flatpak install flathub org.zotero.Zotero"
end

task :system_conf do
  sh "sudo cp --recursive --force --preserve=mode etc /"
end

task :i3_in_plasma do
  sh "systemctl --user daemon-reload"
  sh "systemctl --user mask plasma-kwin_x11.service"
  sh "systemctl --user enable plasma-i3.service"
end

task :redshift do
  sh "systemctl --user daemon-reload"
  sh "systemctl --user enable --now redshift-gtk.service"
end

task :resticprofile do
  sh "resticprofile schedule"
end

task :pipx do
  # Yes, pipx should manage itself
  packages = [
    "pipx",
    "ruff",
    "asciinema",
    "youtube-dl",
    "python-lsp-server",
  ]
  packages.each do |package|
    sh "pipx install '#{package}'"
  end
  sh "pipx inject python-lsp-server pylsp-rope"
  sh "pipx inject python-lsp-server python-lsp-ruff"
end
