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
cargo install exa hexyl fd-find bat ripgrep tokei gping just
END
end

task :linting do
  sh <<END
npm install --global --prefix ~/.local \
            textlint \
            textlint-plugin-latex2e \
            textlint-plugin-rst \
            textlint-rule-write-good \
            textlint-rule-no-start-duplicated-conjunction \
            textlint-rule-max-comma \
            textlint-rule-stop-words \
            textlint-rule-alex
END
end

task :system => [:system_packages, :system_conf]

task :system_packages => [:system_conf] do
  sh <<END
# Desktop environment
de="i3 python3-i3ipc picom rofi nitrogen redshift-gtk autorandr"
# libinput-gestures"

# Python compilation requirements
pyenv="build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev"

# Terminal and shell
shell="alacritty bash-completion zsh tmux"

# Cryptography
crypto="openssh-client"

# Utilities
utils="htop tree rsync"

# Network utilities
netutils="nmap tcpdump dnsutils"

# Programming tools
programming="git vim emacs"

# Web
web="chromium"

# Applications
apps="flatpak notion-app-enhanced"

sudo apt-get install $pyenv $cryptography

# Install dev tools from debian unstable
sudo apt-get install -t unstable $de $shell $utils $netutils $programming $web $apps
END
end

task :flathub => [:system_packages] do
  sh "flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo"
  sh "flatpak install flathub com.spotify.Client"
  sh "flatpak install flathub com.todoist.Todoist"
  sh "flatpak install flathub com.mattermost.Desktop"
  sh "flatpak install flathub us.zoom.Zoom"
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
  sh "systemctl --user enable redshift-gtk.service"
end

task :pipx do
  # Yes, pipx should manage itself
  packages = ["pipx", "black", "isort", "asciinema", "ansible-base", "python-lsp-server"]
  packages.each do |package|
    sh "pipx install #{package}"
  end
  sh "pipx inject python-lsp-server pylsp-rope"
end

task :optimus do
  sh "sudo cp optimus-manager.conf /etc/optimus-manager"
end
