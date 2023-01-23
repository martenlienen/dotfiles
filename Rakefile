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

multitask :tools => [:pyenv, :pyenv_virtualenv, :vim_plug, :antigen]

task :pyenv do
  manage_git_repo "#{Dir.home}/.pyenv", "https://github.com/yyuu/pyenv.git"
end

task :pyenv_virtualenv => :pyenv do
  manage_git_repo "#{Dir.home}/.pyenv/plugins/pyenv-virtualenv", "https://github.com/yyuu/pyenv-virtualenv.git"
end

task :vim_plug do
  manage_git_repo "#{Dir.home}/.vim/plug", "https://github.com/junegunn/vim-plug.git"
end

task :antigen do
  manage_git_repo "#{Dir.home}/.antigen", "https://github.com/zsh-users/antigen.git"
end

multitask :packages => [:emacs, :vim_packages]

task :emacs => :dotfiles do
  ORG_FILES.each do |f|
    sh <<END
emacs --quick --batch --eval \
      "(progn (require 'ob-tangle) (org-babel-tangle-file \\"#{f}\\"))"
END
  end
end

task :vim_packages => [:vim_plug, :dotfiles] do
  sh "vim +PlugUpdate +qall"
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
cargo install exa hexyl fd-find bat ripgrep tokei gping just paru
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
de="i3-gaps wmctrl nitrogen xdotool rofi libinput-gestures"

# Python compilation requirements
pyenv="base-devel openssl zlib xzmake"

# Terminal and shell
shell="alacritty bash-completion zsh tmux"

# Cryptography
crypto="openssh"

# Utilities
utils="htop tree rsync redshift"

# Network utilities
netutils="nmap tcpdump dnsutils"

# Programming tools
programming="git vim emacs nodejs npm"

# Web
web="firefox chromium"

sudo pacman -S --needed $de $pyenv $shell $crypto $utils $netutils $programming $web

systemctl enable --now --user redshift.service

aur_packages="snapd"

paru -S --needed $aur_packages

sudo systemctl enable --now snapd.socket

snaps="todoist spotify"
snap install $snaps

# sudo ufw default allow outgoing
# sudo ufw default deny incoming
# sudo ufw allow ssh
# sudo ufw allow http
# sudo ufw allow https
END
end

task :system_conf do
  sh "sudo cp --recursive --force --preserve=mode usr /"
end

task :pipx do
  # Yes, pipx should manage itself
  packages = ["pipx", "black", "isort", "asciinema", "ansible-base", "python-lsp-server[rope]"]
  packages.each do |package|
    sh "pipx install #{package}"
  end
end

task :optimus do
  sh "sudo cp optimus-manager.conf /etc/optimus-manager"
end
