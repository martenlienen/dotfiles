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

FILES = Find.find("home")
ORG_FILES = FileList.new(FILES.select { |f| f.end_with? ".org" })
ELISP = FileList.new(FILES.select { |f| f.end_with?(".el") })
ELISP.include(ORG_FILES.ext(".el"))
ORG_FILES.gsub!(/^home/, Dir.home)
ELISP.gsub!(/^home/, Dir.home)

task :default => [:dotfiles, :tools, :packages]

task :dotfiles do
  sh "find home -maxdepth 1 -mindepth 1 -exec cp --recursive --preserve=mode {} #{Dir.home} \\;"

  # Update the font cache
  sh "fc-cache"

  # Reload X resources database
  sh "xrdb ~/.Xresources"
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

task :emacs => [:quelpa, :compile_elisp]

task :quelpa => :dotfiles do
  sh "emacs --script home/.emacs.d/quelpa-install.el"
end

task :compile_elisp => :dotfiles do
  ORG_FILES.each do |f|
    sh <<END
emacs --quick --batch --eval \
      "(progn (require 'ob-tangle) (org-babel-tangle-file \\"#{f}\\"))"
END
  end

  ELISP.each do |f|
    # We are not using --quick because we want to have installed libraries
    # available
    sh <<END
emacs --no-init-file --batch --funcall batch-byte-compile #{f}
END
  end
end

task :vim_packages => [:vim_plug, :dotfiles] do
  sh "vim +PlugUpdate +qall"
end

task :system => [:system_packages, :system_conf]

task :yay do
  if not system("pacman -Q yay")
    sh <<END
basedir=$(pwd)

# Create build directory
dir=$(mktemp -d)
cd $dir

# Clone yay package
git clone https://aur.archlinux.org/yay.git

# Build and install
cd yay
makepkg --syncdeps --install --noconfirm

cd "${basedir}"
rm -rf $dir
END
  end
end

task :system_packages => [:yay, :system_conf] do
  sh <<END
# Desktop manager
dm="sddm"

# Desktop environment
de="plasma kdebase i3-wm wmctrl nitrogen xdotool"

# Display backlight
backlight="redshift light"

# Desktop Utilities
de_utils="rofi"

# Fonts
fonts="ttf-fira-sans ttf-fira-mono ttf-fira-code"

# Fix for bluetooth speakers
audio="pulseaudio-bluetooth"

# Security
security="firehol"

# Taking screenshots
screenshot="maim slop"

# Terminal and shell
shell="rxvt-unicode bash-completion zsh"

# Cryptography
crypto="openssl openssh gnome-keyring"

# Utilities
utils="htop tree rsync tab"

# Network utilities
netutils="nmap tcpdump dnsutils"

# Programming tools
programming="git vim emacs ripgrep"

# Web
web="firefox chromium"

yay -S --needed --noconfirm $dm $de $de_utils $fonts $audio $security \
    $screenshot $shell $crypto $utils $netutils $programming $web $backlight

# Start desktop manager on boot
sudo systemctl enable sddm.service

# Connect to the internet
sudo systemctl enable NetworkManager.service

# Autostart the firewall
sudo systemctl enable firehol.service

# Autostart redshift
systemctl --user enable redshift.service

# Add user to the video group so that they can use light
sudo usermod --append --groups video "${USER}"
END
end

task :system_conf do
  sh "sudo cp --recursive --force --preserve=mode etc usr /"
end
