require "find"
require "rake/clean"

def manage_git_repo(path, repo)
  mkdir_p path

  if File.directory?(path + "/.git")
    sh "cd #{path} && git pull"
  else
    if File.directory?(path)
      # Allow cloning into existing directories
      Dir.mktmpdir do |dir|
        sh "git clone #{repo} #{dir}"
        sh "cp -r #{dir}/{*,.*} #{path}"
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
ELISP_BYTECODE = ELISP.ext(".elc")

task :default => [:dotfiles, :tools, :packages]

multitask :tools => [:pyenv, :pyenv_virtualenv, :cask, :vundle, :antigen]

task :pyenv do
  manage_git_repo "#{Dir.home}/.pyenv", "https://github.com/yyuu/pyenv.git"
end

task :pyenv_virtualenv => :pyenv do
  manage_git_repo "#{Dir.home}/.pyenv/plugins/pyenv-virtualenv", "https://github.com/yyuu/pyenv-virtualenv.git"
end

task :cask do
  manage_git_repo "#{Dir.home}/.cask", "https://github.com/cask/cask.git"
end

task :vundle do
  manage_git_repo "#{Dir.home}/.vim/bundle/vundle", "https://github.com/gmarik/vundle.git"
end

task :antigen do
  manage_git_repo "#{Dir.home}/.antigen", "https://github.com/zsh-users/antigen.git"
end

multitask :packages => [:emacs_packages, :vim_packages]

task :emacs_packages => [:cask, :dotfiles] do
  sh "cd #{Dir.home}/.emacs.d && cask"
end

task :vim_packages => [:vundle, :dotfiles] do
  sh "vim +BundleInstall +qall"
end

task :dotfiles => ELISP_BYTECODE do
  sh "find home -maxdepth 1 -mindepth 1 -exec cp --recursive {} #{Dir.home} \\;"
end

rule ".elc" => ".el" do |t|
  # We are not using --quick because we want to have installed libraries
  # available
  sh <<END
emacs --no-init-file --batch --funcall batch-byte-compile #{t.source}
END
end
CLEAN.include(ELISP_BYTECODE)

rule ".el" => ".org" do |t|
  sh <<END
emacs --quick --batch --eval \
      "(progn (require 'ob-tangle) (org-babel-tangle-file \\"#{t.source}\\" \\"#{t.name}\\"))"
END
end
CLEAN.include(ORG_FILES.ext(".el"))

task :system => [:pacaur, :system_packages, :system_conf, :lock_screen]

task :pacaur do
  if not system("pacman -Q pacaur")
    sh <<END
# Create build directory
dir=$(mktemp -d)
cd $dir

# Install dependencies
sudo pacman -S --noconfirm yajl expac

# Install cower
curl -o PKGBUILD https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=cower
makepkg PKGBUILD --skippgpcheck
sudo pacman -U cower*.tar.xz --noconfirm

# Install pacaur
curl -o PKGBUILD https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=pacaur
makepkg PKGBUILD
sudo pacman -U pacaur*.tar.xz --noconfirm

# Clean build directory
cd ~
rm -r $dir
END
  end
end

task :system_packages => :pacaur do
  sh <<END
# XOrg packages
xorg="xorg-server xorg-xrdb xorg-xrandr xorg-xmodmap xorg-xev"

# Desktop manager
dm="lightdm lightdm-gtk-greeter light-git"

# Window manager
wm="i3 rlwrap rofi autorandr-git"

# Sound control
sound="pulseaudio pulseaudio-alsa pavucontrol"

# Network suite
network="networkmanager network-manager-applet"

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
programming="git vim emacs the_silver_searcher"

# Web
web="firefox chromium"

pacaur -S --needed $xorg $dm $wm $network $sound $security $screenshot $shell \
       $crypto $utils $netutils $programming $web

# Start desktop manager on boot
sudo systemctl enable lightdm.service

# Autostart the firewall
sudo systemctl enable firehol.service
sudo systemctl start firehol.service

# Mute on susend
sudo systemctl enable mute-on-suspend.service
END
end

task :lock_screen => :pacaur do
  sh <<END
# Install package
pacaur -S --needed physlock

# Lock on suspend
sudo systemctl enable physlock@#{ENV["USER"]}.service
END
end

task :system_conf do
  sh "sudo cp --recursive etc usr /"
end
