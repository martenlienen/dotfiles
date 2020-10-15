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

  sh "regolith-look refresh"

  sh <<END
systemctl --user enable redshift-gtk.service
systemctl --user start redshift-gtk.service
END
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
# Python compilation requirements
pyenv="make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev"

# Terminal and shell
shell="rxvt-unicode bash-completion zsh ncurses-term tmux"

# Cryptography
crypto="ssh"

# Utilities
utils="htop tree rsync redshift-gtk"

# Network utilities
netutils="nmap tcpdump dnsutils"

# Programming tools
programming="git vim emacs ripgrep nodejs npm"

# Web
web="firefox chromium-browser"

sudo apt-get install --no-install-recommends --assume-yes \
     $pyenv $shell $crypto $utils $netutils $programming $web

sudo ufw default allow outgoing
sudo ufw default deny incoming
sudo ufw allow ssh
sudo ufw allow http
sudo ufw allow https
END
end

task :system_conf do
  sh "sudo cp --recursive --force --preserve=mode etc usr /"
end
