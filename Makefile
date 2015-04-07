RUBY_VERSION = "2.3.0-dev"

all: dotfiles pyenv rbenv ruby-build ruby oh-my-zsh cask emacs-packages \
	compile-elisp vundle vundle-packages

dotfiles:
	find src -maxdepth 1 -mindepth 1 -exec cp --recursive {} $$HOME \;

pyenv:
	./manage-git-repo "$$HOME/.pyenv" "https://github.com/yyuu/pyenv.git"

rbenv:
	./manage-git-repo "$$HOME/.rbenv" "https://github.com/sstephenson/rbenv.git"

ruby-build: rbenv
	./manage-git-repo "$$HOME/.rbenv/plugins/ruby-build" "https://github.com/sstephenson/ruby-build.git"

ruby: ruby-build
	./install-ruby $(RUBY_VERSION)

oh-my-zsh:
	./manage-git-repo "$$HOME/.oh-my-zsh" "git://github.com/robbyrussell/oh-my-zsh.git"

cask:
	./manage-git-repo "$$HOME/.cask" "git://github.com/cask/cask.git"

emacs-packages:
	(cd "$$HOME/.emacs.d" && cask)

compile-elisp: dotfiles
	emacs --batch --funcall batch-byte-compile $$HOME/.emacs.d/{init.el,lisp/*.el}

vundle:
	./manage-git-repo "$$HOME/.vim/bundle/vundle" "https://github.com/gmarik/vundle.git"

vundle-packages:
	vim +BundleInstall +qall
