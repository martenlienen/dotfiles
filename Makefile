all: dotfiles pyenv pyenv-virtualenv rbenv ruby-build antigen cask	\
	emacs-packages compile-elisp vundle vundle-packages

dotfiles:
	find src -maxdepth 1 -mindepth 1 -exec cp --recursive {} $$HOME \;

pyenv:
	./manage-git-repo "$$HOME/.pyenv" "https://github.com/yyuu/pyenv.git"

pyenv-virtualenv:
	./manage-git-repo "$$HOME/.pyenv/plugins/pyenv-virtualenv"				\
										"https://github.com/yyuu/pyenv-virtualenv.git"

rbenv:
	./manage-git-repo "$$HOME/.rbenv" "https://github.com/sstephenson/rbenv.git"

ruby-build: rbenv
	./manage-git-repo "$$HOME/.rbenv/plugins/ruby-build"							\
                    "https://github.com/sstephenson/ruby-build.git"

antigen:
	./manage-git-repo "$$HOME/.antigen" "git://github.com/zsh-users/antigen.git"

cask:
	./manage-git-repo "$$HOME/.cask" "git://github.com/cask/cask.git"

emacs-packages:
	(cd "$$HOME/.emacs.d" && cask)

tangle-init-org:
	emacs --quick --batch --eval									\
				"(progn 																\
					(require 'ob-tangle)									\
				  (find-file \"src/.emacs.d/init.org\")	\
				  (org-babel-tangle))"

compile-elisp: tangle-init-org dotfiles
	emacs --no-init-file --batch --funcall batch-byte-compile	\
				$$HOME/.emacs.d/{init.el,lisp/*.el}

vundle:
	./manage-git-repo "$$HOME/.vim/bundle/vundle"							\
										"https://github.com/gmarik/vundle.git"

vundle-packages: dotfiles
	vim +BundleInstall +qall
