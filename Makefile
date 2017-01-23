all: dotfiles pyenv pyenv-virtualenv antigen cask emacs-packages compile-elisp \
	vundle vundle-packages vicious

dotfiles: tangle-init-org
	find src -maxdepth 1 -mindepth 1 -exec cp --recursive {} $$HOME \;

pyenv:
	./manage-git-repo "$$HOME/.pyenv" "https://github.com/yyuu/pyenv.git"

pyenv-virtualenv:
	./manage-git-repo "$$HOME/.pyenv/plugins/pyenv-virtualenv"				\
										"https://github.com/yyuu/pyenv-virtualenv.git"

antigen:
	./manage-git-repo "$$HOME/.antigen" "git://github.com/zsh-users/antigen.git"

cask:
	./manage-git-repo "$$HOME/.cask" "git://github.com/cask/cask.git"

emacs-packages:
	(cd "$$HOME/.emacs.d" && cask)

.PHONY: tangle-init-org
tangle-init-org: src/.emacs.d/init.el

src/.emacs.d/init.el: src/.emacs.d/init.org
	emacs --quick --batch --eval									\
				"(progn 																\
					(require 'ob-tangle)									\
				  (find-file \"src/.emacs.d/init.org\")	\
				  (org-babel-tangle))"

compile-elisp: dotfiles
	emacs --no-init-file --batch --funcall batch-byte-compile	\
				$$HOME/.emacs.d/{init.el,lisp/*.el}

vundle:
	./manage-git-repo "$$HOME/.vim/bundle/vundle"							\
										"https://github.com/gmarik/vundle.git"

vundle-packages: dotfiles
	vim +BundleInstall +qall

vicious:
	./manage-git-repo "$$HOME/.config/awesome/vicious"	\
									 "http://git.sysphere.org/vicious"

.PHONY : services
services:
	sudo systemctl enable physlock@$$USER.service
