#!/bin/sh

# Configuration
# Which ruby version to install
RUBY_VERSION="2.1.0-rc1"

# Which gems to install
GEMS="pry pry-rails awesome_print bundler spring spring-commands-rspec"

# Indents all output that is piped through it.
indent () {
  sed "s/^/  /"
}

log () {
  echo $1
}

is_git_repository () {
  if [[ -d "$1/.git" ]]; then
    return 0
  else
    return 1
  fi
}

# Manages a git repository.
#
# If the repository does not exist, it will be cloned. If it already exists, it
# will be updated via `git pull`.
#
# $1: name for the repository
# $2: clone path
# $3: repository URL
manage_git_repository () {
  if is_git_repository $2; then
    log "Update $1"
    (cd $2 && git pull | indent)
  else
    log "Clone $1"

    # Allow cloning into existing directories
    name=$(mktemp -u)

    if [[ -d $2 ]]; then
      mv $2 $name
    fi

    git clone $3 $2 | indent

    if [[ -d $name ]]; then
      cp -r $name/* $2
      rm -r $name
    fi
  fi
}

install_rbenv () {
  manage_git_repository "rbenv" "$HOME/.rbenv" "https://github.com/sstephenson/rbenv.git"
}

install_ruby_build () {
  manage_git_repository "ruby-build" "$HOME/.rbenv/plugins/ruby-build" "https://github.com/sstephenson/ruby-build.git"
}

install_ruby_version () {
  is_ruby_version_installed $RUBY_VERSION

  if [[ $? -eq 1 ]]; then
    log "Install ruby $RUBY_VERSION"
    rbenv install $RUBY_VERSION | indent
  fi

  log "Choose global ruby version $RUBY_VERSION"
  rbenv global $RUBY_VERSION | indent
}

is_ruby_version_installed () {
  if [[ `rbenv versions` == *$1* ]]; then
    return 0
  else
    return 1
  fi
}

install_oh_my_zsh () {
  manage_git_repository "oh-my-zsh" "$HOME/.oh-my-zsh" "git://github.com/robbyrussell/oh-my-zsh.git"
}

# Copies the dotfiles from src to the home directory.
install_dotfiles () {
  log "Install dotfiles"
  find src -maxdepth 1 -mindepth 1 -exec cp --recursive {} $HOME \;
}

install_gems () {
  echo "Install gems"

  for gem in $GEMS; do
    if gem query --installed --name-matches "$gem" > /dev/null; then
      echo "Already installed $gem"
    else
      log "Install $gem"
      gem install "$gem" | indent
    fi
  done

  rehash_rbenv
}

rehash_rbenv () {
  echo "Rehash rbenv"
  rbenv rehash | indent
}

install_dotfiles
log
install_rbenv
log
install_ruby_build
log
install_ruby_version
log
install_oh_my_zsh
log
install_gems
log

log "Please restart your shell"
