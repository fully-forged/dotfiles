#!/bin/bash

# Setup homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Setup temporary PATH
export PATH=/usr/local/bin/:$PATH

# Install asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf

source ~/.asdf/asdf.sh

asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git
asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git
asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git

# Install dev tools
brew install \
  autojump \
  bash \
  bash-completion \
  ctags \
  direnv \
  git \
  libsass \
  neovim/neovim/neovim \
  openssl \
  postgresql \
  reattach-to-user-namespace \
  rlwrap \
  sassc \
  ssh-copy-id \
  stow \
  the_silver_searcher \
  tmux \
  tree \
  python

# Install Neovim Python support
pip install --user --upgrade neovim

# Symlink dotfiles with GNU Stow
stow nvim bash tmux input git bin emacs
