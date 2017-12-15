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
  fzf \
  git \
  libsass \
  neovim/neovim/neovim \
  openssl \
  postgresql \
  reattach-to-user-namespace \
  ripgrep \
  rlwrap \
  sassc \
  ssh-copy-id \
  stow \
  the_silver_searcher \
  tmux \
  tree \
  python \
  python3 \
  vim

# Install Neovim Python support
pip install --user --upgrade neovim
pip3 install --user --upgrade neovim

mkdir -p ~/.nvim/undodir

# Symlink dotfiles with GNU Stow
stow nvim vim bash tmux input git bin emacs
