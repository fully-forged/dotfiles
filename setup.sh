# Setup homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

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
  tree

# Symlink dotfiles with GNU Stow
stow nvim
