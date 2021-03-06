set -x EDITOR nvim

set -x PATH $HOME/bin $PATH
set -x PATH $HOME/.local/bin $PATH

set -x FZF_DEFAULT_COMMAND 'rg --files'
set -x FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND

set -x MEMENTO_BASE_URL 'https://memento.fullyforged.com'

set -x ERL_AFLAGS '-kernel shell_history enabled'

source ~/.kiex/scripts/kiex.fish
source $HOME/.kiex/elixirs/.elixir-1.6.4.env.fish

function s
  git status --short
end

function l
  git lg
end

function reflog
  git reflog --pretty=raw | tig --pretty=raw
end

function cl
  git branch --merged | egrep -v "(^\*|master|develop|feat|live)" | xargs git branch -d
end

function cr
  git remote prune origin
end

function m
  memento | less -r
end

eval (direnv hook fish)
