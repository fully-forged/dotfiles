function handle_error () {
  if [ "$?" != "0" ]; then
    echo -e "\n\n$1"
  fi
}

function load_and_handle_errors () {
    source "$1" 2>&1
    handle_error "There was a problem while sourcing $1\n\n\t$SOURCE_OUTPUT"
}

function load_homebrew_command_completions {
  for f in /usr/local/etc/bash_completion.d/*.bash; do
    load_and_handle_errors $f
  done
  unset f
}

function source_modules {
  for f in $HOME/.bashrc.d/*.bash; do
    load_and_handle_errors $f
  done
  unset f
}

function configure_rbenv {
  if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
}

function tmuxify_command_prompt {
  export PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#I_#P") "$PWD")'
}

function configure_editor {
  export EDITOR=/usr/local/bin/nvim
}

export PATH=/usr/local/bin:/usr/local/sbin:$PATH

load_homebrew_command_completions
source_modules
configure_editor
tmuxify_command_prompt

eval "$(direnv hook $0)"

. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

. /usr/local/opt/fzf/shell/completion.bash
. /usr/local/opt/fzf/shell/key-bindings.bash

export PATH=$HOME/bin:$PATH
