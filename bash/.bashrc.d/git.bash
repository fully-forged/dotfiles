alias s='git status --short'
alias l='git lg'

fbr() {
  local branches branch
  branches=$(git branch -a) &&
    branch=$(echo "$branches" | fzf +s +m -e) &&
    git checkout $(echo "$branch" | sed "s:.* remotes/origin/::" | sed "s:.* ::")
}
