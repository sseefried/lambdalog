---
title: Context sensitive aliases for git
tags: git
---
Context sensitive aliases for git

As developers many of us use git. Recently Peteris Krumins wrote a post about git aliases. The basic idea was to make aliases such as

~~~{.bash}
alias gs='git status'
~~~

for common git commands. However, we can already see a problem. The command for GhostScript is gs. What we really need is context sensitive aliases so that the command gs means git status when one is inside a git repository and gs everywhere else in the directory hierarchy. I whipped up a nice little bash script to generate context sensitive aliases.

~~~{.bash}
#!/bin/bash

if [ $# -lt 2 ]; then
  exit 1
fi;

ALIAS_NAME=$1
shift
CMD=$1
shift
(git rev-parse --is-inside-work-tree > /dev/null 2>&1)
if [ $? -eq 0 ]; then 
  $CMD "$@"
else 
  $ALIAS_NAME "$@"
fi
~~~

By checking the return status of git status one can decide whether to call one command or another. You can then define the aliases in a .bashrc (or similar) configuration file.

~~~{.bash}
alias ga='git-alias ga   "git add"'
alias gds='git-alias gds "git diff --staged"'
alias ga='git-alias ga  "git add"'
alias gp='git-alias gp  "git push"'
alias gl='git-alias gl  "git log"'
alias gs='git-alias gs  "git status"'
alias gd='git-alias gd "git diff"'
alias gc='git-alias gc "git commit"'
alias gm='git-alias gm "git commit -m"'
alias gma='git-alias gma "git commit -am"'
alias gb='git-alias gb "git branch"'
alias gc='git-alias gc "git checkout"'
alias gra='git-alias gra "git remote add"'
alias grr='git-alias grr "git remote rm"'
alias gpu='git-alias gpu "git pull"'
alias gcl='git-alias gcl "git clone"'     
~~~

The idea of context sensitive aliases is by no means limited to git. It could be used anywhere where you have a way of detecting whether a property (e.g. whether you’re in a repository or not) holds within a directory hierarchy.
