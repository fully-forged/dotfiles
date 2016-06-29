#!/usr/env/bin bash

function elm-new {
  if [ -n "$1" ]
  then
    echo "=== Creating project folder"
    mkdir $1
    cd $1
    echo "=== Downloading elm.mk"
    curl -o elm.mk https://raw.githubusercontent.com/cloud8421/elm.mk/master/elm.mk
    echo "=== Installing dependencies"
    make -f elm.mk install
    echo "=== Initializing git"
    git init
    echo "=== Done! Enjoy your work"
  else
    echo "Doh! You need to pass the name of the project you want to create, e.g. elm-new awesome_app";
    exit 1;
  fi
}
