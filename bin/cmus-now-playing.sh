#!/bin/bash

if ! cmus-remote -C >/dev/null 2>&1 ; then
    echo >&2 "no music playing"
    exit 1
fi

info=$(cmus-remote -Q)

state=$(echo "$info" | sed -n 's/^status //p')
if [ "$state" = "stopped" ] ; then
    echo >&2 "no song playing currently, aborting!"
    exit 1
fi

if [ "$state" = "playing" ] ; then
  icon=">"
else
  icon="||"
fi

file=$(echo "$info" | sed -n 's/^file //p')
artist=$(echo "$info" | sed -n 's/^tag albumartist //p')
album=$(echo "$info" | sed -n 's/^tag album //p')
title=$(echo "$info" | sed -n 's/^tag title //p')
if [ -z "$title" ] ; then
    title=$(basename "$file" | sed 's/\.[A-Za-z0-9]*$//')
fi

if [ -n "$artist" ] ; then
    msg="$icon $artist - $title"
else
    msg="$icon $title"
fi

echo $msg
