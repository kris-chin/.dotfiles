#!/bin/sh

#Setup X keybindings
xmodmap ~/.xmodmap

#Customize devices with libinput
bash -c '
  $HOME/customize-device-input.sh
'
#Start i3
i3
