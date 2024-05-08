#!/bin/sh

#Setup X keybindings
xmodmap ~/.xmodmap

#Customize devices with libinput
bash -c '
  $HOME/.local/bin/customize-device-input
'
#Start i3
i3
