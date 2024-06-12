#!/bin/sh

#Setup X keybindings
xmodmap ~/.xmodmap

#Customize devices with libinput
bash -c '
  $HOME/.local/bin/customize-device-input
'

#Commented this out because apparently, running i3 via .desktop entry still runs .xinitrc? So I don't need this anymore? Weird..
#i3
