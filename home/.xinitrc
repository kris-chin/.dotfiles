#Setup X keybindings
xmodmap ~/.xmodmap

#Customize devices with libinput
bash -c '
  $HOME/customize-device-input.sh
'
