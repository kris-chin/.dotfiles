#!/bin/bash

#Apparently, this is supposed to be auto-set. Otherwise, it should be set via .bashrc. but I'm gonna do it here. This is intended as some kind of fix for the fact that WSL seems do not have systemd?

NEW_DIR=/run/user/$UID/
sudo mkdir -p $NEW_DIR
sudo chmod a+rwx $NEW_DIR
export XDG_RUNTIME_DIR=$NEW_DIR
