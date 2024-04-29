#!/bin/bash

#0. Set up the directory for creating source
export INSTALL_BASEDIR=/usr/local/src

#In case we dont have permissions to add a folder there
sudo chmod a+rwx $INSTALL_BASEDIR

#install dependencies
sudo pacman -S --needed git base-devel

#1. Clone the official yay repo (SPECIFICIALLY with the most recent changes)
cd $INSTALL_BASEDIR

git clone https://aur.archlinux.org/yay.git

#3. Set up Config options and then Build Yay
cd yay

makepkg -si
