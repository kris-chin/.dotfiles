#!/bin/bash
#Instructions taken from: https://elkowar.github.io/eww/

#0. Set up the directory for creating source
export INSTALL_BASEDIR=/usr/local/src

#In case we don't have permissions, grant them
sudo chmod a+rwx $INSTALL_BASEDIR

#Install dependencies
sudo pacman -S --needed gtk3 pango gdk-pixbuf2 libdbusmenu-gtk3 cairo glib2 gcc-libs glibc

cd $INSTALL_BASEDIR

#clone the repo and build
git clone https://github.com/elkowar/eww
cd eww

cargo build --release --no-default-features --features x11

#after build, edit permissions
cd target/release

chmod +x ./eww

#copy the binary to the .local bin folder so it can be accessed
cp ./eww $HOME/.local/bin/eww
