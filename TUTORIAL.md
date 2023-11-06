# Hey Chin so this file is to help you get re-aquainted with terminal stuff in Arch!!

## Using your kbdx password database 

Your kbdx password database is synced up with google drive. We do this via `rclone` and we have a special script called `sync_passwords.sh` to help us out

We have keepassXC installed on this machine, so just run `keepassxc` to open keepassxc and view your passwords

### keyboard shortcut for keepassXC

I have it mapped to `META + SHIFT + V`

## Connecting to Wifi via terminal

`network-manager` has a CLI called `nmcli` that you can use to connect to wifi via your wifi device.

Use `nmcli device wifi` to look for access points nearby

Use `nmcli device wifi connect [ACCESS_POINT_NAME] password [PASSWORD]` to connect to an access point

### Captive Portals

if you are using a wifi network that has a captive portal, try connecting to:
http://neverssl.com

going through http should probably trigger the portal. also try firefox?

## i3 and X (our desktop environment)

### i3 and X distinctions 

i3 is built on top of X.
X is a server that lets you run graphics on your screen
i3 is a window manager that manages windows on top of X

X is either X11 (latest version of X) or X.org (open source version of X)

That means when you run 'i3 DE', you are really running X Server.
Every process that interacts with this server is a client (i3, any window)

This means to change dpi, we need to configure X in this file:
`~/.Xresources`

(TODO)

### configuring i3

i3 configuration is stored in `~/.config/i3/`
(this is where you can also change keybinds)

### useful i3 keybinds 

#### useful programs:
dmenu - application launcher
xdotool - simulate keyboard presses in X
nemo - my file manager

some of these may be different from the ones I use at work (kinda weird, maybe I'll change it to reflect that)

`MOD + f` fullscreen window

`MOD + d` open dmenu, which is an application launcher that hides the terminal in the background

`shift + MOD + Space` - toggle floating for window
`shift + MOD + q` - close focused window
`MOD + r` - resize mode

### i3 - splitting windows

the way that i3 splits it's windows is through "split" modes.
when you are in "vertical" mode, new windows will split vertically,
when you are in "horizontal" mode, new windows will split horizontally

### Reloading i3 config

messages are sent with `i3-msg`

use `i3-msg reload` (`MOD + SHIFT + c)` to reload
use `i3-msg restart` (`MOD + SHIFT + R)` to restart

### window compositing

we autorun a program called `picom` on i3 that enables window transparency and rounded windows

### configuring i3 colors

i3's colors can be changed in the i3 config, however, there is an easy website to help with the colors
https://thomashunter.name/i3-configurator/

there's also this which auto generates colors:
https://github.com/dylanaraps/pywal

## setting background image / wallpaper

we use a program called `feh` to set the background image. we do this via

`feh --bg-scale [IMAGE DIR]`

this is set again in i3 config on startup

## configuring the themes

we can set up X11 to use GTK themes to configure the common colors for all of our windows 
we do this via an app called `lxappearance`, which customizes X11

## What happens when you initially start up arch linux? (what is the login screen we see?)

(TODO)

## xrandr - X11 + HDMI cable / other monitor
you can use `xrandr` as a diagnostic tool for your monitors and sending certain commands to configure them
this is good for disabling monitors in laptop mode, setting refresh rate, changing DPI, etc

also consider writing a script to easily jump to 144HZ for certian monitors, as well as disabling certain monitors when in widescreen mode
we have some scripts! `ultrawide_display.sh` will run the commands needed to switch to the ultrawide display 

## xrandr-brightness

i installed a program called `brightness-xrandr` to control the screen brightness via xrandr

#shutting down
there is not yet a key command for this from my understanding and this still needs to be made. for now I've just been manually shutting down with the power button

# closing windows
META + SHIFT + Q
