# Hey Chin so this file is to help you get re-aquainted with terminal stuff in Arch!!

## Using your kbdx password database

Your kbdx password database is synced up with google drive. We do this via `rclone` and we have a special script called `sync_passwords.sh` to help us out

We have keepassXC installed on this machine, so just run `keepassxc` to open keepassxc and view your passwords

## rclone and ~/.config
rclone saves remote information in the .config folder, this contains sensitive information, so it's not committed to this repo.
however, when boostrapping, this will be empty. you will need to manually add the remotes again (specificallu gdrive)

### keyboard shortcut for keepassXC

I have it mapped to `META + SHIFT + V`

## Connecting to Wifi via terminal

`network-manager` has a CLI called `nmcli` that you can use to connect to wifi via your wifi device.

Use `nmcli device wifi` to look for access points nearby

Use `nmcli device wifi connect [ACCESS_POINT_NAME] password [PASSWORD]` to connect to an access point

### GUI

There's also the networkManager GUI, called `nm-connection-editor`, feel free to use that as well, since typing in WIFI by name is annoying

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
thunar - my file manager

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

`feh --bg-[BG COMMAND] [IMAGE DIR]`
see `man feh` for more info.

this is set again in i3 config on startup to keep the background

NOTE: do not commit or edit **.fehbg**. This file gets auto-written to when the `feh` command is called with a background arg.
However, for switchable themes, it would make sense to override this. But dont actually include a default .fehbg with the dotfiles!

## configuring the themes (keywords: gtk, GTK)

any application that uses gtk to render GUI will read the current GTK theme that is set.

**we don't need to modify any files** because we use an app called `lxappearance` to set our themes

### gtk themes and WPG

our GTK theme is able to be modified by wpg because we set our GTK theme to FlatColor. This theme is provided by wpgtk and is auto-set by wpgtk whenever you change colors.

if we use a different theme, wpg won't be able to modify the colors of it. to continue, we either would need to modify the new theme or just stay on FlatColof

tbh, flatcolor is fine. i think it looks nice when styled with colors correctly.

## xrandr - X11 + HDMI cable / other monitor

you can use `xrandr` as a diagnostic tool for your monitors and sending certain commands to configure them
this is good for disabling monitors in laptop mode, setting refresh rate, changing DPI, etc

also consider writing a script to easily jump to 144HZ for certian monitors, as well as disabling certain monitors when in widescreen mode
we have some scripts! `ultrawide_display.sh` will run the commands needed to switch to the ultrawide display

## xrandr-brightness

i installed a program called `brightness-xrandr` to control the screen brightness via xrandr

# shutting down, rebooting, logging out
So there is a command to shut down, and reboot. It is `reboot` and `poweroff` respectively. however, apparently, these commands require some sort of authentication to work.
Apparently, you can add these commands to a group in `/etc/sudoers` and add a user to that group in order to prevent the password.
I can't tell if I was able to get it to work or not, but it seems like I can finally run the commands again.

## logging out
This one doesn't have a command. I did set an alias for it though. For some reason, it bugs out with `ly` if we do the standard way to log out. Instead, we should kill all of the processess assocaited with a user in order to get the logout to work.


# closing windows

META + SHIFT + Q

# Remaping Keys

For X11 (which is what I have right now), you should use `xbindkeys`/`xmodmap`.

If you ever decide to use Wayland, you'll need a different solution, there are a few. consider `evremap`

Also, just a reminder, you ALSO have i3 bindings. These are in `~/.config/i3/`

## remapping keys in X

There seems to be two tools for binding keys in X. We have `xkindkeys` and `xmodmap`
I think `xmodmap` is for modifying the literal mappings, whereas `xbindkeys` is for running commands via keybindings. (i think I have some configs in i3, but this is for X itself)

Regarding `xmodmap`, the configs are stored in `~/.xmodmap`, this file is then run within `~/.xinitrc`

# X init

I've put my X keybinds in this file. Feel free to put any additional x-related startup commands in this file.

the file, `.xinitrc` only runs when X is started with `xstart`, when opened via a graphical loading screen, it looks for `.xprofile` instead of `.xinitrc`, so I just symlinked `.xprofile` to `.xinitrc`.

# Display Managers (login screens)

I guess I forgot about Display Managers. But they help the process of starting up the X server with a login screen.
I use `ly`. I just installed it and enabled it's systemd service and its up-and-running just like that

# nerd fonts
Apparently, the upgrade to nerdfonts v3 moved some fonts and a lot of them are obsolete. This can affect some of the plugins in the terminal. 
This can manifest itself in neovim showing an invalid font but the terminal is just fine.
We can use nerdfix to fix any of the plugins that use the obsolete fonts:
`nerdfix fix --recursive /path/to/root`
It does take some time, so feel free to point to the the install-directory instead of root (eg. just target neovim)

## neovim colorschemes and nerd fonts
For some reason, some neovim colorschemes can even break nerd fonts.
eg. A red "✗" wouldnt render on evergarden, but it would render on gruvbox.
eg2. However, the "✗" becomes a box when it's rendered in gruvbox's comment color.

# pacman - "Errors occurred, no packages were upgraded"
This usually occurs if you don't update for a long time. An outdated keyring means that packages signed by new packagers won't be accepted, since the keyring isn't updated.
This is usually fixed with `pacman -Sy archlinux-keyring; pacman -Syu`
I may or may-not have this be a built-in alias. But tbh, this is good to always think about, and having an alias for it might make me forget it tbh.

# bluetooth
arch: I have the bluetooth protocal stack `bluez, bluez-utils` 
I installed `bluetuith` which is a user-friendly TUI for bluetooth connections

## pipewire
There seems to be a strange conflict involving pipewire and pulseaudio. if pipewire is not running, bluetooth conenctions can fail. ensure that everything about pipewire is installed and check online for additional information

# force-closing windows
You can use `xkill` to force-close all processes related to an X window (neat!)

# pacman logs - cleaning out old packages
You can look at every package ever installed in /var/log/pacman.log
Good for trying to figure out what to get rid of

# spotify

I use `spotifyd` to play spotify audio. It's config file requires me to put in a password, I've written a script to output the content of a secret file so I don't commit it here 

I use spotify-tui as a tui for spotify, it uses spotifyd to actually play audio

**If you need to debug spotifyd, try running it as `spotifyd --no-daemon` to get feedback**
