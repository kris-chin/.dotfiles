# Windows Setup

This folder contains windows-specific files.

## WIP Stuff:

 - [ ] Install script in top-level directory that clones THIS windows folder to the windows mount.
 - [ ] Install script in this folder to link these windows files to the system

# Things to keep in mind:
I can't automate everything. So this is just my notes on windows bootstrapping.
These notes are also on your org notes.

## Remapping Caps Lock to CTRL
The simplest way is to use Microsoft PowerToys's Keyboard Manager for this. Ensure that it is running in Administrator so its override applies to all programs (including Vcxsrv)

## WSL Clipboard (ie. pasting into nvim)
This *should* already be supported in your config. See :h wsl-clipboard in nvim's manual just in case.

## WSL's display server
By default, WSL2's display server uses an ugly white Adwaita theme that seems to be super hard to change.
Installing VcXsrv lets you run an X server that actually integrates with the Windows window system.
I have some lines in the config to point WSL to this server by setting the `DISPLAY` variable, however, the IP it points to may change depending on device.

## Autorunning
You can use the Task Scheduler or the Startup Folder in Windows to set up autorunning for processes 

# Software List
This is WIP for now, but ideally I would like to automate the installation of these programs. Some of them can be installed via CMD line, others cant.

- [Borderless Gaming](https://github.com/Codeusa/Borderless-Gaming) (Allows borderless fullscreen for all programs) 
- [VcXsrv](https://github.com/marchaesen/vcxsrv) (X Server for Windows)
- [AutoHotKey](https://www.autohotkey.com/) (Automation for Windows)
- [Microsoft PowerToys](https://learn.microsoft.com/en-us/windows/powertoys/) (Super powerful utilities for Windows)
- [Buttery Taskbar](https://github.com/LuisThiamNye/ButteryTaskbar2) (Completely hides the taskbar, which can be annoying in some fullscreen)
- [Antimicrox](https://github.com/AntiMicroX/antimicrox) (Enables the mapping of controller inputs to keyboard keys)
- [ResizeEnable](http://www.digitallis.co.uk/pc/downloads.html) (Old-ass program that enables the resizing of un-resizable windows)
- [Process Explorer](https://learn.microsoft.com/en-us/sysinternals/downloads/process-explorer) (Find which files or directories are used by programs)
- [TrID](https://mark0.net/soft-trid-e.html) (Identifies a file type by their binary signature)
- [komorebi](https://github.com/LGUG2Z/komorebi) (Tiling window manager for Windows)
- [DeskPins](https://efotinis.neocities.org/deskpins/) (Keeps Windows on top.. Wtf a neocities site??) 
- [TreeSize Free](https://www.jam-software.com/treesize_free) (View the sizes of Directories interactively)
- [CrystalDiskInfo](https://crystalmark.info/en/software/crystaldiskinfo/) and [CrystalDiskMark](https://crystalmark.info/en/software/crystaldiskmark/) (View the information about current disk drive status)
- [Controller Companion](http://controllercompanion.com/) (Provides Controller bindings to navigate your desktop)
- [Playnite](https://playnite.link/) (A Windows-Only frontend for cataloging *all* PC games)
- [UsbTreeView](https://www.uwe-sieber.de/usbtreeview_e.html) (View Details on USB Connections and speed)
