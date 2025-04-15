# Setup

1. Clone the .dotfiles repo into your home directory.

2. Run the `install` script by itself to do the initial setup. Add additional arguments to also include system-specific setup.

| Argument | Description | Notes |
| --- | --- | --- |
| No Argument | Creates symlinks into the home directory |Run this first before anything else|
| `minimal`| A package-manager agnostic, minimal setup. Doesn't install zsh |Not automated: `fd-find`, tpm plugin installation, nvim Mason package installation |
| `rustup`| Installs rust and rustup | |
| `debian` | Installs debian packages and additional software |Kinda broken. Not automated: `vim-plug`. |
| `arch`   | Installs arch packages and additional software |Untested.|
| `emacs-debian` | Installs deps via apt, Builds emacs from source, and then installs doom |Requires `cargo` in $PATH. (install `rustup`) restart shell before running. Doom config relies on existing folders (eg. /home/$USER/org, Obsidian folder) If they don't exist, the whole settings dont get loaded. |
| `emacs-arch` | Installs doom, assuming emacs is already installed |Untested. ALSO requires `cargo` in $PATH|

Note: these confs can be found in the `setup/` directory. I'll add more as I continue to make changes.

# A simple, minimal setup for WSL

When doing stuff on Windows, I don't think it makes sense to install a bunch of extra crap. So these steps help get a very simple WSL setup.

## Steps

0. Create initial symlinks `./install`

1. Install zsh via your package manager eg. `sudo apt install zsh`

2. Install minimal setup `./install minimal`

3. Install rustup `./install rustup`

4. [WIP] Re-open shell so `cargo` is in your $PATH

5. Install doom emacs `./install emacs-debian`
   - Run `doom doctor` and `doom sync` to ensure that any configuration issues are addressed (eg. missing `org` folder)
   - Your config won't load if there is an error.

7. Install SyncThing on Windows and configure it... manually :(
   - Note that we are not installing syncthing on WSL. We're doing it on **Windows**. This lets us keep syncthing running while the PC is on. Not when WSL is open.

9. For your org notes, symlink your `/mnt/c/.../[INSERT_NOTES_HERE]` folder to your WSL filesystem as your `~/org` folder.
   - `M-x treemacs-edit-workspaces` to change default tree

# Cool Stuff!

- Cool scripts!
- Fun eww widgets!
- An emacs library!
- Most colorschemes integrated with wpgtk! (No need to worry about colorschemes!)
- A theme manager written in ruby!
