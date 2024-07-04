# chin's Dotfiles

Just my dotfiles!

## Setup

1. Clone the .dotfiles repo into your home directory.

2. Run the `install` script by itself to do the initial setup. Add additional arguments to also include system-specific setup.

| Argument | Description |
| --- | --- |
| No Argument | Creates symlinks into the home directory |
| `minimal`| A package-manager agnostic, minimal setup. Doesn't install zsh |
| `rustup`| Installs rust and rustup |
| `debian` | Installs debian packages and additional software |
| `arch`   | Installs arch packages and additional software |
| `emacs-debian` | Installs deps via apt, Builds emacs from source, and then installs doom |
| `emacs-arch` | Installs doom, assuming emacs is already installed |

Note: these confs can be found in the `setup/` directory. I'll add more as I continue to make changes.

3. Done!

### A simple, minimal setup for WSL

1. Install zsh via your package manager eg. `sudo apt install zsh`

2. Install minimal setup `./install minimal`

3. Install rustup `./install rustup`

4. Install emacs `./install emacs-debian`

5. Install SyncThing and configure it... manually :(

# Cool Stuff!

- Cool scripts!
- Fun eww widgets!
- An emacs library!
- Most colorschemes integrated with wpgtk! (No need to worry about colorschemes!)
- A theme manager written in ruby!
