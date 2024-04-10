# Theme management
This is my simple system to manange themes for all of my programs.
I'm writing it in ruby cuz why not LMAO

## `active-themes.yml`

This file contains a high-level overview of all active themes for my programs.
Each entry in this file corresponds to a filepath which we can get theme information from.
eg:

```yaml
alacritty:
    color: Garuda
    font: Cozette
```

This entry means that we will take the color information from `alacritty/color/Gaurda`. 
Additionally, font information will be taken from `alacritty/font/Cozette`

I (will) have built-in handling for each of these keywords that modifies the corresponding config file with the theme information. This will scoped to be inside each folder.

### Theme data

Theme data can be saved as any file format, the script just reads the whole contents of the file and overwrites sections of certain configs

## To do:

- [x]  Combine high-level `active-themes` files to change all themes at once 
- [ ]  Support for window-manager / desktop stuff like i3 / feh / macOS / yabai etc
- [ ]  Easily share colors from one theme to another. This is useful for themes that dont exist for the other application. (eg. Flate)
- [ ]  Run additional terminal commands (if necessary)
