# Theme management
This is my simple system to manange themes for all of my programs.

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

### To do:

- [ ]  Combine high-level `active-themes` files to change all themes at once 
- [ ]  Support for window-manager / desktop stuff like i3 / feh / macOS / yabai etc
