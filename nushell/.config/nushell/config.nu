# ~/.config/nushell/config.nu
#
# Nushell-s config file used in engine-q.
#
# It fetches all definitions and environment variables from the `init` module.

def build-config [] { { footer_mode: "50" } }

let config = build-config

use ~/.config/nushell/init.nu *

alias gd = git diff


# alias :q = exit


let my_theme_path = $env.NU_THEME_DIRS.0
use $env.NU_THEME_DIRS.0/tokyo-night.nu

# let theme_path = ([$my_theme_path, "tokyo-night.nu"]|path join)

# use $theme_path

# use $"($my_theme_path)/tokyo-night.nu"
# use $env.NU_THEME_DIRS.0/tokyo-night.nu
# use themes/tokyo-night.nu
# use ~/.config/nushell/themes/tokyo-night.nu


# $env.config = ($env.config | merge {color_config: (tokyo-night)})
#$env.config.color_config = (nushell-dark)

source aliases.nu
# source cargo-completions.nu
# source git-completions.nu
# source poetry-completions.nu
# source npm-completions.nu
# source typst-completions.nu
# source zoxide-menu.nu
# source run-c-cpp.nu
# source activate.nu

# Utilities
# source ~/.config/nushell/scripts/unit-conversions.nu




# My Functions
# source ~/.config/nushell/scripts/create_project.nu


# Load the starship prompt
source ~/.cache/starship/init.nu

# Load the zoxide shell integration
# source ~/.cache/.zoxide.nu

# Load atuin shell integration
# source ~/.cache/atuin/init.nu

