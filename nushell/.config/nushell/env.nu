# Nushell Environment Config File

# let-env STARSHIP_SHELL = "nu"
#
# def create_left_prompt [] {
#     starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)'
# }

# Use nushell functions to define your right and left prompt
# let-env PROMPT_COMMAND = { || create_left_prompt }
# let-env PROMPT_COMMAND_RIGHT = ""
#
# # The prompt indicators are environmental variables that represent
# # the state of the prompt
# let-env PROMPT_INDICATOR = ""
# let-env PROMPT_INDICATOR_VI_INSERT = ": "
# let-env PROMPT_INDICATOR_VI_NORMAL = "〉"
# let-env PROMPT_MULTILINE_INDICATOR = "::: "
#
#




#
# let-env ENV_CONVERSIONS = {
#   "PATH": {
#     from_string: { |s| $s | split row (char esep) | path expand -n }
#     to_string: { |v| $v | path expand -n | str join (char esep) }
#   }
#   "Path": {
#     from_string: { |s| $s | split row (char esep) | path expand -n }
#     to_string: { |v| $v | path expand -n | str join (char esep) }
#   }
# }
#

export-env { load-env {
    XDG_DATA_HOME: ($env.HOME | path join ".local" "share")
    XDG_CONFIG_HOME: ($env.HOME | path join ".config")
    # XDG_STATE_HOME: ($env.HOME | path join ".local" "state")
    XDG_CACHE_HOME: ($env.HOME | path join ".cache")
}}

# Application level glovals
export-env { load-env { 
        IMAGES_HOME: ($env.HOME | path join "media" "images")
        MUSIC_HOME: ($env.HOME | path join "media" "music")
        VIDEOS_HOME: ($env.HOME | path join "media" "videos")
        DOCUMENTS: ($env.HOME | path join "media" "documents")
        DOWNLOADS: ($env.HOME | path join "Downloads")
        # SSH_AGENT_TIMEOUT: 300
        SSH_KEYS_HOME: ($env.HOME | path join ".ssh")
        JUPYTER_CONFIG_DIR: ($env.XDG_CONFIG_HOME | path join "jupyter")
        DEV_HOME: ($env.HOME | path join "DEV")
        ONEDRIVE: ($env.HOME | path join "OneDrive")
        EMACS_HOME: ($env.HOME | path join ".emacs.d")
        CARGO_HOME: ($env.HOME | path join ".cargo")
        CABAL_HOME: ($env.HOME | path join ".cabal")
        PYENV_ROOT: ($env.HOME | path join ".pyenv")
        DOTFILES: ($env.HOME | path join "dotfiles")
        NU_LIB_DIRS: ($nu.config-path | path dirname | path join 'scripts')
        RUST_BACKTRACE: 1
}}


# Allication defaults
$env.BROWSER = "microsoft-edge-dev"
$env.TERMINAL = "alacritty"
$env.EDITOR = 'nvim'
$env.VISUAL = $env.EDITOR
$env.PAGER = "less"
$env.LANG = "en_US.UTF-8"


$env.PATH = (
    $env.PATH | split row (char esep)
    | prepend ($env.HOME | path join ".local" "bin")
    | prepend ($env.CARGO_HOME | path join "bin")
    | prepend ($env.PYENV_ROOT | path join "bin")
    | uniq
)



# Directories to search for scripts when calling source or use
#
# By default, <nushell-config-dir>/scripts is added
# let-env NU_LIB_DIRS = [
#     ($nu.config-path | path dirname | path join 'scripts')
# ]

# Directories to search for plugin binaries when calling register
#
# By default, <nushell-config-dir>/plugins is added

$env.NU_LIB_DIRS = [
    ($nu.config-path | path dirname | path join 'scripts')
]

$env.NU_PLUGIN_DIRS = [
    ($nu.config-path | path dirname | path join 'plugins')
]
$env.NU_THEME_DIRS = [
    ($nu.config-path | path dirname | path join 'themes')
]


# let-env NU_PLUGIN_DIRS = [
#     ($nu.config-path | path dirname | path join 'plugins')
# ]
#
# let-env NU_THEME_DIRS = [
#     ($nu.config-path | path dirname | path join 'themes')
# ]

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# let-env PATH = ($env.PATH | split row (char esep) | prepend '/some/path')


# Manage SSH passphrases
# eval is not available in nushell, so run:
ssh-agent -c -t 300
        | lines 
        | first 2 
        | parse "setenv {name} {value};" 
        | transpose -i -r -d 
        | load-env


# Load the starship prompt
# starship init nu | save -f ~/.cache/starship/init.nu

$env.STARSHIP_SHELL = "nu"

def create_left_prompt [] {
    starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)'
}

# Use nushell functions to define your right and left prompt
$env.PROMPT_COMMAND = { || create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = ""

# The prompt indicators are environmental variables that represent
# the state of the prompt
$env.PROMPT_INDICATOR = ""
$env.PROMPT_INDICATOR_VI_INSERT = ": "
$env.PROMPT_INDICATOR_VI_NORMAL = "〉"
$env.PROMPT_MULTILINE_INDICATOR = "::: "



# Load the zoxide shell integration
# zoxide init nushell | save -f ~/.cache/.zoxide.nu
