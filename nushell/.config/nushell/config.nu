# Nushell Config File
# export def build-prompt [] {
#     let prompt = (starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)')
#     echo $prompt
# }
#
# PROMPT_COMMAND: { build-prompt }

$env.config = ($env.config? | default {} | merge {
    ls: {
        use_ls_colors: true
        clickable_links: false
    }
    rm: {
        always_trash: true
    }
    cd: {
        abbreviations: true
    }
    history: {
        file_format: "sqlite"
    }
    cursor_shape: {
        vi_insert: line
        vi_normal: underscore
    }
    table: {
        mode: rounded
        index_mode: always
        show_empty: true
        trim: {
                methodology: wrapping # wrapping or truncating
                wrapping_try_keep_words: true # A strategy used by the 'wrapping' methodology
                truncating_suffix: "..." # A suffix used by the 'truncating' methodology
        }
    }
        completions: {
                case_sensitive: false # set to true to enable case-sensitive completions
                quick: true  # set this to false to prevent auto-selecting completions when only one remains
                partial: true  # set this to false to prevent partial filling of the prompt
                algorithm: "prefix"  # prefix or fuzzy
                external: {
                        enable: true # set to false to prevent nushell looking into $env.PATH to find more suggestions, `false` recommended for WSL users as this look up may be very slow
                        max_results: 100 # setting it lower can improve completion performance at the cost of omitting some options
                        completer: null # check 'carapace_completer' above as an example
        }
    }
    edit_mode: vi
    show_banner: false
    datetime_format: {
        normal: "%a, %d %b %Y %H:%M:%S %z"
        table: "%Y/%m/%d %H:%M:%S"
    }
})

$env.config.hooks = {
    env_change: {
        PWD: [
            {|before, _|
                if $before == null {
                    let file = ($nu.home-path | path join ".local" "share" "nushell" "startup-times.nuon")
                    if not ($file | path exists) {
                        mkdir ($file | path dirname)
                        touch $file
                    }

                    open $file | append {
                        date: (date now)
                        time: $nu.startup-time
                        version: (version | get version)
                        commit: (version | get commit_hash)
                        build: (version | get build_time)
                    } | save --force $file
                }
            }
            {
                condition: {|_, after| $after | path join 'toolkit.nu' | path exists }
                code: "overlay use --prefix toolkit.nu"
            }
        ]
    }
    display_output: {||
        if (term size).columns >= 100 { table -e } else { table }
    }
}

$env.config.keybindings = [
    # basic shell features
    {
        name: reload
        modifier: control
        keycode: char_r
        mode: [emacs, vi_insert, vi_normal]
        event: {
            send: executehostcommand,
            cmd: "exec nu"
        }
    }
    {
        name: clear
        modifier: control
        keycode: char_l
        mode: [emacs, vi_normal, vi_insert]
        event: {
            send: executehostcommand
            cmd: "clear"
        }
    }
    {
      name: completion_menu
      modifier: none
      keycode: tab
      mode: [emacs vi_normal vi_insert]
      event: {
        until: [
          { send: menu name: completion_menu }
          { send: menunext }
        ]
      }
    }
    {
        name: fuzzy_dir
        modifier: control
        keycode: char_s
        mode: [emacs, vi_normal, vi_insert]
        event: {
                send: executehostcommand
                cmd: "commandline -a (
                    ls **/*
                    | where type == dir
                    | get name
                    | input list --fuzzy
                        $'Please choose a (ansi magenta)directory(ansi reset) to (ansi cyan_underline)insert(ansi reset):'
                )"
        }
     }
     {
        name: fuzzy_history
        modifier: control
        keycode: char_h
        mode: [emacs, vi_normal, vi_insert]
        event: {
                send: executehostcommand
                cmd: "commandline (
                history
                | each { |it| $it.command }
                | uniq
                | reverse
                | input list --fuzzy
                    $'Please choose a (ansi magenta)command from history(ansi reset):'
                )"
                }
        }
]



alias :q = exit


let my_theme_path = $env.NU_THEME_DIRS.0

# use $"($my_theme_path)/tokyo-night.nu"
# use $env.NU_THEME_DIRS.0/tokyo-night.nu
# use themes/tokyo-night.nu
use ~/.config/nushell/themes/tokyo-night.nu


$env.config = ($env.config | merge {color_config: (tokyo-night)})
#$env.config.color_config = (nushell-dark)

source aliases.nu
source cargo-completions.nu
source git-completions.nu
source poetry-completions.nu
source npm-completions.nu
source typst-completions.nu
source zoxide-menu.nu
source run-c-cpp.nu
# source activate.nu

# Utilities
source ~/.config/nushell/scripts/unit-conversions.nu




# My Functions
source ~/.config/nushell/scripts/create_project.nu


# Load the starship prompt
# source ~/.cache/starship/init.nu

# Load the zoxide shell integration
source ~/.cache/.zoxide.nu

# Load atuin shell integration
source ~/.cache/atuin/init.nu
