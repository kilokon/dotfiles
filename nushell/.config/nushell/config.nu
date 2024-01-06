# Nushell Config File
# export def build-prompt [] {
#     let prompt = (starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)')
#     echo $prompt
# }
#
# PROMPT_COMMAND: { build-prompt }

# $env.config = ($env.config? | default {} | merge {
#     ls: {
#         use_ls_colors: true
#         clickable_links: false
#     }
#     rm: {
#         always_trash: true
#     }
#     cd: {
#         abbreviations: true
#     }
#     history: {
#         file_format: "sqlite"
#     }
#     cursor_shape: {
#         vi_insert: line
#         vi_normal: underscore
#     }
#     table: {
#         mode: rounded
#         index_mode: always
#         show_empty: true
#         trim: {
#                 methodology: wrapping # wrapping or truncating
#                 wrapping_try_keep_words: true # A strategy used by the 'wrapping' methodology
#                 truncating_suffix: "..." # A suffix used by the 'truncating' methodology
#         }
#     }
#         completions: {
#                 case_sensitive: false # set to true to enable case-sensitive completions
#                 quick: true  # set this to false to prevent auto-selecting completions when only one remains
#                 partial: true  # set this to false to prevent partial filling of the prompt
#                 algorithm: "prefix"  # prefix or fuzzy
#                 external: {
#                         enable: true # set to false to prevent nushell looking into $env.PATH to find more suggestions, `false` recommended for WSL users as this look up may be very slow
#                         max_results: 100 # setting it lower can improve completion performance at the cost of omitting some options
#                         completer: null # check 'carapace_completer' above as an example
#         }
#     }
#     edit_mode: vi
#     show_banner: false
#     datetime_format: {
#         normal: "%a, %d %b %Y %H:%M:%S %z"
#         table: "%Y/%m/%d %H:%M:%S"
#     }
# })



$env.config = {
    show_banner: false # true or false to enable or disable the welcome banner at startup

    ls: {
        use_ls_colors: true # use the LS_COLORS environment variable to colorize output
        clickable_links: true # enable or disable clickable links. Your terminal has to support links.
    }

    rm: {
        always_trash: false # always act as if -t was given. Can be overridden with -p
    }

    table: {
        mode: rounded # basic, compact, compact_double, light, thin, with_love, rounded, reinforced, heavy, none, other
        index_mode: always # "always" show indexes, "never" show indexes, "auto" = show indexes when a table has "index" column
        show_empty: true # show 'empty list' and 'empty record' placeholders for command output
        padding: { left: 1, right: 1 } # a left right padding of each column in a table
        trim: {
            methodology: wrapping # wrapping or truncating
            wrapping_try_keep_words: true # A strategy used by the 'wrapping' methodology
            truncating_suffix: "..." # A suffix used by the 'truncating' methodology
        }
        header_on_separator: false # show header text on separator/border line
        # abbreviated_row_count: 10 # limit data rows from top and bottom after reaching a set point
    }

    error_style: "fancy" # "fancy" or "plain" for screen reader-friendly error messages

    # datetime_format determines what a datetime rendered in the shell would look like.
    # Behavior without this configuration point will be to "humanize" the datetime display,
    # showing something like "a day ago."
    datetime_format: {
        # normal: '%a, %d %b %Y %H:%M:%S %z'    # shows up in displays of variables or other datetime's outside of tables
        # table: '%m/%d/%y %I:%M:%S%p'          # generally shows up in tabular outputs such as ls. commenting this out will change it to the default human readable datetime format
    }

    explore: {
        status_bar_background: {fg: "#1D1F21", bg: "#C4C9C6"},
        command_bar_text: {fg: "#C4C9C6"},
        highlight: {fg: "black", bg: "yellow"},
        status: {
            error: {fg: "white", bg: "red"},
            warn: {}
            info: {}
        },
        table: {
            split_line: {fg: "#404040"},
            selected_cell: {bg: light_blue},
            selected_row: {},
            selected_column: {},
        },
    }
}






# The default config record. This is where much of your global configuration is setup.
# $env.config = {
#   ls: {
#     use_ls_colors: true # use the LS_COLORS environment variable to colorize output
#     clickable_links: true # enable or disable clickable links. Your terminal has to support links.
#   }
#   rm: {
#     always_trash: false # always act as if -t was given. Can be overridden with -p
#   }
#   cd: {
#     abbreviations: false # allows `cd s/o/f` to expand to `cd some/other/folder`
#   }
#   table: {
#     mode: rounded # basic, compact, compact_double, light, thin, with_love, rounded, reinforced, heavy, none, other
#     index_mode: always # "always" show indexes, "never" show indexes, "auto" = show indexes when a table has "index" column
#     trim: {
#       methodology: wrapping # wrapping or truncating
#       wrapping_try_keep_words: true # A strategy used by the 'wrapping' methodology
#       truncating_suffix: "..." # A suffix used by the 'truncating' methodology
#     }
#   }
#
# }

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

