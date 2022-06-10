set -l commands add apply archive change channel clone credit diff fork init log ls mv pull push record remote remove reset unrecord help

complete -f -c pijul

# global flags
complete -f -c pijul -s h -l help -d "Print help information"
complete -f -c pijul -s V -l version -d "Print version information"

# pijul add
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "add" -d "Adds a path to the tree"
complete -c pijul -n "__fish_seen_subcommand_from add" -l repository -d "Set the repository where this command should run."

# pijul apply
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "apply" -d "Applies one channel to another"
complete -c pijul -n "__fish_seen_subcommand_from apply" -l deps-only  -d "Only apply the dependencies of this change, not the change itself"
complete -c pijul -n "__fish_seen_subcommand_from apply" -l channel    -d "Apply change to this channel"
complete -c pijul -n "__fish_seen_subcommand_from apply" -l repository -d "Set the repository where this command should run."

# pijul archive
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "archive" -d "Creates an archive of the repository"
complete -c pijul -n "__fish_seen_subcommand_from archive" -s k          -d "Do not check certificates (HTTPS remotes only, this option might be dangerous)"
complete -c pijul -n "__fish_seen_subcommand_from archive" -s o          -d "Name of the output file"
complete -c pijul -n "__fish_seen_subcommand_from archive" -l change     -d "Apply these changes after switching to the channel"
complete -c pijul -n "__fish_seen_subcommand_from archive" -l remote     -d "Ask the remote to send an archive"
complete -c pijul -n "__fish_seen_subcommand_from archive" -l state      -d "Archive in this state"
complete -c pijul -n "__fish_seen_subcommand_from archive" -l channel    -d "Use this channel, instead of the current channel"
complete -c pijul -n "__fish_seen_subcommand_from archive" -l repository -d "Set the repository where this command should run"

# pijul change
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "change" -d "Shows information about a particular change"
complete -c pijul -n "__fish_seen_subcommand_from change" -l repository -d "Use the repository at PATH instead of the current directory"

#pijul channel
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "channel" -d "Manages different channels"
complete -c pijul -n "__fish_seen_subcommand_from channel" -a delete -d "Delete a channel. The channel must not be the current channel"
complete -c pijul -n "__fish_seen_subcommand_from channel" -a help   -d "Prints this message or the help of the given subcommand(s)"
complete -c pijul -n "__fish_seen_subcommand_from channel" -a rename -d "Rename a channel"
complete -c pijul -n "__fish_seen_subcommand_from channel" -a switch -d "Switch to a channel. There must not be unrecorded changes in the working copy"

# pijul clone
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "clone" -d "Clones an existing pijul repository"
complete -c pijul -n "__fish_seen_subcommand_from clone" -s k       -d "Do not check certificates (HTTPS remotes only, this option might be dangerous)"
complete -c pijul -n "__fish_seen_subcommand_from clone" -l lazy    -d "Only download changes with alive contents"
complete -c pijul -n "__fish_seen_subcommand_from clone" -l change  -d "Clone this change and its dependencies"
complete -c pijul -n "__fish_seen_subcommand_from clone" -l channel -d "Set the remote channel"
complete -c pijul -n "__fish_seen_subcommand_from clone" -l path    -d "Clone this path only"
complete -c pijul -n "__fish_seen_subcommand_from clone" -l state   -d "Clone this state"

# pijul credit
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "credit" -d "Shows which patch last affected each line of every file"
complete -c pijul -n "__fish_seen_subcommand_from credit" -l repository -d "Set the repository where this command should run"
complete -c pijul -n "__fish_seen_subcommand_from credit" -l channel -d "Use this channel, instead of the current channel"

# pijul diff
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "diff" -d "Shows difference between two channels/changes"
complete -c pijul -n "__fish_seen_subcommand_from diff" -l channel    -d "Use this channel, instead of the current channel"
complete -c pijul -n "__fish_seen_subcommand_from diff" -l repository -d "Set the repository where this command should run"
complete -c pijul -n "__fish_seen_subcommand_from diff" -l json       -d "Output the diff in JSON format instead of the default change text format"
complete -c pijul -n "__fish_seen_subcommand_from diff" -l tag        -d "Add all the changes of this channel as dependencies (except changes implied transitively), instead of the minimal dependencies"
complete -c pijul -n "__fish_seen_subcommand_from diff" -l short      -d "Show a short version of the diff"

# pijul fork
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "fork" -d "Create a new channel"
complete -c pijul -n "__fish_seen_subcommand_from fork" -l change     -d "Apply this change after creating the channel"
complete -c pijul -n "__fish_seen_subcommand_from fork" -l channel    -d "Make the new channel from this channel instead of the current channel"
complete -c pijul -n "__fish_seen_subcommand_from fork" -l repository -d "Set the repository where this command should run"

# pijul init
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "init" -d "Initializes an empty pijul repository"
complete -c pijul -n "__fish_seen_subcommand_from fork" -l channel -d "Set the name of the current channel"

# pijul log
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "log" -d "Show the entire log of changes"
complete -c pijul -n "__fish_seen_subcommand_from log" -l description    -d "Include full change description in the output"
complete -c pijul -n "__fish_seen_subcommand_from log" -l hash-only      -d "Only show the change hashes"
complete -c pijul -n "__fish_seen_subcommand_from log" -l state          -d "Include state identifiers in the output"
complete -c pijul -n "__fish_seen_subcommand_from log" -l channel        -d "Show logs for this channel instead of the current channel"
complete -c pijul -n "__fish_seen_subcommand_from log" -l repository     -d "Set the repository where this command should run"

# pijul ls
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "ls" -d "List files tracked by pijul"
complete -c pijul -n "__fish_seen_subcommand_from ls" -l repository -d "Set the repository where this command should run"

# pijul mv
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "mv" -d "Moves a file in the working copy and the tree"
complete -c pijul -n "__fish_seen_subcommand_from mv" -l repository -d "Set the repository where this command should run"

# pijul pull
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "pull" -d "Pulls changes from a remote upstream"
complete -c pijul -n "__fish_seen_subcommand_from pull" -s k            -d "Do not check certificates (HTTPS remotes only, this option might be dangerous)"
complete -c pijul -n "__fish_seen_subcommand_from pull" -s a -l all     -d "Pull all changes"
complete -c pijul -n "__fish_seen_subcommand_from pull" -l full         -d "Download full changes, even when not necessory"
complete -c pijul -n "__fish_seen_subcommand_from pull" -l channel      -d "Pull into this channel instead of the current channel"
complete -c pijul -n "__fish_seen_subcommand_from pull" -l from-channel -d "Pull from this remote channel"
complete -c pijul -n "__fish_seen_subcommand_from pull" -l path         -d "Only pull to these paths"
complete -c pijul -n "__fish_seen_subcommand_from pull" -l repository   -d "Set the repository where this command should run"

# pijul push
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "push" -d "Pushes changes to a remote upstream"
complete -c pijul -n "__fish_seen_subcommand_from push" -s k          -d "Do not check certificates (HTTPS remotes only, this option might be dangerous)"
complete -c pijul -n "__fish_seen_subcommand_from push" -s a -l all   -d "Pull all changes"
complete -c pijul -n "__fish_seen_subcommand_from push" -l channel    -d "Push from this channel instead of the default channel"
complete -c pijul -n "__fish_seen_subcommand_from push" -l path       -d "Push changes only relating to these paths"
complete -c pijul -n "__fish_seen_subcommand_from push" -l repository -d "Path to the repository. Uses the current repository if the argument is omitted"
complete -c pijul -n "__fish_seen_subcommand_from push" -l to-channel -d "Push to this remote channel instead of the remote's default channel"

# pijul record
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "record" -d "Creates a new change"
complete -c pijul -n "__fish_seen_subcommand_from record" -s S            -d "Sign the change with the default key"
complete -c pijul -n "__fish_seen_subcommand_from record" -s a -l all     -d "Record all paths that have changed"
complete -c pijul -n "__fish_seen_subcommand_from record" -s m -l message -d "Set the change message"
complete -c pijul -n "__fish_seen_subcommand_from record" -l amend        -d "Amend this change instead of creating a new change"
complete -c pijul -n "__fish_seen_subcommand_from record" -l author       -d "Set the author field"
complete -c pijul -n "__fish_seen_subcommand_from record" -l channel      -d "Record the change in this channel instead of the current channel"
complete -c pijul -n "__fish_seen_subcommand_from record" -l repository   -d "Set the repository where this command should run"
complete -c pijul -n "__fish_seen_subcommand_from record" -l timestamp    -d "Set the timestamp field"
complete -c pijul -n "__fish_seen_subcommand_from record" -l tag

# pijul remote
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "remote" -d "Manages remote repositories"
complete -c pijul -n "__fish_seen_subcommand_from remote" -l repository   -d "Set the repository where this command should run"
complete -c pijul -n "__fish_seen_subcommand_from remote" -a delete -d "Deletes the remote"
complete -c pijul -n "__fish_seen_subcommand_from remote" -a help -d "Prints this message or the help of the given subcommand(s)"


# pijul remove
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "remove" -d "Removes a file from the tree and pristine"
complete -c pijul -n "__fish_seen_subcommand_from remove" -l repository   -d "Set the repository where this command should run"

# pijul reset
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "reset" -d "Reverts the working copy to the given hash"
complete -c pijul -n "__fish_seen_subcommand_from reset" -l channel    -d "Reset the working copy to this channel, and change the current channel to this channel"
complete -c pijul -n "__fish_seen_subcommand_from reset" -l repository -d "Set the repository where this command should run"

# pijul unrecord
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "unrecord" -d "Unrecords all changes up to the given hash"
complete -c pijul -n "__fish_seen_subcommand_from unrecord" -l channel    -d "Unrecord changes from this channel instead of the current channel"
complete -c pijul -n "__fish_seen_subcommand_from unrecord" -l repository -d "Set the repository where this command should run"

# pijul help
complete -c pijul -n "not __fish_seen_subcommand_from $commands" -a "help" -d "Print pijul help text"

