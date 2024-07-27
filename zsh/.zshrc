# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time Oh My Zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"


# history setup
HISTFILE=$HOME/.zhistory
SAVEHIST=1000
HISTSIZE=1000
setopt share_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_verify

# completion using arrow keys (based on history)
bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward

# History settings.
# export HISTFILE="${XDG_CACHE_HOME}/zsh/.history"
export HISTTIMEFORMAT="%Y/%m/%d %H:%M:%S:   "
setopt INC_APPEND_HISTORY   # Immediately append commands to history file.
setopt HIST_IGNORE_ALL_DUPS # Never add duplicate entries.
setopt HIST_IGNORE_SPACE    # Ignore commands that start with a space.
setopt HIST_REDUCE_BLANKS   # Remove unnecessary blank lines.

# DIRECTORY STACK
setopt auto_pushd        # automatically push previous directory to the stack
setopt pushd_ignore_dups # ignore duplicates in directory stack
setopt pushd_minus       # swap + and -
setopt pushd_silent      # silend pushd and popd
setopt pushd_to_home     # pushd defaults to $HOME
DIRSTACKSIZE=12

# GENERAL
# setopt menu_complete            # insert first match of the completion
setopt list_packed          # fit more completions on the screen
setopt auto_cd              # change directory by writing the directory name
setopt notify               # report job status immediately
setopt no_flow_control      # disable flow control - Ctrl+S and Ctrl+Q keys
setopt interactive_comments # allow comments
setopt noclobber            # >! or >| for existing files

# Customize spelling correction prompt.
SPROMPT='zsh: correct %F{red}%R%f to %F{green}%r%f [nyae]? '

#
# zsh-autosuggestions
#

# Customize the style that the suggestions are shown with.
# See https://github.com/zsh-users/zsh-autosuggestions/blob/master/README.md#suggestion-highlight-style
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='bold'

#
# zsh-syntax-highlighting
#

# Set what highlighters will be used.
# See https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters.md
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

# Customize the main highlighter styles.
# See https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters/main.md#how-to-tweak-it
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[comment]='fg=242'

export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE='50'       # only show suggestions when the command is less than 50 characters long
export ZSH_AUTOSUGGEST_MANUAL_REBIND='true'       # can be set to true for better performance.
export ZSH_AUTOSUGGEST_HISTORY_IGNORE="?(#c200,)" # ignore history entries that are longer than 200 characters

# Rust debug for tracing and other logging
export RUST_LOG=debug



if [[ ! -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions ]]; then
  echo "Installing zsh-autosuggestions"
  git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
fi

if [[ ! -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting ]]; then
  echo "Installing zsh-syntax-highlighting"
  git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
fi

if [[ ! -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-completions ]]; then
  echo "Installing zsh-completions"
  git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:-${ZSH:-~/.oh-my-zsh}/custom}/plugins/zsh-completions
fi


source "$HOME/.cargo/env"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=( 
  archlinux 
  aliases
  zsh-autosuggestions 
  zsh-completions
  zsh-syntax-highlighting 
  cabal
  colored-man-pages
  copybuffer
  docker 
  dotenv 
  extract
  fzf
  git
  mise
  rust
  starship
  zoxide
  )


if [[ -z "$ZDOTDIR" ]]; then
  # If ZDOTDIR is not set, assign it to $HOME
  ZDOTDIR="$HOME"
fi


fpath+=${ZSH_CUSTOM:-${ZSH:-~/.oh-my-zsh}/custom}/plugins/zsh-completions/src

ZSH_DOTENV_FILE=.dotenv

[ -n "$WEZTERM_PANE" ] && export NVIM_LISTEN_ADDRESS="/tmp/nvim$WEZTERM_PANE"

#Direnv 
eval "$(direnv hook zsh)"``

# Zoxide
eval "$(zoxide init zsh)"

eval "$(mise activate zsh)"
# Atuin
# export ATUIN_NOBIND="true"
eval "$(atuin init zsh)"

# felix
source <(command fx --init)

# opam configuration
[[ ! -r /home/aviik/.opam/opam-init/init.zsh ]] || source /home/aviik/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

source $ZSH/oh-my-zsh.sh

autoload -Uz compinitzstyle ':omz:update' mode reminder

for dump in "$ZDOTDIR"/.zcompdump(N.mh+24); do
  compinit
done
compinit -C
compinit
_comp_options+=(globdots)

# zstyle ':completion:*' menu select=2
# zstyle ':completion:*' auto-description 'specify: %d'
# zstyle ':completion:*' completer _expand _complete _correct _approximate
# zstyle ':completion:*' format 'Completing %d'
# zstyle ':completion:*' group-name ''

zstyle ':completion:*' menu select # select completions with arrow keys
zstyle ':completion:*' group-name '' # group results by category
zstyle ':completion:::::' completer _expand _complete _ignored _approximate # enable approximate matches for completion
zstyle ':completion:*' format '%F{magenta}  %d %f'
zstyle ':completion:*:options' description yes
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format '%F{yellow}  %d (errors: %e) %f'
zstyle ':completion:*:descriptions' format '%F{magenta}  %d %f'
zstyle ':completion:*:messages' format '%F{blue} 𥉉%d %f'
zstyle ':completion:*:warnings' format '%F{red}  No matches found... %f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:default' menu select=2
zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
zstyle ':omz:update' mode reminder




source ~/repos/sourcing/zabb/zabb.plugin.zsh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by Oh My Zsh libs,
# plugins, and themes. Aliases can be placed here, though Oh My Zsh
# users are encouraged to define aliases within a top-level file in
# the $ZSH_CUSTOM folder, with .zsh extension. Examples:
# - $ZSH_CUSTOM/aliases.zsh
# - $ZSH_CUSTOM/macos.zsh
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
# Shell Jump
alias eb="exec bash"
alias ef="exec fish"
alias ez="exec zsh"
alias en="exec nu"

alias cls="clear"
alias j="just"
alias sync_notes="onedrive --synchronize --single-directory 'mynotes'"
alias sync_dev="onedrive --synchronize --single-directory 'dev'"
alias fxd="fx ~/dotfiles"
alias fxs="fx ~/sync/NO_LOGIC_HERE" 
alias fxo="fx ~/OneDrive"




#keybindings
bindkey -v
#
# ^ = Ctrl
# ^[ = Alt
# \e or \E = Escape

bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^[r' _atuin_search_widget

source /home/aviik/.config/broot/launcher/bash/br
