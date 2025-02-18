# Check if the ZSHAREDDIR exists
if [ ! -d "$ZSHAREDDIR" ]; then
  echo "Directory $ZSHAREDDIR does not exist. Creating now..."
  mkdir -p "$ZSHAREDDIR"
  echo "Directory $ZSHAREDDIR created."
fi
export ZSH="$HOME/.oh-my-zsh"



setopt share_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_verify
setopt sharehistory

# completion using arrow keys (based on history)
# bindkey '^[[A' history-search-backward
# bindkey '^[[B' history-search-forward

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
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#8c92ac, bold'
# export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#ff00ff,bg=cyan,bold,underline"
export  ZSH_AUTOSUGGEST_STRATEGY=(history completion) 
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
# export RUST_LOG=debug



if [[ ! -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions ]]; then
  echo "Installing zsh-autosuggestions"
  git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
fi

source ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
# source ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-completions/zsh-completions.zsh

source $ZSH/oh-my-zsh.sh
plugins+=( 
  # zsh-autosuggestions 
  zsh-completions
  zsh-syntax-highlighting 
  direnv
  zsh-pyenv
  fzf
)
# autoload -Uz compinitzstyle ':omz:update' mode reminder
# Use modern completion system. Other than enabling globdots for showing
# hidden files, these ares values in the default generated zsh config.
autoload -Uz compinit
for dump in "$ZDOTDIR"/.zcompdump(N.mh+24); do
  compinit
done
compinit -C
compinit
_comp_options+=(globdots)


# zmodload zsh/complist
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
# Environment Variables
# zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}
# # # Man
# zstyle ':completion:*:manuals' separate-sections true
# zstyle ':completion:*:manuals.(^1*)' insert-sections true

ZSH_DOTENV_FILE=.dotenv

[ -n "$WEZTERM_PANE" ] && export NVIM_LISTEN_ADDRESS="/tmp/nvim$WEZTERM_PANE"

#
# Starship Prompt
eval "$(starship init zsh)"

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


# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8


source $ZDOTDIR/functions/aliases.zsh
source $ZDOTDIR/functions/zfuncs.zsh
source $ZDOTDIR/functions/zabb.plugin.zsh
source $ZDOTDIR/functions/yazi.zsh
source $ZDOTDIR/functions/mise_completions.zsh

source /home/aviik/.config/broot/launcher/bash/br
# Change Yazi's CWD to PWD on subshell exit
# if [[ -n "$YAZI_ID" ]]; then
# 	function _yazi_cd() {
# 		ya pub dds-cd --str "$PWD"
# 	}
# 	add-zsh-hook zshexit _yazi_cd
# fi
#
#
# if [[ -n "$NVIM_LISTEN_ADDRESS" ]]; then
#     echo "Zsh is running inside Neovim"
#     # You can add any specific commands you want to run in this environment
# fi
if [[ -n "$NVIM" ]]; then
    echo "Zsh is running inside Neovim"
    # Additional custom behavior for Neovim can go here
fi



. "$HOME/.cargo/env"

# bun completions
[ -s "/home/aviik/.bun/_bun" ] && source "/home/aviik/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"



