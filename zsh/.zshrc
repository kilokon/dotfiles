fpath+=~/.zfunc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Enable colors and change prompt:
autoload -U colors && colors



# install zsh-autosuggestions and zsh-syntax-highlighting from aur/arch


# Colors ==================================================================
#------------------------------

source ~/repos/sourcing/F-Sy-H/F-Sy-H.plugin.zsh




# History stuff =============================================
#------------------------------
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt hist_ignore_dups  # Collapse two consecutive idential commands.
setopt hist_find_no_dups  # Ignore duplicates when searching history.
setopt share_history  # Share across concurrent sessions (append immediately, read from files, add timestamps).
setopt hist_ignore_space  # Lines that begin with space are not recorded.
setopt hist_verify  # Don't auto-execute selected history entry.
setopt hist_ignore_all_dups  # If a history entry would be duplicate, delete older copies.

# Comletion =====================================================
# ------------------------------
if [[ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]]; then 
       . /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 
fi        

autoload -U compinit && compinit -u
zstyle ':completion:*' menu select
# Auto complete with case insenstivity
#zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.cache/zsh_completion
# Make completion match at any part of the string (not just the beginning).
# (Note this isn't fuzzy though; it looks for an exact match).
zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
# Shift tab to navigate backwards
bindkey '^[[Z' reverse-menu-complete


zmodload zsh/complist
compinit


_comp_options+=(globdots)		# Include hidden files.

# Globals ========================================================
# -----------------------------

export EDITOR="nvim"
export VISUAL="nvim"
export STARSHIP_CONFIG=~/.config/zsh_starship.toml
export BROWSER="microsoft-edge-dev"

# Alias ===========================================================
# -----------------------------

alias zrc="$EDITOR ~.zshrc"
alias grep='grep --color=auto'
alias vi="nvim"
alias cls="clear"
alias ls="exa -1"
alias ll="exa -alh --git"
alias tree="exa --tree"
alias lfancy="exa --icons"
alias ef="exec fish"
alias eb="exec bash"
alias ez="exec zsh"
alias cd..="cd .."
alias cd...="cd ../.."
alias cd....="cd ../../.."
alias dev="cd $HOME/Dev/"
# alias hx="helix"
alias nvdot="nvim ~/.dotfiles/"
#alias emac="emacsclient -c ~/.dotfiles/emacs/.emacs.d/init.el &"
#alias dotfiles="$EDITOR $DOTFILES_DIR"
alias pl="ps aux | grep"
alias k="kill -9"
alias sync_notes="onedrive --synchronize --single-directory 'mynotes'"
alias sync_dev="onedrive --synchronize --single-directory 'dev'"
#alias kmo="kmonad $DOTFILES/kmonad/.config/kmonad/ansi_qwerty_circle_87keys.kbd"

#------------------------------
# ShellFuncs
#------------------------------
# -- coloured manuals
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    man "$@"
}

#------------------------------
# ShellFuncs
#------------------------------
# -- coloured manuals
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    man "$@"
}

[ -n "$WEZTERM_PANE" ] && export NVIM_LISTEN_ADDRESS="/tmp/nvim$WEZTERM_PANE"

# Zoxide
eval "$(zoxide init zsh)"
eval "$(starship init zsh)"
eval "$(pyenv virtualenv-init -)"

source ~/repos/sourcing/zabb/zabb.plugin.zsh



