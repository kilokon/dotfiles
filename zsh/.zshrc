setopt COMBINING_CHARS
# echo "Loading zshrc"

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

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'


# Fuzzy matching of completions for when you mistype them:
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

zstyle -e ':completion:*:approximate:*' \
        max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
# Ignore completion functions for commands you don’t have
zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always


autoload -U compinit && compinit -u
# zstyle ':completion:*' menu select
# # Auto complete with case insenstivity
# #zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
# zstyle ':completion::complete:*' use-cache on
# zstyle ':completion::complete:*' cache-path ~/.cache/zsh_completion
# # Make completion match at any part of the string (not just the beginning).
# # (Note this isn't fuzzy though; it looks for an exact match).
# zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
# # Shift tab to navigate backwards
# bindkey '^[[Z' reverse-menu-complete


zmodload zsh/complist
compinit


_comp_options+=(globdots)		# Include hidden files.


# Colors ==================================================================
#------------------------------

source ~/repos/sourcing/F-Sy-H/F-Sy-H.plugin.zsh
autoload -U colors && colors


# Globals ========================================================
# -----------------------------

export EDITOR="nvim"
export VISUAL="nvim"
export STARSHIP_CONFIG=~/.config/zsh_starship.toml
export BROWSER="microsoft-edge-dev"

# Vim Mode =======================================================
# -----------------------------
#source "$HOME/repos/zsh-vim-mode/zsh-vim-mode.plugin.zsh"

# https://github.com/jeffreytse/zsh-vi-mode
source "$HOME/repos/zsh-vi-mode/zsh-vi-mode.plugin.zsh"

# Alias ===========================================================
# -----------------------------

alias zrc="$EDITOR ~/.zshrc"
alias grep='grep --color=auto'
alias vi="nvim"
alias cls="clear"
alias ls="eza -1"
alias ll="eza -alh --git"
alias tree="eza --tree"
alias lfancy="eza --icons"
alias ef="exec fish"
alias eb="exec bash"
alias ez="exec zsh"
alias en="exec nu"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias dev="cd $HOME/Dev/"
alias gg="emacsclient -c -e '(magit-status)'" # magit
alias emacs="emacsclient -t"
alias emac="emacsclient -c ~/.dotfiles/emacs/.emacs.d/init.el &"
#alias emacs="emacs -nw"
alias emacs29="emacs -nw --init-directory ~/dotfiles/emacs/emacs29"
# alias hx="helix"
alias nvdot="nvim ~/.dotfiles/"
#alias emac="emacsclient -c ~/.dotfiles/emacs/.emacs.d/init.el &"
#alias dotfiles="$EDITOR $DOTFILES_DIR"
alias pl="ps aux | grep"
alias k="kill -9"
alias sync_notes="onedrive --synchronize --single-directory 'mynotes'"
alias sync_dev="onedrive --synchronize --single-directory 'dev'"
#alias kmo="kmonad $DOTFILES/kmonad/.config/kmonad/ansi_qwerty_circle_87keys.kbd"
alias bevc="gh gist view 9c7afb882391fcc1264b8a7d4f8dcb6e | xsel -ib"
# alias nvim="nvim -p"
alias code="code --profile aviik"
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


#Direnv 
eval "$(direnv hook zsh)"

#Pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"

# Zoxide
eval "$(zoxide init zsh)"
source ~/repos/sourcing/zabb/zabb.plugin.zsh

# Starship
eval "$(starship init zsh)"

# Pyenv
eval "$(pyenv init -)"
eval "$(pyenv init - --no-rehash zsh)"
eval "$(pyenv virtualenv-init -)"

# Atuin
export ATUIN_NOBIND="true"
eval "$(atuin init zsh)"



export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

autoload -U add-zsh-hook
load-nvmrc() {
  local nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$(nvm version)" ]; then
      nvm use
    fi
  elif [ -n "$(PWD=$OLDPWD nvm_find_nvmrc)" ] && [ "$(nvm version)" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc


vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
[ -f "/home/aviik/.ghcup/env" ] && source "/home/aviik/.ghcup/env" # ghcup-env


fpath=( ~/.zfunc "${fpath[@]}" )
# echo $fpath
# autoload -Uz mkcd
autoload -Uz create-c-project
autoload -Uz $(ls ~/.zfunc)

fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line -w
  else
    zle push-input -w
    zle clear-screen -w
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

# fzf
#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# which green >& /dev/null && source "$( green --completion-file )"



#keybindings
bindkey -v
#
# ^ = Ctrl
# ^[ = Alt
# \e or \E = Escape

bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^[r' _atuin_search_widget

mkcd() {
  mkdir -p "$1" && cd "$1"
}
