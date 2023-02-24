fpath+=~/.zfunc

# install zsh-autosuggestions and zsh-syntax-highlighting from aur/arch

if [[ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]]; then 
       . /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 
        
       (( ! ${+ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE} )) &&
       typeset -g ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'

       # Prefix to use when saving original versions of bound widgets
       (( ! ${+ZSH_AUTOSUGGEST_ORIGINAL_WIDGET_PREFIX} )) &&
       typeset -g ZSH_AUTOSUGGEST_ORIGINAL_WIDGET_PREFIX=autosuggest-orig-

       # Strategies to use to fetch a suggestion
       # Will try each strategy in order until a suggestion is returned
       (( ! ${+ZSH_AUTOSUGGEST_STRATEGY} )) && {
        typeset -ga ZSH_AUTOSUGGEST_STRATEGY
        ZSH_AUTOSUGGEST_STRATEGY=(history)
        }
        # Pty name for capturing completions for completion suggestion strategy
        (( ! ${+ZSH_AUTOSUGGEST_COMPLETIONS_PTY_NAME} )) &&
        typeset -g ZSH_AUTOSUGGEST_COMPLETIONS_PTY_NAME=zsh_autosuggest_completion_pty
fi

if [[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
  . /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

#------------------------------
# History stuff
#------------------------------
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000



export EDITOR="nvim"
export VISUAL="nvim"
export STARSHIP_CONFIG=~/.config/zsh_starship.toml
export BROWSER="microsoft-edge-dev"

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


# Zoxide
eval "$(zoxide init zsh)"
# Starship prompt
eval "$(starship init zsh)"

# autoload -Uz compinit promptinit
# compinit
# promptinit
#
# # This will set the default prompt to the walters theme
# prompt walters
