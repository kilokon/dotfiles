fpath+=~/.zfunc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Enable colors and change prompt:
autoload -U colors && colors



# install zsh-autosuggestions and zsh-syntax-highlighting from aur/arch

if [[ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]]; then 
       . /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 
fi        
#        (( ! ${+ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE} )) &&
#        typeset -g ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
#
#        # Prefix to use when saving original versions of bound widgets
#        (( ! ${+ZSH_AUTOSUGGEST_ORIGINAL_WIDGET_PREFIX} )) &&
#        typeset -g ZSH_AUTOSUGGEST_ORIGINAL_WIDGET_PREFIX=autosuggest-orig-
#
#        # Strategies to use to fetch a suggestion
#        # Will try each strategy in order until a suggestion is returned
#        (( ! ${+ZSH_AUTOSUGGEST_STRATEGY} )) && {
#         typeset -ga ZSH_AUTOSUGGEST_STRATEGY
#         ZSH_AUTOSUGGEST_STRATEGY=(history)
#         }
#         
#         # Widgets that clear the suggestion
#         (( ! ${+ZSH_AUTOSUGGEST_CLEAR_WIDGETS} )) && {
#                 typeset -ga ZSH_AUTOSUGGEST_CLEAR_WIDGETS
#                 ZSH_AUTOSUGGEST_CLEAR_WIDGETS=(
#                         history-search-forward
#                         history-search-backward
#                         history-beginning-search-forward
#                         history-beginning-search-backward
#                         history-substring-search-up
#                         history-substring-search-down
#                         up-line-or-beginning-search
#                         down-line-or-beginning-search
#                         up-line-or-history
#                         down-line-or-history
#                         accept-line
#                         copy-earlier-word
#                 )
#         }
#
#         # Widgets that accept the entire suggestion
#         (( ! ${+ZSH_AUTOSUGGEST_ACCEPT_WIDGETS} )) && {
#                 typeset -ga ZSH_AUTOSUGGEST_ACCEPT_WIDGETS
#                 ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=(
#                         forward-char
#                         end-of-line
#                         vi-forward-char
#                         vi-end-of-line
#                         vi-add-eol
#                 )
#         }
#
#         # Widgets that accept the entire suggestion and exec:wute it
#         (( ! ${+ZSH_AUTOSUGGEST_EXECUTE_WIDGETS} )) && {
#                 typeset -ga ZSH_AUTOSUGGEST_EXECUTE_WIDGETS
#                 ZSH_AUTOSUGGEST_EXECUTE_WIDGETS=(
#                 )
#         }
#
#
#                 # Widgets that accept the suggestion as far as the cursor moves
#                 (( ! ${+ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS} )) && {
#                 typeset -ga ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS
#                 ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS=(
#                         forward-word
#                         emacs-forward-word
#                         vi-forward-word
#                         vi-forward-word-end
#                         vi-forward-blank-word
#                         vi-forward-blank-word-end
#                         vi-find-next-char
#                         vi-find-next-char-skip
#                 )
#         }
#
#         # Widgets that should be ignored (globbing supported but must be escaped)
#         (( ! ${+ZSH_AUTOSUGGEST_IGNORE_WIDGETS} )) && {
# 	typeset -ga ZSH_AUTOSUGGEST_IGNORE_WIDGETS
# 	ZSH_AUTOSUGGEST_IGNORE_WIDGETS=(
# 		orig-\*
# 		beep
# 		run-help
# 		set-local-history
# 		which-command
# 		yank
# 		yank-pop
# 		zle-\*
# 	)
# }
#
#
#         # Pty name for capturing completions for completion suggestion strategy
#         (( ! ${+ZSH_AUTOSUGGEST_COMPLETIONS_PTY_NAME} )) &&
#         typeset -g ZSH_AUTOSUGGEST_COMPLETIONS_PTY_NAME=zsh_autosuggest_completion_pty
# fi
#
if [[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
  . /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

#------------------------------
# History stuff
#------------------------------
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

autoload -U compinit && compinit -u
zstyle ':completion:*' menu select
# Auto complete with case insenstivity
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.



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
# Starship prompt
eval "$(starship init zsh)"

# autoload -Uz compinit promptinit
# compinit
# promptinit
#
# # This will set the default prompt to the walters theme
# prompt walters
eval "$(pyenv virtualenv-init -)"
