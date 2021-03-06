if status is-interactive
    # Commands to run in interactive sessions can go here
end

function fish_greeting
end


alias vi="nvim"
alias cls="clear"
alias ls="exa -1"
alias ll="exa -alh --git"
alias tree="exa --tree"
alias lfancy="exa --icons"
alias ef="exec fish"
alias eb="exec bash"
alias cd..="cd .."
alias nvxmo="nvim ~/.dotfiles/xmonad/.config/xmonad/xmonad.hs"
alias emac="emacsclient -c ~/.dotfiles/emacs/.emacs.d/init.el &"
alias dotfiles="$EDITOR $DOTFILES_DIR"
alias pl="ps aux | grep"
alias k="kill -9"
alias kmo="kmonad $DOTFILES/kmonad/.config/kmonad/ansi_qwerty_circle_87keys.kbd"



starship init fish | source
zoxide init fish | source
###############



# Load pyenv automatically by appending
# the following to ~/.config/fish/config.fish:

pyenv init - | source
###########

  # Restart your shell for the changes to take effect.
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/aviik/.ghcup/bin # ghcup-env
