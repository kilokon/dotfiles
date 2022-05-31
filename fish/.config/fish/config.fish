if status is-interactive
    # Commands to run in interactive sessions can go here
end

function fish_greeting
end


alias vi="nvim"
alias cls="clear"
alias ls="exa"
alias ll="exa -alh"
alias tree="exa --tree"
alias ef="exec fish"
# alias cd="z"

starship init fish | source
zoxide init fish | source


set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/aviik/.ghcup/bin # ghcup-env
