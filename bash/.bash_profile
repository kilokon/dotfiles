#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#if [ -e /home/aviik/.nix-profile/etc/profile.d/nix.sh ]; then . /home/aviik/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
#. "$HOME/.cargo/env"

#setxkbpmap -option ctrl:nocaps

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
export PATH=~/dev/exercism:$PATH
export PATH=$PATH:/usr/local/go/bin
export EDITOR="emacsclient -c"
export DOTFILES_DIR="$HOME/.dotfiles/"


#LC_ALL=C
#export LC_ALL

#kmonad ~/.dotfiles/kmonad/.config/kmonad/ansi_qwerty_circle_87keys.kbd
# Load pyenv automatically by appending
# the following the
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"


source /home/aviik/.config/broot/launcher/bash/br
