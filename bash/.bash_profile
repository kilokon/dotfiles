#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#if [ -e /home/aviik/.nix-profile/etc/profile.d/nix.sh ]; then . /home/aviik/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
#. "$HOME/.cargo/env"


# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
export PATH=~/dev/exercism:$PATH

#LC_ALL=C
#export LC_ALL
