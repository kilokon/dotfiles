#
# ~/.bashrc
#
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

    
PS1='[\u@\h \W]\$ '
. "$HOME/.cargo/env"


export SCRIPT_DIR="$HOME/.scripts/bash"

export EDITOR="emacsclient -c"

export DOTFILES_DIR="$HOME/.dotfiles/"

export DEV="$HOME/Dev/"
# If not running interactively, don't do anything
if [[ -n "$PS1" ]] ; then

    # ignore ls, bg, fg, exit commands
    export HISTIGNORE="ls:[bf]g:exit"

        # don't put duplicate lines in the history. See bash(1) for more options
    # don't overwrite GNU Midnight Commander's setting of `ignorespace'.
    HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
    # ... or force ignoredups and ignorespace
    HISTCONTROL=ignoreboth

    # append to the history file, don't overwrite it
    shopt -s histappend

    # for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize
    
    #  scripts directory append.
    [ -f "${script_dir}/aliases" ] && source "${script_dir}/aliases"
    [ -f "${script_dir}/functions" ] && source "${script_dir}/functions"
    
    [ -f ~/.fzf.bash ] && source ~/.fzf.bash

    eval "$(starship init bash)"
    eval "$(zoxide init bash)"

fi # end of 'if [[ -n "$PS1" ]] ; then'


GPG_TTY=$(tty)
export GPG_TTY






# Load pyenv automatically by appending
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# nvm appending
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Exract anything with ex
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   unzstd $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}



