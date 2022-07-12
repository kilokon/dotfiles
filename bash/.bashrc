

PS1='[\u@\h \W]\$ '
. "$HOME/.cargo/env"

# Application cache if not stated elsewhere.
script_dir="${XDG_CACHE_HOME:-$HOME/.scripts}/bash"
[ -f "${script_dir}/aliases" ] && source "${script_dir}/aliases"
[ -f "${script_dir}/functions" ] && source "${script_dir}/functions"

export PATH=$PATH:"${script_dir}/surf_scripts"



## sumneko-lua installed from instructions at 
## https://jdhao.github.io/2021/08/12/nvim_sumneko_lua_conf/
export PATH="$HOME/tools/lua-language-server/bin:$PATH"
eval "$(starship init bash)"


GPG_TTY=$(tty)
export GPG_TTY

[ -f ~/.fzf.bash ] && source ~/.fzf.bash


eval "$(zoxide init bash)"

[ -f "/home/aviik/.ghcup/env" ] && source "/home/aviik/.ghcup/env" # ghcup-env

source ~/.bash_completion/alacritty


# Load pyenv automatically by appending


export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"


source /home/aviik/.config/broot/launcher/bash/br




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



