# Zsh root
ZDOTDIR=$HOME/.config/zsh
ZSHAREDDIR=$HOME/.local/share/zsh

# history setup
HISTFILE=$HOME/.zhistory
SAVEHIST=1000
HISTSIZE=1000

export PATH="$HOME/.local/bin:$HOME/.cargo/bin:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$HOME/.local/share/mise/shims:$PATH"
# Force scons to use 6 CPU threads 
export SCONSFLAGS="-j6"
export EDITOR='nvim'
