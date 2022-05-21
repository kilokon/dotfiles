

PS1='[\u@\h \W]\$ '
. "$HOME/.cargo/env"

## sumneko-lua installed from instructions at 
## https://jdhao.github.io/2021/08/12/nvim_sumneko_lua_conf/
export PATH="$HOME/tools/lua-language-server/bin:$PATH"


[ -f ~/.fzf.bash ] && source ~/.fzf.bash



eval "$(zoxide init bash)"
