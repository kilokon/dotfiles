alias nv = nvim
alias cls = clear
alias ll = exa -al --icons
alias lt = exa -aT
alias ga = git add
alias gb = git branch
alias cat = bat --number
alias cr = cargo run
alias cb = cargo build
alias gdoc = cd ~/OneDrive/doc/personal_notes/


def --env yy [...args] {
  let tmp = (mktemp -t "yazi-cwd.XXXXXX")
  yazi ...$args --cwd-file $tmp
  let cwd = (open $tmp)
  if $cwd != "" and $cwd != $env.PWD {
    cd $cwd
  }
  rm -fp $tmp
}
