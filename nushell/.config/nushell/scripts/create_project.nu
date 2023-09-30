#!/usr/bin/nu

#create a project directory with a mit license and a Readme.md
#add a .gitignore file
def cproj [
  name: string,
  --vcs(-v): string = "git",
] {
        mkdir $name
        cd $name
        touch Readme.md
        touch .gitignore
        touch LICENSE
        echo "Cargo init? [Y/n]"
        let choice = (input)
        if $choice in ["Y" "y"] {
                echo "Lib/Bin/Workspace? [l/b/w] default: l"
                let choice = (input)
                if $choice in ["L" "l"] {
                        $"cargo init --lib --vcs=$(vcs)"
                } else if $choice in ["B" "b"] {
                        $"cargo init --bin --vcs=$(vcs)"
                } else if $choice in ["W" "w"] {
                        $"cargo init --workspace --vcs=$(vcs)"
                } else {
                        $"cargo init --lib --vcs=$(vcs)"
                }

        } else {
                echo "Project created. Run 'cargo init' to initialize a cargo project"
        }
}


#mkdir and cd into it

def _ispath [path: string] {
        if '/' in $path {
                return true
        } else {
                return false
        }
}


# def mkcd [dir: string] {mkdir $dir; cd $dir}
def mkcd [dir: string] {
        if (_ispath $dir) {
                mkdir $dir
                enter $dir
        } else {
                let dir_new = ($env.PWD | path join $dir)
                mkdir $dir_new
                enter $dir_new
        }

}

def newnu [name] {
    $"#!/usr/bin/nu(char nl)def main [arg1] { $arg1 }(char nl)" | save $name
    chmod +x $name
    $"Created `($name)` Nu script"
}

def newsh [name] {
    $"#!/usr/bin/sh(char nl)echo $1(char nl)" | save $name
    chmod +x $name
    $"Created `($name)` sh script"
}

