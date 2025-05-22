source aliases.nu
$env.config.show_banner = false
$env.config.buffer_editor = "nvim"

def --env mkcd [dir:string] { mkdir $dir; cd $dir }



# plugin add nu_plugin_inc
# plugin add nu_plugin_polars

# direnv
$env.config = {
    hooks: {
        env_change: {
            PWD: (
                $env.config.hooks.env_change.PWD? | default []
                | append {||
                    if (which direnv | is-empty) {
                        return
                    }
                    direnv export json | from json | default {} | load-env
                }
            )
        }
    }
}
$env.config = ($env.config | update hooks.env_change.PWD {
	append {
		condition: {|_, after| ($after | path join 'Cargo.lock' | path exists) }
		code: {
			$env.PATH = (
				$env.PATH
					| prepend ($env.PWD | path join 'target/debug')
					| prepend ($env.PWD | path join 'target/release')
					| uniq
				)
		}
	}
})



# themes
source tokyo-storm.nu

# completions
source bitwarden-cli-completions.nu
source cargo-completions.nu
source docker-completions.nu
source gh-completions.nu
source git-completions.nu
source just-completions.nu
source make-completions.nu
source pytest-completions.nu
source rustup-completions.nu
source npm-completions.nu
source uv-completions.nu
# source zoxide-completions.nu

# Scripts
source zoxide.nu
source direnv.nu
source bookmark_for_dir.nu
overlay use alias-finder.nu 
