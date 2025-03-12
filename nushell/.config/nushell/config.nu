source aliases.nu
$env.config.show_banner = false
$env.config.buffer_editor = "nvim"

def --env mkcd [dir:string] { mkdir $dir; cd $dir }



# plugin add nu_plugin_inc
# plugin add nu_plugin_polars
