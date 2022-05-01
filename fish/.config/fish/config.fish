if status is-interactive
    # Commands to run in interactive sessions can go here
end

function fish_greeting
end


alias vi="nvim"
alias cls="clear"
starship init fish | source
