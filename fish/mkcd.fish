function mkcd
    if test -n "$argv"
        mkdir -p "$argv[1]"
        cd "$argv[1]"
    else
        echo "Usage: mkcd <name>"
    end
end
