function fcd --description "Fuzzy change directory"
    set -l tmpfile (mktemp)
    fd --type d | fzf --tiebreak=length > $tmpfile
    set -l destdir (cat $tmpfile)
    rm $tmpfile

    if test -z "$destdir"
        return 1
    end

    cd $destdir
end
