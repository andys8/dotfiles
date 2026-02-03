function _git_repo_slug
    set -l dir "$argv[1]"
    test -d "$dir"; or return 1

    set -l url (git -C "$dir" remote get-url origin 2>/dev/null)
    test -z "$url"; and return 1

    echo "$url" | sed -E 's#^.*[:/]([^/]+/[^/]+)(\.git)?$#\1#'
end

function _worktree_root
    set -l dir "$argv[1]"
    set -l slug (_git_repo_slug "$dir" 2>/dev/null)
    if test -n "$slug"
        echo "$HOME/.local/share/worktrees/$slug"
    else
        echo "$HOME/.local/share/worktrees/"(basename "$dir")
    end
end

function jt --description "Jump to a worktree in the current git repository"
    set -l git_root (git rev-parse --show-toplevel 2>/dev/null)
    if test -z "$git_root"
        echo "jt: not in a git repository" >&2
        return 1
    end

    set -l wt_root (_worktree_root "$git_root")

    if test -n "$argv[1]" -a -d "$wt_root/$argv[1]"
        cd "$wt_root/$argv[1]"
        return
    end

    cd "$git_root"; or return 1

    set -l tmp (mktemp -t lazyworktree.XXXXXX)
    lazyworktree --output-selection="$tmp"
    set -l rc $status

    if test $rc -eq 0 -a -s "$tmp"
        set -l selected (cat "$tmp")
        test -n "$selected" -a -d "$selected"; and cd "$selected"
    end

    rm -f "$tmp"
    return $rc
end

function jl --description "Jump to the last selected worktree"
    set -l git_root (git rev-parse --show-toplevel 2>/dev/null)
    if test -z "$git_root"
        echo "jl: not in a git repository" >&2
        return 1
    end

    set -l last_selected (_worktree_root "$git_root")/.last-selected
    if test -f "$last_selected"
        set -l selected (cat "$last_selected")
        if test -n "$selected" -a -d "$selected"
            cd "$selected"
            return
        end
    end

    echo "No last selected worktree found" >&2
    return 1
end

complete -c jt -f -a '(set -l r (git rev-parse --show-toplevel 2>/dev/null); and for w in (_worktree_root "$r")/*/; basename "$w"; end)'
