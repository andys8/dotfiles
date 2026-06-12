function worktree-fzf -d "Fast worktree switcher with fzf"
    set -l lines (git worktree list)
    set -l sorted
    for line in $lines
        set -l wt_path (echo "$line" | awk '{print $1}')
        set -l mtime (stat -f %m "$wt_path" 2>/dev/null; or echo 0)
        echo "$mtime"(printf '\t')"$line"
    end | sort -rn -t (printf '\t') -k1,1 | sed 's/^[0-9]*	//' | read -z sorted

    set -l query $argv[1]
    set -l current_branch (git rev-parse --abbrev-ref HEAD 2>/dev/null)

    if test -n "$query"
        set -l branches (echo -n "$sorted" | sed 's/.*\[//;s/\]//')
        for branch in $branches
            if test "$branch" = "$query"
                if test "$query" != "$current_branch"
                    wt switch "$query"
                end
                return 0
            end
        end
    end

    set -l fzf_args --height=~40% --reverse --no-sort --header='Switch worktree'
    test -n "$query"; and set fzf_args $fzf_args --query "$query"

    set -l selection (echo -n "$sorted" | sed 's/.*\[//;s/\]//' | fzf $fzf_args)

    test -n "$selection"; or return 0

    if test "$selection" != "$current_branch"
        wt switch "$selection"
    end
end
