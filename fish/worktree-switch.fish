function worktree-switch -d "Fast worktree switcher with fzf"
    set -l lines (git worktree list)
    set -l sorted
    for line in $lines
        set -l wt_path (echo "$line" | awk '{print $1}')
        set -l mtime (stat -c %Y "$wt_path" 2>/dev/null; or echo 0)
        echo "$mtime"(printf '\t')"$line"
    end | sort -rn -t (printf '\t') -k1,1 | sed 's/^[0-9]*	//' | read -z sorted
    set -l selection (echo -n "$sorted" | sed 's/.*\[//;s/\]//' | fzf --height=~40% --reverse --no-sort --header='Switch worktree')

    test -n "$selection"; or return 0

    set -l current_branch (git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if test "$selection" = "$current_branch"
        return 0
    end

    wt switch "$selection"
end
