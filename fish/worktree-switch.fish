function worktree-switch -d "Fast worktree switcher with fzf"
    set -l selection (git worktree list | fzf --height=~40% --reverse --no-sort --header='Switch worktree')

    test -n "$selection"; or return 0

    set -l worktree_path (echo "$selection" | awk '{print $1}')
    set -l current_path (git rev-parse --show-toplevel 2>/dev/null; or pwd)

    if test "$worktree_path" = "$current_path"
        return 0
    end

    wt switch (echo "$selection" | sed 's/.*\[//;s/\]//')
end
