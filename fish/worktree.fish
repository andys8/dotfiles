function worktree --description "Create a git worktree and cd to it"
    set -l repo_name (basename (git rev-parse --show-toplevel))
    set -l default_branch (git default-branch)
    set -l current_branch (git branch --show-current)
    set -l branch

    if test -z "$argv[1]"
        set branch $current_branch
        if test "$branch" = "$default_branch"
            echo "Error: Cannot create worktree from default branch without specifying a branch name" >&2
            return 1
        end
        echo "No branch specified, using current branch: $branch"
        echo "Switching to $default_branch first..."
        git checkout $default_branch
    else
        set branch $argv[1]
        echo "Using branch: $branch"
    end

    set -l branch_safe (string replace -a / - $branch)
    set -l worktree_path "../$repo_name-$branch_safe"

    echo "Creating worktree at: $worktree_path"
    git worktree add "$worktree_path" "$branch"
    echo "Worktree created successfully at: $worktree_path"

    cd "$worktree_path"
end
