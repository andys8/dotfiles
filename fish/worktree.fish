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

    if not git show-ref --verify --quiet refs/heads/$branch
        echo "Branch not found locally, fetching from remote..."
        git fetch

        # Check if branch exists remotely or locally after fetch
        if not git show-ref --verify --quiet refs/heads/$branch; and not git show-ref --verify --quiet refs/remotes/origin/$branch
            echo "Branch '$branch' does not exist. Creating new branch from $default_branch..."
            git branch $branch $default_branch
        end
    end

    echo "Creating worktree at: $worktree_path"
    git worktree add "$worktree_path" "$branch"
    echo "Worktree created successfully at: $worktree_path"

    cd "$worktree_path"

    echo "Installing tools"
    if asdf install
        echo "Tools installed successfully"
    else
        echo "Warning: Failed to asdf install faled" >&2
    end

    echo "Installing dependencies..."
    if ni
        echo "Dependencies installed successfully"
    else
        echo "Warning: Failed to install dependencies with 'ni'" >&2
    end
end
