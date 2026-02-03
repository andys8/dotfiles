function worktree --description "Create a git worktree and cd to it"
    set -l worktree_path (git-worktree-create $argv)
    or return 1

    cd "$worktree_path"
end
