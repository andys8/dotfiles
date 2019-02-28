workflow "CI" {
  on = "push"
  resolves = ["shellcheck"]
}

action "shellcheck" {
  uses = "actions/bin/shellcheck@master"
  args = "*.sh"
}
