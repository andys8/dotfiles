#!/usr/bin/fish

omf --version

if [ $status -ne 0 ]
  echo "Install omf"
  curl -L https://get.oh-my.fish | fish
end

omf update
