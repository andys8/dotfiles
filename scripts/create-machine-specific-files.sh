#!/bin/bash

files=(
	~/.gitconfig.machine
	~/.profile.machine
	~/.vimrc.machine
	~/bin/startup.sh
)

# Create missing files
for file in "${files[@]}"; do
	if [ ! -f "$file" ]; then
		echo "File '$file' was missing, and is now created"
		touch "$file"
	fi
done

echo "Machine specific files exist"
