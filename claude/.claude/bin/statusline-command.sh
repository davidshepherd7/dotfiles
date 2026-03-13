#!/bin/bash
input=$(cat)

model=$(echo "$input" | jq -r '.model.display_name')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
cwd=$(echo "$input" | jq -r '.workspace.current_dir')

# Git branch (skip optional locks to avoid blocking)
branch=$(git -C "$cwd" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null)

# Build progress bar (10 chars wide)
if [ -n "$used" ]; then
    filled=$(echo "$used" | awk '{printf "%d", ($1 / 10 + 0.5)}')
    bar=""
    for i in $(seq 1 10); do
        if [ "$i" -le "$filled" ]; then
            bar="${bar}#"
        else
            bar="${bar}-"
        fi
    done
    context_str="[${bar}] ${used}%"
else
    context_str="[----------] --%"
fi

# Compose output
if [ -n "$branch" ]; then
    printf "\033[32m%s\033[0m  %s  \033[33m%s\033[0m" "$model" "$context_str" "$branch"
else
    printf "\033[32m%s\033[0m  %s" "$model" "$context_str"
fi
