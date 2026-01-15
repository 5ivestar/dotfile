#!/usr/bin/env bash
set -euo pipefail

cmd="${1:-}"
shift || true

die() { echo "gworktree: $*" >&2; exit 1; }

need_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

is_git_repo() {
  git rev-parse --is-inside-work-tree >/dev/null 2>&1
}

usage() {
  cat >&2 <<'EOF'
Usage:
  gworktree add [-b] <branch>
    -b : create new branch <branch> from current HEAD

  gworktree list
    calls: gwq list

  gworktree remove
    calls: gwq remove
EOF
  exit 2
}

# Copy .vscode/settings.json from current repo to new worktree and exclude from tracking (per-worktree)
copy_and_exclude_vscode_settings() {
  local src_root="$1"
  local dst_root="$2"

  local src_settings="$src_root/.vscode/settings.json"
  local dst_settings="$dst_root/.vscode/settings.json"

  if [[ -f "$src_settings" ]]; then
    mkdir -p "$dst_root/.vscode"
    cp -f "$src_settings" "$dst_settings"
  else
    # nothing to copy; still exclude (in case it gets created later)
    mkdir -p "$dst_root/.vscode"
  fi

  # Per-worktree exclude: write into that worktree's git dir info/exclude
  local dst_gitdir
  dst_gitdir="$(git -C "$dst_root" rev-parse --git-dir)"
  mkdir -p "$dst_gitdir/info"

  local exclude_file="$dst_gitdir/info/exclude"
  # avoid duplicates
  if [[ ! -f "$exclude_file" ]] || ! grep -qxF ".vscode/settings.json" "$exclude_file"; then
    echo ".vscode/settings.json" >> "$exclude_file"
  fi

  # If the file is tracked, .git/info/exclude won't stop tracking.
  # Mark it skip-worktree in THIS worktree so changes won't show up.
  if git -C "$dst_root" ls-files --error-unmatch ".vscode/settings.json" >/dev/null 2>&1; then
    git -C "$dst_root" update-index --skip-worktree ".vscode/settings.json" || true
  fi
}

# Copy all .env files from current repo to new worktree (preserving directory structure)
copy_env_files() {
  local src_root="$1"
  local dst_root="$2"

  # Find all .env files recursively
  while IFS= read -r -d '' src_env; do
    # Get relative path from src_root
    local rel_path="${src_env#$src_root/}"
    local dst_env="$dst_root/$rel_path"
    local dst_dir
    dst_dir="$(dirname "$dst_env")"

    mkdir -p "$dst_dir"
    cp -f "$src_env" "$dst_env"
    echo "  copied: $rel_path"
  done < <(find "$src_root" -name ".env" -type f -print0 2>/dev/null)
}

case "$cmd" in
  add)
    need_cmd git
    is_git_repo || die "run inside a git repository"

    create_branch=false
    if [[ "${1:-}" == "-b" ]]; then
      create_branch=true
      shift
    fi

    branch="${1:-}"
    [[ -n "$branch" ]] || usage

    # Current directory name as prefix (e.g., nexus)
    cur_root="$(pwd)"
    prefix="$(basename "$cur_root")"

    new_dir="$(cd .. && pwd)/${prefix}-${branch}"

    if [[ -e "$new_dir" ]]; then
      die "target directory already exists: $new_dir"
    fi

    if $create_branch; then
      # Create branch from current HEAD and add worktree
      git worktree add -b "$branch" "$new_dir"
    else
      # Add existing branch as worktree
      git worktree add "$new_dir" "$branch"
    fi

    copy_and_exclude_vscode_settings "$cur_root" "$new_dir"
    copy_env_files "$cur_root" "$new_dir"

    echo "Created worktree:"
    echo "  dir:    $new_dir"
    echo "  branch: $branch"
    ;;

  list)
    need_cmd gwq
    gwq list
    ;;

  remove)
    need_cmd gwq
    gwq remove
    ;;

  ""|-h|--help|help)
    usage
    ;;

  *)
    usage
    ;;
esac
