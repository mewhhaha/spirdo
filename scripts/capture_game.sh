#!/usr/bin/env bash
set -euo pipefail

output_path="${1:-/tmp/spirdo-game.png}"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
display_number=$((100 + $$ % 400))
display=":${display_number}"
run_dir="$(mktemp -d)"
xvfb_pid=""
game_pid=""

cleanup() {
  if [[ -n "$game_pid" ]]; then
    kill "$game_pid" 2>/dev/null || true
    wait "$game_pid" 2>/dev/null || true
  fi
  if [[ -n "$xvfb_pid" ]]; then
    kill "$xvfb_pid" 2>/dev/null || true
    wait "$xvfb_pid" 2>/dev/null || true
  fi
  rm -rf "$run_dir"
}
trap cleanup EXIT

for command_name in Xvfb ffmpeg cabal; do
  if ! command -v "$command_name" >/dev/null; then
    echo "game capture requires $command_name" >&2
    exit 1
  fi
done

Xvfb "$display" -screen 0 960x540x24 -nolisten tcp -ac -extension GLX \
  >"$run_dir/xvfb.log" 2>&1 &
xvfb_pid=$!

for _ in {1..50}; do
  if [[ -S "/tmp/.X11-unix/X${display_number}" ]]; then
    break
  fi
  if ! kill -0 "$xvfb_pid" 2>/dev/null; then
    cat "$run_dir/xvfb.log" >&2
    exit 1
  fi
  sleep 0.1
done

if [[ ! -S "/tmp/.X11-unix/X${display_number}" ]]; then
  echo "Xvfb did not create display $display" >&2
  exit 1
fi

game_binary="$(cd "$repo_root/examples" && cabal list-bin spirdo-examples:exe:spirdo-game)"
game_environment=(
  env
  "DISPLAY=$display"
  SDL_VIDEO_DRIVER=x11
  "VK_LOADER_LAYERS_DISABLE=~implicit~"
)

if [[ -n "${SPIRDO_VULKAN_ICD:-}" ]]; then
  game_environment+=("VK_ICD_FILENAMES=$SPIRDO_VULKAN_ICD")
fi
if [[ -n "${SPIRDO_VULKAN_LIB_DIR:-}" ]]; then
  game_environment+=("LD_LIBRARY_PATH=$SPIRDO_VULKAN_LIB_DIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}")
fi

"${game_environment[@]}" "$game_binary" >"$run_dir/game.log" 2>&1 &
game_pid=$!

for _ in {1..50}; do
  if ! kill -0 "$game_pid" 2>/dev/null; then
    cat "$run_dir/game.log" >&2
    exit 1
  fi
  sleep 0.1
done

mkdir -p "$(dirname "$output_path")"
ffmpeg -hide_banner -loglevel error \
  -f x11grab -video_size 960x540 -i "$display.0" \
  -frames:v 1 -y "$output_path"

echo "$output_path"
