#!/usr/bin/env bash
set -euo pipefail

if (( $# > 1 )); then
  echo "usage: $0 [output.png]" >&2
  exit 2
fi

output_path="${1:-/tmp/spirdo-game.png}"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
run_dir="$(mktemp -d)"
xvfb_pid=""
game_pid=""
capture_succeeded=false

cleanup() {
  if [[ -n "$game_pid" ]]; then
    kill "$game_pid" 2>/dev/null || true
    wait "$game_pid" 2>/dev/null || true
  fi
  if [[ -n "$xvfb_pid" ]]; then
    kill "$xvfb_pid" 2>/dev/null || true
    wait "$xvfb_pid" 2>/dev/null || true
  fi
  if [[ "$capture_succeeded" == true ]]; then
    rm -rf "$run_dir"
  else
    echo "game capture diagnostics: $run_dir" >&2
    for log_path in "$run_dir/xvfb.log" "$run_dir/game.log"; do
      if [[ -s "$log_path" ]]; then
        echo "== $log_path ==" >&2
        sed -n '1,200p' "$log_path" >&2
      fi
    done
  fi
}
trap cleanup EXIT

for command_name in Xvfb ffmpeg cabal; do
  if ! command -v "$command_name" >/dev/null; then
    echo "game capture requires $command_name" >&2
    exit 1
  fi
done

display_file="$run_dir/display"
Xvfb -displayfd 3 -screen 0 960x540x24 -nolisten tcp -ac -extension GLX \
  3>"$display_file" \
  >"$run_dir/xvfb.log" 2>&1 &
xvfb_pid=$!

for _ in {1..50}; do
  if [[ -s "$display_file" ]]; then
    break
  fi
  if ! kill -0 "$xvfb_pid" 2>/dev/null; then
    cat "$run_dir/xvfb.log" >&2
    exit 1
  fi
  sleep 0.1
done

if [[ ! -s "$display_file" ]]; then
  echo "Xvfb did not allocate a display" >&2
  exit 1
fi

read -r display_number <"$display_file"
display=":${display_number}"

(
  cd "$repo_root/examples"
  cabal build spirdo-examples:exe:spirdo-game
)
game_binary="$(cd "$repo_root/examples" && cabal list-bin spirdo-examples:exe:spirdo-game)"
game_environment=(
  env
  "DISPLAY=$display"
  SDL_VIDEO_DRIVER=x11
)

if [[ -n "${SPIRDO_VULKAN_ICD:-}" ]]; then
  if [[ "$SPIRDO_VULKAN_ICD" != /* || ! -f "$SPIRDO_VULKAN_ICD" ]]; then
    echo "SPIRDO_VULKAN_ICD must name an existing absolute ICD manifest: $SPIRDO_VULKAN_ICD" >&2
    exit 1
  fi
  game_environment+=("VK_DRIVER_FILES=$SPIRDO_VULKAN_ICD")
  game_environment+=("VK_ICD_FILENAMES=$SPIRDO_VULKAN_ICD")
  game_environment+=("VK_LOADER_LAYERS_DISABLE=~implicit~")
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

capture_succeeded=true
echo "$output_path"
