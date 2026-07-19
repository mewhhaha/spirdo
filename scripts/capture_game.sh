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
last_ship_match_luma="missing"
last_crystal_match_luma="missing"

require_game_running() {
  if kill -0 "$game_pid" 2>/dev/null; then
    return
  fi

  cat "$run_dir/game.log" >&2
  exit 1
}

scene_matches_capture() {
  local ship_statistics
  local crystal_statistics

  if ! ship_statistics="$(
    ffmpeg -hide_banner -nostats -loglevel info \
      -i "$output_path" \
      -vf "crop=80:80:440:250,format=rgb24,geq=r='if(gte(b(X,Y),140)*gte(g(X,Y),90)*gte(b(X,Y),r(X,Y)*1.2),255,0)':g='if(gte(b(X,Y),140)*gte(g(X,Y),90)*gte(b(X,Y),r(X,Y)*1.2),255,0)':b='if(gte(b(X,Y),140)*gte(g(X,Y),90)*gte(b(X,Y),r(X,Y)*1.2),255,0)',signalstats,metadata=print" \
      -f null - 2>&1
  )"; then
    echo "failed to validate the captured ship region" >&2
    printf '%s\n' "$ship_statistics" >&2
    exit 1
  fi
  if ! crystal_statistics="$(
    ffmpeg -hide_banner -nostats -loglevel info \
      -i "$output_path" \
      -vf "crop=80:80:315:170,format=rgb24,geq=r='if(gte(r(X,Y),150)*gte(r(X,Y),g(X,Y)*1.2)*gte(r(X,Y),b(X,Y)*1.5),255,0)':g='if(gte(r(X,Y),150)*gte(r(X,Y),g(X,Y)*1.2)*gte(r(X,Y),b(X,Y)*1.5),255,0)':b='if(gte(r(X,Y),150)*gte(r(X,Y),g(X,Y)*1.2)*gte(r(X,Y),b(X,Y)*1.5),255,0)',signalstats,metadata=print" \
      -f null - 2>&1
  )"; then
    echo "failed to validate the captured crystal region" >&2
    printf '%s\n' "$crystal_statistics" >&2
    exit 1
  fi

  last_ship_match_luma="$(sed -n 's/.*lavfi\.signalstats\.YAVG=//p' <<<"$ship_statistics")"
  last_crystal_match_luma="$(sed -n 's/.*lavfi\.signalstats\.YAVG=//p' <<<"$crystal_statistics")"
  if [[ -z "$last_ship_match_luma" || -z "$last_crystal_match_luma" ]]; then
    echo \
      "capture validation did not produce match coverage: ship YAVG=${last_ship_match_luma:-missing}; crystal YAVG=${last_crystal_match_luma:-missing}" \
      >&2
    exit 1
  fi

  awk -v ship="$last_ship_match_luma" -v crystal="$last_crystal_match_luma" \
    'BEGIN { exit !(ship >= 22 && crystal >= 18.5) }'
}

stop_child() {
  local child_pid="$1"
  local watchdog_pid
  if [[ -z "$child_pid" ]]; then
    return
  fi

  kill "$child_pid" 2>/dev/null || true
  (
    sleep 2
    kill -KILL "$child_pid" 2>/dev/null || true
  ) &
  watchdog_pid=$!
  wait "$child_pid" 2>/dev/null || true
  kill "$watchdog_pid" 2>/dev/null || true
  wait "$watchdog_pid" 2>/dev/null || true
}

cleanup() {
  stop_child "$game_pid"
  stop_child "$xvfb_pid"
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

for command_name in Xvfb awk cabal ffmpeg; do
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
  "SPIRDO_CAPTURE_READY_FILE=$run_dir/game-ready"
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
  if [[ "$SPIRDO_VULKAN_LIB_DIR" != /* || ! -d "$SPIRDO_VULKAN_LIB_DIR" ]]; then
    echo "SPIRDO_VULKAN_LIB_DIR must name an existing absolute directory: $SPIRDO_VULKAN_LIB_DIR" >&2
    exit 1
  fi
  game_environment+=("LD_LIBRARY_PATH=$SPIRDO_VULKAN_LIB_DIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}")
fi

"${game_environment[@]}" "$game_binary" >"$run_dir/game.log" 2>&1 &
game_pid=$!

for _ in {1..100}; do
  if [[ -s "$run_dir/game-ready" ]]; then
    break
  fi
  require_game_running
  sleep 0.1
done

if [[ ! -s "$run_dir/game-ready" ]]; then
  echo "game did not report capture readiness within 10 seconds" >&2
  require_game_running
  exit 1
fi

mkdir -p "$(dirname "$output_path")"
scene_visible=false
for _ in {1..40}; do
  require_game_running
  ffmpeg_status=0
  ffmpeg -hide_banner -loglevel error \
    -f x11grab -draw_mouse 0 -video_size 960x540 -i "$display.0" \
    -frames:v 1 -y "$output_path" || ffmpeg_status=$?
  require_game_running
  if (( ffmpeg_status != 0 )); then
    exit "$ffmpeg_status"
  fi
  if scene_matches_capture; then
    scene_visible=true
    break
  fi
  sleep 0.1
done

if [[ "$scene_visible" != true ]]; then
  echo \
    "capture did not contain sufficient matching pixels before the deadline: ship YAVG=$last_ship_match_luma (required 22); crystal YAVG=$last_crystal_match_luma (required 18.5)" \
    >&2
  exit 1
fi

capture_succeeded=true
echo "$output_path"
