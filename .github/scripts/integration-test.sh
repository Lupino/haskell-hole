#!/usr/bin/env bash
set -euo pipefail

MODE="${1:-}"

if [[ "${MODE}" != "l2r" && "${MODE}" != "r2l" ]]; then
  echo "Usage: $0 <l2r|r2l>"
  exit 2
fi

print_logs() {
  echo "==== ${BACKEND_LOG} ===="
  cat "${BACKEND_LOG}" || true
  echo "==== ${HOLED_LOG} ===="
  cat "${HOLED_LOG}" || true
  echo "==== ${HOLE_LOG} ===="
  cat "${HOLE_LOG}" || true
}

pick_free_port() {
  # Randomized high port to avoid collisions across repeated local runs.
  echo "$((20000 + RANDOM % 30000))"
}

ensure_running() {
  if ! kill -0 "${BACKEND_PID}" 2>/dev/null; then
    echo "Backend process exited early"
    print_logs
    exit 1
  fi
  if ! kill -0 "${HOLED_PID}" 2>/dev/null; then
    echo "holed process exited early"
    print_logs
    exit 1
  fi
  if ! kill -0 "${HOLE_PID}" 2>/dev/null; then
    echo "hole process exited early"
    print_logs
    exit 1
  fi
}

kill_tree() {
  local pid="$1"
  local sig="$2"
  local child

  if ! kill -0 "${pid}" 2>/dev/null; then
    return 0
  fi

  for child in $(pgrep -P "${pid}" 2>/dev/null || true); do
    kill_tree "${child}" "${sig}"
  done

  kill "-${sig}" "${pid}" 2>/dev/null || true
}

if [[ "${MODE}" == "l2r" ]]; then
  BACKEND_PORT="$(pick_free_port)"
  HOLE_PORT="$(pick_free_port)"
  OUT_PORT="$(pick_free_port)"
  BACKEND_LOG=/tmp/l2r-backend.log
  HOLED_LOG=/tmp/l2r-holed.log
  HOLE_LOG=/tmp/l2r-hole.log

  python3 -m http.server "${BACKEND_PORT}" --bind 127.0.0.1 >"${BACKEND_LOG}" 2>&1 &
  BACKEND_PID=$!

  stack exec -- holed \
    -H "tcp://127.0.0.1:${HOLE_PORT}" \
    --addr "tcp://127.0.0.1:${OUT_PORT}" >"${HOLED_LOG}" 2>&1 &
  HOLED_PID=$!

  # Avoid flaky startup races where hole connects before holed is listening.
  sleep 1

  stack exec -- hole \
    -H "tcp://127.0.0.1:${HOLE_PORT}" \
    --addr "tcp://127.0.0.1:${BACKEND_PORT}" >"${HOLE_LOG}" 2>&1 &
  HOLE_PID=$!

  FAILURE_MSG="Local to Remote integration test failed"
else
  BACKEND_PORT="$(pick_free_port)"
  HOLE_PORT="$(pick_free_port)"
  OUT_PORT="$(pick_free_port)"
  BACKEND_LOG=/tmp/r2l-backend.log
  HOLED_LOG=/tmp/r2l-holed.log
  HOLE_LOG=/tmp/r2l-hole.log

  python3 -m http.server "${BACKEND_PORT}" --bind 127.0.0.1 >"${BACKEND_LOG}" 2>&1 &
  BACKEND_PID=$!

  stack exec -- holed \
    -H "tcp://127.0.0.1:${HOLE_PORT}" \
    --addr "tcp://127.0.0.1:${BACKEND_PORT}" \
    --use-remote-to-local >"${HOLED_LOG}" 2>&1 &
  HOLED_PID=$!

  # Avoid flaky startup races where hole connects before holed is listening.
  sleep 1

  stack exec -- hole \
    -H "tcp://127.0.0.1:${HOLE_PORT}" \
    --addr "tcp://127.0.0.1:${OUT_PORT}" \
    --use-remote-to-local >"${HOLE_LOG}" 2>&1 &
  HOLE_PID=$!

  FAILURE_MSG="Remote to Local integration test failed"
fi

cleanup() {
  kill_tree "${HOLE_PID}" TERM
  kill_tree "${HOLED_PID}" TERM
  kill_tree "${BACKEND_PID}" TERM
  sleep 0.2
  kill_tree "${HOLE_PID}" KILL
  kill_tree "${HOLED_PID}" KILL
  kill_tree "${BACKEND_PID}" KILL
  wait "${HOLE_PID}" "${HOLED_PID}" "${BACKEND_PID}" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

for _i in $(seq 1 40); do
  ensure_running
  if curl -fsS "http://127.0.0.1:${OUT_PORT}/" >/dev/null 2>&1; then
    exit 0
  fi
  sleep 0.25
done

echo "${FAILURE_MSG}"
print_logs
exit 1
