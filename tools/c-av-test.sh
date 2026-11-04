#!/usr/bin/env bash
# Call between the Haskell peer and the c-toxcore peer, with a local network of
# C DHT nodes. MODE "hs-call" has the Haskell peer call, "c-call" the C peer.
# The C peer sends back the audio and video it gets, and the Haskell peer
# hangs up after a few seconds.
set -eu

C_PEER="$1"
AV_PEER="$2"
MODE="$3"
WORK="${TEST_TMPDIR:-$(mktemp -d)}"

C_PID=
AV_PID=
cleanup() {
  kill $C_PID $AV_PID 2>/dev/null || true
  wait 2>/dev/null || true
}
trap cleanup EXIT

# wait_for FILE PATTERN SECONDS
wait_for() {
  for _ in $(seq $(($3 * 10))); do
    if grep -q "$2" "$1"; then
      return 0
    fi
    sleep 0.1
  done
  return 1
}

fail() {
  echo "FAIL: $1"
  for log in "$WORK"/*.log; do
    echo "=== $log"
    tail -n 50 "$log"
  done
  exit 1
}

case "$MODE" in
  hs-call) C_AV=--av AV_ROLE=--call ;;
  c-call) C_AV=--av-call AV_ROLE=--answer ;;
  *) fail "unknown mode $MODE" ;;
esac

"$C_PEER" --dht-nodes 8 --accept-requests "$C_AV" >"$WORK/c-peer.log" 2>"$WORK/c-peer-stderr.log" &
C_PID=$!
wait_for "$WORK/c-peer.log" '^TOX_ID ' 10 || fail "C peer did not start"
C_ID=$(sed -n 's/^TOX_ID //p' "$WORK/c-peer.log")
C_NODE="127.0.0.1:$(sed -n 's/^UDP_PORT //p' "$WORK/c-peer.log"):$(sed -n 's/^DHT_ID //p' "$WORK/c-peer.log")"

"$AV_PEER" --bootstrap "$C_NODE" --add "$C_ID" "$AV_ROLE" >"$WORK/av-peer.log" 2>&1 &
AV_PID=$!

wait_for "$WORK/c-peer.log" '^FRIEND_CONNECTION 0 TOX_CONNECTION_' 30 || fail "the peers did not connect"
if [ "$MODE" = hs-call ]; then
  wait_for "$WORK/c-peer.log" '^AV_CALL 0 1 1$' 10 || fail "C peer was not called"
  wait_for "$WORK/av-peer.log" '^PEER_CHANGED True True$' 10 || fail "Haskell peer did not see the answer"
else
  wait_for "$WORK/av-peer.log" '^INVITED True True$' 10 || fail "Haskell peer was not called"
  wait_for "$WORK/av-peer.log" '^ANSWER Right ()$' 10 || fail "Haskell peer could not answer"
fi
wait_for "$WORK/c-peer.log" '^AV_AUDIO 0 960 1 48000$' 10 || fail "C peer got no audio"
wait_for "$WORK/c-peer.log" '^AV_VIDEO 0 160 120$' 10 || fail "C peer got no video"
wait_for "$WORK/av-peer.log" '^AUDIO_RECEIVED 48000 1 960$' 10 || fail "Haskell peer got no audio back"
wait_for "$WORK/av-peer.log" '^VIDEO_RECEIVED 160 120$' 10 || fail "Haskell peer got no video back"
wait_for "$WORK/av-peer.log" '^HUNG_UP Right ()$' 10 || fail "Haskell peer did not hang up"
wait_for "$WORK/c-peer.log" '^AV_STATE 0 2$' 10 || fail "C peer did not see the call end"

echo "PASS"
