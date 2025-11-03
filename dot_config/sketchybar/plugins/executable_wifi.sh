#!/usr/bin/env sh
# plugins/wifi.sh

# --- colors & config ---------------------------------------------------------
. "$CONFIG_DIR/colors.sh"   # provides $PEACH, $WHITE, etc.  :contentReference[oaicite:1]{index=1}
. "$CONFIG_DIR/icons.sh"

ICON_WIFI="$WIFI_CONN_ICON" # choose what you like
ICON_WIRED="󰈁"              # Nerd Font ethernet (or "⎈", "ETH")
ICON_DOWN="􀙧"              # disconnected

# --- mouse hover handling (color only) ---------------------------------------
if [ "$SENDER" = "mouse.entered" ]; then
  # make label peach on hover
  sketchybar --set "$NAME" label.color=$PEACH icon.color=$WHITE
  exit 0
elif [ "$SENDER" = "mouse.exited" ]; then
  # restore default label color to white on exit
  sketchybar --set "$NAME" label.color=$WHITE icon.color=$PEACH
  exit 0
fi

# --- connectivity detection --------------------------------------------------
# find Wi-Fi device (e.g., en0)
WIFI_DEV="$(networksetup -listallhardwareports \
  | awk '/^Hardware Port: Wi-Fi$/{getline; sub(/^Device: /,""); print $0}')"

# any active wired en* that isn't Wi-Fi?
ETH_ACTIVE=""
for dev in $(ifconfig -l | tr ' ' '\n' | grep '^en'); do
  [ "$dev" = "$WIFI_DEV" ] && continue
  if ifconfig "$dev" 2>/dev/null | grep -q 'status: active'; then
    ETH_ACTIVE="$dev"
    break
  fi
done

if [ -n "$ETH_ACTIVE" ]; then
  # wired: set icon + label text; DO NOT touch label.color here
  sketchybar --set "$NAME" icon="$ICON_WIRED" label="Wired"
  exit 0
fi

# otherwise, check Wi-Fi SSID
SSID="$($CONFIG_DIR/bin/ssid.zsh)"

if [ -n "$SSID" ]; then
  sketchybar --set "$NAME" icon="$ICON_WIFI" label="$SSID"
else
  sketchybar --set "$NAME" icon="$ICON_DOWN" label="Disconnected"
fi

