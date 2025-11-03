#!/bin/bash
WIFI=(
  update_freq=10
  icon=$WIFI_CONN_ICON
  icon.color=$PEACH
  background.color=$BG_SEC_COLR
  script="$PLUGIN_DIR/wifi.sh"
	click_script='open "x-apple.systempreferences:com.apple.Network-Settings.extension"'
)

sketchybar --add item wifi right \
           --set wifi "${WIFI[@]}" \
           --subscribe wifi wifi_change mouse.entered mouse.exited
