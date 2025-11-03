#!/bin/bash

. "$CONFIG_DIR/colors.sh"   # provides $PEACH, $WHITE, etc.  :contentReference[oaicite:1]{index=1}

CORE_COUNT=$(sysctl -n machdep.cpu.thread_count)
CPU_INFO=$(ps -eo pcpu,user)
CPU_SYS=$(echo "$CPU_INFO" | grep -v $(whoami) | sed "s/[^ 0-9\.]//g" | awk "{sum+=\$1} END {print sum/(100.0 * $CORE_COUNT)}")
CPU_USER=$(echo "$CPU_INFO" | grep $(whoami) | sed "s/[^ 0-9\.]//g" | awk "{sum+=\$1} END {print sum/(100.0 * $CORE_COUNT)}")

CPU_PERCENT="$(echo "$CPU_SYS $CPU_USER" | awk '{printf "%.0f\n", ($1 + $2)*100}')"

if [ "$SENDER" = "mouse.entered" ]; then
  # make label peach on hover
  sketchybar --set "$NAME" label.color=$RED icon.color=$WHITE
  exit 0
elif [ "$SENDER" = "mouse.exited" ]; then
  # restore default label color to white on exit
  sketchybar --set "$NAME" label.color=$WHITE icon.color=$RED
  exit 0
fi

sketchybar --set $NAME label="$CPU_PERCENT%"
