#!/usr/bin/env sh
set -e

# Fix udev.sh to use OpenRC instead of systemctl (safe to run repeatedly)
if grep -q 'systemctl' /opt/displaylink/udev.sh; then
  echo ">> Patching udev.sh to use OpenRC..."
  sed -i \
    's|systemctl start --no-block displaylink|rc-service displaylink start|g;
     s|systemctl stop displaylink|rc-service displaylink stop|g' \
    /opt/displaylink/udev.sh
  echo "   Done."
fi

# Load evdi kernel module if not already loaded
if ! lsmod | grep -q evdi; then
  echo ">> Loading evdi module..."
  modprobe evdi
else
  echo ">> evdi already loaded."
fi

# Make evdi load at boot if not already configured
if ! grep -q '^evdi' /etc/modules 2>/dev/null; then
  echo ">> Configuring evdi to load at boot..."
  echo 'evdi' >> /etc/modules
fi

# Reload udev rules
echo ">> Reloading udev rules..."
udevadm control --reload-rules
udevadm trigger --subsystem-match=usb

# Give udev a moment to create devices
sleep 2

# Restart DisplayLink service
echo ">> Restarting DisplayLink service..."
rc-service displaylink restart
sleep 2

# Wait for evdi devices to appear (udev can be slow).
# Modern evdi exposes DRM cards (e.g. /dev/dri/card2) instead of /dev/evdi*,
# so we accept either as success.
echo ">> Waiting for evdi devices..."
evdi_cards() {
  for c in /sys/class/drm/card*/device/uevent; do
    [ -f "$c" ] && grep -q '^DRIVER=evdi$' "$c" && echo "${c%/device/uevent}"
  done
}
i=0
while [ $i -lt 10 ]; do
  [ -n "$(evdi_cards)" ] && break
  ls /dev/evdi* >/dev/null 2>&1 && break
  sleep 1
  i=$((i + 1))
done

found_cards="$(evdi_cards)"
if [ -n "$found_cards" ] || ls /dev/evdi* >/dev/null 2>&1; then
  echo ">> evdi devices found:"
  [ -n "$found_cards" ] && echo "$found_cards" | sed 's|/sys/class/drm/|   - |'
  ls /dev/evdi* 2>/dev/null | sed 's|^|   - |'
else
  echo "!! No evdi devices found after 10s."
  echo "   Make sure the dock is plugged in, then run this script again."
  exit 1
fi
