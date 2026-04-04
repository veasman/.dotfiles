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

# Wait for evdi devices to appear (udev can be slow)
echo ">> Waiting for evdi devices..."
i=0
while [ $i -lt 10 ]; do
  ls /dev/evdi* >/dev/null 2>&1 && break
  sleep 1
  i=$((i + 1))
done

if ls /dev/evdi* >/dev/null 2>&1; then
  echo ">> evdi devices found: $(ls /dev/evdi*)"
else
  echo "!! No /dev/evdi* devices found after 10s."
  echo "   Make sure the dock is plugged in, then run this script again."
  exit 1
fi

# Apply xrandr config
echo ">> Applying xrandr settings..."
xrandr --setprovideroutputsource 1 0 || true
xrandr --setprovideroutputsource 2 0 || true
xrandr \
  --output eDP-1 --off \
  --output DVI-I-1-1 --mode 1920x1080 --rotate left --pos 0x0 \
  --output DP-2 --mode 1920x1080 --rotate normal --primary --pos 1080x0 \
  --output DVI-I-2-2 --mode 1920x1080 --rotate normal --pos 3000x0 \
  || true

echo ">> Done!"
