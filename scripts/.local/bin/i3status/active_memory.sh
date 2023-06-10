#!/bin/sh

free -h | awk '/^Mem:/ { print $3-$6-$7 }'
