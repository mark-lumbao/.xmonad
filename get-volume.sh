#!/bin/bash
# Get the maximum volume of any pulseaudio default sink channel
# Dependency: pactl, grep, xargs, awk
vol=$(pactl get-sink-volume $(pactl get-default-sink) | grep -Eo '(.|..|...)%' | xargs | awk 'BEGIN {FS=" "} {print $1}')

echo $vol

exit 0
