#!/bin/sh

sudo launchctl list | fgrep surfboard
sudo launchctl stop local.surfboard
sudo launchctl remove local.surfboard
sudo launchctl list | fgrep surfboard
sudo rm -f /Library/LaunchAgents/surfboard.plist

