#!/bin/sh

sudo launchctl list | fgrep surfboard
sudo launchctl stop local.surfboard
sudo launchctl remove local.surfboard
sudo launchctl list | fgrep surfboard
sudo rm /Library/LaunchAgents/surfboard.plist

