#!/bin/sh

which=$1

sudo launchctl list | fgrep $which
sudo launchctl stop local.$which
sudo launchctl remove local.$which
sudo launchctl list | fgrep $which
sudo rm -f /Library/LaunchAgents/$which.plist

