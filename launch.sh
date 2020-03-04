#!/bin/sh

sudo cp surfboard.plist /Library/LaunchDaemons

sudo launchctl load /Library/LaunchDaemons/surfboard.plist

sudo launchctl list | fgrep surfboard

