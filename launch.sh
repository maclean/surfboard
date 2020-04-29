#!/bin/sh

which=$1

sudo cp $which.plist /Library/LaunchDaemons

sudo launchctl load /Library/LaunchDaemons/$which.plist

sudo launchctl list | fgrep $which

