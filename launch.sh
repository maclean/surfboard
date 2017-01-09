#!/bin/sh

sudo cp surfboard.plist /Library/LaunchDaemons
launchctl unload /Library/LaunchDaemons/surfboard.plist
launchctl load /Library/LaunchDaemons/surfboard.plist
# launchctl start local.surfboard
