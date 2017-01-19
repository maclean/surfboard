#!/bin/sh

sudo cp surfboard.plist /Library/LaunchDaemons
if false; then
    launchctl unload /Library/LaunchDaemons/surfboard.plist
    launchctl load /Library/LaunchDaemons/surfboard.plist
    launchctl start local.surfboard
else

    # not sure bootstrap is needed
    sudo launchctl bootstrap user/501/local.surfboard \
	/Library/LaunchDaemons/surfboard.plist

    launchctl enable user/501/local.surfboard
    launchctl load /Library/LaunchDaemons/surfboard.plist

    launchctl list | fgrep surfboard
    launchctl print gui/501/local.surfboard
fi

