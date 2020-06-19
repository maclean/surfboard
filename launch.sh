#!/bin/sh

if [ $# -lt 1 ]; then
    echo "Usage: $0 whichplist"
    echo "Eg: $0 surfboard"
    exit 1
fi

which=$1

sed -e s/\$USER/$USER/ -e s,\$HOME,$HOME, $which.plist > /tmp/$which.plist

sudo cp /tmp/$which.plist /Library/LaunchDaemons

sudo launchctl load /Library/LaunchDaemons/$which.plist

sudo launchctl list | fgrep $which

