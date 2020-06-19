#!/bin/sh

# Install a user plist file in launchd, substituting some
# environment variables first

if [ $# -lt 1 ]; then
    echo "Usage: $0 whichplist"
    echo "Eg: $0 surfboard"
    exit 1
fi

which=$1

if [ ! -f $which.plist ]; then
    echo "$which.plist does not exist"
    exit 1
fi

SURFDIR=${SURFDIR:=$HOME/surfboard}
if [ ! -d $SURFDIR ]; then
    echo "$SURFDIR directory does not exist. Create, or set SURFDIR appropriately"
    exit 1
fi

PASSWD=$SURFDIR/surfboard_password.txt
if [ ! -f $PASSWD ]; then
    echo "$PASSWD does not exist"
    exit 1
fi

if [ $(stat -L -f %Op $PASSWD) != "100600" ]; then
    echo "Changing permissions on $PASSWD"
    chmod 0600 $PASSWD
fi

SURFPATH="$(cd "$(dirname "$0")" && pwd -P)"
if [ ! -d $SURFPATH ]; then
    echo "$SURFPATH does not exist. Set SURFPATH appropriately"
    exit 1
fi

SURF_IP=${SURF_IP:=192.168.100.1}

echo "USER=$USER"
echo "SURFDIR=$SURFDIR"
echo "SURFPATH=$SURFPATH"
echo "SURF_IP=$SURF_IP"

sed -e s/\$USER/$USER/ -e s,\$SURFDIR,$SURFDIR, -e s,\$SURFPATH,$SURFPATH, -e s/\$SURF_IP/$SURF_IP/ $which.plist > /tmp/$which.plist

sudo cp /tmp/$which.plist /Library/LaunchDaemons

sudo launchctl load /Library/LaunchDaemons/$which.plist

sudo launchctl list | fgrep $which

