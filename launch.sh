#!/bin/sh

# Substitute some environment variables in a plist file, then
# load it into launchd

if [ $# -lt 1 ]; then
    echo "Usage: $0 whichplist"
    echo "Eg: $0 surfboard"
    exit 1
fi

which=$1

cd "$(dirname "$0")"
SURFPATH="$(pwd -P)"

if [ ! -f $which.plist ]; then
    echo "$which.plist does not exist"
    exit 1
fi

SURFDIR=${SURFDIR:=$HOME/surfboard}
if [ ! -d $SURFDIR ]; then
    echo "$SURFDIR directory does not exist. Create it, or set SURFDIR to the path"
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

SURF_IP=${SURF_IP:=192.168.100.1}

echo "USER=$USER"
echo "SURFDIR=$SURFDIR"
echo "SURFPATH=$SURFPATH"
echo "SURF_IP=$SURF_IP"

tmpfile=$(mktemp)
trap 'rm -f $tmpfile;' EXIT

sed -e s/\$USER/$USER/ -e s,\$SURFDIR,$SURFDIR, -e s,\$SURFPATH,$SURFPATH, -e s/\$SURF_IP/$SURF_IP/ $which.plist > $tmpfile

sudo cp $tmpfile /Library/LaunchDaemons/$which.plist

sudo launchctl load /Library/LaunchDaemons/$which.plist

sudo launchctl list | fgrep $which

