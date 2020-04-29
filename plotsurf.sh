#!/bin/sh

in=$(mktemp /tmp/plotsurf_XXXXXX)
out=$(mktemp /tmp/plotsurf_XXXXXX)
echo $in $out

trap '{ rm $in $out; }' EXIT

cat > $in<< EOD
png(file="$HOME/surfboard/plotsurf1.png")
png(file="$HOME/surfboard/plotsurf2.png")
plotsurf()
EOD

R --slave --no-save CMD BATCH $in $out

[ $status ] && cat $out 1>&2
