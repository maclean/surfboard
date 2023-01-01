#!/bin/sh

# ping
PATH=$PATH:/sbin

in=$(mktemp /tmp/plotsurf_XXXXXX)
out=$(mktemp /tmp/plotsurf_XXXXXX)

trap '{ rm $in $out; }' EXIT

# pdf=$HOME/surfboard/surfboard_$(date +%Y).pdf
pdf=$HOME/surfboard/surfboard.pdf

cat > $in << EOD
pdf(file="$pdf", onefile=TRUE, title="Surfboard Status")
err <- tryCatch(plotsurf(logerr=TRUE), error = function(e) e)

status <- 0
if (is(err, "error")) {
    print(err)
    status <- 10
}

dev.off()
q(save="no", status=status)
EOD

R --slave --no-save CMD BATCH $in $out

[ $? -ne 0 ] && cat $out 1>&2

# Copy plots to router. Use ping to wakeup network if necessary
ping -q -o -c 4 openwrt.lan > $out 2>&1 || cat $out 1>&2

scp $pdf /tmp/surfboard.err openwrt.lan:/www/surfboard 1>&2

