#!/bin/sh

in=$(mktemp /tmp/plotsurf_XXXXXX)
out=$(mktemp /tmp/plotsurf_XXXXXX)
# echo $in $out

trap '{ rm $in $out; }' EXIT

cat > $in<< EOD
pdf(file="$HOME/surfboard/plotsurf.pdf", onefile=TRUE)
err <- tryCatch(plotsurf(), error = function(e) e)

status <- 0
if (is(err, "error")) {
    print(err)
    status <- 10
}

q(save="no", status=status)
EOD

R --slave --no-save CMD BATCH $in $out

[ $? -ne 0 ] && cat $out 1>&2
