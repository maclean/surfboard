
# Read and plot signal quality data that has been extracted
# by surfboard.py from a Surfboard SB6190 cable modem.
# This R code depends on the "eolts" and "isfs" R packages
# at http://www.eol.ucar.edu/software/R/, for manipulating
# and plotting time-series.

surfboard <- function(file=file.path(Sys.getenv("HOME"),"surfboard.dat.gz"))
{
    x <- scan(file=file,sep=",", quiet=TRUE, what=list(
            datetime="", channel=1, status="",
            modulation="", id=1, freq=1.0, power=1.0,
            snr=1.0, corrected=1,uncorrected=1))

    times <- utime(x$datetime,in.format="%Y-%m-%d %H:%M:%S")
    uchans <- unique(x$channel)

    tsl <- list()

    for (chan in uchans) {
        ic <- x$channel == chan
        dx <- c(x$power[ic],x$snr[ic],
                x$corrected[ic], x$uncorrected[ic])
        tx <- times[ic]
        xts <- dat(nts(matrix(dx,ncol=4), tx,
            names=c("power","snr","corr","uncorr"),
            units=c("dBmV","dB","","")))

        tsl[chan] <- list(list(ts=xts,freq=unique(x$freq[ic]),
		id=unique(x$id[ic]),frequnits="MHz"))
    }
    tsl
}

plotsurf <- function()
{
    Sys.setenv(PROJECT="")
    x <- surfboard()
    par(mfrow=c(2,2),ask=TRUE)
    nc <- length(x)
    nval <- NULL
    corr <- NULL
    uncorr <- NULL
    for (ic in 1:nc) {
        corx <- x[[ic]]$ts[-1,"corr"]
        corx@data <- diff(x[[ic]]$ts[,"corr"]@data)
        uncorx <- x[[ic]]$ts[-1,"uncorr"]
        uncorx@data <- diff(x[[ic]]$ts[,"uncorr"]@data)

        nvalx <- corx + uncorx
        colnames(nvalx) <- "total"
        nval <- Cbind(nval,nvalx)
        corr <- Cbind(corr,corx)
        uncorr <- Cbind(uncorr,uncorx)
    }
    # look for modem restarts when the successive difference of
    # correctables or uncorrectables are negative.
    restart <- dat(nts(apply(corr@data,1,function(x) { any(!is.na(x) & x < 0)}),
            positions(corr),names="restarts",units="")) |
        dat(nts(apply(uncorr@data,1,function(x) { any(!is.na(x) & x < 0)}),
            positions(uncorr),names="restarts",units=""))

    for (ic in 1:nc) {
        if (!all(is.na(nval[,ic])) && max(nval[,ic],na.rm=TRUE) > 10) {
            t1 <- start(x[[ic]]$ts)
            t2 <- end(x[[ic]]$ts)

            freq <- x[[ic]]$freq
            funits <- x[[ic]]$frequnits
            id <- x[[ic]]$id
            titlestr <- paste0("Channel ",ic, ", id=",id,
                ", freq=",freq," ",funits)

            pos  <- !is.na(nval[!restart,ic]) & nval[!restart,ic] > 0
            plot(nval[pos,ic],title=titlestr,type="b",xlim=c(t1,t2))

            pcu <- uncorr[pos,ic] / nval[pos,ic] * 100
            units(pcu) <- "%"
            plot(pcu,title=titlestr,type="b",xlim=c(t1,t2))

            plot(x[[ic]]$ts[,"snr"],title=titlestr,type="b",xlim=c(t1,t2))
            plot(x[[ic]]$ts[,"power"],title=titlestr,type="b",xlim=c(t1,t2))
        }
    }
}
