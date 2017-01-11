
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
            SNR=1.0, CorrCw=1, UncorrCw=1))

    times <- utime(x$datetime,in.format="%Y-%m-%d %H:%M:%S")
    uchans <- unique(x$channel)

    tsl <- list()

    for (chan in uchans) {
        ic <- x$channel == chan
        dx <- c(x$power[ic],x$SNR[ic],
                x$CorrCw[ic], x$UncorrCw[ic])
        tx <- times[ic]
        xts <- dat(nts(matrix(dx,ncol=4), tx,
            names=c("power","SNR","CorrCw","UncorrCw"),
            units=c("dBmV","dB","","")))

        tsl[chan] <- list(list(ts=xts,freq=unique(x$freq[ic]),
		id=unique(x$id[ic]),frequnits="MHz"))
    }
    tsl
}

plotsurf <- function(channels=NULL)
{
    Sys.setenv(PROJECT="")
    x <- surfboard()
    par(mfrow=c(2,2),ask=TRUE)
    nc <- length(x)

    alltoo <- FALSE
    if (is.null(channels)) {
        channels <- 1:nc
        alltoo <- TRUE
    }
    else {
        ac <- !is.na(match(channels, 0))
        if (any(ac)) {
            channels <- channels[!ac]
            alltoo <- TRUE
        }
    }
       
    nval <- NULL
    corr <- NULL
    uncorr <- NULL
    snr <- NULL
    pow <- NULL
    for (ic in 1:nc) {

        corx <- d_by_dt(x[[ic]]$ts[-1,"CorrCw"],dtmax=86400,lag=1,time=1) * 3600
	units(corx) <- "hr^-1"

        uncorx <- d_by_dt(x[[ic]]$ts[-1,"UncorrCw"],dtmax=86400,lag=1,time=1) * 3600
	units(uncorx) <- "hr^-1"

        nvalx <- corx + uncorx
        colnames(nvalx) <- "CodeWords"
        nval <- Cbind(nval,nvalx)
        corr <- Cbind(corr,corx)
        uncorr <- Cbind(uncorr,uncorx)
        
        snr <- Cbind(snr, x[[ic]]$ts[,"SNR"])
        pow <- Cbind(pow, x[[ic]]$ts[,"power"])
    }
    # look for modem restarts when the successive difference of
    # correctables or uncorrectables are negative.
    restart <- dat(nts(apply(corr@data,1,function(x) { any(!is.na(x) & x < 0)}),
            positions(corr),names="restarts",units="")) |
        dat(nts(apply(uncorr@data,1,function(x) { any(!is.na(x) & x < 0)}),
            positions(uncorr),names="restarts",units=""))

    if (any(restart)) {
        nval[restart,] <- NA
        uncorr[restart,] <- NA
    }
        
    t1 <- start(x[[1]]$ts)
    # t1 <- utime("2017 jan 2 05:00")
    t2 <- end(x[[1]]$ts)

    clip("CodeWords", 0.5, 1.e100)
    clip("UncorrCW", 0, 100)

    for (ic in channels) {

        pos  <- !is.na(nval[,ic]) & nval[,ic]

        if (any(pos)) {

            freq <- x[[ic]]$freq
            funits <- x[[ic]]$frequnits
            id <- x[[ic]]$id
            titlestr <- paste0("Channel ",ic, ", id=",id,
                ", freq=",freq," ",funits)


            plot(nval[pos,ic],title=titlestr,type="b",xlim=c(t1,t2),log="y")

            pcu <- uncorr[pos,ic] / nval[pos,ic] * 100
	    colnames(pcu) <- "UncorrCW"
            units(pcu) <- "%"
            plot(pcu,title=titlestr,type="b",xlim=c(t1,t2))

            plot(snr[,ic],title=titlestr,type="b",xlim=c(t1,t2))
            plot(pow[,ic],title=titlestr,type="b",xlim=c(t1,t2))
        }
    }

    if (alltoo) {
        # Plot total and error %age across all channels
        titlestr <- "All channels"

        nval[, 1] <- apply(nval@data, 1, function(x) { sum(x, na.rm=TRUE) })
        uncorr[, 1] <- apply(uncorr@data, 1, function(x) { sum(x, na.rm=TRUE) })

        # pos  <- nval[,1] > 0

        plot(nval[,1], title=titlestr,type="b",xlim=c(t1,t2),log="y")

        pcu <- uncorr[,1] / nval[,1] * 100
        colnames(pcu) <- "UncorrCW"
        units(pcu) <- "%"
        plot(pcu,title=titlestr,type="b",xlim=c(t1,t2))

        snr@data[,1] <- apply(snr, 1, function(x) { mean(x, na.rm=T) })
        pow@data[,1] <- apply(pow, 1, function(x) { mean(x, na.rm=T) })

        plot(snr[,1], title=titlestr, type="b", xlim=c(t1,t2))
        plot(pow[,1], title=titlestr, type="b", xlim=c(t1,t2))
        # }
    }
}
