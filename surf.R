# vim: set expandtab autoindent ts=4 sw=4:

# Read and plot signal quality data that has been extracted
# by surfboard.py from a Surfboard SB6190 cable modem.
# This R code depends on the "eolts" and "isfs" R packages
# at http://www.eol.ucar.edu/software/R/, for manipulating
# and plotting time-series.

surfboard <- function(file=file.path(Sys.getenv("HOME"),"surfboard","surfboard.dat.gz"))
{
    surfd <- scan(file=file,sep=",", quiet=TRUE, what=list(
            datetime="", channel=1, status="",
            modulation="", id=1, freq=1.0, power=1.0,
            SNR=1.0, CorrCw=1, UncorrCw=1))

    times <- utime(surfd$datetime,in.format="%Y-%m-%d %H:%M:%S")

    # The mapping from channel number to channel id has changed from time to time.
    # It appears that the ids are mapped one-to-one to frequencies,
    # so we'll use the id as the main key
    uids <- sort(unique(surfd$id))

    tsl <- list()

    for (id in uids) {
        mx <- surfd$id == id & !is.na(times)
        dx <- c(surfd$power[mx],surfd$SNR[mx],
                surfd$CorrCw[mx], surfd$UncorrCw[mx])
        tx <- times[mx]
        xts <- dat(nts(matrix(dx,ncol=4), tx,
            names=c("power","SNR","CorrCw","UncorrCw"),
            units=c("dBmV","dB","","")))

        tsl[id] <- list(list(ts=xts, freq=unique(surfd$freq[mx]),
            channels=unique(surfd$channel[mx]), frequnits="MHz"))
    }
    tsl
}

plotsurf <- function(ids=0)
{
    surfd <- surfboard()
    nid <- length(surfd)

    alltoo <- FALSE
    if (is.null(ids)) {
        ids <- 1:nid
        alltoo <- TRUE
    }
    else {
        ac <- !is.na(match(ids, 0))
        if (any(ac)) {
            ids <- ids[!ac]
            alltoo <- TRUE
        }
    }

    Sys.setenv(PROJECT="")
    ask <-  (alltoo + length(ids)) > 1
    par(mfrow=c(2,2))
       
    corr <- NULL
    uncorr <- NULL
    snr <- NULL
    pow <- NULL

    for (dx in surfd) {

        # This modem does not report the number of codewords that didn't need correcting,
        # just the number of corrected and the unncorrectables. So we don't actually
        # know the number of successful or failed corrections as a percentage of the
        # total number of codewords.

        # rate (per hour) of codewords that were corrected
        corx <- d_by_dt(dx$ts[,"CorrCw"],dtmax=86400,lag=1,time=1) * 3600
        units(corx) <- "hr-1"
        corr <- Cbind(corr,corx)

        # rate (per hour) of uncorrectable codewords
        uncorx <- d_by_dt(dx$ts[,"UncorrCw"],dtmax=86400,lag=1,time=1) * 3600
        units(uncorx) <- "hr-1"
        uncorr <- Cbind(uncorr,uncorx)

        snr <- Cbind(snr, dx$ts[,"SNR"])
        pow <- Cbind(pow, dx$ts[,"power"])
    }
    # look for modem restarts when the successive difference of
    # correctables or uncorrectables are negative.
    restart <- dat(nts(apply(corr@data,1,function(x) { any(!is.na(x) & x < 0)}),
            positions(corr),names="restarts",units="")) |
        dat(nts(apply(uncorr@data,1,function(x) { any(!is.na(x) & x < 0)}),
            positions(uncorr),names="restarts",units=""))

    if (any(restart)) {
        corr[restart,] <- NA
        uncorr[restart,] <- NA
    }
        
    # total rate of codewords that needed correction
    badcw <- corr + uncorr
    colnames(badcw) <- rep("BadCodeWords", ncol(badcw))
    units(badcw) <- rep("hr-1", ncol(badcw))

    t1 <- start(snr)
    # t1 <- utime("2017 jan 2 05:00")
    t2 <- end(snr)

    clip("BadCodeWords", 0.1, 1.e100)
    clip("UncorrCW", 0, 100)

    for (id in ids) {

        freq <- surfd[[id]]$freq
        funits <- surfd[[id]]$frequnits
        chans <- surfd[[id]]$channels
        titlestr <- paste0("Channel id=",id,
            ", freq=",freq," ",funits, ", chan #s=",
            paste(chans, collapse=","))

        # make log plot to expand lower values. Plot 0 as 0.1
        zcw  <- !is.na(badcw[,id]) & badcw[,id] == 0
        badcw[zcw,id] <- 0.1
        plot(badcw[,id], title=titlestr, type="b", xlim=c(t1,t2), log="y")
        badcw[zcw,id] <- 0
        par(ask=ask)

        pcu <- uncorr[,id] / badcw[,id] * 100
        colnames(pcu) <- "UncorrCW"
        units(pcu) <- "%"
        plot(pcu,title=titlestr,type="b",xlim=c(t1,t2))

        plot(snr[,id],title=titlestr,type="b",xlim=c(t1,t2))
        plot(pow[,id],title=titlestr,type="b",xlim=c(t1,t2))
    }

    if (alltoo) {
        # Plot total and error %age across all ids
        titlestr <- "All channels"

        badcw[, 1] <- apply(badcw@data, 1, function(x) { sum(x, na.rm=TRUE) })
        uncorr[, 1] <- apply(uncorr@data, 1, function(x) { sum(x, na.rm=TRUE) })

        # make log plot to expand lower values. Plot 0 as 0.1
        zcw  <- !is.na(badcw[,1]) & badcw[,1] == 0
        badcw[zcw,1] <- 0.1
        plot(badcw[,1], title=titlestr,type="b",xlim=c(t1,t2),log="y")
        badcw[zcw,1] <- 0

        pcu <- uncorr[,1] / badcw[,1] * 100
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
