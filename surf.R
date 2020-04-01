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

    # The mapping from channel number to channel id is not constant,
    # nor is the mapping from channel number or channel id to frequency.
    # Use the frequency as the main key
    ufreqs <- sort(unique(surfd$freq))

    tsl <- list()

    for (freq in ufreqs) {
        mx <- surfd$freq == freq & !is.na(times)
        dx <- c(surfd$power[mx],surfd$SNR[mx],
                surfd$CorrCw[mx], surfd$UncorrCw[mx])
        tx <- times[mx]
        xts <- dat(nts(matrix(dx,ncol=4), tx,
            names=c("power","SNR","CorrCw","UncorrCw"),
            units=c("dBmV","dB","","")))

        tsl[as.character(freq)] <- list(list(ts=xts,
            channels=unique(surfd$channel[mx]),
            ids=unique(surfd$id[mx]),
            frequnits="MHz"))
    }
    tsl
}

plotsurf <- function(freqs=0,file=file.path(Sys.getenv("HOME"),"surfboard","surfboard.dat.gz"), palate="Heat", ncolors=10)
{
    surfd <- surfboard(file=file)

    allfreqs <- sort(as.integer(names(surfd)))
    cat("Frequencies=",paste(allfreqs,collapse=", "),"\n")

    alltoo <- FALSE
    if (is.null(freqs)) {
        freqs <- allfreqs
        calcfreqs <- freqs
        alltoo <- TRUE
    }
    else {
        freqs <- sort(freqs)
        zf <- !is.na(match(freqs, 0))
        if (any(zf)) {
            freqs <- freqs[!zf]
            calcfreqs <- allfreqs
            alltoo <- TRUE
        }
        else calcfreqs <- freqs
    }

    # create two gragphics windows
    if (is.null(dev.list())) {
        getOption("device")()
        getOption("device")()
    }


    Sys.setenv(PROJECT="")
    ask <- (alltoo + length(freqs)) > 1
    par(mfrow=c(2,2))
       
    corr <- NULL
    uncorr <- NULL
    snr <- NULL
    pow <- NULL

    logbad <- FALSE

    for (freq in calcfreqs) {
        dx <- surfd[[as.character(freq)]]

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

    for (freq in freqs) {

        cfreq <- as.character(freq)
        nfreq <- match(freq, calcfreqs)

        funits <- surfd[[cfreq]]$frequnits
        chans <- surfd[[cfreq]]$channels
        ids <- surfd[[cfreq]]$ids
        titlestr <- paste0("Freq=", freq, " ", funits,
            ", ids=", paste(ids,collapse=","),
            ", channel#=", paste(chans,collapse=","))

        if (logbad) {
            # make log plot to expand lower values. Plot 0 as 0.1
            zcw  <- !is.na(badcw[,nfreq]) & badcw[,nfreq] == 0
            badcw[zcw,nfreq] <- 0.1
        }
        plot(badcw[,nfreq], title=titlestr, type="b", xlim=c(t1,t2),
            log=ifelse(logbad,"y",""))
        if (logbad) badcw[zcw,nfreq] <- 0

        pcu <- uncorr[,nfreq] / badcw[,nfreq] * 100
        pcu[zcw,] <- 0
        colnames(pcu) <- "UncorrCW"
        units(pcu) <- "%"
        plot(pcu,title=titlestr,type="b",xlim=c(t1,t2))

        plot(snr[,nfreq],title=titlestr,type="b",xlim=c(t1,t2))
        plot(pow[,nfreq],title=titlestr,type="b",xlim=c(t1,t2))

        par(ask=ask)
    }

    # heatmaps of x=time, y=frequency, z=variable.
    # Need legends, time scale on X
    pcu <- uncorr / badcw * 100
    zcw <- !is.na(badcw) & badcw == 0.
    pcu[zcw] <- 0
    colnames(pcu) <- rep("UncorrCW", ncol(pcu))
    units(pcu) <- rep("%", ncol(pcu))

    colors <- hcl.colors(ncolors, palate, rev=TRUE)

    tx <- positions(badcw)
    t1 <- tx[1]
    t2 <- tx[length(tx)]

    timeaxis_setup(t1,t2)

    title <- paste0(unique(colnames(badcw)), " (", unique(units(badcw)),")")
    image(z=badcw@data, x=tx-t1, y=allfreqs, col=colors, ylab="MHz",
        main=title, xaxt="n", xlab="")
    timeaxis(1, time.zone=badcw@time.zone)
    timeaxis(3, labels=FALSE, time.zone=badcw@time.zone)

    title <- paste0(unique(colnames(pcu)), " (", unique(units(pcu)),")")
    image(z=pcu@data, x=tx-t1, y=allfreqs, zlim=c(0,100), col=colors, ylab="MHz", main=title, xaxt="n", xlab="")
    timeaxis(1, time.zone=badcw@time.zone)
    timeaxis(3, labels=FALSE, time.zone=badcw@time.zone)

    tx <- positions(snr)
    t1 <- tx[1]
    t2 <- tx[length(tx)]
    tscale <- t2 - t1
    title <- paste0(unique(colnames(snr)), " (", unique(units(snr)),")")
    image(z=snr@data, x=tx - t1, y=allfreqs, col=colors, ylab="MHz", main=title, xaxt="n", xlab="")
    timeaxis(1, time.zone=badcw@time.zone, date.too=TRUE)
    timeaxis(3, labels=FALSE, time.zone=badcw@time.zone)

    title <- paste0(unique(colnames(pow)), " (", unique(units(pow)),")")
    image(z=pow@data, x=tx - t1, y=allfreqs, col=colors, ylab="MHz", main=title, xaxt="n", xlab="")
    timeaxis(1, time.zone=badcw@time.zone, date.too=TRUE)
    timeaxis(3, labels=FALSE, time.zone=badcw@time.zone)

    # par(ask=ask)

    if (length(dev.list()) == 2) {
        dev.set(dev.next())
        par(mfrow=c(2,2))
    }

    if (FALSE && Sys.getenv("R_GUI_APP_VERSION") != "") {
        require("plotly")
        plot_ly(z=t(badcw@data), x=tx, y=allfreqs, type="heatmap")
        plot_ly(z=t(pcu@data), x=tx, y=allfreqs, type="heatmap")
        tx <- (as.numeric(positions(pow)) - t1) / 86400
        plot_ly(z=t(pow@data), x=tx, y=allfreqs, type="heatmap")
         plot_ly(z=t(snr@data), x=tx, y=allfreqs, type="heatmap")
    }

    if (alltoo) {
        # Plot total and error %age across all frequencies
        titlestr <- "All frequencies"

        badcw[, 1] <- apply(badcw@data, 1, function(x) { sum(x, na.rm=TRUE) })
        uncorr[, 1] <- apply(uncorr@data, 1, function(x) { sum(x, na.rm=TRUE) })

        if (logbad) {
            # make log plot to expand lower values. Plot 0 as 0.1
            zcw  <- !is.na(badcw[,1]) & badcw[,1] == 0
            badcw[zcw,1] <- 0.1
        }
        plot(badcw[,1], title=titlestr,type="b",xlim=c(t1,t2),
            log=ifelse(logbad,"y",""))
        if (logbad) badcw[zcw,1] <- 0

        pcu <- uncorr[,1] / badcw[,1] * 100
        zcw <- !is.na(badcw[,1]) & badcw[,1] == 0.
        pcu[zcw,] <- 0
        colnames(pcu) <- "UncorrCW"
        units(pcu) <- "%"
        plot(pcu,title=titlestr,type="b",xlim=c(t1,t2))

        snr@data[,1] <- apply(snr, 1, function(x) { mean(x, na.rm=T) })
        pow@data[,1] <- apply(pow, 1, function(x) { mean(x, na.rm=T) })

        plot(snr[,1], title=titlestr, type="b", xlim=c(t1,t2))
        plot(pow[,1], title=titlestr, type="b", xlim=c(t1,t2))
    }
}
