# vim: set expandtab autoindent ts=4 sw=4:

# Read and plot signal quality data that has been extracted
# by surfboard.py from a Surfboard SB6190 cable modem.
# This R code depends on the "eolts" and "isfs" R packages
# at http://www.eol.ucar.edu/software/R/, for manipulating
# and plotting time-series.

surfboard <- function(file=file.path(Sys.getenv("HOME"),"surfboard","surfboard.dat.gz"), modemtz="MST", do_dat=FALSE)
{
    surfd <- scan(file=file,sep=",", quiet=TRUE, what=list(
            datetime="", channel=1, status="",
            modulation="", id=1, freq=1.0, power=1.0,
            SNR=1.0, CorrCw=1, UncorrCw=1))

    times <- utime(surfd$datetime,in.format="%Y-%m-%d %H:%M:%S",
        time.zone=modemtz)

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
        xts <- nts(matrix(dx,ncol=4), tx,
            names=c("power","SNR","CorrCw","UncorrCw"),
            units=c("dBmV","dB","",""))
        if (do_dat) xts <- dat(xts)

        tsl[as.character(freq)] <- list(list(ts=xts,
            channels=unique(surfd$channel[mx]),
            ids=unique(surfd$id[mx]),
            frequnits="MHz"))
    }
    tsl
}

plotsurf <- function(freqs=0,
    file=file.path(Sys.getenv("HOME"),"surfboard","surfboard.dat.gz"),
    palette="Heat", ncolors=10, modemtz="MST", logerr=FALSE, do_dat=FALSE)
{
    # not really important, but time on the modem is standard time,
    # not adjusted for daylight savings time
    surfd <- surfboard(file=file, modemtz=modemtz, do_dat=do_dat)

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

    # create two graphics windows
    if (is.null(dev.list())) {
        getOption("device")()
        getOption("device")()
    }


    Sys.setenv(PROJECT="")
    ask <- (alltoo + length(freqs)) > 1
    par(mfrow=c(4,1))
       
    # According to a May 2016 post on dslreports.com from telcodad:
    # Your modem inspects every packet on its cable segment to check which codewords
    # are addressed to it. This number of packets is expressed as "Total Unerrored Codewords.'"
    # "Corrected" are the number of those codewords that were corrected using FEC.
    # "Uncorrectables" are the number of codewords that were so corrupted that your modem
    # couldn't correct, and therefore had to request the CMTS to resend to them.
    # Those are the ones to be most concerned about.

    # The status page does not report the number of total unerrored codewords.
    # So we don't actually know the number of errors (successful or failed corrections)
    # as a percentage of the total number of codewords.

    # We'll track the sum of the corrected and uncorrectables as a rate per hour, and the
    # percentage of uncorrectables in the sum.

    corr <- NULL    # rate (per hour) of codewords that were corrected
    uncorr <- NULL  # rate (per hour) of uncorrectable codewords
    snr <- NULL
    pow <- NULL

    for (freq in calcfreqs) {
        dx <- surfd[[as.character(freq)]]

        # compute rate from the successive differences in the sum of correctables
        # d_by_dt returns per second, convert to per hour
        corx <- d_by_dt(dx$ts[,"CorrCw"],dtmax=86400,lag=1,time=1) * 3600
        units(corx) <- "hr-1"
        corr <- Cbind(corr,corx)

        uncorx <- d_by_dt(dx$ts[,"UncorrCw"],dtmax=86400,lag=1,time=1) * 3600
        units(uncorx) <- "hr-1"
        uncorr <- Cbind(uncorr,uncorx)

        snr <- Cbind(snr, dx$ts[,"SNR"])
        pow <- Cbind(pow, dx$ts[,"power"])
    }

    # look for modem restarts when the successive difference of
    # correctables or uncorrectables are negative.
    restart <- nts(apply(corr@data,1,function(x) { any(!is.na(x) & x < 0)}),
            positions(corr),names="restarts",units="") |
        nts(apply(uncorr@data,1,function(x) { any(!is.na(x) & x < 0)}),
            positions(uncorr),names="restarts",units="")

    if (any(restart)) {
        corr[restart,] <- NA
        uncorr[restart,] <- NA
    }
        
    # total rate of codewords that needed correction
    cwerr <- corr + uncorr
    colnames(cwerr) <- rep("CodeWordErrors", ncol(cwerr))
    units(cwerr) <- rep("hr-1", ncol(cwerr))

    t1 <- start(snr)
    # t1 <- utime("2017 jan 2 05:00")
    t2 <- end(snr)

    # clip("CodeWordErrors", 0.1, 1.e100)
    # clip("UncorrCW", 0, 100)

    for (freq in freqs) {

        cfreq <- as.character(freq)
        nfreq <- match(freq, calcfreqs)

        funits <- surfd[[cfreq]]$frequnits
        chans <- surfd[[cfreq]]$channels
        ids <- surfd[[cfreq]]$ids
        titlestr <- paste0("Freq=", freq, " ", funits,
            ", ids=", paste(ids,collapse=","),
            ", channel#=", paste(chans,collapse=","))

        zcw  <- !is.na(cwerr[,nfreq]) & cwerr[,nfreq] == 0
        if (logerr) {
            # make log plot to expand lower values. Plot 0 as 0.1
            cwerr[zcw,nfreq] <- 0.1
        }
        plot(cwerr[,nfreq], type="b", xlim=c(t1,t2),
            log=if (logerr) "y" else "")

        if (!do_dat) {
            timeaxis(3, labels=TRUE, time.zone=cwerr@time.zone, date.too=FALSE,
                xlab=FALSE)
            axis(4)
        }

        if (logerr) cwerr[zcw,nfreq] <- 0

        pcu <- uncorr[,nfreq] / cwerr[,nfreq] * 100
        pcu[zcw,] <- 0
        colnames(pcu) <- "UncorrCW"
        units(pcu) <- "%"

        plot(pcu,type="b",xlim=c(t1,t2))
        if (!do_dat) {
            timeaxis(3, labels=FALSE, time.zone=pcu@time.zone, date.too=FALSE,
                xlab=FALSE)
            axis(4)
        }

        plot(snr[,nfreq],type="b",xlim=c(t1,t2))
        if (!do_dat) {
            timeaxis(3, labels=FALSE, time.zone=snr@time.zone, date.too=FALSE,
                xlab=FALSE)
            axis(4)
        }

        if (!do_dat) {
            plot(pow[,nfreq],type="b",xlim=c(t1,t2))
            timeaxis(3, labels=FALSE, time.zone=pow@time.zone, date.too=FALSE,
                xlab=FALSE)
            axis(4)
            title(main=titlestr, line=-par("cex.main"), outer=TRUE)
        }
        else plot(pow[,nfreq],type="b",xlim=c(t1,t2), title=titlestr)

        par(ask=ask)
    }

    eolts::set_plot_margins()

    # heatmaps of x=time, y=frequency, z=variable.
    # Need legends, time scale on X
    pcu <- uncorr / cwerr * 100
    zcw <- !is.na(cwerr) & cwerr == 0.
    pcu[zcw] <- 0
    colnames(pcu) <- rep("UncorrCW", ncol(pcu))
    units(pcu) <- rep("%", ncol(pcu))

    colors <- hcl.colors(ncolors, palette, rev=TRUE)

    tx <- positions(cwerr)
    t1 <- tx[1]
    t2 <- tx[length(tx)]

    timeaxis_setup(t1,t2)

    title <- paste0(if (logerr) "log10 " else "",
        unique(colnames(cwerr)), " (", unique(units(cwerr)),")")
    if (logerr) {
        # make log plot to expand lower values. Plot 0 as 0.1
        zcw  <- !is.na(cwerr) & cwerr == 0
        cwerr[zcw] <- 0.1
    }

    image(z=if(logerr) log10(cwerr@data) else cwerr@data, x=tx-t1,
        y=allfreqs, col=colors, ylab="MHz", xaxt="n", xlab="")

    timeaxis(1, labels=FALSE, time.zone=cwerr@time.zone)
    timeaxis(3, labels=TRUE, time.zone=cwerr@time.zone, date.too=FALSE,
        xlab=FALSE)
    axis(side=4)
    mtext(title, side=3, line=1.5, cex=0.8)
    if (logerr) cwerr[zcw] <- 0

    title <- paste0(unique(colnames(pcu)), " (", unique(units(pcu)),")")
    set_plot_margins()
    image(z=pcu@data, x=tx-t1, y=allfreqs, zlim=c(0,100), col=colors,
        ylab="MHz", xaxt="n", xlab="")
    timeaxis(1, labels=FALSE, time.zone=cwerr@time.zone)
    timeaxis(3, labels=FALSE, time.zone=cwerr@time.zone)
    axis(side=4)
    mtext(title, side=3, line=0.5, cex=0.8)

    tx <- positions(snr)
    t1 <- tx[1]
    t2 <- tx[length(tx)]
    tscale <- t2 - t1
    title <- paste0(unique(colnames(snr)), " (", unique(units(snr)),")")
    set_plot_margins()
    image(z=snr@data, x=tx - t1, y=allfreqs, col=colors,
        ylab="MHz", xaxt="n", xlab="")
    timeaxis(1, labels=FALSE, time.zone=cwerr@time.zone)
    timeaxis(3, labels=FALSE, time.zone=cwerr@time.zone)
    axis(side=4)
    mtext(title, side=3, line=0.5, cex=0.8)

    title <- paste0(unique(colnames(pow)), " (", unique(units(pow)),")")
    set_plot_margins()
    image(z=pow@data, x=tx - t1, y=allfreqs, col=colors,
        ylab="MHz", xaxt="n", xlab="")
    timeaxis(1, time.zone=cwerr@time.zone, date.too=TRUE)
    timeaxis(3, labels=FALSE, time.zone=cwerr@time.zone)
    axis(side=4)
    mtext(title, side=3, line=0.5, cex=0.8)
    logo_stamp()

    # par(ask=ask)

    if (length(dev.list()) == 2) {
        dev.set(dev.next())
        par(mfrow=c(4,1))
    }

    if (FALSE && Sys.getenv("R_GUI_APP_VERSION") != "") {
        require("plotly")
        plot_ly(z=t(cwerr@data), x=tx, y=allfreqs, type="heatmap")
        plot_ly(z=t(pcu@data), x=tx, y=allfreqs, type="heatmap")
        tx <- (as.numeric(positions(pow)) - t1) / 86400
        plot_ly(z=t(pow@data), x=tx, y=allfreqs, type="heatmap")
        plot_ly(z=t(snr@data), x=tx, y=allfreqs, type="heatmap")
    }

    if (alltoo) {
        # Plot total and error %age across all frequencies
        titlestr <- NULL

        cwerr[, 1] <- apply(cwerr@data, 1, function(x) { sum(x, na.rm=TRUE) })
        uncorr[, 1] <- apply(uncorr@data, 1, function(x) { sum(x, na.rm=TRUE) })

        if (logerr) {
            # make log plot to expand lower values. Plot 0 as 0.1
            zcw  <- !is.na(cwerr[,1]) & cwerr[,1] == 0
            cwerr[zcw,1] <- 0.1
        }
        plot(cwerr[,1], type="b",xlim=c(t1,t2), log=if(logerr) "y" else "")

        if (!do_dat) {
            timeaxis(3, time.zone=cwerr@time.zone, date.too=FALSE, xlab=FALSE)
            axis(side=4)
        }
        if (logerr) cwerr[zcw,1] <- 0

        pcu <- uncorr[,1] / cwerr[,1] * 100
        zcw <- !is.na(cwerr[,1]) & cwerr[,1] == 0.
        pcu[zcw,] <- 0
        colnames(pcu) <- "UncorrCW"
        units(pcu) <- "%"

        plot(pcu, type="b",xlim=c(t1,t2))
        if (!do_dat) {
            timeaxis(3, labels=FALSE, time.zone=cwerr@time.zone, date.too=FALSE)
            axis(side=4)
        }

        snr@data[,1] <- apply(snr, 1, function(x) { mean(x, na.rm=T) })

        plot(snr[,1], type="b", xlim=c(t1,t2))
        if (!do_dat) {
            timeaxis(3, labels=FALSE, time.zone=cwerr@time.zone, date.too=FALSE)
            axis(side=4)
        }

        pow@data[,1] <- apply(pow, 1, function(x) { mean(x, na.rm=T) })

        titlestr <- "All frequencies"

        plot(pow[,1], type="b", xlim=c(t1,t2))
        if (!do_dat) {
            timeaxis(3, labels=FALSE, time.zone=cwerr@time.zone, date.too=FALSE)
            axis(side=4)
            title(main=titlestr, line=-par("cex.main"), outer=TRUE)
            logo_stamp()
        }
    }
    invisible(NULL)
}
