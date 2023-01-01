# surfboard
Extract info from status page of Arris Surfboard SB6190 cable modem

## Setup for macOS

### Install python3 and lxml module

python3 is part of the macOS developer tools. If it isn't fully installed, run it to bring up a dialog to install the developer tools:

    /usr/bin/python3 --version

Or download and install it from https://www.python.org/downloads/mac-osx/, which will put it on /usr/local/bin.

Then install the lxml and requests modules

    sudo -H python3 -m pip install --upgrade pip
    python3 -m pip install lxml
    python3 -m pip install requests

### surfboard.py
From a terminal window, create a surfboard directory, for example, on $HOME.

    mkdir $HOME/surfboard
    cd $HOME/surfboard

If the surfboard directory is other than $HOME/surfboard, set SURFDIR environment variable.

    export SURFDIR=/somewhere

Create a surfboard password file with only user read/write permission on the surfboard directory.

    touch surfboard_password.txt
    chmod 0600 surfboard_password.txt
    echo "password" > surfboard_password.txt

Clone this git repository. In this example it is placed in the surfboard directory.

    git clone https://github.com/maclean/surfboard.git

If the surfboard IP is other than 192.168.100.1 set SURF_IP.

    export SURF_IP=10.0.0.1

Test surfboard.py. The working directory should be the surfboard directory, in order for the password file to be found. On success, surfboard.py writes the comma-delimited data to stdout.

    surfboard/surfboard.py ${SURF_IP:=192.168.100.1}

Run launch.sh from the git repository to schedule the python script to run from launchd every 4 hours.

    surfboard/launch.sh surfboard

Compressed data will be written to surfboard.dat.gz in the surfboard directory.

### R setup for macOS

Install R from https://cran.r-project.org/bin/macosx/.

Then, from a terminal window, install required packages

    R --vanilla
    options(repos=c("http://cran.us.r-project.org","https://archive.eol.ucar.edu/software/R"))
    install.packages(c("splusTimeDate","gWidgets2","splusTimeSeries", "quantreg", "maps", "Rcpp", "RUnit"))
    install.packages(c("eolts","isfs"))

Compile surfboard/surf.R in R:

    R --vanilla --slave --restore --save -e 'source("surfboard/surf.R")'

Schedule plotsurf to run every night

    surfboard/launch.sh plotsurf
