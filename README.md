# surfboard
Extract info from status page of Arris Surfboard SB6190 cable modem

## Setup for macOS
From a terminal window, create a surfboard directory, for example, on $HOME.
```shell
mkdir $HOME/surfboard
cd $HOME/surfboard
```

If the surfboard data directory is other than $HOME/surfboard, set SURFDIR environment variable.
```shell
export SURFDIR=/somewhere
```

Create a surfboard password file with only user read/write permission on the surfboard data directory
```shell
touch surfboard_password.txt
chmod 0600 surfboard_password.txt
echo "password" > surfboard_password.txt
```

Clone this git repository. In this example it is placed in the surfboard directory.
```shell
git clone https://github.com/maclean/surfboard.git
```

If the surfboard IP is other than 192.168.100.1 set SURF_IP.
```shell
export SURF_IP=10.0.0.1
```

Test surfboard.py. The working directory should be the surfboard directory, in order for the password file to be found. On success, surfboard.py writes the comma-delimited data to stdout.
```shell
surfboard/surfboard.py $SURF_IP
```

Run launch.sh from the git repository to schedule the python script to run every 4 hours.
```shell
surfboard/launch.sh surfboard
```

Compressed data will be written to surfboard.dat.gz in the surfboard directory.
