#!/usr/bin/env python3
# vim: set expandtab autoindent ts=4 sw=4:

# Extract useful information about signal quality from an
# Arris Surfboard SB6170 cable modem.
# This modem provides a web page of the downstream and upstream signal quality
# for each channel at http://192.168.100.1/cgi-bin/status.
# This python script grabs that page, and uses XPath expressions to
# extract the fields of interest, generating comma-delimited output.
# of the time and all rows in the "Downstream Bonded Channels" table.
# The time is grabbed from the "Current System Time" line.

# The upstream information is not currently extracted, but that could be added.
# Adding a runstring parameter, down or up, may be the best way to implement that.

import sys, os, subprocess, getopt
from lxml import html
from lxml import etree
import requests
import datetime
import time
import random

debugParse = False
ip = "192.168.100.1"

# Wait some time after being invoked by launchd. 3 or 4 seconds
# on a waking Macbook Air is usually sufficient for the ethernet
# to come up
sleepsec = 4

# How many pings to send before sending http request.
# Some of this extra trouble may be required because
# Energy-Efficient Ethernet is enabled on the modem.
pings = 0
# How long to wait between pings, -i option to ping
pingwait = 1

def eprint(*args, **kwargs):
    print(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:"),
        *args, file=sys.stderr, **kwargs)

def getsurf():

    if debugParse:
        try:
            tree = html.parse('Status.html')
        except Exception as e:
            eprint(e)
            return os.EX_IOERR
    else:

        # Wait for network to come up from system sleep
        if sleepsec > 0:
            time.sleep(sleepsec)

        # Try to bring up network device with ping.
        if pings > 0:
            try:
                ping = subprocess.run(["ping", "-i", str(pingwait), "-c", str(pings), "-n", ip],
                    stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL)
                if ping.returncode != 0:
                    eprint("warning: {} returned {}".format(
                        ' '.join(ping.args), ping.returncode))
            except Exception as e:
                eprint(e)

        # read surfboard admin password from file on working directory
        try: 
            with open('surfboard_password.txt', 'r') as pwdfile:
                passwd = pwdfile.readline().strip()
        except Exception as e:
            eprint(e)
            return os.EX_IOERR

        login_url = 'http://' + ip + '/cgi-bin/adv_pwd_cgi'
        status_url = 'http://' + ip + '/cgi-bin/status'
        logout_url = 'http://' + ip + '/cgi-bin/status#'
        ar_nonce = '{:08d}'.format(random.randint(0,99999999))

        payload = {
            'username': 'admin',
            'password': passwd,
            'ar_nonce': ar_nonce
        }

        try:
            with requests.Session() as s:
                p = s.post(login_url, data=payload, timeout=30)
                # print(p.text)
                if p.status_code != requests.codes.ok:
                    eprint("{}, code={}".format(login_url,p.status_code))

                # An authorised request.
                r = s.get(status_url, timeout=30)
                if r.status_code != requests.codes.ok:
                    eprint("{}, code={}".format(status_url,r.status_code))

                tree = html.fromstring(r.text)

                lo = s.get(logout_url, timeout=30)
                if lo.status_code != requests.codes.ok:
                    eprint("{}, code={}".format(logout_url,lo.status_code))

                if tree is None:
                    eprint("{}, no content, code={}".format(status_url,r.status_code))
                    return os.EX_IOERR

        except Exception as e:
            eprint(e)
            return os.EX_IOERR

    try:
        timeel = tree.xpath('//*[text()=\'Current System Time:\']')
        if not timeel or len(timeel) < 1:
            eprint("Time not found")
            return os.EX_IOERR

        if timeel[0].tag != 'p':
            timeel = timeel[0].xpath('./ancestor::p')
            if not timeel or len(timeel) < 1:
                eprint("Time not found")
                return os.EX_IOERR

        timestr = timeel[0].text_content().encode("UTF-8").decode()

        timestr = timestr.split(':', 1)
        if not timestr or len(timestr) != 2:
            eprint("time={}, not parseable".format(timestr))
            return os.EX_IOERR

        timestr = timestr[1].strip()

        try:
            timeval = datetime.datetime.strptime(timestr,'%a %b %d %H:%M:%S %Y')
        except ValueError as e:
            eprint("time={}, not parseable: {}".format(timestr,e))
            return os.EX_IOERR

        tbls = tree.xpath('//table')

        for tbl in tbls:
            # look for Downstream Bonded Channels table
            if tbl.xpath('.//*[contains(text(),"Downstream Bonded Channels")]'):

                rows = tbl.getchildren()
                for row in rows:
                    # first row has only the "Downstream ..." th
                    # second row has "Channel" header
                    tds = row.xpath('./td')
                    if len(tds) == 0 or tds[0].text_content() == "Channel":
                        continue
                            
                    vals = [col.text_content().encode('UTF-8').decode().strip() for col in tds]
                    if len(vals) < 7:
                        eprint("Only {} values in table row".format(len(vals)))
                        continue

                    vals[4] = vals[4].replace('MHz','').strip()
                    vals[5] = vals[5].replace('dBmV','').strip()
                    vals[6] = vals[6].replace('dB','').strip()
                    vals = [val.replace('----','') for val in vals]
                    print("{0},{1}".format(timeval,','.join(vals)))

    except etree.XPathEvalError as e:
        eprint('xpath exception={}'.format(e))
        return os.EX_IOERR

    return os.EX_OK

def Usage(argv0):

    print("{} [-d] [-h] [-i i] [-p p] [-s s] [IP]\n"
        "    -d: debug, read Status.html instead of fetching status from surfboard\n"
        "    -h: help\n"
        "    -i i: seconds to wait between pings, -i ping option, default: {}\n"
        "    -p p: number of pings to send to ip before fetching status, default: {}\n"
        "    -s s: how many seconds to sleep before fetching status, default: {}\n"
        "    IP: address of surfboard, default: {}".format(argv0, pingwait, pings, sleepsec, ip), file=sys.stderr)


if __name__ == '__main__':

    err = os.EX_USAGE

    try:
        opts = getopt.getopt(sys.argv[1:],'dhi:p:s:')
        for name, value in opts[0]:
            if name == '-d':
                debugParse = True
            elif name == '-h':
                Usage(sys.argv[0])
                sys.exit(err)
            elif name == '-i':
                pingwait = int(value)
            elif name == '-p':
                pings = int(value)
            elif name == '-s':
                sleepsec = int(value)
    except (getopt.GetoptError, ValueError) as exc:
        eprint(str(exc))
        Usage(sys.argv[0])
        sys.exit(err)

    if len(opts[1]) > 0:
        ip = opts[1][0]

    err = getsurf()
    sys.exit(err)

