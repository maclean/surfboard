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

from __future__ import print_function

import sys, os
from lxml import html
from lxml import etree
import requests
import datetime
import time
import random

def eprint(*args, **kwargs):
    print(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:"),
        *args, file=sys.stderr, **kwargs)


def getsurf():

    if True:

        try: 
            with open('surfboard_password.txt', 'r') as pwdfile:
                passwd = pwdfile.readline().strip()
        except Exception as e:
            eprint(e)
            return os.EX_IOERR

        login_url = 'http://192.168.100.1/cgi-bin/adv_pwd_cgi'
        status_url = 'http://192.168.100.1/cgi-bin/status'
        logout_url = 'http://192.168.100.1/cgi-bin/status#'
        ar_nonce = '{:08d}'.format(random.randint(0,99999999))
        payload = {
            'username': 'admin',
            'password': passwd,
            'ar_nonce': ar_nonce
        }

        try:
            with requests.Session() as s:
                p = s.post(login_url, data=payload)
                # print(p.text)
                if p.status_code != requests.codes.ok:
                    eprint("{}, code={}".format(login_url,p.status_code))

                # An authorised request.
                r = s.get(status_url)
                if r.status_code != requests.codes.ok:
                    eprint("{}, code={}".format(status_url,r.status_code))

                tree = html.fromstring(r.text)

                lo = s.get(logout_url)
                if lo.status_code != requests.codes.ok:
                    eprint("{}, code={}".format(logout_url,lo.status_code))

                if tree is None:
                    eprint("{}, no content, code={}".format(status_url,r.status_code))
                    return os.EX_IOERR

        except Exception as e:
            eprint(e)
            return os.EX_IOERR
    else:
        try:
            tree = html.parse('Status.html')
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
        if timestr[0] == '<':
            timestr = timestr.split('>', 1)
            if timestr and len(timestr) == 2:
                timestr = timestr[1].strip()

        ctimestr = timestr.split('<', 1)

        if ctimestr and len(ctimestr) == 2:
            timestr = ctimestr[0]

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

if __name__ == '__main__':
    err = getsurf()
    sys.exit(err)

