#!/usr/bin/env python3
# vim: set expandtab autoindent ts=4 sw=4:

from __future__ import print_function

import sys
from lxml import html
from lxml import etree
import requests
import datetime
import time

def eprint(*args, **kwargs):
    print(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:"),
        *args, file=sys.stderr, **kwargs)

def getsurf():

    if True:
        url = 'http://192.168.100.1/cgi-bin/status'
        for i in (range(0,3)):
            code = requests.codes.ok
            exc = None
            try:
                page = requests.get(url)
                code = page.status_code
                if code == requests.codes.ok:
                    tree = html.fromstring(page.content)
                    break
            except Exception as e:
                exc = e
                time.sleep(2)
        if exc:
            eprint(exc)
            return
        if code != requests.codes.ok:
            eprint("{}, code={}".format(url,code))
            return
    else:
        try:
            tree = html.parse('/tmp/status')
        except Exception as e:
            eprint(e)
            return
    try:
        timeel = tree.xpath('//*[text()=\'Current System Time:\']/ancestor::p')
        if not timeel or len(timeel) < 1:
            eprint("Time not found")
            return

        timestr = timeel[0].text_content().encode("UTF-8").decode()
        timestr = timestr.split(':', 1)
        if not timestr or len(timestr) != 2:
            eprint("time={}, not parseable".format(timestr))
            return

        timestr = timestr[1].strip()

        try:
            timeval = datetime.datetime.strptime(timestr,'%a %b %d %H:%M:%S %Y')
        except ValueError as e:
            eprint("time={}, not parseable: {}".format(timestr,e))
            return

        rows = tree.xpath('//*[text()=\'Downstream Bonded Channels\']/ancestor::table/tr[position()>2]')

        for row in rows:
            vals = [col.text_content().encode('UTF-8').decode().strip() for col in row.getchildren()]
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
        return

if __name__ == '__main__':
    getsurf()

