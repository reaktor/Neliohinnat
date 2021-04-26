# -*- coding: utf-8 -*-
#

import requests as req
import re
import urllib
import os
import cPickle as pickle

from bs4 import BeautifulSoup
from datetime import datetime

base_url = 'http://www.asuntojenhinnat.fi'

cities = [u"helsinki", u"espoo", u"vantaa", u"kauniainen", u"kirkkonummi",
          u"sipoo", u"tuusula", u"järvenpää", "kerava", "turku",
          "tampere", "salo"]


def get_query_ids(url):
    r = req.request('GET', url)

    soup = BeautifulSoup(r.text)
    # sform = soup.find(id='search-form')
    token = soup.find(id='form__token').get('value')
    session_id = r.cookies['PHPSESSID']
    return {'token': token, 'session_id': session_id}


def parse_data_date(response):
    soup = BeautifulSoup(response.text)
    z = soup.find('footer').find_all(class_='span4')[2].find('a').text
    m = re.search(r'[0-9]{1,2}\.[0-9]{1,2}\.[0-9]{4}', z)
    dt = datetime.strptime(m.group(), '%d.%m.%Y').date()
    return dt


def get_city_data(city, limit=10):
    query_ids = get_query_ids(base_url + '/myydyt-asunnot')
    cookies = {'PHPSESSID': query_ids['session_id']}
    # "postalcode" :"' + "{0:05d}".format(postalcode) + '"}'
    dimension = u'{"city":"' + city + u'"}'
    params = u'{"sort":"price","sortOrder":"DESC","limit":' +\
             unicode(limit) + u',"search":""}'

    url = base_url + '/api/apartments/{}/{}'.format(
        urllib.quote_plus(dimension.encode('utf8')),
        urllib.quote_plus(params.encode('utf8')))

    response = req.get(url, cookies=cookies)
    return response.json()


def load_scraped_data(datadir=None):
    if datadir is None:
        datadir = os.getcwd()
    datafiles = [f for f in os.listdir(datadir) if f.split('.')[-1] == 'pyobj']

    old_data = []
    for f in datafiles:
        fname = datadir + '/' + f
        with open(fname, 'rb') as f:
            old_data.append(pickle.load(f))
    return old_data


def parse_sales_dict(it):
    iv = []
    iv.append(it.get('city'))
    iv.append(it.get('apartment'))
    iv.append(int(it.get('build_year')))
    iv.append(it.get('boolean_elevator') == u'true')
    iv.append(int(it.get('price')))
    iv.append(it.get('sold_month'))
    iv.append(int(it.get('rooms')))
    iv.append(it.get('neighbourhood'))
    iv.append(float(it.get('price_per_square')))
    iv.append(it.get('boolean_sauna') == u'true')
    iv.append(float(it.get('size')))
    return iv


if __name__ == '__main__':
    old_data = load_scraped_data()

    data_updates = [d.get('data_updated') for d in old_data]

    r = req.get(base_url + '/myydyt-asunnot')
    data_updated = parse_data_date(r)

    new_data_available = all([d < data_updated for d in data_updates])

    if new_data_available:
        dl = [get_city_data(c, 0).get('data') for c in cities]
        sales = reduce(lambda x, y: x+y, dl)

        scrape_result = {'scrape_date': datetime.today().date(),
                         'data_updated': data_updated,
                         'data_set': sales}

        fname = 'asuntojenhinnat-' + \
                scrape_result['scrape_date'].strftime('%Y-%m-%d') +\
                '.pyobj'

        with open(fname, 'wb') as f:
            pickle.dump(scrape_result, f)
    else:
        print "no new data available"

