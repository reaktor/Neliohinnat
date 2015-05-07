Neliohinnat
===========


## Introduction

Apartment price trends across Finland, on the level of zip codes, based on open data from Tilastokeskus.

## Data sources

Apartment price data for the postal codes is from [Statistics Finland][statfi] open data [API][statfi-api] (see [Terms of Use][statfi-terms]). 
Postal code region names, municipalities and population data is from Statistics Finland [Paavo - Open data by postal code area][paavo]. Postal code area map is from [Duukkis] and licensed under CC BY 4.0.

The data sets are accessed with the [pxweb and [gisfin] package from [rOpenGov]. See the script `source/get_data.R` for details.

[statfi]: http://tilastokeskus.fi/meta/til/ashi.html
[statfi-api]: http://www.stat.fi/org/avoindata/api.html
[statfi-terms]: http://tilastokeskus.fi/org/lainsaadanto/yleiset_kayttoehdot_en.html
[paavo]: http://www.tilastokeskus.fi/tup/paavo/index_en.html
[pxweb]: https://github.com/ropengov/pxweb
[rOpenGov]: http://ropengov.github.io/
[gisfin]: https://github.com/ropengov/gisfin
[Duukkis]: http://www.palomaki.info/apps/pnro/

## Source code

See the `source`-folder.


## Statistical model

See description in Finnish in [Louhos-blog](http://louhos.github.io/news/2015/05/07/asuntohintojen-muutokset/). English description coming later.