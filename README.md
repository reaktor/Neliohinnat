Neliohinnat
===========


## Introduction

Apartment price trends across Finland, on the level of zip codes, based on open data from Tilastokeskus. See the interactive visualisation [Kannattaakokauppa](http://kannattaakokauppa.fi/#/) and related blog posts at [Reaktor](http://reaktor.com/blog/asuntojen-trendit-ja-miten-niista-tehdaan-luotettavia-ennusteita) and [Louhos](http://louhos.github.io/news/2015/05/07/asuntohintojen-muutokset/). 

Also news coverage at [HS](http://www.hs.fi/kotimaa/a1430886950224) and [Tivi](http://www.tivi.fi/Kaikki_uutiset/2015-05-07/Ryhtym%C3%A4ss%C3%A4-asuntokaupoille-Katso-miten-asuntosi-hinta-kehittyy-tulevaisuudessa-3221240.html).

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