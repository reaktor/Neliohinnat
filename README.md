Neliohinnat
===========

## LICENSE

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />
This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.

## UPDATE 9.4.2021

The service was updated to include apartment price data and predictions up to 2020. Data fetching queries were fixed to match pxweb's new api and minor refactoring was done. Data and sources are now in `update_2021` folder.

The model was revised quite thoroughly to include yearly varying coefficients for demographic covariates from StatFi. The temporal part of the model is no more fixed to be quadratic but is essentially nonparametric thanks to the yearly coefficients. Instead of forecasting future prices, which is difficult due to unpredictable real world events, the focus is now on nowcasting present and past prices. The blog post at [Reaktor's blog](https://www.reaktor.com/blog/how-to-estimate-housing-prices-with-demographic-data/) as well as the actual [source code](https://github.com/reaktor/Neliohinnat/blob/master/update_2021/source/models/nominal_emp_model.stan) describe the updated model in more detail. Last, a facelift was given to the interactive visualisation.

## Introduction

Apartment price trends across Finland, on the level of zip codes, based on open data from Tilastokeskus. See the interactive visualisation [Kannattaakokauppa](https://kannattaakokauppa.fi/#/) and related blog post at [Reaktor](https://www.reaktor.com/blog/how-to-estimate-housing-prices-with-demographic-data/) as well as older ones in [rOpenGov](http://ropengov.github.io/r/2015/06/11/apartment-prices/), and [Louhos](http://louhos.github.io/news/2015/05/07/asuntohintojen-muutokset/). 

Discussion on [Hacker News](https://news.ycombinator.com/item?id=9503580).

Also news coverage at [Talouselämä](https://www.talouselama.fi/uutiset/te/d022d625-3669-4451-9e9a-9c97bb4c7378), [Kauppalehti](https://www.kauppalehti.fi/uutiset/asuntojen-hinnat-kohosivat-porilaisella-alueella-nopeammin-kuin-helsingin-ytimessa-katso-hakukoneesta-kuuluuko-naapurustosi-kovimpien-nousijoiden-listalle/dd8313d4-f8c4-4190-b74c-c366deb2713d) and [Kaleva](https://www.kaleva.fi/kaijonharjun-ja-linnanmaan-asuntojen-hinnat-nousiv/3561834).

## Data sources

Apartment price data for the postal codes is from [Statistics Finland][statfi] open data [API][statfi-api] (see [Terms of Use][statfi-terms]). 
Postal code region names, municipalities, population data, and map are from Statistics Finland [Paavo - Open data by postal code area][paavo]. Map has been simplified by removing small islands and by reducing the amount of corners of the polygons.

The data sets are accessed with the [pxweb] and [geofi] package from [rOpenGov]. See the script `source/get_data.R` for details.

[statfi]: http://tilastokeskus.fi/meta/til/ashi.html
[statfi-api]: http://www.stat.fi/org/avoindata/api.html
[statfi-terms]: http://tilastokeskus.fi/org/lainsaadanto/yleiset_kayttoehdot_en.html
[paavo]: http://www.tilastokeskus.fi/tup/paavo/index_en.html
[pxweb]: https://github.com/ropengov/pxweb
[rOpenGov]: http://ropengov.github.io/
[geofi]: https://github.com/ropengov/geofi

## Source code

See the `update_2021/source`-folder for latest source code.


## Statistical model

See description in English in [Reaktor-blog](https://www.reaktor.com/blog/how-to-estimate-housing-prices-with-demographic-data/).

## Future development

The latest update was quite big. Development possibilities would be to at least model prices of apartments with different room numbers separately with something hierarchical since the data is already available from StatFi. Another obvious possibility is to further study the effects of the covariates and perhaps introduce new ones from Paavo or from other open data sources.