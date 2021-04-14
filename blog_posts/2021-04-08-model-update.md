

Some years ago, we needed apartment price estimates for areas for which such are not provided due to too small population or too few transactions. We ended up creating a model based on the hierarchy of zip codes and making a public demonstration out of it --- Kannattaako Kauppa. Although the associated quadratic temporal model served well in its purpose years ago, its limits have become apparent, and the pandemic with its new anomalies now finally pushed us to update not only data but model as well. 

The orignal model is detailed in a [blog post](http://ropengov.org/2015/06/a-hierarchical-model-of-finnish-apartment-prices/). We see too drawbacks. First,  spatial smoothness, or generalizing of evidence spatially, is based almost entirely on the zip hierarchy (the exception being population density). Alternatives would be explicit spatial adjacencies, which way we didn't go yet, and the known demographics of the areas, which are now in the model. Then there is the quadratic assumption of temporal development. That was justifiable six years ago and is still to a degree --- but now, something that takes the peculiarities of single years into account is more interesting and better fits the data. 

## Demographics help with sparse observations

In the new model, predictive power mainly becomes from _demographic covariates_, 23 variables selected from the open data that describe the people, households, apartments and professions of the zip code areas. The covariates have yearly coefficients. This means that the temporal structure in the model is much more flexible than in the original model. Zip-code prefix hierarchy is still there, but with the quadratic term dropped, allowing adjacent regions to be similarly deviant from the prices predicted by covariates. 

But what use is "predictive power" for past prices? When one has only a few transactions, the price level of the place at the given year is left uncertain. By accident, those sales maybe on the high or low side of the scale. Trends estimated from such data are again a bit more hazy, and many "top performers" on ranking lists turn out to be statistical flukes. So good modeling, if nothing else, shows us the uncertainty. But a good model also generalizes over regions similar in demography (covariates) or location (adjacency), and understands that declines and rises tend to continue, to a degree to be estimated from data. 

More radically, looking at slots of combined year and zip code, around 90% of all reported transactions have happened in less than 30% of the slots. Meanwhile, some 24k or 75% of the slots have _in total_ less than one hundred transaction. (Note that many postal code areas have no apartments either, the model predicts a price there as well.) Essentially, a vast majority of the estimates provided by the model are extrapolations, as can be seen in the figure below. So predictive power is needed to fill the holes, preferably so that we know how uncertain the fill is. 

![Only sparse price data available](https://raw.githubusercontent.com/reaktor/Neliohinnat/henrika_2021_factorial/figs/sparsitymap.png)

As an extra benefit of having demographics in the model, we see how their effects on prices change over time. The covaraite coefficients provide us interesting insights into the effects of the pandemic, but before taking a closer look at those, let’s dive into some details of the model.

## The core model

As said, we use demographic data from Statistics Finland. The whole Paavo database contains some 100-odd entries per postal code area with information about population, households, employment, jobs, properties and so forth. Based on a nonparametric model, we picked a bit over 20 of the most relevant ones of those to keep the amount of parameters reasonable and the results interpretable. Then, we normalised the figures with relevant counts, e.g. number of service jobs by total number of jobs in an area, and used logit transformation to get good covariates. Last, the covariates were standardized.

The open data provided by Statistics Finland is highly invaluable as it is rare to be able to use especially demographic data with such level of granularity. However, due to understandable privacy reasons, data from postal areas with too few people is censored. And as you might know, Finland is sparsely populated meaning the amount of censored entries is non-negligible. As one of the key purposes of the model is to estimate property prices in areas where they are not known, we cannot simply drop censored data points but need to impute them. For that, we use state-of-art multiple imputation package Amelia II which could be a topic for another blog post. Here, it shall be enough to say it’s far better than imputing with zeros of averages.

Another challenge in the demographic data is that it is a snapshot from 2018 and 2019. We know it is a bit dubious to use demographics from one year to predict the prices of previous years. Then again, the data is available neither as time series nor as yearly snapshots. Assuming demographics change a lot slower than apartment prices, we need to accept the inconvenience as it is obviously better than not using demographics at all.

## When in doubt, trust the empiria

After running the first model estimations and calculating the first nowcasts, we faced yet another challenge. The model worked well but its fit was a tad off on some postal areas despite existing observations. Of course, that is not an issue or a mistake per se but an indication that the model thinks the prices should be something else they are - a possible sign of under or over valuation. However, we all know apartment prices are a complex issue, and the market is essentially right. Our covariates are imperfect not containing data on e.g. lakeside closenesses or commuting times. Thus, instead of trusting the model, we decided to go empirical. As the nowcast of the earlier model was simply its fit, the fit is now a prior. Then, we plug in the actual observations as evidence and use the posterior of that as our prediction. In other words, if there is evidence the prices differ from what the model first thinks, we trust the evidence.

Essentially, the actual model is
$$
\hat{p}_{it} = b_t x_i + \sum_j ( c^j_{i1} + c^j_{i2} t )
$$
$$
\theta_{it} \sim N\left( \hat{p}_{it}  , \sigma^p_t \right)
$$
$$
p_{it} \sim t(\theta_{it}, \frac{ \sigma } { \sqrt{n_{it}} }, v),
$$

where $i$ refers to the postal code, $t$ is time in years, $p_{it}$ is the observed price in area $i$ and year $t$, $\theta_{it}$ is the latent price level, $\sigma$ is the measurement error related to the number of transactions $n_{it}$, $v$ the degrees of freedom, $\hat{p}_{it}$ the expected average price, $\sigma^p_{t}$ the year-specific deviation, $b_t$ the year-specific coefficient vector, $x_i$ the covariate vector, and $c^j_{i}$ the coefficients related to area $i$ according to the three-level spatial hierarchy.

The predictions and their confidence intervals presented in the actual service are then the quantiles of $\theta_{it}$. Due to the importance of the demographic covariates, we do not produce any predictions for areas with zero residents. They would simply not be meaningful.

If you are interested in the details, please take a look at the [source code](https://github.com/reaktor/Neliohinnat/blob/henrika_2021_factorial/update_2021/source/models/nominal_emp_model.stan).

## Covid-19 puts urbanisation to halt?

Now, back to the big picture. Urbanisation is a persistent trend in Finland. Overall, living space per person and mean income are the main predictors in the model, and their coefficients increase monotonically year by year, implying increasing centralization: High prices are rising, and vice versa. If one looks at the principal variation of the $\textrm{year}\times\textrm{covariate}$ matrix, the main axis is almost monotonic in time, reflecting this development.  The second component is high in areas of middle-level education, income in the mid tertile, and small apartment sizes per resident. The component could vaguely be called a suburb index. But the variance of the second component is only 14% of the first one. 

The picture below illustrates how the the apartment prices have changed over years from the perspective of these two components.  Year 2020 certainly looks anomalous, although posterior uncertainty is pretty high on PC2. 

![Space of principal variation of the covariate coefficientws, by year. PC1, the axis of explaining most of the variation, is almost monotonic in time, representing continuing urbanisation. PC2 reflects remaining variation, maybe related to prices on suburbs or more genrally, less crowded areas around city centers.](../figs/princomps-2020.png)

The phenomenon is readily present on our maps (FIXME: linkki). Prices have increased on some previously fairly stable suburbs, whereas city centres received relatively modest increases. Outside the largest cities prices used to decrease, but now the estimate is weak growth almost across the western half of Finland.

## Room for improvement

As a conclusion, the model got quite a big face lift, from an almost purely "pseudospatial" zip-prefix structure to one with demographic covariates, and an partly nonparametric temporal structure. Are we satisfied? No…

First, the zip-code hierarchy is unlikely to have intrinsic predictive power: It is just a proxy for spatial adjacency. Efficient implementations of true adjacency-based spatial random effects are now [available](https://mc-stan.org/users/documentation/case-studies/icar_stan.html), and those could be adapted to our model, to either provide parametric deviations from predictions of covariates as zip codes now do, or a fully non-parametric spatiotemporal random effect. 

We currently have poor temporal coverage of covariates, they are snapshots from years 2018--2019. If available, using year-wise covariates would improve accuracy and make interpretation of temporal changes of covariate coefficients safer. 

Because even more sparse data is available about apartments with various numbers of rooms, one clear opportunity is to take these into the model, and build an associated hierarchy. Modeling apartment heterogeneity would improve overall price estimates, and on dense areas give information of prices specific to apartment types. And of course, the development of prices of different apartments may sometimes diverge in interesting ways form the viewpoint of covariates. 

Many other spatial datasets could be appended to the covariates, or modelled separately. Voting is one obvious choice. 

