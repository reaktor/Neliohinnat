# Here we go again, although this is not the final title

Some years ago, we needed apartment price estimates for areas for which such are not provided, due to too small population or too few transactions. We ended up creating a model based on the hierarchy of zip codes and making a public demonstration out of it --- Kannattaako Kauppa. Although the associated quadratic temporal model served well in its purpose years ago, its limits have become apparent, and the pandemic with its new anomalies now finally pushed us to update not only data but model as well. 

The orignal model is detailed in a [blog post](http://ropengov.org/2015/06/a-hierarchical-model-of-finnish-apartment-prices/). We see too main drawbacks. First,  spatial smoothness, or generalizing of evidence spatially, is based almost entirely on the zip hierarchy (the exception being population density). Alternatives would be explicit spatial adjacencies, which way we didn't go yet, and the known demographics of the areas, which are now in the model. Then there is the quadratic temporal form. That was justifiable six years ago and is still to a degree --- but now, something that takes the peculiarities of single years into account is more interesting and better fits the data. 

## Demographics help with sparse observations

In the new model, predictive power mainly becomes from _demographic covariates_, 23 variables selected from the open data that describe the geographical extent, people, households, apartments and professions of the zip code areas. The covariates have yearly coefficients in the model, so the model has gained temporal flexibility compared to the original. Zip-code prefix hierarchy is still there, but with the quadratic term dropped. This the local price levels and trends to deviate from the predictions of covariates. 

What use is "predictive power" for past prices? When one has only a few transactions, price level and especially trend are mostly unknown. By accident, those few sales maybe on the high or low side of the scale. Many "top performers" on ranking lists turn out to be statistical flukes. So a good model, if nothing else, shows the uncertainty. But a good model also generalizes over similar or nearby regions, and over time. So the model can partially fill holes and uncertainties in the data, to the degree continuities in real prices support it. And we know that there is temporal, spatial and demographic regularity in the real world. 

Looking at slots of combined year and zip code, around 90% of all reported transactions are in about 18% of the slots. Meanwhile, some 31k or 93% of the slots have less than one hundred transaction, and 73% have less than ten. Not that many postal areas have no apartments either! But even on areas with aparments, data is sparse, and modelling is needed to fill the holes.

![Only sparse price data available](https://raw.githubusercontent.com/reaktor/Neliohinnat/henrika_2021_factorial/figs/sparsitymap.png)

As an additional benefit of having demographics in the model, we see how they affect prices over time. Covariate coefficients provide us interesting insights into the effects of the pandemic, but before taking a closer look at those, let’s dive into some details of the model.

## The core model

As said, we use demographic data from Statistics Finland. The whole Paavo database contains some 100-odd entries per postal code area, with information about population, households, employment, jobs, properties and so forth. Based on a nonparametric model, we picked a bit over 20 of the most relevant ones of those to keep the amount of parameters reasonable and the results interpretable. Then, we normalised the figures with relevant counts, e.g. number of service jobs by total number of jobs in an area, etc., and used logit transformation to get good covariates. Finally, the covariates were standardized.

The open data provided by Statistics Finland is a rare treasure, accurate demographic data with such level of granularity. However, due to understandable privacy reasons, data from postal areas with too few people is censored. And as you might know, Finland is sparsely populated meaning the amount of censored entries is non-negligible. As one of the key purposes of the model is to estimate property prices in areas where they are not known, we cannot simply drop censored data points but need to impute them. For that, we use state-of-art multiple-imputation package Amelia II (link), which could be a topic for another blog post. Here, let's just say using it is far better than imputing with zeros or mean values. (In future, missingness could be included in the core model.)

Another challenge in the demographic data is its snapshot quality: The information is from years 2018 and 2019. We know it is  dubious to use demographics from one year to predict the prices of past or following years. Then again, complete temporal covariate data is available neither as time series nor as yearly snapshots. Assuming demographics change a lot slower than apartment prices, we just have to be careful with interpretations, especially causal ones. For predictions, using a snapshot of demography is obviously better than not using demographics at all.

## When in doubt, trust the empiria

After running the first model fits and nowcasts, we faced yet another challenge. The model worked well but its fit was a tad off on some postal areas despite existing observations. Of course, that is not an issue or a mistake per se but an indication that the model thinks the prices should be something else they are - a possible sign of under or over valuation. However, we all know apartment prices are a complex issue, and the market is essentially right. Our covariates are imperfect not containing data on e.g. lakeside closenesses or commuting times. Thus, instead of trusting the model, we decided to go empirical. As the nowcast of the earlier model was simply its fit, the fit is now a prior. Then, we plug in the actual observations as evidence and use the posterior of that as our prediction. In other words, if there is evidence the prices differ from what the model first thinks, we trust the evidence.

The essential parts of the model are
$$
h_{it} = x_i^T \beta_t + \sum_l \left( a_{li} t + b_{li} \right) + u_{it} + \epsilon_{it}\;,
$$
$$
u_{it} \sim N\!\left(0  , \sigma^u_t \right)\;,
$$
$$
\epsilon_{it} \sim t\! \left(0,\, \frac{ \sigma^\epsilon } { \sqrt{n_{it}} },\, \nu \right)\;.
$$

Of indices, $i$ refers to the postal coden and $t$ is the year. Then $h$ are the observed log-price, $x$ are covariates, $\beta$ are (yearly) covariate coefficients. The sum is the "pseudospatial" random effect built on the postal-code prefix hierarchy with levels $l$, and varying trends $a$ and price levels $b$. This relatively rigid structure is loosened by the $i \times t$ -level random effect $u$. Model terms up to this point are supposed to represent underlying real prices. Note that the last term has yearly spread $\sigma^u_{t}$, allowing some years to be more deviant than others. 

Finally, finite numbers of observed transactions $n$ cause "measurement error" $\epsilon$. Assuming indepedent transactions, the variance of this error scales with $1/n$, but we have left space for outliers by using a Student-t distribution with its degrees of freedom $\nu$ parametrized to the model. They are fitted to $\nu$≈2. Note that if the measurement errors were gaussian, $u$ could be marginalized away. (We tried that, and it of course accelerates fitting manyfold.) Although technically the parameter set $(\nu, \sigma^\epsilon, \sigma^u)$ is well identifiable, on the basis of various trials, we are not convinced that estimates of these are robust enough to assumptions that do not quite hold.

On the web site, $h$ are the black points denoting observations, and reported price estimates and credible intervals are the part of the model without measurement error $\epsilon$, that is, $\exp (h-\epsilon)$. Due to the importance of the demographic covariates, we do not produce any predictions for areas with zero residents. 

In addition to the structure detailed above, the model of course has priors, and for example covariances for coefficients $a, b$. If you are interested in the details, please take a look at the [source code](https://github.com/reaktor/Neliohinnat/blob/henrika_2021_factorial/update_2021/source/models/nominal_emp_model.stan).

## Covid-19 puts urbanisation to halt?

Now, back to the big picture. Urbanisation is a persistent trend in Finland. Overall, living space per person and mean income are the main predictors in the model, and their coefficients increase monotonically year by year, implying increasing centralization: High prices are rising, and vice versa. If one looks at the principal variation of the $\textrm{year}\times\textrm{covariate}$ matrix, the main axis is almost monotonic in time, reflecting this development.  The second component is high in areas of middle-level education, income in the mid tertile, and small apartment sizes per resident. The component could vaguely be called a suburb index. But the variance of the second component is only 14% of the first one. 

The picture below illustrates how the the apartment prices have changed over years from the perspective of these two components.  Year 2020 certainly looks anomalous, although posterior uncertainty is pretty high on PC2. 

![Space of principal variation of the covariate coefficientws, by year. PC1, the axis of explaining most of the variation, is almost monotonic in time, representing continuing urbanisation. PC2 reflects remaining variation, maybe related to prices on suburbs or more genrally, less crowded areas around city centers.](../figs/princomps-2020.png)

The phenomenon is readily present on our maps (FIXME: linkki). Prices have increased on some previously fairly stable suburbs, whereas city centres received relatively modest increases. Outside the largest cities prices used to decrease, but now the estimate is weak growth almost across the western half of Finland.

## Room for improvement

As a conclusion, the model got quite a big face lift, from an almost purely "pseudospatial" zip-prefix structure to one with demographic covariates, and an partly nonparametric temporal structure. Are we satisfied? No…

First, the zip-code hierarchy is unlikely to have intrinsic predictive power: It is just a proxy for spatial adjacency. Efficient implementations of true adjacency-based spatial random effects are now [available](https://mc-stan.org/users/documentation/case-studies/icar_stan.html), and those could be adapted to our model, to either provide parametric deviations from predictions of covariates as zip codes now do, or a fully non-parametric spatiotemporal random effect. 

We currently have poor temporal coverage of covariates, they are snapshots from years 2018--2019. If available, using year-wise covariates would improve accuracy and make interpretation of temporal changes of covariate coefficients safer. 

Many covariates are proportions of population, households, or other denominators. Uncertainty of these varies by the size of the area, this could be included in the model. We think the effect on the results would be modest. 

Because even more sparse data is available about apartments with various numbers of rooms, one clear opportunity is to take these into the model, and build an associated hierarchy. Modeling apartment heterogeneity would improve overall price estimates, and on dense areas give information of prices specific to apartment types. And of course, the development of prices of different apartments may sometimes diverge in interesting ways form the viewpoint of covariates. 

Many other spatial datasets could be appended to the covariates, or modelled separately. Voting is one obvious choice. 

