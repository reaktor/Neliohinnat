

Some years ago, we needed apartment price estimates for areas for which such are not provided due to too small population or too few transactions. We ended up creating a model based on the hierarchy of zip codes and making a public demonstration out of it --- Kannattaako Kauppa. Although the associated quadratic temporal model served well in its purpose years ago, its limits have become apparent, and the pandemic with its new anomalies now finally pushed us to update not only data but model as well. 

The orignal model is described in detail in a [blog post](http://ropengov.org/2015/06/a-hierarchical-model-of-finnish-apartment-prices/). We see too drawbacks. First,  spatial smoothness, or extrapolation of evidence, is based almost entirely on the zip hierarchy (the exception being population density). Alternatives would be explicit spatial adjacencies, which way we didn't go yet, and the known demographics of the areas, which are now in the model. Then there is the quadratic assumption of temporal development. That was justifiable six years ago and is still to a degree --- but now, something that takes the peculiarities of single years into account is more interesting and better fits the data. 

## Demographics help with sparse observations

The new model stands on the foundations of the old one. There is still the spatial hierarchy based on postal codes meaning if there are no observations from a postal area, its predictions will be more or less close to those of the neighboring ones. However, we use that hierarchy only for the average level - in technical terms intercept - and for a linear trend. The quadratic trend component has been dropped. Then, in addition to the spatial hierarchy, we include demographic covariates from open data with yearly varying coefficients. In brief, that means the temporal structure is more flexible than previously and the model uses demographics when extrapolating the predictions.

The importance of extrapolation might not be intuitive but is a very essential part of the model. Around 90% of all reported transactions have happened in less than 30% of the 33k-odd postal area - year combinations. On the other hand, there are less than a hundred transactions in total from over 75% - that is some 24k - postal area - year combos. Essentially, a vast majority of the predictions of the model are extrapolations, as can be seen in the figure below.

![Only sparse price data available](https://raw.githubusercontent.com/reaktor/Neliohinnat/henrika_2021_factorial/figs/sparsitymap.png)

Thanks to using covariates for the extrapolation, we are also able to see how the effects of them, e.g. apartment size per person or median income, have changed over time. The yearly covariates provide us interesting insights into the effects of the pandemic, but before taking a closer look at those, let’s dive into some details of the model.

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

## Corona put urbanisation to a halt

Now, back to the big picture and the insights we obtained. First, urbanisation is a constant trend in Finland. It might not be trivial to see in the individual coefficients, but looking at the first two components of principal component analysis, it becomes evident. The first component can be considered some sort of a rurality index - it is high in areas with low employment, high unemployment, little service jobs, and low income. Then, the second principal component is high in areas with people with high school and bachelor degrees but not master degrees, people with income in mid tertile, and areas with small apartment sizes per resident. In other words, the component could be vaguely called a suburb-index.

The picture below illustrates how the the apartment prices have changed over years from the perspective of the principal components. A year left to the previous one means prices have decreased in areas with high first component - the rurality index. A year above the previous one then means prices have increased in suburbish areas. Clearly, 2020 increased suburban prices even more than 2016 while rural prices did not decrease but even increased a bit. However, looking at the previous years, it is quite likely the urbanisation has not stopped completely.

![Transition from first principal compontent to the second](https://raw.githubusercontent.com/reaktor/Neliohinnat/henrika_2021_factorial/figs/princomps-2020.png)

The phenomenon is easily distinguishable also by looking at our map and the actual predictions. Prices have increased in some previously fairly stable suburbs whereas city centre prices have increased a bit less than earlier. If previously apartment prices have decreased almost everywhere outside the largest cities, now our model estimates there is weak growth almost through the whole western half of Finland.

## Room for improvement

As a conclusion, the model got quite a big facelift from a purely spatial solution to one with demographic covariates and an essentially nonparametric temporal structure. Are we satisfied? Probably not. A clear opportunity we have yet to touch is modeling the prices of apartments with different room numbers with something hierarchical as data on that level is openly available. And there are other spatial datasets that could be appended to the covariates. If something, it is clear there is still room for improvement.

