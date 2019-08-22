# Bayesian Sports Modelling

Repository for my dissertation 'Bayesian Sports Modelling'. Full dissertation can be found [here](https://github.com/rchan26/football_stan/blob/master/Report/5004m.pdf)  and a small summary of the model can be found [here](https://github.com/rchan26/football_stan/blob/master/Report/report.pdf).

## Abstract

We consider the task of predicting football results in the Premier League and propose a Bayesian hierarchical model. In our model, we aim to estimate the characteristics of teams that contribute to a team either winning or losing a football match and use these to predict the score or outcome of future games. The model will be implemented using the Bayesian inference software [Stan](https://mc-stan.org) and the statistical programming language [R](https://www.r-project.org). We also discuss several methods to test the predictive strength of our model and use these methods to compare with other models implemented previously. The data used throughout the report is taken from the [Football-Data website](http://www.football-data.co.uk/).

## Notes

Might make this into a package in the future, but for now everything is in scripts and functions needed to be loaded from each file. Relative links are used to load in files, but is a bit messy. See [examples](https://github.com/rchan26/football_stan/tree/master/R/examples) folder to see example usage. 

The most important files are in the [Stan](https://github.com/rchan26/football_stan/tree/master/Stan) folder.
In this folder:

* [biao_blangiardo.stan](https://github.com/rchan26/football_stan/blob/master/Stan/biao_blangiardo.stan) - [Biao and Blangiardo's model (2010)](http://discovery.ucl.ac.uk/16040/) 
* [model_chan.stan](https://github.com/rchan26/football_stan/blob/master/Stan/model_chan.stan) - model described in [here](https://github.com/rchan26/football_stan/blob/master/Report/report.pdf)
* [poisson_model.stan](https://github.com/rchan26/football_stan/blob/master/Stan/poisson_model.stan) - basically the same model as [model_chan.stan](https://github.com/rchan26/football_stan/blob/master/Stan/model_chan.stan) but with Poisson distribution
* the ones with `_update` are a crude way to update the model posteriors by making a Gaussian approximation to the previous posterior to then use a prior, not the best in all honesty but produces okay results. Quick work around of updating with new data rather than fit the posterior to the full data set with new data.
* the ones with `_time` includes a way to try and add some time dependency on the games - did not really work on this / try it out much.

## Model Assessment

I had some code to assess my models using cross validation, the Brier score and Rank Probability Score (RPS). Code is in [Model_Assessment](https://github.com/rchan26/football_stan/tree/master/Model_Assessment).
