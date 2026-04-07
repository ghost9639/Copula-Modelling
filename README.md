
# Table of Contents

1.  [Portfolio Risk Management in R](#org00d082e)
2.  [Features](#org5f47ab5)
3.  [CRAN Dependencies](#org3612388)
4.  [Methodology](#org85c089a)
    1.  [Preparing the Dataset](#org87ca630)
    2.  [Variance-Covariance Model](#orgc9d1be4)
    3.  [Copula Joint Risk Modelling](#orga88fc29)
    4.  [Loss Estimation](#org56e43da)
5.  [Future Improvements](#org730f36f)
6.  [References](#org2101a1c)



<a id="org00d082e"></a>

# Portfolio Risk Management in R

This project implements a lightweight, minimal dependency approach to estimating the portfolio risk using:

1.  The Variance-Covariance Linear Model,
2.  A Gaussian copula with *t*-distributed marginals.

A full report is available in the [documentation file](Report.pdf). The methods employed are based on (McNeil, Alexander J. and Frey, Rüdiger and Embrechts, Paul, 2015), targeting readability, reproducibility, and minimal statistical "black-boxing".

A minimal dependency [R script](src/Main.R) has been written supplying key functions automatically implementing Copula and variance-covariance VaR and AVaR calculation methods. This script only calls MASS (a default library) and data.table (a commonly included library).

    ## Call the Linear Approximation function
    linear_approx <- linearModelRisk("2021-02-21", "2023-02-21", cleaned_close_prices[,-1], cleaned_close_prices[,1], as.numeric(my_shares[,-1]), alpha)
    sprintf("The linear VaR is £%.2f and the AVaR is £%.2f at the %i%% CI.",
            linear_approx[[1]], linear_approx[[2]], (1 - alpha) * 100)

    >> "The linear VaR is £1928.48 and the AVaR is £2207.67 at the 99% CI."

    VaRs <- copulaRiskCalculator (sample_start, sample_end, cleaned_close_prices[,-1], cleaned_close_prices[,1],
                                  as.numeric(my_shares[,-1]), simulations = 30000, alpha = alpha)
    
    sprintf("The Gaussian Copula with t-distributed marginals finds a VaR of £%.2f and an AVaR of £%.2f at the %i%% CI.",
            VaRs[[1]], VaRs[[2]], (1-alpha) * 100)

    >> "The Gaussian Copula with t-distributed marginals finds a VaR of £2108.10 and an AVaR of £2619.25 at the 99% CI."

The functions are documented using roxygen2. My process building and designing the functions is loosely documented in [an R markdown file](src/Main.rmd).


<a id="org5f47ab5"></a>

# Features

1.  Variance-Covariance VaR and AVaR,
2.  Copula-Based Joint Risk Modelling,
3.  Monte Carlo Simulation of loss distributions,
4.  Key Risk Metrics,
    1.  Value at Risk,
    2.  Expected Shortfall,
5.  Visualisation
    1.  Clear distribution plots,
    2.  Joint distribution crossplots.


<a id="org3612388"></a>

# CRAN Dependencies

This project has very few dependencies.

    suppressMessages(library(tidyverse)) # Tidyverse supplies data manipulation and plots
    suppressMessages(library (here)) # here enables reproducible pathing
    suppressMessages(library(MASS))  # MASS provides multivariate Gaussian simulations

**No risk or copula modules are used**, all models implemented from "first principles". The report is in [a pdf](Report.pdf), the [main project file](src/Main.rmd) is also available. All data used is kept in "data/", and referenced in the Main file using \`here\`.


<a id="org85c089a"></a>

# Methodology


<a id="org87ca630"></a>

## Preparing the Dataset

1.  Basic exploratory analysis of dataset,
2.  Conversion to logged returns,
3.  Cleaning dataset of anomalies.


<a id="orgc9d1be4"></a>

## Variance-Covariance Model

Assumptions:

1.  Normally distributed log returns,
2.  Linearised profit approximation (first order terms of Taylor expansion),

Method:   

1.  Calculate sample mean and covariance,
2.  Calculate VaR directly,
3.  Calculate AVaR analytically.


<a id="orga88fc29"></a>

## Copula Joint Risk Modelling

1.  Fitted *t*-distributions to marginals,
2.  Fitted Gaussian copula to marginal uniform distributions,
3.  Generated joint samples from copula,
4.  Applied marginal distributions to convert back to stock price changes.


<a id="org56e43da"></a>

## Loss Estimation

1.  Monte Carlo simulation of potential loss portfolios,
2.  Required quantile collected for VaR,
3.  Exceeding values averaged for AVaR.


<a id="org730f36f"></a>

# Future Improvements

1.  Generalised functions for both,
2.  Backlinked API for functions to modularly support each other,
3.  GARCH volatility modelling,
4.  Backtesting? Stress-testing?


<a id="org2101a1c"></a>

# References

\##+cite<sub>export</sub>: csl harvard-university-of-leeds.csl

McNeil, Alexander J. and Frey, Rüdiger and Embrechts, Paul (2015). *Quantitative Risk Management: Concepts, Techniques and Tools*, Princeton University Press.

