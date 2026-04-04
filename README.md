
# Table of Contents

1.  [Portfolio Risk Management in R](#org6e5ffe8)
2.  [Features](#org7829e1e)
3.  [CRAN Dependencies](#org3221267)
4.  [Methodology](#orgf376761)
    1.  [Preparing the Dataset](#org14ce5bb)
    2.  [Variance-Covariance Model](#org6465c21)
    3.  [Copula Joint Risk Modelling](#orgaf2008f)
    4.  [Loss Estimation](#orgc78cc27)
5.  [Future Improvements](#org9602c86)
6.  [References](#orge41be1c)



<a id="org6e5ffe8"></a>

# Portfolio Risk Management in R

This project implements a lightweight, minimal dependency approach to estimating the portfolio risk using:

1.  The Variance-Covariance Linear Model,
2.  A Gaussian copula with *t*-distributed marginals.

A full report is available in the [documentation file](Report.pdf). The methods employed are based on (McNeil, Alexander J. and Frey, Rüdiger and Embrechts, Paul, 2015), targeting readability, reproducibility, and minimal statistical "black-boxing". 


<a id="org7829e1e"></a>

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


<a id="org3221267"></a>

# CRAN Dependencies

This project has very few dependencies.

    suppressMessages(library(tidyverse)) # Tidyverse supplies data manipulation and plots
    suppressMessages(library (here)) # here enables reproducible pathing
    suppressMessages(library(MASS))  # MASS provides multivariate Gaussian simulations

**No risk or copula modules are used**, all models implemented from "first principles". The report is in [a pdf](Report.pdf), the [main project file](src/Main.rmd) is also available. All data used is kept in "data/", and referenced in the Main file using \`here\`.


<a id="orgf376761"></a>

# Methodology


<a id="org14ce5bb"></a>

## Preparing the Dataset

1.  Basic exploratory analysis of dataset,
2.  Conversion to logged returns,
3.  Cleaning dataset of anomalies.


<a id="org6465c21"></a>

## Variance-Covariance Model

Assumptions:

1.  Normally distributed log returns,
2.  Linearised profit approximation (first order terms of Taylor expansion),

Method:   

1.  Calculate sample mean and covariance,
2.  Calculate VaR directly,
3.  Calculate AVaR analytically.


<a id="orgaf2008f"></a>

## Copula Joint Risk Modelling

1.  Fitted *t*-distributions to marginals,
2.  Fitted Gaussian copula to marginal uniform distributions,
3.  Generated joint samples from copula,
4.  Applied marginal distributions to convert back to stock price changes.


<a id="orgc78cc27"></a>

## Loss Estimation

1.  Monte Carlo simulation of potential loss portfolios,
2.  Required quantile collected for VaR,
3.  Exceeding values averaged for AVaR.


<a id="org9602c86"></a>

# Future Improvements

1.  Generalised functions for both,
2.  Backlinked API for functions to modularly support each other,
3.  GARCH volatility modelling,
4.  Backtesting? Stress-testing?


<a id="orge41be1c"></a>

# References

\##+cite<sub>export</sub>: csl harvard-university-of-leeds.csl

McNeil, Alexander J. and Frey, Rüdiger and Embrechts, Paul (2015). *Quantitative Risk Management: Concepts, Techniques and Tools*, Princeton University Press.

