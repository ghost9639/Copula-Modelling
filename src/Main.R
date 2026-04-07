library(MASS)
library(data.table)

#' Given a matrix of stock_prices and a vector time_index,
#' this function returns a matrix of stock returns with
#' the corresponding slice of time.
returnGenerator <- function (stock_prices, time_index) {

    logged_prices <- log(as.matrix(stock_prices))
    
    returns <- diff(logged_prices)

    return (data.table(
               time_index[-1],
               returns
           ))

}

#' Given a start date, an end date, a matrix of stocks, a vector of the dates, a numeric
#' vector of shares, and optionally an alpha level (default 0.01 for 99% CI), this function
#' will calculate and return the VaR and AVaR using the variance-covariance method.
linearModelRisk <- function (start_date, end_date, data_matrix, date_vector, shares, alpha = 0.01) {

    ## making returns matrix
    data_matrix <- as.data.table(data_matrix)
    returns <- returnGenerator (data_matrix, date_vector)
    diffed_dates <- date_vector[-1,]

    ## somewhat slow on massive datasets but only way to not force column to be called "Date" or something specific
    sample_set <- returns[apply(diffed_dates, 1, function (x) start_date < x & x < end_date),]

    returns_mat <- sample_set[,-1]

    ## Parameter Estimation
    load_means <- colMeans (returns_mat)
    load_covs <- cov(returns_mat)

    ## last price
    final_close <- as.numeric(
        data_matrix[apply(date_vector, 1, function(x) x == end_date)]
    )

    ## Model Calcuation
    weighted <- shares * final_close
    lin_mu <- sum(weighted * load_means)

    lin_sigma <- sqrt(
        drop(t(weighted) %*% as.matrix(load_covs) %*% weighted)
    )

    lin_VaR <- -lin_mu + lin_sigma * qnorm (1-alpha)
    lin_AVaR <- -lin_mu + lin_sigma * dnorm(qnorm (1-alpha))/alpha

    VaRs <- data.table (
        VaR = lin_VaR,
        AVaR = lin_AVaR
    )

    return(VaRs)
}

#' Given a data vector, this function will fit some t distribution dof by maximising the log-likelihood.
#' The dof ranges from 1 to 20 proceeding in increments of 0.01. It is possible that this function returns
#' NA if it cannot fit a single t distribution with these specifications.
fitting_marginals <- function (series) {
    ## just a set of test dfs
    
    trial_dfs <- c(100:2000) / 100
    df_log_lik <- rep(0, 1900)
    
    for (i in c(100:2000)) {
        ## MASS t distribution fitting

        tryCatch({
            fit <- fitdistr(
                series,
                "t",
                df = trial_dfs[i], # optional df
                start = list(
                    m  = median(series),
                    s  = IQR(series) / 2
                ),
                lower = c(m = -Inf, s = 1e-8)
            )

            df_log_lik[i] = fit$loglik
        }, error = function(e) {
            df_log_lik = NA
        })
        
    }

    ## return (df_log_lik)
    return (trial_dfs[which.max(df_log_lik)])
}

#' This function requires a start date, an end date, a matrix of stock close prices, a vector
#' of dates, a (purely numeric) vector of shares, and optionally: the simulation count and a
#' alpha level for the confidence interval. The function returns a matrix of the copula
#' VaR and AVaR. It fits a Gaussian copula and t-distributions to the stock prices, but the
#' main bottleneck is optimising the t-distribution degrees of freedom.
copulaRiskCalculator <- function (start_date, end_date, data_matrix, date_vector, shares, simulations = 30000, alpha = 0.01) {

    final_close <- data_matrix [apply(date_vector, 1, function(x) x == end_date),]
    returns <- returnGenerator (data_matrix, date_vector)
    return_stocks <- returns[,-1] # our returns stock prices
    return_dates <- returns[,1]   # our dates for the returns

    sample_set <- return_stocks[apply(return_dates, 1, function (x) start_date < x & x < end_date),]

    ## fitting my personal marginal distributions (using an experimental MLE dof for the t distribution)
    dofs <- rep(0, ncol(sample_set))
    for (i in c(1:ncol(sample_set))) {
        dofs[i] <- fitting_marginals(sample_set[[i]])
    }

    ## t marginals fits
    fits <- lapply(c(1:ncol(sample_set)), function(i) {
        tryCatch({
            MASS::fitdistr(
                      sample_set[[i]],
                      "t",
                      df = dofs[i],
                      start = list(
                          m = median(sample_set[[i]]),
                          s = IQR(sample_set[[i]]) / 2
                      ),
                      lower = c(m = -Inf, s = 1e-8)
                  )
        }, error = function(e) {
            NULL
        })
    })


    ## uniforms for marginals
    uniforms <- matrix (0, nrow = nrow(sample_set), ncol = ncol(sample_set))
    for (i in c(1:ncol(sample_set))) {

        m <- fits[[i]]$estimate["m"]
        s <- fits[[i]]$estimate["s"]
        
        uniforms[,i] <- pt((sample_set[[i]] - m) / s, df = dofs[i])
    }


    ## copula parameter fitting
    Y <- qnorm(uniforms)
    sigma_hat <- crossprod(Y) / nrow (Y)

    c_means <- rep(0,ncol(sample_set)) # copula means
    Det <- diag(1 / sqrt(diag(sigma_hat)))
    P_hat <- Det %*% sigma_hat %*% Det # copula covariance

    joint_estimate <- MASS::mvrnorm(n = simulations, mu = c_means, Sigma = P_hat)

    sim_marginals <- lapply(c(1:ncol(sample_set)), function(i) {

        m <- fits[[i]]$estimate["m"]
        s <- fits[[i]]$estimate["s"]

        qt(pnorm(joint_estimate[,i]), df = dofs[i]) * s + m
    })

    sim_returns <- data.table(do.call (cbind, sim_marginals))

    sim_prices <- exp(as.matrix(sim_returns)) *
        matrix(as.numeric(final_close),
               nrow = nrow(sim_returns),
               ncol = length(as.numeric(final_close)),
               byrow = TRUE)

    sim_portfolio_value <- sim_prices %*% shares
    initial_value <- sum(as.numeric(final_close) * shares)

    profits <- sim_portfolio_value - initial_value

    copula_VaR <- quantile(profits,probs=alpha)
    copula_AVaR <- mean(profits[profits <= copula_VaR])

    ## sprintf("The copula estimated VaR is £%.2f, and the copula estimated AVaR is £%.2f at the %.2f confidence interval",
    ##         -copula_VaR, -copula_AVaR, 1-alpha)

    VaRs <- data.table (
        VaR = -copula_VaR,
        AVaR = -copula_AVaR
    )

    return(VaRs)
}

## =========================
## Main
## =========================

## My Initial Data reading

close_prices <- fread ("data/Data_Student_201605543.csv") # closing prices
shares <- fread ("data//share_numbers.csv")
my_shares <- shares[6,] # shares


## data cleaning
risk_factors <- returnGenerator (close_prices[,-1], as.IDate(close_prices$Date))

head(risk_factors)

## removing my personal anomalies
cleaned_close_prices <- close_prices[c(TRUE, risk_factors$"Asset 1" < 1 & risk_factors$"Asset 1" > -1),]
risk_factors <- risk_factors[risk_factors$"Asset 1" < 1 & risk_factors$"Asset 1" > -1,]


## viz
hist(risk_factors$"Asset.1", breaks = 100)
hist(risk_factors$"Asset.2", breaks = 100)
hist(risk_factors$"Asset.3", breaks = 100)
hist(risk_factors$"Asset.4", breaks = 100)

summary(risk_factors) # looks good

## We want to identify our sample set of dates
sample_start_date <- as.IDate("2021-02-21")
sample_end_date <- as.IDate("2023-02-21")
alpha <- 0.01


## Call the Linear Approximation function
linear_approx <- linearModelRisk("2021-02-21", "2023-02-21", cleaned_close_prices[,-1], cleaned_close_prices[,1], as.numeric(my_shares[,-1]), alpha)
sprintf("The linear VaR is £%.2f and the AVaR is £%.2f at the %i%% CI.",
        linear_approx[[1]], linear_approx[[2]], (1 - alpha) * 100)



## copula call
VaRs <- copulaRiskCalculator (sample_start_date, sample_end_date, cleaned_close_prices[,-1], cleaned_close_prices[,1],
                      as.numeric(my_shares[,-1]), simulations = 30000, alpha = alpha)

sprintf("The Gaussian Copula with t-distributed marginals finds a VaR of £%.2f and an AVaR of £%.2f at the %i%% CI.",
        VaRs[[1]], VaRs[[2]], (1-alpha) * 100)

