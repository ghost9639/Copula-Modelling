## required libraries for risk modelling package
library(MASS)
library(data.table)

## required libraries for stress testing project  
library(zoo)
library(aTSA)
library(FinTS)
library(here)
library(readxl)

# documentation stuff
library(document)
library(roxygen2)
library(devtools)


#' Given a matrix of stock_prices and a vector time_index,
#' this function returns a matrix of stock returns with
#' the corresponding slice of time.
#'
#' @param stock_prices numeric matrix of prices
#' @param time_index vector of dates or time indices
#' @return data.table of returns
#' @export
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
#'
#' @param data_matrix the matrix of stock values, must be numeric only
#' @param date_vector the vector of dates as data.table::IDate parsable dates
#' @param shares a numeric vector of investments per stock
#' @param alpha the required VaR_(1-alpha)%, defaults to 0.01 for a VaR greater than 99% of potential losses
#' @param start_date a string or data.table::IDate parsable date, used as start of sample period, defaults to first date
#' @param end_date a string or data.table::IDate parsable date, used as end of sample period, defaults to last date
#' @return data.table of VaR and AVaR
#' @export
linearModelRisk <- function (data_matrix, date_vector, shares, alpha = 0.01, start_date = NA, end_date = NA) {

    ## making returns matrix
    data_matrix <- as.data.table(data_matrix)
    returns <- returnGenerator (data_matrix, date_vector)
    diffed_dates <- as.data.table(date_vector[-1,])

    if (is.na(start_date)) {
        start_date <- diffed_dates[1]
    }

    
    if (is.na(end_date)) {
        end_date <- diffed_dates[dim(diffed_dates)[1]]
    }

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
#'
#' @param series a vector of values that needs a t distribution fitted
#' @return double value maximising log-likelihood in range
#' @export
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

#' This function requires a matrix of stock close prices, a vector of dates, a (purely numeric) 
#' vector of shares, and optionally: the simulation count, an alpha level, a start date, and
#' an end date. The function returns a matrix of the copula VaR and AVaR. It fits a Gaussian
#' copula and t-distributions to the stock prices, the main bottleneck is optimising the
#' t-distribution degrees of freedom.
#'
#' @param data_matrix the matrix of stock values, must be numeric only
#' @param date_vector the vector of dates as data.table::IDate parsable dates
#' @param shares a numeric vector of investments per stock
#' @param simulations a number of simulations to undertake (must be an integer), defaults to 30,000
#' @param alpha the required VaR_(1-alpha)%, defaults to 0.01 for a VaR greater than 99% of potential losses
#' @param start_date a string or data.table::IDate parsable date, used as start of sample period, defaults to first date
#' @param end_date a string or data.table::IDate parsable date, used as end of sample period, defaults to last date
#' @return data.table of VaR and AVaR
#' @export
GaussCopulaTMarginals <- function (data_matrix, date_vector, start_date = NA, end_date = NA) {

    returns <- returnGenerator (data_matrix, date_vector)
    return_stocks <- returns[,-1] # our returns stock prices
    return_dates <- returns[,1]   # our dates for the returns

    if (is.na(start_date)) {
        start_date <- return_dates[1]
    }

    
    if (is.na(end_date)) {
        end_date <- return_dates[dim(return_dates)[1]]
    }

    final_close <- data_matrix [apply(date_vector, 1, function(x) x == end_date),]
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

    ## fitted_copula <- data.table(
    ##     'means' = c_means,
    ##     "covariance matrix" = P_hat,
    ##     "t-fits" = fits
    ## )

    fitted_copula <- list(c_means, P_hat, dofs, fits)
    
    return (fitted_copula)
}


GaussCopulaTMarginalsSimulator <- function (c_means, P_hat, dofs, fits, shares, final_close, simulations = 30000, alpha = 0.01) {

    joint_estimate <- MASS::mvrnorm(n = simulations, mu = c_means, Sigma = P_hat)

    sim_marginals <- lapply(c(1:length(c_means)), function(i) {
        
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


    ## print("printing simulated prices")
    ## print(sim_prices)
    ## print("printing shares")
    ## print(shares)
    sim_portfolio_value <- sim_prices %*% as.numeric(shares)
    initial_value <- sum(as.numeric(final_close) * as.numeric(shares))

    ## print("error 5")
    profits <- sim_portfolio_value - initial_value

    ## print("error 6")
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
shares <- fread ("data/share_numbers.csv")
my_shares <- shares[6,] # shares


## data cleaning
risk_factors <- returnGenerator (close_prices[,-1], as.IDate(close_prices$Date))

head(risk_factors)

## removing my personal anomalies
cleaned_close_prices <- close_prices[c(TRUE, risk_factors$"Asset 1" < 1 & risk_factors$"Asset 1" > -1),]
risk_factors <- risk_factors[risk_factors$"Asset 1" < 1 & risk_factors$"Asset 1" > -1,]


## viz
hist(risk_factors$"Asset 1", breaks = 100)
hist(risk_factors$"Asset 2", breaks = 100)
hist(risk_factors$"Asset 3", breaks = 100)
hist(risk_factors$"Asset 4", breaks = 100)

summary(risk_factors) # looks good

## We want to identify our sample set of dates
sample_start_date <- as.IDate("2021-02-21")
sample_end_date <- as.IDate("2023-02-21")
alpha <- 0.01


## Call the Linear Approximation function
linear_approx <- linearModelRisk(data_matrix = cleaned_close_prices[,-1], date_vector= cleaned_close_prices[,1], shares= as.numeric(my_shares[,-1]),alpha= 0.01, start_date = "2021-02-21", end_date = "2023-02-21")
sprintf("The linear VaR is £%.2f and the AVaR is £%.2f at the %i%%-th CI.",
        linear_approx[[1]], linear_approx[[2]], (1 - alpha) * 100)




## copula call

## small settings for what we want
est_date <- as.IDate(tail(cleaned_close_prices$Date, n=1)) # we want to predict the next day after the sample
final_close <- cleaned_close_prices[cleaned_close_prices$Date == est_date,-1] # we will use the last day's close

val <- GaussCopulaTMarginals (cleaned_close_prices[,-1], cleaned_close_prices[,1]) # fits the copula

## predicts 30,000 outcomes and returns the 99%-th percentile
VaRs <- GaussCopulaTMarginalsSimulator (val[[1]], val[[2]], val[[3]], val[[4]], my_shares[,-1], final_close)

sprintf("The Gaussian Copula with t-distributed marginals finds a VaR of £%.2f and an AVaR of £%.2f at the %i%%-th CI.",
        VaRs[[1]], VaRs[[2]], (1-alpha) * 100)
val





## Stress testing project
## Now suppose we explore a real portfolio of stocks
here::here()
all_stock_values <- data.table(read_xlsx(here("data", "stock_port.xlsx"), sheet = "Sheet1"))

all_stock_values$Date <- as.Date(all_stock_values$Date, origin = "1899-12-29")

stock_values <- all_stock_values[all_stock_values$Date > as.IDate("2024-01-01") & all_stock_values$Date < as.IDate("2026-05-19"),]

## note that we do have 4 missing values in Evolve Enhanced Yield Bond
## since we need to take differences, this will become 8 missing variables
## instead of this, we can use the time series library `zoo` to interpolate the missing vars
stock_values$"Evolve Enhanced Yield Bond" = na.approx(stock_values$"Evolve Enhanced Yield Bond")

summary(stock_values)
tail(stock_values)
stock_values # close price dataset

port_risk_factors <- returnGenerator (stock_values[,-1], as.IDate(stock_values$Date))

## this dataset is full until 19/05/2026, so that becomes our final test date
summary(port_risk_factors)
port_risk_factors

## all returns data stationary
adf.test(x = port_risk_factors$APPLE, nlag = 2)
adf.test(x = port_risk_factors$NVIDIA, nlag = 2)
adf.test(x = port_risk_factors$"USD Dollar Fund Unhedged", nlag=2)
adf.test(x = port_risk_factors$"PIMCO ETF", nlag=2)
adf.test(x = port_risk_factors$"Evolve Enhanced Yield Bond", nlag=2)

## Apple and PIMCO exhibit autoregressive heteroscedastic error
ArchTest(port_risk_factors$APPLE)
ArchTest(port_risk_factors$NVIDIA)
ArchTest(port_risk_factors$"USD Dollar Fund Unhedged")
ArchTest(port_risk_factors$"PIMCO ETF")
ArchTest(port_risk_factors$"Evolve Enhanced Yield Bond")

sample_start_date <- as.IDate("2024-01-02")
sample_end_date <- as.IDate("2026-05-19")
alpha <- 0.01

hist(port_risk_factors$APPLE, breaks = 100) # anomalies above |0.1|,
hist(port_risk_factors$NVIDIA, breaks = 100)
hist(port_risk_factors$"PIMCO ETF", breaks = 100)
hist(port_risk_factors$"USD Dollar Fund Unhedged", breaks = 100)
hist(port_risk_factors$"Evolve Enhanced Yield Bond", breaks = 100)

## cleaned risk factors for portfolio
cleaned_port_risk_factors <- port_risk_factors[c(port_risk_factors$"APPLE" < 1 & port_risk_factors$"APPLE" > -1),]

## we have 596 sampled days
summary(cleaned_port_risk_factors)
cleaned_port_risk_factors


## suppose we have the shares
port_shares = data.table(NVIDIA = 120, APPLE = 150, "USD Fund" = 200, "Evolve Enhanced Yield Bond" = 150, "PIMCO ETF" = 150)

## Call the Linear Approximation function (default time period is entire dataset)
linear_approx_new <- linearModelRisk(data_matrix = stock_values[,-1],
                                     date_vector= stock_values[,1],
                                     shares= as.numeric(port_shares))

sprintf("The linear VaR is £%.2f and the AVaR is £%.2f at the %i%%-th CI.",
        linear_approx_new[[1]], linear_approx_new[[2]], (1 - alpha) * 100)

## [1] "The linear VaR is £2918.42 and the AVaR is £3358.89 at the 99%-th CI."

## copula call

## small settings for what we want
est_date <- as.IDate(tail(stock_values$Date, n=1)) # we want to predict the next day after the sample
final_close <- stock_values[stock_values$Date == est_date,-1] # we will use the last day's close

val <- GaussCopulaTMarginals (stock_values[,-1], stock_values[,1]) # fits the copula
## predicts 30,000 outcomes and returns the 99%-th percentile
VaRs <- GaussCopulaTMarginalsSimulator (val[[1]], val[[2]], val[[3]], val[[4]], port_shares, final_close)
sprintf("The Gaussian Copula with t-distributed marginals finds a VaR of £%.2f and an AVaR of £%.2f at the %i%%-th CI.",
        VaRs[[1]], VaRs[[2]], (1-alpha) * 100)

## [1] "The Gaussian Copula with t-distributed marginals finds a VaR of £3101.86 and an AVaR of £4427.88 at the 99%-th CI."


## we can now stress test

## first we use the CAPM to set base market relationships
fama_french_factors <- fread(here("data", "F-F_Research_Data_Factors_daily 2.csv"))
fama_french_factors[,Date := as.IDate(as.character(Date), format = "%Y%m%d")]
fama_french_factors <- fama_french_factors[fama_french_factors$Date > as.IDate("2024-01-02") & fama_french_factors$Date < as.IDate("2026-05-19")]
fama_french_factors
summary(fama_french_factors)

fama_french_techret <- fread(here("data", "HiTechReturns.csv"))
fama_french_techret[,Date := as.IDate(as.character(Date), format = "%Y%m%d")]
fama_french_techret <- fama_french_techret[fama_french_techret$Date > as.IDate("2024-01-02") & fama_french_techret$Date < as.IDate("2026-05-19")]
fama_french_techret # all of our industry stocks are high tech
summary(fama_french_techret)


industry_exc <- fama_french_techret$"HiTec Average Value Weighted Returns" - fama_french_factors$RF
head(industry_exc)

## I mixed up "RF-RM" when writing the name initially, the actual calculation is correct
factors <- data.table("RF-RM" = (fama_french_factors$"Mkt-RF" - fama_french_factors$RF), "TechExc" = industry_exc)
factors

tech_CAPM <- lm(industry_exc ~ factors$"RF-RM")
tech_CAPM$coefficients # slightly better than the average market unsurprisingly

mod <- lm(port_risk_factors$APPLE ~ factors$"RF-RM" + industry_exc)
mod
head(mod$residuals)

port_risk_factors
fama_french_factors
mymodels <- list()
myresiduals <- data.table()
mycoefs <- data.table()

## capm for everything in portfolio
for (i in (cleaned_port_risk_factors[,-1])) {

    mod <- lm((i - fama_french_factors[[5]]) ~ factors$"RF-RM" + industry_exc)
    mymodels <- append(mymodels, list(i = mod))
    myresiduals <- cbind(myresiduals, mod$residuals)
    mycoefs <- cbind(mycoefs, mod$coefficients)

}

colnames(myresiduals) <- colnames(cleaned_port_risk_factors[,-1])
names(mymodels) <- colnames(cleaned_port_risk_factors[,-1])
colnames(mycoefs) <- colnames(cleaned_port_risk_factors[,-1])
row.names(mycoefs) <- c("alpha", "market beta", "tech beta")

for (col in myresiduals) {
    print(shapiro.test(col)) # residuals are normally distributed
    print(fitdistr(col, "normal")) # basically distributed around 0
}

## r_{i,t} - r_{f,t} = \alpha_i + \beta_{i,m}(r_{m,t} - r_{f,t}) + \beta_{i,I} I_t + \epsilon_{i,t}

## we can simulate these stocks under specific shocks to the risk free rate, market returns, and the tech industry
## Scenario 1: tech downturn where the market return drops 8%, the risk free rate drops 2%, and the tech market drops 10%
## Scenario 2: Heavy Downturn where market return drops 10%, the risk free rate drops 5%, and the tech market drops 20%
## Scenario 3: Market downturn where market return drops 25%, risk free rate drops 5%, and tech market drops 5%

mkt_rf_ret_change <- -0.08
rfr_ret_change <- -0.02
tech_ret_change <- -0.10

## we can simulate returns like this
red_returns <- data.table()
for (i in 1:(length(cleaned_port_risk_factors[,-1]))){
    redmod <- mycoefs[[1,i]] + mycoefs[[2,1]] * (factors$"RF-RM") +
        mycoefs[[3,i]] * industry_exc + (mymodels[[i]])$residuals + fama_french_factors[[5]]
    red_returns <- cbind(red_returns, redmod)
}

red_returns

## first set tech industry to suffer specific shock according to its own fitted CAPM equation
red_tech <- tech_ret_change + tech_CAPM$coefficients[[1]] + tech_CAPM$coefficients[[2]] *
    (factors$"RF-RM" + mkt_rf_ret_change) + tech_CAPM$residuals + rfr_ret_change + fama_french_factors[[5]]

plot(red_tech, col="red")
lines(industry_exc, col="blue") # interesting how much the risk free rate influences the shock in my data

summary(red_tech)
summary(industry_exc)


## Then set new bad outcomes for each stock according to their fitted industry CAPM model

red_returns <- data.table()
for (i in 1:(length(cleaned_port_risk_factors[,-1]))){
    redmod <- mycoefs[[1,i]] + mycoefs[[2,i]] * (factors$"RF-RM" + mkt_rf_ret_change) + 
        mycoefs[[3,i]] * red_tech + (mymodels[[i]])$residuals + fama_french_factors[[5]] + rfr_ret_change
    red_returns <- cbind(red_returns, redmod)
}

red_returns
summary(red_returns)
summary(cleaned_port_risk_factors[,-1])
colnames(red_returns) <- colnames(cleaned_port_risk_factors[,-1])

plot(red_returns[[1]],col="red")
lines(cleaned_port_risk_factors[[2]],col="blue")

red_vals <- exp(red_returns)
red_port <- data.table(cumprod(rbind(stock_values[1,-1], red_vals)))
returnGenerator(red_port, stock_values[,1])
red_returns
plot(red_port[[2]])


## the variance covariance method falls apart here because the dataset is so long that the portfolio becomes
## so worthless after ~100 days that the potential loss on the final close is maybe half of a penny
## copula doesn't fall into this because the distribution is fit to the dataset but the simulations are done
## off a given final close value

## copula call

## small settings for what we want
est_date <- as.IDate(tail(stock_values$Date, n=1)) # we want to predict the next day after the sample
final_close <- stock_values[stock_values$Date == est_date,-1] # we will use the last day's close

val <- GaussCopulaTMarginals (red_port, stock_values[,1]) # fits the copula
## val
## predicts 30,000 outcomes and returns the 99%-th percentile
VaRs <- GaussCopulaTMarginalsSimulator (val[[1]], val[[2]], val[[3]], val[[4]], port_shares, final_close)
## port_shares[,-1]
sprintf("The Gaussian Copula with t-distributed marginals finds a VaR of £%.2f and an AVaR of £%.2f at the %i%%-th CI.",
        VaRs[[1]], VaRs[[2]], (1-alpha) * 100)

## [1] "The Gaussian Copula with t-distributed marginals finds a VaR of £5172.16 and an AVaR of £6408.59 at the 99%-th CI."
## [2] "The Gaussian Copula with t-distributed marginals finds a VaR of £8264.95 and an AVaR of £9498.29 at the 99%-th CI."
## [3] "The Gaussian Copula with t-distributed marginals finds a VaR of £8196.56 and an AVaR of £9459.90 at the 99%-th CI."
