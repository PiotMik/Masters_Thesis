library(rvinecopulib)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(xts)
library(edfun)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Stock prices using quantmod

getSymbols(c("AAPL","AMZN"), from = '2017-01-01',
           to = "2018-03-01",warnings = FALSE,
           auto.assign = TRUE)

AAPL <- AAPL[,'AAPL.Adjusted'] %>% Delt(type="log") %>% na.trim()
AMZN <- AMZN[,'AMZN.Adjusted'] %>% Delt(type="log") %>% na.trim()
colnames(AMZN) <- c("Price")
colnames(AAPL) <- c("Price")

Data <- merge.xts(AAPL,AMZN)
colnames(Data) <-c("AAPL","AMZN")

# Fitting marginals

AAPL_fit <- edfun(as.numeric(Data$AAPL))  # empirical distribution
AAPL_inv_ecdf <- AAPL_fit$qfun            # quantile function
AAPL_ecdf <- AAPL_fit$pfun                # ECDF 
AAPL_pdf <-  AAPL_fit$dfun                # KSDens
AAPL_transform <- AAPL_ecdf(Data$AAPL)    # probability integral transform (these will be uniform)

AMZN_fit <- edfun(as.numeric(Data$AMZN))
AMZN_inv_ecdf <- AMZN_fit$qfun
AMZN_ecdf <- AMZN_fit$pfun
AMZN_pdf <- AMZN_fit$dfun
AMZN_transform <- AMZN_ecdf(Data$AMZN)

par(mfrow=c(1,2))
curve(AAPL_ecdf, min(Data$AAPL),max(Data$AAPL), main = "ECDF")
curve(AAPL_pdf, min(Data$AAPL),max(Data$AAPL), main = "EPDF")

plot(AMZN_transform, AAPL_transform)      # transformed variables to fit copula over

## Fitting a bivariate copula
fitted_copula <- bicop(data.frame(AAPL_transform, AMZN_transform)) # copula fitting

## visualization
plot(fitted_copula, margins = "unif") # copula density
contour(fitted_copula, margins = "unif")

plot(fitted_copula,margins="norm") # transformed to standard normal
contour(fitted_copula,margins="norm")

# Generating random samples
u <- rbicop(580, fitted_copula)
plot(u)

# Reconstructing the simulating marginals
AAPL_simulated <- AAPL_inv_ecdf(u[,1])
AMZN_simulated <- AMZN_inv_ecdf(u[,2])

# Checking the marginals distribution
par(mfrow=c(1,2))
qqplot(x=AAPL_simulated, y=as.numeric(Data$AAPL), main = "AAPL")
abline(c(0,1), col=2)
qqplot(x=AMZN_simulated, y=as.numeric(Data$AMZN), main = "AMZN")
abline(c(0,1), col=2)

