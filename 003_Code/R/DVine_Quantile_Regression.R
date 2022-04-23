library(quantreg)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(tidyquant)
library(vinereg)

# for graph saving
path = "C:/Users/admin/Desktop/Classes/Inne/Quant/KNMF konferencja/"
save_graphics <- function(name, path, file_extension){
  ggsave(
    paste(name,".", file_extension,sep=""),
    plot = last_plot(),
    device = file_extension,
    path = path,
    scale = 1,
    dpi = 300,
    limitsize = TRUE
  )  
}

# Helper functions:
get_data <- function(asset_tickers, risk_factors_tickers, from_date_str, to_date_str){
  
  price_getter <- function(tickers, from_, to_){
    
    tq_get(tickers,
           from =from_,
           to = to_,
           get = "stock.prices") %>%
      drop_na() %>%
      select(symbol,date,adjusted) %>%
      rename('Ticker'= 'symbol','Date'='date','Price'='adjusted') %>%
      drop_na() %>%
      mutate(Ticker = str_remove(Ticker,fixed("^")))
  }
  
  return(list(assets=price_getter(asset_tickers,from_date_str,to_date_str),
              risk_factors = price_getter(risk_factors_tickers,from_date_str,to_date_str)
  )
  )
}
data_to_wide <- function(data){
  
  
  wide_maker <- function(df){
    df %>% pivot_wider(id_cols = Date, names_from = Ticker, values_from = colnames(df)[3]) %>% drop_na()
  }
  
  return(lapply(data,wide_maker))
}
calc_logreturns <- function(data){
  
  logrets_calculator <- function(df){
    df %>%
      group_by(Ticker) %>%
      tq_transmute(select = Price,
                   mutate_fun = periodReturn,
                   period = "daily",
                   type="log",
                   col_rename="Daily.Return")%>%
      ungroup()
  }
  return(lapply(data, logrets_calculator))
}
calc_cumrets <- function(ret_data, plot_= FALSE){
  
  cumrets_calculator <- function(df){
    df %>%
      group_by(Ticker) %>% mutate(Cumulative.Return = cumsum(Daily.Return)) %>% ungroup() %>% select(-Daily.Return)
  }
  
  cumrets_data <- lapply(ret_data, cumrets_calculator)
  
  if(plot_){
    
    cumrets_plot_fn <- function(df){
      df %>%
        ggplot(aes(x= Date, y = Cumulative.Return, col = Ticker)) +
        geom_line()+
        labs(x='Date',y="Cumulative return")+
        theme_tq()+theme(legend.position="right")
    }
    
    cumrets_plots <- lapply(cumrets_data, cumrets_plot_fn)
    grid.arrange(cumrets_plots$assets + ggtitle("Assets and risk factors"), cumrets_plots$risk_factors,nrow=2)

    cumrets_plots2 <- cumrets_data$assets %>% ggplot(aes(x= Date, y= Cumulative.Return, color = (Ticker)))+
      geom_line(alpha = 0.3)+ theme_tq() +
      geom_line(data = cumrets_data$Portfolio, aes(x=Date, y = Cumulative.Return), lwd = 1)+
      ggtitle("Portfolio cumulative returns")+ labs(y="Cumulative Return")
    print(cumrets_plots2)
  }
  
  return(cumrets_data)
}
plot_densities <- function(ret_data){
  
  density_plot_maker <- function(df){
    
    plt <- df %>%
      ggplot(aes(x = Daily.Return, fill = Ticker)) +
      labs(x = "Daily logreturns", y = "Density") +
      theme_tq() +
      scale_fill_tq() + 
      facet_wrap(~ Ticker, ncol = 1)
    if(df$Ticker[1]=="Portfolio"){
      plt <- plt+ geom_density(alpha = 0.5, fill = "royalblue4")
    } else{
      plt <- plt+ geom_density(alpha=0.5)
    }
  }
  grid.arrange(density_plot_maker(ret_data$Portfolio),
               density_plot_maker(ret_data$risk_factors),
               ncol=2)
}


# Data download and preprocessing
assets <- c("VOD","UL","BP","MSFT","KO","SBUX","SSUMY","HMC","ITOCY") #
risk_factors <- c("^FTSE","^GSPC","^N225","^VIX")

Data <- get_data(assets, risk_factors, "2019-01-01","2021-01-01")
Ret_Data <- calc_logreturns(Data)

# Setup an equal-weighted portfolio 
x<- Data$assets %>% dplyr::filter(Date==head(Date,1)) %>% select(Price) %>% mutate(Price=1/Price)%>%sum()

portfolio_weights <- Data$assets %>%
  dplyr::filter(Date==head(Date,1)) %>%
  mutate(symbol = Ticker,
         weights = 1/Price/x) %>%
  select(symbol, weights)

# calculate returns
Ret_Data$Portfolio <- tq_portfolio(data = Ret_Data$assets,
                                 assets_col = Ticker,
                                 returns_col = Daily.Return,
                                 weights = portfolio_weights,
                                 col_rename = "Daily.Return") %>% mutate(Ticker = "Portfolio") %>% select(Ticker,Date,Daily.Return)

Cumret_Data <- calc_cumrets(Ret_Data, plot_=FALSE)
plot_densities(Ret_Data)

Ret_Data_wide <- Ret_Data %>% data_to_wide()
regression_df <- left_join(Ret_Data_wide$Portfolio, Ret_Data_wide$risk_factors, by="Date") %>% drop_na() %>% select(-Date)


rm(Cumret_Data,Data,portfolio_weights,Ret_Data,Ret_Data_wide)


#### Fitting the model
library(VineCopula)
library(TSP)
library(rafalib)
library(fGarch)
library(kde1d)

fits <- list()
res <- list()
pvalues <- rep(0,5)
box_pvalues <- rep(0,5)
udata <- matrix(0,dim(regression_df)[1],5)
for(i in 1:5){
  fits[[i]] <- garchFit(formula=~arma(0,1)+garch(1,1), data=regression_df[,i],
                       trace=FALSE, control=list(maxit=10000),
                       include.mean=TRUE, cond.dist="std")
  res[[i]] <- residuals(fits[[i]], standardize=TRUE)
  pvalues[i] <- ks.test(res[[i]], "pstd", 0, 1, coef(fits[[i]])[6])$p.value #check t-student ok?
  box_pvalues[i] <- Box.test(res[[i]])$p.value
  udata[,i] <- pstd(res[[i]], nu=coef(fits[[i]])[6]) #PIT
}
colnames(udata)<-colnames(regression_df)

udata<-as.copuladata(udata)
pairs(udata)

# Copula quantile regression
library(vinereg)
quant_cop_reg <- vinereg(Portfolio ~ .,
                         family = "par", #(tylko parametryczne)
                         selcrit ="aic", # kryterium wyboru struktury
                         data = udata)
summary(quant_cop_reg)
## Struktura:
summary(quant_cop_reg$vine)
# Drzewo:
plot(quant_cop_reg$vine,
     tree="ALL",  # wszystkie drzewa
     var_names="use", # opisz wierzcholki 
     edge_labels= "family_tau", #opisz kopuly (krawedzie),
     type=1
)
# Wykresy konturowe:
contour(quant_cop_reg$vine,
        edge_labels= "family" #opisz kopuly (krawedzie),
)

summary(quant_cop_reg$margins)

qs <- c(0.05,seq(0.1,0.9,0.1),0.95)
plot_effects(quant_cop_reg,
             alpha=qs)

quantcop_plot <- plot_effects(quant_cop_reg,
                              alpha=qs)
quantcop_plot +  ggtitle("D-vine CQR") + labs(x="Risk factor innovation (copula scale)", y="Portfolio innovation (copula scale)")

#save_graphics("CQR",path,"svg")

# Stress tests:
library(scales)
library(RColorBrewer)

build_formulas <- function(risk_factors_all, stressed_risk_factor){
  formulas <- list()
  covariates<- stressed_risk_factor
  risk_factors_all <- risk_factors_all[risk_factors_all!=stressed_risk_factor]
  for(i in 1:length(risk_factors_all)){
    response <- risk_factors_all[i]
    formulas[[response]] <- paste(response,"~",covariates,sep=" ")
    covariates <- paste(covariates,response,sep=" + ")
  }
  return(formulas)
}
perform_regressions <- function(sequential_formulas, ...){
  
  regressions <- lapply(sequential_formulas, function(x){vinereg(as.formula(x), ...)})
  return(regressions)
}

perform_predictions <- function (model, sequential_regressions,stressed_risk_factor,stressed_risk_factor_quantile){
  
  
  marginal_distributions <- model$margins
  response <- data.frame()
  response[1,stressed_risk_factor] <- qkde1d(stressed_risk_factor_quantile,
                                             marginal_distributions[[stressed_risk_factor]])
  
  response_variable_names <- names(sequential_regressions)
  for(response_variable in response_variable_names){
    response[1,response_variable] <- as.numeric(predict(sequential_regressions[[response_variable]],
                                                        newdata = response)) 
  }
  # responses will be on the data scale!
  
  
  return(response) 
}

stress_test <- function(model, stressed_risk_factor, stressed_risk_factor_quantile, alphas = NA, ...){
  
  risk_factors_all <- model$order
  marginal_distributions<- model$margins
  sequential_formulas <- build_formulas(risk_factors_all, stressed_risk_factor)
  sequential_regressions <- perform_regressions(sequential_formulas, family = "nonpar", selcrit="aic", data=udata)
  sequential_predictions <- perform_predictions(model,
                                                sequential_regressions,
                                                stressed_risk_factor,
                                                stressed_risk_factor_quantile)
  
  Portfolio_predictions <- predict(model, newdata =sequential_predictions, alpha = alphas)
  portfolio_cdf <- function(x){pkde1d(x,marginal_distributions[["Portfolio"]])}

  uniform_transform <- function(x,marginal_name){
    return(pkde1d(x,marginal_distributions[[marginal_name]]))
  }
  
  sequential_predictions<- sequential_predictions %>%
    pivot_longer(cols=everything(),
                 names_to="ticker",
                 values_to="prediction") %>%
    rowwise()%>%
    mutate(quantile = pkde1d(prediction,marginal_distributions[[ticker]] )) %>%
    select(ticker,quantile) %>%
    pivot_wider(names_from=ticker,values_from=quantile)
  
  Stress_test_results <- list(Scenarios = sequential_predictions,
                              Response= sapply(Portfolio_predictions, #back to u-scale
                                               portfolio_cdf))
  return(Stress_test_results)
}
plot_stress_test_results <- function(stress_test_results,type){
  
  if(type=="all"){
    stressed_rf_name <- quant_cop_reg$order
    
    stress_plot <- ggplot(stress_test_results, 
                          aes(x=Quantile,
                              y =factor(Variable),
                              shape = factor(RF_level),
                              alpha = factor(RF_level),
                              color=(Variable==Stressed_Risk_Factor)))+
      geom_point(size=3)+
      geom_vline(xintercept= c(0.5), linetype='dashed')+
      facet_wrap(~Stressed_Risk_Factor) +
      labs(y="Response") +
      ggtitle("Stress test results: mean")+
      scale_color_manual(breaks = c(FALSE, TRUE),
                         values=c("black", "red"))
      
    if(length(unique(stress_test_results$RF_level))==2){
      stress_plot<- stress_plot +
        scale_alpha_manual(breaks= unique(stress_test_results$RF_level),
                           values=c(0.3,1))+
      labs(color = "Stressed variable", shape = "Stress level (quantile)", alpha =  "Stress level (quantile)")
    }
    
  }else{
    stress_plot <- stress_test_results %>% 
      dplyr::filter(Quantile != "mean") %>%
      mutate(Quantile = as.numeric(Quantile))%>%
      ggplot(aes(x=Estimate,
                 y = factor(RF_level),
                 color = Quantile))+
      geom_point()+
      facet_wrap(~Stressed_Risk_Factor)+
      scale_colour_gradientn(limits = c(0,1), colours=rainbow(5))+
      labs(y="Stress level", x= "Quantile estimate")+
      geom_vline(xintercept=0.5,linetype="dashed")+
      ggtitle("Stressed portfolio quantiles")
  }
  return(stress_plot)
}

stress_test_results_to_df <- function(stress_test_results){
  DF<- stress_test_results$Scenarios %>%
    pivot_longer(cols = everything(), names_to="Variable", values_to="Quantile") %>%
    bind_rows(data.frame(Variable ="Portfolio", Quantile = as.numeric(stress_test_results$Response[1])))%>%
    mutate(Stressed_Risk_Factor = names(stress_test_results$Scenarios)[1],
           RF_level = as.numeric(stress_test_results$Scenarios[1])) 
  return(DF)
}
stress_test_results_to_response_quantiles <- function(stress_test_results){
  DF<- data.frame(Quantile = names(stress_test_results$Response),
                  Estimate = as.numeric(stress_test_results$Response))
                  
  DF<- DF %>%  mutate(Stressed_Risk_Factor = names(stress_test_results$Scenarios)[1],
           RF_level = as.numeric(stress_test_results$Scenarios[1])) 
  return(DF)
  
}


risk_factors_all <- quant_cop_reg$order
stress_plots <- list()
Stress_test_results <- data.frame()
Response_quantiles <- data.frame()

for(stress_level in c(0.05,0.1)){
  for(risk_factor in risk_factors_all){
    new_stress_test_results <- stress_test(quant_cop_reg, 
                                           risk_factor,
                                           stress_level,
                                           alphas = c(0.01,0.05,seq(0.1,0.9,0.1),0.95,0.99,NA))
    
    Stress_test_results <- bind_rows(Stress_test_results,
                                     stress_test_results_to_df(new_stress_test_results))
    Response_quantiles <- bind_rows(Response_quantiles,
                                    stress_test_results_to_response_quantiles(new_stress_test_results))
  }
  Stress_test_results <- Stress_test_results %>% mutate(RF_level = round(RF_level,2))
  Response_quantiles <- Response_quantiles %>% mutate(RF_level = round(RF_level,2))
  
}
plot_stress_test_results(Stress_test_results, type = "all")
plot_stress_test_results(Response_quantiles, type = "Portfolio")



# Example stress scenario
stress_scenario <- data.frame(FTSE = 0.5, GSPC = 0.5, N225 = 0.5, VIX = 0.95)
quantile_stress_results <- predict(quant_cop_reg,
                                   newdata = stress_scenario,
                                   alpha = c(0.05,0.1,0.2,0.3,
                                             0.4,0.5,0.6,0.7,
                                             0.8,0.9,0.95))
