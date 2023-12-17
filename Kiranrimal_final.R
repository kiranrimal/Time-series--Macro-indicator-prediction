install.packages("pacman")
pacman::p_load(tidyverse,ggplot2,tseries,
               forecast,tsibble,tsbox,xtable,broom,janitor,urca,ggExtra,patchwork,vars)
source("https://www.dropbox.com/scl/fi/9qq0l58zr7vdfk45v1suj/intord.R?rlkey=1dt0exp7lnekuhbixdhti8euy&dl=1")

install.packages("fredr")
library(fredr)
fredr_set_key("YourAPIKEY")


#unemployment rate
UNRATE <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2023-04-01"),
  frequency = "q"
)

#federal fund rate
FEDFUNDS <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2023-04-01"),
  frequency = "q"
)
#CPI index
CPI <- fredr(
  series_id = "CPALTT01USM657N",
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2023-04-01"),
  frequency = "q"
)

# Real Gross Domestic Product
GDPC1 <- fredr(
  series_id = "A191RO1Q156NBEA", #Percent Change from Quarter One Year Ago,
  observation_start = as.Date("1960-01-01"),  # Seasonally Adjusted Annual Rate
  observation_end = as.Date("2023-04-01"),
  frequency = "q"
)

timeseries <- data.frame(UNRATE$date,UNRATE$value,FEDFUNDS$value,CPI$value,GDPC1$value)
#timeseries<- select(data$date,data$UNRATE,data$FEDFUNDS, data$CPALTT01USM657N, data$A191RO1Q156NBEA)
timeseries_data <-setNames(timeseries, c("Date","UNEMPL","FED","CPI","RGDP"))

names(timeseries_data)

#i. Order of Integration
intord(data$UNEMPL) #I1
intord(data$FED) #I1
intord(data$CPI) #I0
intord(data$RGDP) #I0


#ii. co-integration
#Engle-Granger test

#CPI VS Unemployment
lin_combination1 <- lm(CPI~ UNEMPL - 1, data = data)$residuals
lin_combination2 <- lm(CPI~RGDP - 1, data = data)$residuals
lin_combination3 <- lm(UNEMPL~RGDP - 1, data = data)$residuals
lin_combination4 <- lm(CPI~FED - 1, data = data)$residuals
lin_combination5 <- lm(UNEMPL~FED - 1, data = data)$residuals
intord(lin_combination1) #co-int
intord(lin_combination2) #co-int
intord(lin_combination3) #co-int
intord(lin_combination4) ##co-int
intord(lin_combination5) #nope



#iii. separate data in Train and test
#Present the results of your VAR/VECM model, including coefficients,
#standard errors, p-values, and any other relevant statistics
#after stationary 
UNEMPL = diff(timeseries_data$UNEMPL)
FED <- diff(timeseries_data$FED)
CPI <- timeseries_data$CPI[1:253]
RGDP <- timeseries_data$RGDP[1:253]
data <- data.frame(UNEMPL,FED,CPI,RGDP)

trainset <- data[1:(nrow(data)-10), ]
testset <- data[(nrow(data)-9):nrow(data), ]
  
 # Choose lag order using AIC
la
#only edndogenous variable used
#we do not care about Statonary in VAR, as we do not care about inference. 
var_model<-VAR(trainset[,c('UNEMPL','RGDP','CPI')],
               lag.max=4,
               type='both',
               ic='AIC',
               exogen = trainset$FED)
var_model
summary(var_model)
coef(var_model)
--------------------------------------------------------------------------------
#iv. #model
alt_1 <- VAR(trainset[,c('UNEMPL','RGDP','CPI')],
               p = 2,
             type  = 'none')

FED_df <- data.frame(FED_diff = trainset$FED)

alt_2 <- VAR(trainset[, c('RGDP', 'CPI','UNEMPL')],
             type = 'both',
             p = 2,
             exogen = FED_df)

CPI_df <- data.frame(CPI = trainset$CPI)

alt_3 <- VAR(trainset[, c('UNEMPL', 'RGDP',"CPI")],
             type = 'none',
             p = 3)

#forecast of test set: Endogenuous 
var_1_pred <- predict(alt_1, n.ahead = 10)
x11(); par(mai=rep(0.4, 4)); plot(var_1_pred)
x11(); par(mai=rep(0.4, 4)); fanchart(var_1_pred)

future_FED <- data.frame(FED_diff = rep(last(trainset$FED), 10))
var_2_pred <- predict(alt_2, n.ahead = 10, dumvar = future_FED)
x11(); par(mai=rep(0.4, 4)); plot(var_2_pred)

future_CPI <- data.frame(CPI = rep(last(trainset$CPI), 10))
var_3_pred <- predict(alt_3, n.ahead = 10, dumvar = future_CPI)
x11(); par(mai=rep(0.4, 4)); plot(var_3_pred)


#loss distributions for model 1
mse_var_1_unempl<- mean((var_1_pred$fcst$UNEMPL[,1] - testset$UNEMPL)^2)
mse_var_2_unempl <- mean((var_2_pred$fcst$UNEMPL[,1] - testset$UNEMPL)^2)
mse_var_3_unempl <- mean((var_3_pred$fcst$UNEMPL[,1] - testset$UNEMPL)^2)

# Model 2
mse_var_1_rgdp <- mean((var_1_pred$fcst$RGDP[,1] - testset$RGDP)^2)
mse_var_2_rgdp <- mean((var_2_pred$fcst$RGDP[,1] - testset$RGDP)^2)
mse_var_3_rgdp <- mean((var_3_pred$fcst$RGDP[,1] - testset$RGDP)^2)

#model 3
mse_var_1_cpi <- mean((var_1_pred$fcst$CPI[,1] - testset$CPI)^2)
mse_var_2_cpi <- mean((var_2_pred$fcst$CPI[,1] - testset$CPI)^2)
mse_var_3_cpi <- mean((var_3_pred$fcst$CPI[,1] - testset$CPI)^2)

# Combining MSEs for comparison
mse_values <- data.frame(
  UNEMPL = c(mse_var_1_unempl, mse_var_2_unempl, mse_var_3_unempl),
  RGDP = c(mse_var_1_rgdp, mse_var_2_rgdp, mse_var_3_rgdp),
  CPI = c(mse_var_1_cpi, mse_var_2_cpi, mse_var_3_cpi),
  row.names = c('Model 1', 'Model 2', 'Model 3')
)

# Printing the MSE values for comparison
print(mse_values)
-------------------------------------------------------------------------------
# Function to calculate MAE
calculate_mae <- function(forecast, actual) {
    mean(abs(forecast - actual), na.rm = TRUE)
  }

# Model Evaluation: MAE
mae_var_1_unempl <- calculate_mae(var_1_pred$fcst$UNEMPL[,1], testset$UNEMPL)
mae_var_2_unempl <- calculate_mae(var_2_pred$fcst$UNEMPL[,1], testset$UNEMPL)
mae_var_3_unempl <- calculate_mae(var_3_pred$fcst$UNEMPL[,1], testset$UNEMPL)

# Model 2
mae_var_1_rgdp <- calculate_mae(var_1_pred$fcst$RGDP[,1], testset$RGDP)
mae_var_2_rgdp <- calculate_mae(var_2_pred$fcst$RGDP[,1], testset$RGDP)
mae_var_3_rgdp <- calculate_mae(var_3_pred$fcst$RGDP[,1], testset$RGDP)

# Model 3
mae_var_1_cpi <- calculate_mae(var_1_pred$fcst$CPI[,1], testset$CPI)
mae_var_2_cpi <- calculate_mae(var_2_pred$fcst$CPI[,1], testset$CPI)
mae_var_3_cpi <- calculate_mae(var_3_pred$fcst$CPI[,1], testset$CPI)

# Combining MAEs for comparison
mae_values <- data.frame(
  UNEMPL = c(mae_var_1_unempl, mae_var_2_unempl, mae_var_3_unempl),
  RGDP = c(mae_var_1_rgdp, mae_var_2_rgdp, mae_var_3_rgdp),
  CPI = c(mae_var_1_cpi, mae_var_2_cpi, mae_var_3_cpi),
  row.names = c('Model 1', 'Model 2', 'Model 3')
)

# Print the MAE values
print("MAE Values:")
print(mae_values)

  
------------------------------------------------------------------------
#v. Impulse response function


#vi.orthogonal impulse response
#for each variable
plot(irf(var_model, n.ahead = 10, ortho = T))



#vii. table with the forecast error variance decomposition
#pick best model 
fevd(alt_2, n.ahead = 10) 

