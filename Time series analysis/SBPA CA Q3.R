
#Load packages
  pacman::p_load(forecast, tseries, fUnitRoots, tidyverse, fastDummies, lmtest, lubridate, tidyverse, forecastHybrid, astsa)

#Set working directory
  setwd("E:/NUS/EBAC/EBA5002 Business Analytics Practice/2 PA Predictive Analytics/CA Doc/CA/data/ridership")

#######################################################################################
#====Amtrak====
  #Load file
    Rider = read.csv('AmtrakBig_CA_Question-3.csv', check.names = FALSE)
    head(Rider, n = 4)
    
  #Data cleaning and preparation
    class(Rider) #Check for data frame
    sapply(Rider, class) #Check class of each column
    glimpse(Rider)
    summary(Rider)
    
  #Check N/A
    Rider %>%
      filter(!complete.cases(.))
    
  #Edit error in t
    Rider <- Rider %>%
      mutate(t = 1:nrow(Rider)) %>%
      mutate(Ridership = ((Ridership))) #Log transformation as the variation is not constant
    
  #Check if type is Time Series
    is.ts(Rider$Ridership)
  
  #Convert to stationarity --- activate this only after generating the overall plots
    Rider$Ridership_Ori <- Rider$Ridership
    
    Rider <- Rider %>%
      mutate(Ridership = Ridership_Ori) #adfTest p-value = 0.6567 
      #mutate(Ridership = log(Ridership_Ori)) #adfTest p-value = 0.7716
      #mutate(Ridership = log(Ridership_Ori, base = exp(10))) #adfTest p-value = 0.7716
      #mutate(Ridership = (Ridership_Ori**(1/2))) #adfTest p-value = 0.7209 
      #mutate(Ridership = log(log(Ridership_Ori))) #adfTest p-value = 0.7815 
      #mutate(Ridership = (Ridership_Ori**0.2)) #adfTest p-value = 0.7615
      #mutate(Ridership = (Ridership_Ori**10)) #adfTest p-value = 0.04786 
    
      
    #Search for best transformation
    # RiderTest <- Rider %>%
        # mutate(Ridership = log(Ridership_Ori)); RiderTestTS = ts(RiderTest$Ridership, frequency = 12, start = c(2005, 1)); adfTest(RiderTestTS); ts.plot(RiderTestTS, ma(RiderTestTS, order = 12), lty = c(1:1), col = c('black','red'), xlab = "Year", ylab = "Ridership") #Plot moving average with month = 12
    
    
  #Select window --- activate this only after generating overall plots
    Rider_all <- Rider #Create a copy of original Rider
    
    # Rider <- Rider %>%
      # filter(t > 60) # Select only linear increase, start from 2010


  #Convert to Time Series (frequency = 12 months)
    RiderTS_all = ts(Rider_all$Ridership, frequency = 12, start = c(2005, 1)); RiderTS_all
    is.ts(RiderTS_all)
  
    
  #Select window for analysis
    RiderTS = window(RiderTS_all, start = c(2010, 3), end = c(2018, 3)); RiderTS
    RiderTS_base = window(RiderTS_all, start = c(2010, 1)); RiderTS_base
    
  #Check Time Series
    start(RiderTS); end(RiderTS); frequency(RiderTS) #Start year; #End year; #Period

    cycle(RiderTS) #Print 2D table of time period


  #Plot Time Series
    autoplot(RiderTS)
    ts.plot(RiderTS, ma(RiderTS, order = 12), lty = c(1:1), col = c('black','red'), xlab = "Year", ylab = "Ridership") #Plot moving average with month = 12

  #Plot decomposition
    ggseasonplot(RiderTS, ylab= "Ridership") #Plot to see if there is increase year-on-year
    ggmonthplot(RiderTS, ylab = "Ridership") #Plot to see if there is increase year-on-year
    ggsubseriesplot(RiderTS, ylab = "Ridership") #Plot to see if there is increase year-on-year
    boxplot(RiderTS ~ cycle(RiderTS)) #Plot to see if there is increase year-on-year
    plot(decompose(RiderTS)) #Plot decomposition #Default: additive

    
  #Convert to stationarity
    #adf.test(RiderTS, alternative = "stationary", k = 0) 
    adfTest(RiderTS) #p should be < 0.05 for stationarity
    

######################################################################################
#====Forecasting====
  #Set training and test data based on time
    forecast_time = 6 #Split training and test data wrt training data
    RiderTS_test = subset(RiderTS, start = length(RiderTS) - forecast_time + 1); cycle(RiderTS_test)
    RiderTS_trg = subset(RiderTS, end = length(RiderTS) - (forecast_time)); cycle(RiderTS_trg)

    
  #50-50 test
    # RiderTS_test = subset(RiderTS, start = length(RiderTS) - round(length(RiderTS)/2) + 1); cycle(RiderTS_test)
    
    # RiderTS_trg = subset(RiderTS, end = length(RiderTS) - round(length(RiderTS)/2)); cycle(RiderTS_trg)
    
    
  #Window-based test
    #RiderTS_trg = window(RiderTS_all, start = c(2010, 4), end = c(2015, 3)); cycle(RiderTS_trg)
    #RiderTS_test = window(RiderTS_all, start = c(2015, 4), end = c(2017, 3)); cycle(RiderTS_test)
   
    
    
    #To use for actual forecasting
      #RiderTS_trg = RiderTS 
    
    
    
  #----Holt's Winter Exponential Smoothing----
    #RiderTS_HW <- HoltWinters(RiderTS_trg, gamma = TRUE); RiderTS_HW$SSE
    #RiderTS_HW <- hw(RiderTS_trg, lambda = BoxCox.lambda(RiderTS), seasonal = 'additive', initial = "optimal", h = forecast_time)
    RiderTS_HW <- hw(RiderTS_trg, seasonal = 'additive', initial = "optimal", h = forecast_time)
    checkresiduals(RiderTS_HW)
    
    #For testing
    # H1 <- hw(RiderTS_trg, seasonal = 'additive', lambda = BoxCox.lambda(RiderTS), initial = "optimal", h = forecast_time)
    # H2 <- hw(RiderTS_trg, seasonal = 'additive', initial = "optimal", h = forecast_time)
    # H <- tibble(x= "H1", y=accuracy(H1, RiderTS))
    # H <- add_row(H, x= "H2", y=accuracy(H2, RiderTS))
    # H
    
    #For test of 1 year
    # Error in decompose(ts(x[1L:wind], start = start(x), frequency = f), seasonal) : 
    #   time series has no or less than 2 periods
    
    #For training set
    #Warning message:
    # In modeldf.default(object) :
    #   Could not find appropriate degrees of freedom for this model.
    
    #RiderTS_HW_test = forecast(RiderTS_test, model = RiderTS_HW) #Error
    #RiderTS_HW_test <- RiderTS_HW %>%
    #forecast(h = forecast_time)
    
    #RiderTS_HW_test = forecast(RiderTS_test, model = RiderTS_HW, h = forecast_time) #HW automatically forecast
    RiderTS_HW_test = RiderTS_HW
    
    #summary(RiderTS_HW_test)
    RiderTS_testsummary <- tibble(x = "Holt-Winters Exp", y = accuracy(RiderTS_HW, RiderTS))
    #RiderAcc <- accuracy(RiderTS_HW_test) #Record accuracy of model for comparison
    
    
    plot(RiderTS_HW)

    RiderTS_HW %>%
      forecast(h = forecast_time) %>%
      autoplot(xlab = "Year", ylab = "Ridership") + autolayer(RiderTS_base) + 
      theme_classic()
    
    
    
  #----Auto-ARIMA----
    #RiderTS_aa = auto.arima(RiderTS_trg, lambda = BoxCox.lambda(RiderTS), approximation = FALSE, stepwise = FALSE, parallel = TRUE)
    RiderTS_aa = auto.arima(RiderTS_trg, approximation = FALSE, stepwise = FALSE, parallel = TRUE)
    coeftest(RiderTS_aa)
    summary(RiderTS_aa)
    checkresiduals(RiderTS_aa)
    
    RiderTS_aa_test = forecast(RiderTS_aa, h = forecast_time)
    #RiderTS_aa_test = forecast(RiderTS_test, model = RiderTS_aa, h = forecast_time)
    #Error:
    # Error in stats::arima(x = x, order = order, seasonal = seasonal, include.mean = include.mean,  : 
    #                         too few non-missing observations
    
    
    
    summary(RiderTS_aa_test)
    #RiderTS_testsummary <- tibble(x = "Auto-ARIMA", y = accuracy(RiderTS_aa_test))
    RiderTS_testsummary <- add_row(RiderTS_testsummary, x = "Auto-ARIMA", y = accuracy(RiderTS_aa_test, RiderTS))
    #RiderAcc <- accuracy(RiderTSaa_test) #Record accuracy of model for comparison

    RiderTS_aa %>%
      forecast(h = forecast_time) %>%
      autoplot(xlab = "Year", ylab = "Ridership") + autolayer(RiderTS_base) + 
      theme_classic()
    
    

  #----ETS----
    RiderTS_ets = ets(RiderTS_trg, lambda = BoxCox.lambda(RiderTS_trg))
    #RiderTS_ets = ets(RiderTS_trg)
    RiderTS_test
    summary(RiderTS_ets)
    checkresiduals(RiderTS_ets)
    
    #RiderTS_ets_test = forecast(RiderTS_test, model = RiderTS_ets, h = forecast_time)
    RiderTS_ets_test = forecast(RiderTS_ets, h = forecast_time) #forecast RiderTS_ets is the same as RiderTS_test
    summary(RiderTS_ets_test)

    RiderTS_testsummary <- add_row(RiderTS_testsummary, x = "ETS", y = accuracy(RiderTS_ets_test, RiderTS))
    
    #RiderAcc <- rbind(RiderAcc, accuracy(RiderTSets_test)) #Record accuracy of model for comparison
    
    RiderTS_ets %>%
      forecast(h = forecast_time) %>%
      autoplot(xlab = "Year", ylab = "Ridership") + autolayer(RiderTS_base) + 
      theme_classic() 
    

    
  #----ARIMA----
    #Augmented Dickey-Fuller Test p < 0.05 for stationarity (H0, p>0.05): non-stationarity)
    adfTest(RiderTS)
    
    
    #Lag plot
      RiderTS_lag = window(RiderTS, start = 2010) #Select window
      gglagplot(RiderTS_lag, do.lines = FALSE, colour = FALSE, lags = 24) #Plot points instead of lines and colours
    
    #Differencing
      nonseasonal_diff = 1
      seasonal_diff = 1
      
      RiderTS_trg %>% 
        diff(differences = nonseasonal_diff) %>% #non-seasonal differencing
        diff(lag = 12, differences = seasonal_diff) %>% #seasonal differencing
        #acf2()
        ggAcf() #Display ACF, PACF
      
      RiderTS_trg %>% 
        diff(differences = nonseasonal_diff) %>% #non-seasonal differencing
        diff(lag = 12, differences = seasonal_diff) %>% #seasonal differencing
        adfTest() #Display ADF test statistics, p should be < 0.05
    
    #Create ARIMA model
      #For testing 
      Arima(RiderTS_trg, order = c(2, 1, 1), seasonal = c(0, 1, 1), lambda = BoxCox.lambda(RiderTS)) %>%
      #checkresiduals()
        
      #Arima(RiderTS_trg, order = c(1, 2, 1), seasonal = c(1, 1, 1)) %>%
      forecast(h = forecast_time) %>%
      autoplot(xlab = "Year", ylab = "Ridership") + autolayer((RiderTS_base)) + 
      theme_classic()
        
      
      
        
      RiderTS_a = Arima(RiderTS_trg, order = c(2, 1, 0), seasonal = c(0, 1, 1))
      #RiderTS_a = Arima(RiderTS_trg, order = c(1, 1, 0), seasonal = c(0, 1, 1), lambda = BoxCox.lambda(RiderTS))
      #RiderTS_a = Arima(RiderTS_trg, order = c(1, 1, 0), seasonal = c(1, 1, 0))
      #RiderTS_a = Arima(RiderTS_trg, order = c(1, 1, 0), seasonal = c(0, 1, 1))
      #RiderTS_a = Arima(RiderTS_trg, order = c(0, 1, 1), seasonal = c(1, 1, 3))
      #RiderTS_a = Arima(RiderTS_trg, order = c(2, 1, 0), seasonal = c(0, 1, 0))
      #RiderTS_a = Arima(RiderTS_trg, order = c(1, 1, 0), seasonal = c(0, 1, 0))
      coeftest(RiderTS_a)
      summary(RiderTS_a)
      checkresiduals(RiderTS_a)
      
      #RiderTS_a_test = forecast(RiderTS_test, model = RiderTS_a, h = forecast_time)
      RiderTS_a_test = forecast(RiderTS_a, h = forecast_time)
      #RiderTSa_test = Arima(RiderTS_test, model = RiderTSa, h = forecast_time) #Same thing as above
      summary(RiderTS_a_test)
      
      RiderTS_a %>%
        forecast(h = forecast_time) %>%
        autoplot(xlab = "Year", ylab = "Ridership") + 
        autolayer(RiderTS_base) +
        theme_classic() 
      
      RiderTS_testsummary <- add_row(RiderTS_testsummary, x = "ARIMA", y = accuracy(RiderTS_a_test, RiderTS))
      #RiderAcc <- rbind(RiderAcc, accuracy(RiderTSa_test)) #Record accuracy of model for comparison
      
      
      
      
  #---- TBATS model ----
    RiderTS_TBATS <- tbats(RiderTS_trg, lambda = BoxCox.lambda(RiderTS))
    checkresiduals(RiderTS_TBATS)
    
    RiderTS_TBATS_test = forecast(RiderTS_TBATS, h = forecast_time)
    summary(RiderTS_TBATS_test)
    
    RiderTS_testsummary <- add_row(RiderTS_testsummary, x = "TBATS", y = accuracy(RiderTS_TBATS_test, RiderTS))
    
    RiderTS_TBATS_test %>%
      forecast(h = forecast_time) %>%
      autoplot(xlab = "Year", ylab = "Ridership") + autolayer((RiderTS_base)) + 
      theme_classic() 
      
    
    
  #---- Combo model ----
    #   Combi = (RiderTSaa_test[["mean"]] + RiderTSets_test[["mean"]])/2
    
    RiderTS_combo <- hybridModel(RiderTS_trg, models = "aest", weights = "equal", lambda = BoxCox.lambda(RiderTS), a.args = list(approximation = FALSE), parallel = TRUE, num.cores = 4L)
    
    #RiderTS_combo <- hybridModel(RiderTS_trg, models = "aest", weights = "equal", a.args = list(approximation = FALSE), parallel = TRUE, num.cores = 4L)
    #a (auto.arima), e (ets), f (thetam), n (nnetar), s (stlm), t (tbats), and z (snaive)
    #weights = "equal" or "insample.errors" or "cv.errors"
    #errorMethod = "MASE" or "RMSE" or "MAE" or "MASE"
    checkresiduals(RiderTS_combo)
    
  #Plot different models
    par(cex = 0.5) 
    plot(RiderTS_combo, type = "fit", xlab = "Year", ylab = "Ridership") #Original TS graph in black
    par(cex = 1)
    
    plot(RiderTS_combo, type = "fit", xlab = "Year", ylab = "Ridership", ggplot = TRUE) #ggplot without original TS graph
    
    #RiderTS_combo_test = forecast(RiderTS_test, model = RiderTS_combo) #Error
    RiderTS_combo_test = forecast(RiderTS_combo, h = forecast_time)
    summary(RiderTS_combo_test)
    
    RiderTS_testsummary <- add_row(RiderTS_testsummary, x = "Combination", y = accuracy(RiderTS_combo_test, RiderTS))
    

    RiderTS_combo %>%
      forecast(h = forecast_time) %>%
      autoplot(xlab = "Year", ylab = "Ridership") + autolayer((RiderTS_base)) + 
      theme_classic() 
    
    accuracy(RiderTS_combo, individual = TRUE)
    
    
    
#######################################################################################
  #Plot graph
    #autoplot(window(RiderTS_all, start = c(2014, 1), end = c(2015)), ylab = "Ridership") +
    autoplot(window(RiderTS_all, start = c(2017, 1), end = c(2018, 3)), ylab = "Ridership", lwd = 1) +
      #autolayer(RiderTS_HW_test, series = "HW", PI = FALSE) +
      autolayer(RiderTS_ets_test, series = "ETS", PI = FALSE) +
      autolayer(RiderTS_aa_test, series = "Auto-ARIMA", PI = FALSE) +
      #autolayer(RiderTS_a_test, series = "ARIMA", PI = FALSE) +
      #autolayer(RiderTS_TBATS_test, series = "TBATS", PI = FALSE) +
      autolayer(RiderTS_combo_test, series = "Combination (Auto-ARIMA, ETS, STLM, TBATS)", PI = FALSE) + #lwd = 1, 
      #autolayer(RiderTS_combi_test, series = "New Combination", lwd = 1, alpha = 1) +
      theme_classic()
    
    #RiderTS_combi_test = (RiderTS_HW_test[["mean"]] + RiderTS_TBATS_test[["mean"]])/2
    
  #Compare accuracy of models
    #RiderAcc = as.data.frame(RiderAcc)
    #RiderAcc$Model <- c("Auto-ARIMA", "ETS", "ARIMA")
    
    #RiderAcc %>%
    #  select(Model, everything()) #Place t as first column
    
    RiderTS_testsummary
    
    RiderTS_combo_test
    
  
    
  ####Forecasting graph####
    RiderTS_a_test = forecast(Arima(RiderTS, order = c(2, 0, 0), seasonal = c(0, 1, 1), include.drift = TRUE), h = forecast_time)
    
    accuracy(RiderTS_aa_test)
    
    
    autoplot(window(RiderTS, start = c(2010,1)), ylab = "Ridership", lwd = 1) + 
      autolayer(RiderTS_a_test, series = "Auto-ARIMA", PI = TRUE, lwd = 1, alpha = 0.3) 
    #autolayer(RiderTS_combo_test, series = "Combination (Auto-ARIMA, ETS, STLM)", PI = TRUE, lwd = 1, alpha = 0.5) +
      theme_classic()

    #RiderTS_HW_test = hw(RiderTS_trg, seasonal = 'additive', initial = "optimal")
    
    
#######################################################################################
#Cross-validation
f_HW <- function(x, h = forecast_time) {
  #forecast(hw(x, lambda = BoxCox.lambda(RiderTS), seasonal = 'additive'))
  forecast(RiderTS_HW, h = forecast_time)
}


f_aa <- function(x, model, h = forecast_time) {
  forecast(RiderTS_aa, h = forecast_time)
}


f_ets <- function(x, model, h = forecast_time) {
  #forecast(ets(x, model = model, lambda = BoxCox.lambda(RiderTS)), h = h)
  forecast(RiderTS_ets, h = forecast_time)
  }


f_a <- function(x, model, h = forecast_time) {
  forecast(RiderTS_a, h = forecast_time)
}


f_TBATS <- function(x, model, h = forecast_time) {
  forecast(RiderTS_TBATS, h = forecast_time)
}


f_combo <- function(x, model, h = forecast_time) {
  #RiderTS_combo_test
  forecast(RiderTS_combo, h = forecast_time)
}


# Compute cross-validated errors for up to x steps ahead
E_HW <- tsCV(RiderTS, forecastfunction = f_HW, h = forecast_time)
RiderTS_CVtestsummary <- tibble(x = "Holt-Winter", y = sqrt(mean(E_HW^2, na.rm=TRUE)))


E_aa <- tsCV(RiderTS, forecastfunction = f_aa, h = forecast_time)
RiderTS_CVtestsummary <- add_row(RiderTS_CVtestsummary, x = "Auto-ARIMA", y = sqrt(mean(E_aa^2, na.rm=TRUE)))


E_ets <- tsCV(RiderTS, forecastfunction = f_ets, h = forecast_time)
RiderTS_CVtestsummary <- add_row(RiderTS_CVtestsummary, x = "ETS", y = sqrt(mean(E_ets^2, na.rm=TRUE)))


E_a <- tsCV(RiderTS, forecastfunction = f_a, h = forecast_time)
RiderTS_CVtestsummary <- add_row(RiderTS_CVtestsummary, x = "ARIMA", y = sqrt(mean(E_a^2, na.rm=TRUE)))


E_TBATS <- tsCV(RiderTS, forecastfunction = f_TBATS, h = forecast_time)
RiderTS_CVtestsummary <- add_row(RiderTS_CVtestsummary, x = "TBATS", y = sqrt(mean(E_TBATS^2, na.rm=TRUE)))


E_combo <- tsCV(RiderTS, forecastfunction = f_combo, h = forecast_time)
write.csv(E_combo, "E_combo.csv")
RiderTS_CVtestsummary <- add_row(RiderTS_CVtestsummary, x = "Combo", y = sqrt(mean(E_combo^2, na.rm=TRUE)))


RiderTS_CVtestsummary

# # Compute the MSE values and remove missing values
# mse <- colMeans(e^2, na.rm = TRUE)
# 
# # Plot the MSE values against the forecast horizon
# data.frame(h = 1:6, MSE = mse) %>%
#   ggplot(aes(x = h, y = MSE)) + geom_point()
