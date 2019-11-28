#A0052270U Ong Kian Eng

#Set working directory
  setwd("E:/NUS/EBAC/EBA5002 Business Analytics Practice/1 SB Statistics Bootcamp/EBAC BAP StatsBootCamp Day 5/Workshop - day 5")
  
#Install and load packages
  pacman::p_load(tidyverse, purrr, stringr, data.table, modelr, caret, datarium, car, corrplot, caTools, knitr, ROCR, IRdisplay, e1071, earth)
######################################################################################
#Load multiple files
  #Load multiple files at a go
    files = c('marketing.csv', 'salaries.csv')
    multiple_files = map(files, read.csv)
    str(multiple_files)
  

######################################################################################
#mtcars

  #Structure of dataset
    str(mtcars)
    glimpse(mtcars)
    summary(mtcars)
  
    
  #Split dataset and create models for the various (split) datasets
    models <- mtcars %>%
      split(.$cyl) %>%
      map (~lm(mpg ~ wt, data = .))
  
    
  #Return R-squared values of each model
    models %>%
      map(summary) %>%
      map_dbl("r.squared")


#######################################################################################
#Chick Weight
  
  #Structure of dataset
    str(ChickWeight)
    glimpse(ChickWeight)
    summary(ChickWeight)
  
    
  #Split datasets
    datasets <- ChickWeight %>%
      split(.$Diet)
  
    
  #Save individual datasets
    paths <- str_c(names(datasets), 'diet', '.csv')
    
    walk2(datasets, paths, write.csv)
  
    
  #Create nested dataframe
    df = ChickWeight
    split_by_diet <- df %>%
      group_by(Diet) %>%
      nest()
    
  #Create Linear Regression Model
    LinearModel = function(df){
      lm(weight ~ Time + Chick, data = df)
    }
    
  #Create dataframe containing Regression output
    split_by_diet <- split_by_diet %>%
      mutate(model = map(split_by_diet$data, LinearModel))

        
  #Create summary of Regression and sort by R-squared value
    split_by_diet %>%
      mutate(quality = map(model, broom::glance)) %>%
      unnest(quality, .drop = T) %>%
      arrange(desc(adj.r.squared))
    
  #Add residuals to summary
    split_by_diet <- split_by_diet %>%
      mutate(resids = map2(data, model, add_residuals))
    
    split_by_diet

######################################################################################
#Task1
  #Input data
    set.seed(10)
    
    df = data.frame(Emp.id = 1:9, 
                    dep = sample(rep(c('Front-desk', 'Back-office', 'Reception'), each = 3)),
                    Bonus.Cur = sample(2000:5000, replace = TRUE, size = 9),
                    Bonus.Prev = sample(2000:5000, replace = TRUE, size = 9),
                    Increment.Cur = round(runif(9, 0.0, 0.05), digits = 2),
                    Increment.Prev = round(runif(9, 0.0, 0.05), digits = 2) #runif: random variables from uniform distribution
                    )
    

  #Calculate average bonus and increment received by each employee
    df$avg.bonus <- (df$Bonus.Cur + df$Bonus.Prev)/2
    df$avg.incr <- (df$Increment.Cur + df$Increment.Prev)/2
    
    
  #Plot
      df %>%
        ggplot(aes(x = avg.incr, y = avg.bonus, fill=factor(Emp.id))) + 
        geom_point() + 
        facet_wrap(~ dep) +
        geom_text(aes(label = Emp.id, vjust = -0.5, nudge_y = 0))
        #geom_label(aes(label = Emp.id, nudge_y = 0.1))
        
      
########################################################################################
#Task 2
  #Filter data based on time, weight, chick, diet
    ChickWeight %>%
      ggplot(aes(x = Time, y = weight)) + 
      geom_line(aes(colour = factor(Chick))) + 
      facet_grid(~ Diet) +
      geom_smooth(method = 'loess', stat = "smooth", se = FALSE, colour='red') + 
      labs(x="Time", y="Weight")
      #geom_text(aes(label = Chick, vjust = -0.5))
  
  #Save file
    ggsave("Chick.png", dpi="retina", scale = 2, width = 10)
    
    
########################################################################################
#Task 3
  #Load multiple files at a go
    #filename = c(rep(2010:2019))
    #filename <- list.files(pattern = "20[10-19].xlsx")
    Filename = list.files(paste(getwd(),'/tourism', sep="")) #List all files stored in tourism folder
    Files = map(str_c('./tourism/', Filename), readxl::read_xlsx) #Files stored in tourism folder
    
    str(Files)
    
    #Combine data into 1 dataframe
      Tour <- bind_rows(Files)
    
    #Data Exploration
      glimpse(Tour)
      summary(Tour)
      
    #Data Preparation
      Tour$Total = rowSums(Tour[,3:14], na.rm = TRUE)

      TourN <- Tour %>%
        filter(Year != 2019)
      
    #Create nested dataframes for regression
      TourY <- TourN %>%
        group_by(Continent) %>%
        nest()
      
    #Step-wise linear regression
      LM = function(df){
        step(lm(Total ~ Year, data = df))
      }
      
      TourLM <- TourY %>%
        mutate(model = map(TourY$data, LM))
      
      TourLM %>%
        mutate(quality = map(model, broom::glance)) %>%
        unnest(quality, .drop = T) %>%
        arrange(adj.r.squared)
      
      TourLM <- TourLM %>%
        mutate(resids = map2(data, model, add_residuals))
      
      TourLM
      
      
    #Create plots for LM
      TourN %>%
        ggplot(aes(x = Year, y = Total, colour = Continent)) +
        geom_line() + 
        #stat_smooth(method = 'loess', colour = 'red', alpha = 0.1, lwd = 0.5, linetype="dashed") +
        geom_smooth(method = 'lm', colour = 'grey', alpha = 0.1, lwd = 0.5, se = TRUE, linetype="dashed") +
        labs(title="Number of Passengers to Singapore") +
        facet_wrap(~Continent, scales = 'free', labeller = label_context)
      
      
      
    #Step-wise quadratic regression
      QM = function(df){
        step(lm(Total ~ Year + I(Year^2), data = df))
      }
      
      TourQM <- TourY %>%
        mutate(model = map(TourY$data, QM))
      
      TourQM %>%
        mutate(quality = map(model, broom::glance)) %>%
        unnest(quality, .drop = T) %>%
        arrange(adj.r.squared)
      
      TourQM <- TourQM %>%
        mutate(resids = map2(data, model, add_residuals))
    
      TourQM
      
      
    #Create plots for QM
      TourN %>%
        ggplot(aes(x = Year, y = Total, colour = Continent)) +
        geom_line() + 
        #stat_smooth(method = 'loess', colour = 'red', alpha = 0.1, lwd = 0.5, linetype="dashed") +
        geom_smooth(method = 'lm', colour = 'grey', alpha = 0.1, lwd = 0.5, se = TRUE, linetype="dashed", formula = y ~ x + I(x^2)) +
        labs(title="Number of Passengers to Singapore") +
        facet_wrap(~Continent, scales = 'free', labeller = label_context)
      
      
    #Reshape dataframe to long form
      TourL <- Tour %>%
        gather('TMonth', 'Count', -Continent, -Year, convert = TRUE) %>%
        mutate(MthYr = str_c(TMonth, "/", Year)) %>%
        filter(TMonth != 'Total')
      
      TourL$MthYr <- lubridate::parse_date_time(TourL$MthYr, "%m %Y")
      
      
    #Create plots for QM
      TourL %>%
        filter(Count != '0') %>%
        
        ggplot(aes(x = MthYr, y = Count, colour = Continent)) +
        geom_line() + 
        #stat_smooth(method = 'loess', colour = 'red', alpha = 0.1, lwd = 0.5, linetype="dashed") +
        geom_smooth(method = 'lm', colour = 'grey', alpha = 0.1, lwd = 0.5, se = TRUE, linetype="dashed", formula = y ~ x + I(x^2)) +
        labs(title="Number of Passengers to Singapore") +
        facet_wrap(~Continent, scales = 'free', labeller = label_context)
      
      
      # TourL$Q <- quarter(TourL$MthYr)
      # 
      # #Create plots for QM
      # TourL %>%
      #   filter(Count != '0') %>%
      #   
      #   ggplot(aes(x = Q, y = Count, colour = Continent)) +
      #   geom_line() + 
      #   #stat_smooth(method = 'loess', colour = 'red', alpha = 0.1, lwd = 0.5, linetype="dashed") +
      #   geom_smooth(method = 'lm', colour = 'grey', alpha = 0.1, lwd = 0.5, se = TRUE, linetype="dashed", formula = y ~ x + I(x^2)) +
      #   labs(title="Number of Passengers to Singapore") +
      #   facet_wrap(~Continent, scales = 'free', labeller = label_context)
      # 
      
      
      
########################################################################################
#Task 4
      
  #Load file
    Delay <- read.csv('Delay.csv', check.names = T)
  
  #Descriptive statistics
    glimpse(Delay)
  
  #Convert data type
    colfac = c('Weather', 'DAY_WEEK', 'DAY_OF_MONTH')
    Delay[, colfac] <- lapply(Delay[ ,colfac], as.factor)
    
    table(Delay$Flight.Status)
    contrasts(Delay$Flight.Status)
    Delay$Flight.Status.N = recode_factor(Delay$Flight.Status, 'delayed' = 1, 'ontime' = 0)
    contrasts(Delay$Flight.Status)
    table(Delay$Flight.Status)
  
  #Change date and time format
    Delay$CRS_DEP_TIME <- str_c(str_sub(Delay$CRS_DEP_TIME, start=1, nchar(Delay$CRS_DEP_TIME)-2), ':', str_sub(Delay$CRS_DEP_TIME, start = -2), ':00')
    
    Delay$DEP_TIME <- str_c(str_sub(Delay$DEP_TIME, start=1, nchar(Delay$DEP_TIME)-2), ':', str_sub(Delay$DEP_TIME, start = -2), ':00')
    
    Delay$DEP_TIME[1271] = "00:10:00"
    
    library(lubridate)
    Delay$FL_DATE = lubridate::dmy(Delay$FL_DATE)
    Delay$CRS_DEP_TIME = lubridate::hms(Delay$CRS_DEP_TIME)
    Delay$DEP_TIME = lubridate::hms(Delay$DEP_TIME)
    
    #Compute duration of delay
    #Delay$TimeDelay = lubridate::interval((Delay$CRS_DEP_TIME),(Delay$DEP_TIME))
      Delay$Delay_Min = as.numeric(Delay$CRS_DEP_TIME - Delay$DEP_TIME, "min")
    
    #Compute hour of delay
      Delay$Delay_Time_Hour <- lubridate::hour(Delay$CRS_DEP_TIME)
    
  #Explore data
    colMeans(is.na(Delay))!=0
    
    Delay %>%
      filter(!is.na(Delay_Min)) %>%
      summary()
    
  #Flight carrier with most number of delays
    Delay %>%
      filter(!is.na(Delay_Min)) %>%
      filter(Flight.Status=="delayed") %>%
      group_by(CARRIER) %>%
      summarise(Num_Delay = n()) %>%
      arrange(desc(Num_Delay))
    
    #Finding: 
      # CARRIER Num_Delay
      # <fct>   <int>
      # 1 DH        137 - Top carrier with most number of delays
      # 2 RU         94
      # 3 MQ         80
      # 4 DL         47
      # 5 US         35
      # 6 CO         26
      # 7 UA          5
      # 8 OH          4

    
  #Flight carrier with longest median delays
    Delay %>%
      filter(!is.na(Delay_Min)) %>%
      filter(Flight.Status=="delayed") %>%
      filter(Delay_Min > 0) %>%
      group_by(CARRIER) %>%
      summarise(Avg_Delay_Min = median(Delay_Min)) %>%
      arrange(desc(Avg_Delay_Min))
    
    #Finding:
    # CARRIER Avg_Delay_Min
    # <fct>           <dbl>
    # 1 DH                5  - Carrier with longest median delay (Median used because of outliers)
    # 2 MQ                4.5
    # 3 CO                4  
    # 4 RU                4  
    # 5 US                3  
    # 6 DL                1 
    
    
  #Create model using stepwise function
    colnames(Delay)
    
    DelayFit <- step(glm(Flight.Status.N ~ CARRIER + DISTANCE + DEST + ORIGIN + Weather + DAY_WEEK:Delay_Time_Hour, data = Delay, family = binomial))
    #Hypothesis: Origin and Destination were used because they may be the ones causing the delays at the origin and destination airports. Distance is used because longer haul flights may have delays. Certain Day of week and Time of Flight may result in delays. Weather may have an impact on delays.
    
    summary(DelayFit)
    
    attach(DelayFit)
      pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)
    detach(DelayFit)
    #p-value: 2.178774e-43
    
  #Predict whether flight is likely to be delayed
    #Flight delay is largely due to Carrier (US, DL, CA, OH), Origin (DCA), Day of week (All days except Day 6).
      
      # Coefficients:
      #                             Estimate Std. Error z value Pr(>|z|)    
      # (Intercept)                 1.35015    0.41836   3.227 0.001250 ** 
      # CARRIERDH                   0.50736    0.36154   1.403 0.160522    
      # CARRIERDL                   0.93003    0.29124   3.193 0.001406 ** 
      # CARRIERMQ                  -0.03612    0.27998  -0.129 0.897364    
      # CARRIEROH                   1.54558    0.66041   2.340 0.019268 *  
      # CARRIERRU                   0.45399    0.31427   1.445 0.148573    
      # CARRIERUA                   1.00878    0.71211   1.417 0.156601    
      # CARRIERUS                   1.33957    0.30226   4.432 9.34e-06 ***
      # ORIGINDCA                   0.79063    0.29361   2.693 0.007087 ** 
      # ORIGINIAD                   0.35521    0.28947   1.227 0.219778    
      # Weather1                  -17.88564  410.22941  -0.044 0.965224    
      # DAY_WEEK1:Delay_Time_Hour  -0.09497    0.01651  -5.753 8.77e-09 ***
      # DAY_WEEK2:Delay_Time_Hour  -0.07359    0.01734  -4.245 2.19e-05 ***
      # DAY_WEEK3:Delay_Time_Hour  -0.07592    0.01660  -4.573 4.80e-06 ***
      # DAY_WEEK4:Delay_Time_Hour  -0.05559    0.01666  -3.336 0.000849 ***
      # DAY_WEEK5:Delay_Time_Hour  -0.07338    0.01623  -4.522 6.14e-06 ***
      # DAY_WEEK6:Delay_Time_Hour  -0.01471    0.02060  -0.714 0.475139    
      # DAY_WEEK7:Delay_Time_Hour  -0.10338    0.01621  -6.379 1.78e-10 ***
      # ---
      #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
      # 
      # (Dispersion parameter for binomial family taken to be 1)
      # 
      # Null deviance: 2168.5  on 2200  degrees of freedom
      # Residual deviance: 1918.6  on 2183  degrees of freedom
      # AIC: 1954.6
      # 
      # Number of Fisher Scoring iterations: 15
      
      
      #Original threshold
        Delay %>% 
          group_by(Flight.Status) %>%
          summarise(percentage = n()/nrow(Delay))
        #Threshold set at 0.194
      
      #Set training and testing
        #Set initial seed
          set.seed(100)
      
        #Duplicate dataframe
          data = Delay
          model = DelayFit
        
        #Create a boolean flag to split data
          splitData = sample.split(data$Flight.Status, SplitRatio = 0.7)
      
      #Split data
        #Create train and test datasets
          train_set = data[splitData,]
        
          nrow(train_set)/nrow(data)
          
          test_set = data[!splitData,]
          nrow(test_set)/nrow(data)
          
          colnames(train_set)
      
      #Test it on the train set
        trainPredict = predict(model, newdata = train_set, type = 'response')
      
      #Assign 0s or 1s for the values
        p_class = ifelse(trainPredict > 0.194, 1,0)
      
        matrix_table = table(train_set$Flight.Status, p_class)
        matrix_table
      
      #Accuracy 
        accuracy = sum(diag(matrix_table))/sum(matrix_table)
        round(accuracy, 3)
      #Accuracy: 0.821
      
      #Test it on the test set
        testPredict = predict(model, newdata = test_set, type = 'response')
        
        p_class = ifelse(testPredict > 0.194, 1,0)
        
        matrix_table = table(test_set$Flight.Status, p_class)
        matrix_table
      
      #Accuracy 
        accuracy = sum(diag(matrix_table))/sum(matrix_table)
        round(accuracy, 3)
        #Accuracy: 0.818
      
      #Sort probability
        head(sort(testPredict, decreasing = T),10)
      
      #Create lift chart
        pred = prediction(trainPredict, train_set$Flight.Status)
        perf = performance(pred, "lift", "rpp")
        plot(perf, main="lift curve", xlab = 'Proportion (Sorted probability)')
        
      #p-value for model
        with(model, pchisq(null.deviance - deviance,
                           df.null - df.residual, lower.tail=F))
        
        confusionMatrix(table(p_class, test_set$Flight.Status), positive='1')
        
        options(repr.plot.width=8, repr.plot.height=6)
      
      #ROC: Cut-offs determined visually
        pred <- prediction(trainPredict, train_set$Flight.Status)
        
        perf <- performance(pred, "tpr", "fpr")
        
        plot( perf, colorize = TRUE,
              print.cutoffs.at = seq(0,1,0.1), 
              text.adj = c(-0.2, 1.7))
        
        cost.perf = performance(pred, "cost")
        pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
        
        cost.perf = performance(pred, "cost", cost.fp = 4, cost.fn = 1)
        pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
      
      
    
#########################################################################################
#Task 5
  
  #Load file
    pima <- read.csv('pima-indians-diabetes.csv', col.names = c("Pregnant", "Plasma_Glucose", "Dias_BP", "Triceps_Skin", "Serum_Insulin", "BMI", "DPF", "Age", "Diabetes"))

    head(pima)
    
    glimpse(pima)
  
  #Data exploration: Convert data type
    pima$Diabetes = factor(pima$Diabetes)
    #                  levels = c(0, 1),
    #                  labels = c('Normal', 'Diabetes'))

    plot(pima)
    
  #Data exploration: Check NA cases
    colMeans(is.na(pima))
  
  #Data exploration: Summary
    pimaN <- pima %>%
      filter(complete.cases(.)) %>%
      filter(Plasma_Glucose != 0) %>% #Cannot have 0 Plasma Glucose conc
      filter(Dias_BP != 0) %>% #Cannot have 0 BP
      filter(Triceps_Skin != 0) %>% #Cannot have 0 Tricep
      filter(Serum_Insulin != 0) %>% #Cannot have 0 serum insulin
      filter(BMI != 0) #Cannot have 0 BMI
      
    summary(pimaN)
  
    
  #Create logistic regression model
    #Check level of confidence based on Diabetes
    pimaFit <- step(glm(Diabetes ~ Pregnant + Plasma_Glucose + Dias_BP + Triceps_Skin +  Serum_Insulin + BMI + DPF + Age,
                   family = binomial,
                   data = pimaN))
    summary(pimaFit)
    
    attach(pimaFit)
      pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)
    detach(pimaFit)
    #Finding: p-value = 2.763931e-31 < 0.05, the model can be used.
      
    #Finding: Regression equation: P(Diabetes) = 1 / (1 + e^-(0.083953*Pregnant (Number of times pregnant) + 0.036458*Plasma_Glucose (Plasma Glucose concentration to a 2 hours in an oral glucose tolerance test) + 0.078139*BMI (Body Mass Index) + 1.150913*DPF (Diabetes pedigree function) + 0.034360*Age (years) - 9.992080)
      #Note: Serum_Insulin should have an effect on Diabetes in real life, though the model does not suggest that it is significant.

      
      #Original threshold
        pimaN %>% 
          group_by(Diabetes) %>%
          summarise(percentage = n()/nrow(pimaN)*100)
        #Threshold set at 0.332
      
      #Set training and testing
        #Set initial seed
          set.seed(100)
        
        #Duplicate dataframe
          data = pimaN
          model = pimaFit
          
        #Create a boolean flag to split data
          splitData = sample.split(data$Diabetes, SplitRatio = 0.7)
        
        #Split data
          #Create train and test datasets
          train_set = data[splitData,]
          
          nrow(train_set)/nrow(data)
        
          test_set = data[!splitData,]
          nrow(test_set)/nrow(data)
        
          colnames(train_set)
        
      #Test it on the train set
        trainPredict = predict(model, newdata = train_set, type = 'response')
      
        #Assign 0s or 1s for the values
          p_class = ifelse(trainPredict > 0.332, 1,0)
          
          matrix_table = table(train_set$Diabetes, p_class)
          matrix_table
        
        #Accuracy 
          accuracy = sum(diag(matrix_table))/sum(matrix_table)
          round(accuracy, 3)
          #Accuracy: 0.788
      
      #Test it on the test set
        testPredict = predict(model, newdata = test_set, type = 'response')
      
        p_class = ifelse(testPredict > 0.332, 1,0)
      
        matrix_table = table(test_set$Diabetes, p_class)
        matrix_table
      
      #Accuracy 
        accuracy = sum(diag(matrix_table))/sum(matrix_table)
        round(accuracy, 3)
        #Accuracy: 0.799
      
      #Sort probability
        head(sort(testPredict, decreasing = T),10)
      
      #Create lift chart
        pred = prediction(trainPredict, train_set$Diabetes)
        perf = performance(pred, "lift", "rpp")
        plot(perf, main="lift curve", xlab = 'Proportion (Sorted probability)')
      
      #p-value for model
        with(model, pchisq(null.deviance - deviance,
                           df.null - df.residual, lower.tail=F))
        
        confusionMatrix(table(p_class, test_set$Diabetes), positive='1')
      
        options(repr.plot.width=8, repr.plot.height=6)
      
      #ROC: Cut-offs determined visually
        pred <- prediction(trainPredict, train_set$Diabetes)
        
        perf <- performance(pred, "tpr", "fpr")
        
        plot( perf, colorize = TRUE,
              print.cutoffs.at = seq(0,1,0.1), 
              text.adj = c(-0.2, 1.7))
        
        cost.perf = performance(pred, "cost")
        pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
        
        cost.perf = performance(pred, "cost", cost.fp = 4, cost.fn = 1)
        pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
      

#########################################################################################
#Task 6
  #Load file
    rest <- read.csv('restaurant/studenmunds_restaurants.csv', check.names = F)
    
    head(rest)
  
  #Data Exploration
    glimpse(rest)
    summary(rest)
  
  
  #Create histogram of Y for normal distribution (p > 0.05), else transform Y 
    PerformanceAnalytics::chart.Correlation(rest)
    shapiro.test(rest$sales)
    
  #Fit linear regression model
    #t or z-test to check if beta = 0 (i.e. t-value for each variable)
    #Goodness of fit of model (Check adjusted R^2 that is close to 1; F-statistics and p-value < 0.05)
    attach(rest)
      restFit <- lm(sales ~ competition + population + income)
      summary(restFit)
    detach(rest)    
  
  #Relative importance of variables
    plot(relaimpo::calc.relimp(restFit, rela = TRUE, sort = TRUE))
      
  #Residuals normality check (qqplot or Durbin Watson Test) (Ensure LINE with shapiro of p>0.05)
    par(mfrow=c(2,2))
    plot(restFit)
    
    fitted(restFit)
    residuals(restFit)
    shapiro.test(residuals(restFit))
      
    #Finding: LINE assumption is not met.
  
  #Finding: Regression equation: Sales (Gross Sales volume at each chain) = -9.075e+03*competition (Number of direct competitors in 2 mile radius) + 3.547e-01*population (Number of people in 3 mile radius) + 1.288e+00*income (Average household income) + 1.022e+05
  #Number of people in 3 mile radius and Competition are the best predictors of Sales.
    
  #Load file
    test <- read.csv('restaurant/test_data.csv', check.names = F)
  
  #Predict Sales based on Linear Regression
    predictTest <- predict(restFit, newdata = test)
    test <- cbind(test, predict.lm(restFit, test, interval = "predict"))
  
    #   fit       lwr      upr
    # 1 113878.91 82186.75 145571.1
    # 2  87383.76 54945.04 119822.5
    # 3  82684.25 49127.73 116240.8
    # 4 121567.37 90989.42 152145.3
    # 5 114717.57 60995.01 168440.1
    # 6  93383.88 61668.40 125099.4
    # 7  72317.21 34257.98 110376.4
    # 8 122469.81 91152.92 153786.7
    # 9  77768.15 39654.43 115881.9

    #Test data
      # df = rest
      # SSE = sum((test_set$sales - predictTest)^2)
      # SST = sum((test_set$sales - mean(df$sales))^2)
      # R_Sq= 1 - (SSE / SST)
      # round(R_Sq,4)
      

#########################################################################################
#Task 7
    
  #Load file
    uni <- read.csv('uni.csv')
  
  #Descriptive statistics
    summary(uni)
    glimpse(uni)
    
    contrasts(uni$rank)
    
  #Recode
    #uni$rank1 = factor(uni$rank, levels = c(2, 3, 1), ordered = TRUE)
    
  #Question 1: Is there significant difference in the salaries of males and females?
    #H0: μ(Male) = μ(Female): There is no significant difference in the salaries of males and females
    #H1: μ(Male) =/= μ(Female): There is no significant difference in the salaries of males and females
    
    #Create regression model
      summary(lm(salary ~ sex, data = uni))
      #Finding: There is no statistically significant difference between salary and sex (Adjusted R-squared = modulus(-0.03004) < 0.7; Model p-value = 0.8476 > 0.05; sexMale = 0.848 > 0.05). Furthermore, the sample size for females is only 3 out of 34, which is too small to make any reliable conclusion.
      
    
  #Question 2: Is there significant difference between the salaries of Assistant/Associate/Profs?
    
    #Create regression model
      summary(lm(salary ~ rank, data = uni))
      #Finding: There is statistically significant difference between salary and different types of Professorship (Adjusted R-squared = 0.4678 < 0.7; Model p-value = 2.152e-05 < 0.05; rankProf = 0.0389 < 0.05). However, the sample size for Associate Prof (3) and Assistant Prof (8) is too small to make any reliable conclusion.
     
      
  #Question 3: What are the significant predictors of a faculty salary as per given data?
      
    #Create regression model
      summary(lm(salary ~ rank + discipline + yrs.since.phd + yrs.service + sex, data = uni))
      #Finding: There is statistically significant difference between salary and various variables (Adjusted R-squared = 0.5989 < 0.7; Model p-value = 1.59e-05 < 0.05). However, the best predictors are yrs.since.phd (Years since PhD), Discipline B, rankAsstProf (Rank of Assistant Prof).
      
    #New model: Drop variable sex lowest t-value = -0.937 and high p-value = 0.357083 > 0.05
      summary(lm(salary ~ rank + discipline + yrs.since.phd + yrs.service, data = uni))
    
    #New model: Drop variable yrs.service lowest t-value = -1.163 and high p-value = 0.25448 > 0.05
      summary(lm(salary ~ rank + discipline + yrs.since.phd, data = uni))
    
    #Best model:
      uniFit <- lm(salary ~ rank + discipline + yrs.since.phd, data = uni)
      summary(uniFit)

      
    # Coefficients:
    #                 Estimate  Std. Error  t value   Pr(>|t|)    
    # (Intercept)     73277.7   12448.1     5.887     2.17e-06 ***
    #   rankAsstProf  -23205.8  11906.6     -1.949    0.06103 .  
    # rankProf        16766.9   12218.0     1.372     0.18049    
    # disciplineB     26002.2   7605.2      3.419     0.00188 ** 
    #   yrs.since.phd 638.5     371.1       1.720     0.09601 .  
    # ---
    #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    # 
    # Residual standard error: 17200 on 29 degrees of freedom
    # Multiple R-squared:  0.6448,	Adjusted R-squared:  0.5958 
    # F-statistic: 13.16 on 4 and 29 DF,  p-value: 3.14e-06
      
    #Relative importance of variables
      plot(relaimpo::calc.relimp(uniFit, rela = TRUE, sort = TRUE))
      
      