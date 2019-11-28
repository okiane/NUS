#EBA5001 DABP Day 3 Project Assignment
#Ang Khee Hwa Randi (A0198526N)
#Chua Fu Lin Eugene (A0199046U)
#Koh Zhi Rong, Chester (A0125379X)
#Ong Kian Eng (A0052270U)


#Load Relevant Packages and set working directory for both questions
pacman::p_load(readxl, tidyverse, tidyverse, reshape, lubridate, ggplot2, plyr, lattice, dplyr, scales, stringr, RCurl, zoo)
setwd("E:/NUS/EBAC/EBA5001 Management of Business Analytics Project/1 Data Analytics Process and Best Practices/Assignment/Assignment 2")

##Question 1: Test results of programming class##
##Load Relevant Dataset and explore dataset
  response <- read_excel("response.xlsx",col_names=TRUE)
  response <- as.data.frame(response)
  
  key<-read_excel("key.xlsx",col_names=TRUE)
  key<-as.data.frame(key)

##Data Clean-up: Check for missing values
  sapply(response,function(x)sum(is.na(x),na.rm=TRUE))
  #Findings: No missing values
  
  
#Rename columns for easier reference.
  colnames(key)<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")
  
  colnames(response)<-c("ID","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")

#Obtain unique responses for each question
  unique_responses <- vector("list",10)
  for(i in 2:ncol(response)){
    unique_responses[[i-1]]<-unique(response[,i])
  }

  str(key)
  str(response)

  
##Generate results of responses
  results <- data.frame(matrix(nrow=nrow(response)))
  results$ID <- response[,1]
  results[,1] <- NULL
  results$Q1 <- ifelse(response$Q1=="TRUE",1,0)
  results$Q2 <- ifelse(response$Q2=="Numeric",1,0)
  results$Q3 <- ifelse(response$Q3=="TRUE",1,0)
  results$Q4 <- ifelse(response$Q4=="FALSE",1,0)
  results$Q5 <- ifelse(response$Q5=="NA",1,0)
  results$Q6 <- ifelse(response$Q6=="Error",1,0)
  results$Q7 <- ifelse(response$Q7=="Error",1,0)
  results$Q8 <- ifelse(response$Q8=="Error",1,0)
  results$Q9 <- ifelse(response$Q9=="summary(df)",1,0)
  results$Q10 <- ifelse(response$Q10=="data.frame",1,0)
  results$Score <- rowSums(results[,2:ncol(results)])

#Display results of students 
  results %>%
    select(ID, Score)

#Number of correct answer for each question
  results_long <- results %>% gather("Question","Value", 2:(ncol(results)-1))

  
##Summary of class score (Proportion of correct responses) for each question
  resultKPI <- setNames(aggregate(results_long$Value, list(Question=results_long$Question), FUN = sum), c('Question', 'Correct_No')) %>% 
    mutate(Correct_Percent=percent(round(Correct_No/75,2))) %>% 
    arrange(Correct_No)


##Summary of class performance
  performance <- results %>% 
    group_by(Score) %>% 
    tally() %>% 
    mutate(Percent=percent(round(n/sum(n),2)))

#Note: Used to plot bar_plot, do not remove without changing bar_plot reference
  resultSummary <- results_long[,3:4] %>% 
    group_by(Question, Value) %>% 
    tally()
  
  resultSummary <- resultSummary %>% 
    subset(Value==1) %>% 
    mutate(Correct_prop=round(n/75,2)) %>% 
    arrange((n))
  
  summary(results)

  
##Visualisations of Questions
#Heat-map of students' score
  ggplot(results_long ,aes(x=as_factor(Question),y=as_factor(ID),fill=revalue(as_factor(Value),c('1'='Correct','0'='Wrong')))) + 
    geom_tile(color="white",size=0.5) + 
    #scale_fill_brewer(palette="Set1",direction=1) + 
    scale_fill_manual(values = c("red", "springgreen4"))+
    labs(title="Heatmap of student's correct/ wrong response across questions", fill='Legend', x='Question', y='Student ID')


##Bar chart to see class's score distribution
  ggplot(performance,aes(x=performance$Score, y=performance$n)) + 
    geom_bar(stat='identity') + 
    geom_text(aes(label=paste(n, " (", Percent, ")", sep=""),vjust=-0.15)) + 
    labs(title="Student Score Distribution", x='Score', y='Count')


##Difficulty index: Bar chart to see proportion and pinpoint which question has highest wrong
  ggplot(resultSummary,aes(x=as_factor(Question), y=as.numeric(Correct_prop)*100)) + 
    geom_bar(stat="identity") + 
    geom_text(aes(label=paste(n, " (", Correct_prop*100,"%)", sep=""),vjust=-0.15)) + 
    labs(title="Difficulty Analysis: Proportion of correct reponses across questions", x="Question", y='Proportion of Correct Answers/ %')
  

##Discrimination index: Bar chart to see performance of students between overall high-scoring and low-scoring candidates for that question
#Note: Only questions with many students giving the wrong responses are evaluated. Same logic will apply for other questions too.
  #Question 1
    q1_wrong <- results_long[(results_long$Question=="Q1") & (results_long$Value==0),]
    
    ggplot(q1_wrong, aes(x=Score)) + 
      geom_bar() + 
      geom_text(stat='count',aes(label=..count..),vjust=-0.15) + 
      labs(title="Discrimination analysis: Score Distribution of Students Who Got Question 1 Wrong", x='Score', y='Count')
   
  
  #Question 6
    q6_wrong <- results_long[(results_long$Question=="Q6") & (results_long$Value==0),]
    ggplot(q6_wrong, aes(x=as.factor(Score))) + 
      geom_bar() + 
      geom_text(stat='count',aes(label=..count..),vjust=-0.15) + 
      labs(title="Discrimination analysis: Score Distribution of Students Who Got Question 6 Wrong", x='Score', y='Count')
    

##Distractor Analysis: Bar chart to see various responses from students 
#Note: Correct answer is not included
  q6_responses <- response$Q6 %>% 
    count() %>% 
    filter(x %in% c('Even', 'Odd'))
  
  ggplot(q6_responses, aes(x=reorder(as.factor(x), -freq), y=freq)) + 
    geom_bar(stat='identity') + 
    geom_text(aes(label=freq),  vjust=-0.15) + 
    labs(title="Distractor Analysis: Frequency Distribution of Students Who Got Question 6 Wrong", x='Wrong Responses', y='Count')


##############################################################################################
#Question 2
##Load Relevant Dataset and explore dataset
  bike <- getURL("https://raw.githubusercontent.com/idc9/stor390/master/data/bikes_2011.csv")
  
  bike <- read.csv(text=bike, header=TRUE)

##Data Clean-up: Check for missing values
  sapply(bike, function(x) sum(is.na(x), na.rm=TRUE))
  #Findings: No missing values

################################Formulating business question#################################
###Business Question 1: What is the general trend of bicycle ridership over the span of a year?
  ##Line graph to visualize overall ridership by months
    monthlyBike <- bike %>%
      select(mnth, cnt) %>%
      dplyr::group_by(mnth) %>%
      dplyr::summarise(Count = sum(cnt,na.rm=TRUE))
    
    monthlyBike$yearmth <- seq(as.Date("2011-01-01"), as.Date("2011-12-31"), by="months")
    
    bikeTS <- zoo(monthlyBike$Count, monthlyBike$yearmth)
    
    
    autoplot(bikeTS) + 
      geom_rect(aes(xmin = as.Date('2011-01-01'), ymin = -Inf, xmax = as.Date('2011-03-31'), ymax = Inf), fill = "steelblue", alpha = 0.01) +
      geom_rect(aes(xmin = as.Date('2011-07-01'), ymin = -Inf, xmax = as.Date('2011-09-30'), ymax = Inf), fill = "yellow", alpha = 0.01) +
      
      labs(title="Ridership by Months", x="Date", y="Number of riders") + 
      scale_x_date(date_breaks="3 months") +
      theme(plot.title=element_text(hjust=0.5))


###Business Question 2: What are the key variables that affect bicycle ridership?
  ##Correlation heat map to see correlation between variables
  #http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
    bikecordata <- bike[,c(3, 5, 6, 8, 10:17)]
    
    bikecordata <- round(cor(bikecordata), 2)
    
    bikecordata <- cbind(expand.grid(dimnames(bikecordata)), value = as.vector(bikecordata))
    
    ggplot(bikecordata,aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
      scale_fill_gradient2(low="red", mid="white", high=muted("green"), name="Pearson\nCorrelation") +
      theme_minimal() + 
      theme(axis.text.x=element_text(angle=90)) + 
      geom_text(aes(label=bikecordata$value),size=3) + 
      labs(title="Correlation Heatmap between Variables")
    
  
###Business Question 3: How does the bicycle ridership change within a 24-hour period for different days of the week - between working weekdays and non-working days (weekends and holidays)? Also, how does the usage patterns differ between registered and casual users? 
  ##Summary table of mean ridership for non-holiday weekdays and weekends
    #Average ridership for non-holiday working weekdays (5 days)
      bike %>%
        filter(holiday=="0" & workingday=="1") %>%
        dplyr::group_by(workingday) %>%
        dplyr::summarise(Users = sum(cnt)/5, casUser = sum(casual)/5, regUser = sum(registered)/5)
      
    #Average ridership for non-holiday weekends (2 days)
      bike %>%
        filter(holiday=="0" & workingday=="0") %>%
        dplyr::group_by(workingday) %>%
        dplyr::summarise(Users = sum(cnt)/2, casUser = sum(casual)/2, regUser = sum(registered)/2)
  
  
  ##Summary table to find out most popular day
    #Total ridership
    bike %>%
      dplyr::group_by(weekday) %>%
      dplyr::summarise(count = sum(cnt)) %>%
      dplyr::arrange(desc(count))
    
    #Registered ridership
    bike %>%
      dplyr::group_by(weekday) %>%
      dplyr::summarise(count = sum(registered)) %>%
      dplyr::arrange(desc(count))
   
    #Casual ridership
    bike %>%
      dplyr::group_by(weekday) %>%
      dplyr::summarise(count = sum(casual)) %>%
      dplyr::arrange(desc(count)) 
  
  
  ##Clock Plot to find out ridership over course of day for different categories of days (working days, non-holiday weekends, holidays)
  #http://www.r-graph-gallery.com/49-clock-plot/
    #Filter to obtain data for Clock Plot
      #Non-holiday weekdays
      workDay <- bike %>% 
        dplyr::filter(holiday==0 & workingday==1) %>% 
        dplyr::group_by(hr) %>% 
        dplyr::summarise(avg_rider=mean(cnt))
      
      #Non-holiday weekends
      restDay <- bike %>% 
        dplyr::filter(holiday==0 & workingday==0) %>% 
        dplyr::group_by(hr) %>% 
        dplyr::summarise(avg_rider=mean(cnt))
      
      #Holidays
      holiday <- bike %>% 
        dplyr::filter(holiday==1) %>% 
        dplyr::group_by(hr) %>% 
        dplyr::summarise(avg_rider=mean(cnt))
    
    #Clock Plot Function
    clock.plot <- function (x, col = rainbow(n), ...) {
      if( min(x)<0 ) x <- x - min(x)
      if( max(x)>0 ) x <- x/max(x)
      n <- length(x)
      if(is.null(names(x))) names(x) <- 0:(n-1)
      m <- 1.05
      plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '', ...)
      a <- pi/2 - 2*pi/200*0:200
      polygon( cos(a), sin(a) )
      v <- .02
      a <- pi/2 - 2*pi/n*0:n
      segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a) )
      segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3)
      ca <- -2*pi/n*(0:50)/50
      for (i in 1:n) {
        a <- pi/2 - 2*pi/n*(i-1)
        b <- pi/2 - 2*pi/n*i
        polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
        v <- .1
        text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
      }
    }
    
    #Clock Plot for Non-holiday weekdays and weekends, Holidays
    opar<-par(no.readonly=TRUE)
    
    par(mfrow=c(1,3), cex=0.8, cex.main = 1.3, mai=c(0.1,0.1,0.3,0.1))
    
    clock.plot(as.vector(workDay$avg_rider), main = "Non-holiday Weekdays")
    clock.plot(as.vector(restDay$avg_rider), main = "Non-holiday Weekends")
    clock.plot(as.vector(holiday$avg_rider), main = "Holidays")
    
    par(opar)
  
  
  ##Line graph to find out peak period(s) - time and day for casual and registered users
    #Note: 0 is Sunday
    
    #Non-holidays
    #Registered user by day and hour
    bikeDay <- bike %>%
      dplyr::filter(holiday=="0") %>%
      dplyr::group_by(weekday, hr) %>%
      dplyr::summarise(casUser = sum(casual), regUser = sum(registered))
    
      ggplot(bikeDay) +
      aes(x=hr, y=regUser, colour=as.factor(weekday)) +
      geom_line() +
      labs(x="Time of day / hour", y="Number of Registered Riders", title = "Number of Registered Riders for Non-holidays") +
      scale_color_brewer(palette="Paired") +
      scale_colour_discrete(name = "Day of Week", labels= c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun"))
    
    #Casual user by day and hour
    ggplot(bikeDay) +
      aes(x=hr, y=casUser, colour=as.factor(weekday)) +
      geom_line() +
      labs(x="Time of day / hour", y="Number of Casual Riders", title = "Number of Casual Riders for Non-holidays") +
      scale_color_brewer(palette="Paired") +
      scale_colour_discrete(name = "Day of Week", labels= c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun"))

  
###Business Question 4: How does temperature impact ridership?
  ##Time Series Plot to visualize trends between temperature and ridership
  #http://www.r-graph-gallery.com/105-plotting-time-series/
  
  #Time Plot 
    #Define plot titles
      #lab.temp <- "Actual temperature / °C"
      lab.atemp <- "Feels like temperature / °C"
      #lab.hum <- "Humidity"
      #lab.windspeed <- "Windspeed"
      lab.cnt <- "Total Number of Riders"
      lab.reg <- "Total Number of Registered Riders"
      lab.cas <- "Total Number of Casual Riders"
  
    #Custom strip function:
    my.strip <- function(which.given, which.panel, ...) {
      strip.labels <- c(lab.reg, lab.cas, lab.cnt,lab.atemp)
      panel.rect(0, 0, 1, 1, col="#ffe5cc", border=1) #default lattice strip background colour
      panel.text(x=0.5, y=0.5, adj=c(0.5, 0.55), cex=0.95,
                 lab=strip.labels[which.panel[which.given]])
    }
  
    #Define X axis date range:
      bike$dteday <- as.Date(bike$dteday)
      xlim <- range(bike$dteday)
  
    #Define annual quarters for plot grid line markers:
      d <- seq(from=as.Date("2011-01-01"), to=as.Date("2012-01-01"), by=365/4)
  
    #Define colours for raw & smoothed data:
      col.raw <- "#40E0D0" #colset[2] } see note above
      col.smo <- "#E41A1C" #colset[1] }
      col.lm <- "grey20"
  
    #Create multipanel plot:
      xyplot(registered + casual + cnt + atemp~ dteday, data = bike,
             scales=list(y="free", rot=0), xlim=xlim,
             strip=my.strip, outer=TRUE, layout=c(1, 4, 1), xlab="Date", ylab="",
             panel=function(x, y, ...) {panel.grid(h=-1, v=0) # plot default horizontal gridlines
               panel.abline(v=d, col="grey90") # custom vertical gridlines
               panel.xyplot(x, y, ..., type="l", col=col.raw, lwd=0.5) # raw data
               panel.loess(x, y, ..., col=col.smo, span=0.14, lwd=0.5) # smoothed data
               panel.abline(h=median(y, na.rm=TRUE), lty=2, col=col.lm, lwd=1) # median value
             },
             key=list(text=list(c("raw data", "smoothed curve", "median value")),
                      title="Relationship between Temperature and Ridership",
                      col=c(col.raw, col.smo, col.lm), lty=c(1, 1, 2),
                      columns=2, cex=0.95,
                      lines=TRUE
             ),
      )
  
  
  ##Line graph to see relationship between temperature and number of riders
    #Registered Riders
    bike %>%
      dplyr::mutate(workingday = recode_factor(workingday, '0' = "Non-working day", '1' = "Working day")) %>%
      dplyr::group_by(workingday, hr) %>%
      dplyr::summarise(avgTemp = mean(atemp), RegUser=sum(registered), CasUser=sum(casual)) %>%
    
    ggplot() + 
      geom_line(aes(x=hr, y=RegUser)) +
      labs(x="Hour of day", y="Number of Registered Riders") + 
      facet_wrap(.~workingday) +
      geom_line(aes(x=hr, y=avgTemp*500000), colour="blue") +
      geom_text(x=10,y=100000,label="Number of Registered Riders") +
      geom_text(x=10,y=190000,aes(label="Average Temperature")) +
      scale_y_continuous(sec.axis = sec_axis(~./500000, name = "Average Temperature"))
  
    #Casual Riders
    bike %>%
      dplyr::mutate(workingday = recode_factor(workingday, '0' = "Non-working day", '1' = "Working day")) %>%
      dplyr::group_by(workingday, hr) %>%
      dplyr::summarise(avgTemp = mean(atemp), RegUser=sum(registered), CasUser=sum(casual)) %>%
  
    ggplot() +
      geom_line(aes(x=hr, y=CasUser)) +
      labs(x="Hour of day", y="Number of Casual Riders")+
      facet_wrap(.~workingday) +
      geom_line(aes(x=hr, y=avgTemp*500000), colour="blue") +
      geom_text(x=10,y=100000,label="Number of Casual Riders") +
      geom_text(x=10,y=190000,aes(label="Average Temperature")) +
      scale_y_continuous(sec.axis = sec_axis(~./500000, name = "Average Temperature"))
  

###Business Question 5: Business Question 5: How do different variables under different weather situations impact ridership?
  ##Recode for clarity
    bike$weathersit <- recode(bike$weathersit, '1' = "Clear", '2' = "Misty", '3' = "Light Rain / Snow", '4' = "Heavy Rain / Snow")
    bike$season <- recode(bike$season, '1' = "Spring", '2' ="Summer", '3' ="Autumn", '4' ="Winter")
    
    ##Calculate median for each weather situation
    bike %>%
      dplyr::group_by(weathersit) %>%
      dplyr::summarise("Total Ridership" = median(cnt), "Registered Riders" = median(registered), "Casual Riders" = median(casual))
    
    
  ##Area chart to see relationship between weather condition and ridership
  ##Weather Situation
     ##Temperature
      #Registered riders
      bike %>%
        dplyr::group_by(weathersit, atemp) %>%
        dplyr::summarise(Med = median(atemp), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
        
        ggplot() +
        geom_area(aes(x=Med, y=RegUser)) +
        labs(x="Median Feeling Temperature", y="Median Number of Registered Users") +
        facet_wrap(~weathersit)
      
      #Casual users
      bike %>%
        dplyr::group_by(weathersit, atemp) %>%
        dplyr::summarise(Med = median(atemp), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
        
        ggplot() +
        geom_area(aes(x=Med, y=CasUser)) +
        labs(x="Median Feeling Temperature", y="Median Number of Casual Users") +
        facet_wrap(~weathersit)
      
    ##Windspeed
      #Registered users
      bike %>%
        dplyr::group_by(weathersit, windspeed) %>%
        dplyr::summarise(Med = median(windspeed), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
        
        ggplot() +
        geom_area(aes(x=Med, y=RegUser)) +
        labs(x="Median Windspeed", y="Median Number of Registered Users") +
        facet_wrap(~weathersit)
      
      #Casual users
      bike %>%
        dplyr::group_by(weathersit, windspeed) %>%
        dplyr::summarise(Med = median(windspeed), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
        
        ggplot() +
        geom_area(aes(x=Med, y=CasUser)) +
        labs(x="Median Windspeed", y="Median Number of Casual Users") +
        facet_wrap(~weathersit)
      
      
    ##Humidity
      #Registered users
      bike %>%
        dplyr::group_by(weathersit, hum) %>%
        dplyr::summarise(Med = median(hum), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
        
        ggplot() +
        geom_area(aes(x=Med, y=RegUser)) +
        labs(x="Median Humidity", y="Median Number of Registered Users") +
        facet_wrap(~weathersit)
      
      #Casual users
      bike %>%
        dplyr::group_by(weathersit, hum) %>%
        dplyr::summarise(Med = median(hum), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
        
        ggplot() +
        geom_area(aes(x=Med, y=CasUser)) +
        labs(x="Median Humidity", y="Median Number of Casual Users") +
        facet_wrap(~weathersit)
      
      
      
  ##Season + Weather Situation
  bike$seasoncond <- paste(bike$weathersit, bike$season, sep=" ")
    
    ##Temperature
      #Registered riders
      bike %>%
        dplyr::group_by(seasoncond, atemp) %>%
        dplyr::summarise(Med = median(atemp), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
      
        ggplot() +
        geom_area(aes(x=Med, y=RegUser)) +
        labs(x="Median Feeling Temperature", y="Median Number of Registered Users") +
        facet_wrap(~seasoncond)
      
      #Casual users
      bike %>%
        dplyr::group_by(seasoncond, atemp) %>%
        dplyr::summarise(Med = median(atemp), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
      
        ggplot() +
        geom_area(aes(x=Med, y=CasUser)) +
        labs(x="Median Feeling Temperature", y="Median Number of Casual Users") +
        facet_wrap(~seasoncond)
  
    ##Windspeed
      #Registered users
      bike %>%
        dplyr::group_by(seasoncond, windspeed) %>%
        dplyr::summarise(Med = median(windspeed), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
      
      ggplot() +
        geom_area(aes(x=Med, y=RegUser)) +
        labs(x="Median Windspeed", y="Median Number of Registered Users") +
        facet_wrap(~seasoncond)
      
      #Casual users
      bike %>%
        dplyr::group_by(seasoncond, windspeed) %>%
        dplyr::summarise(Med = median(windspeed), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
      
      ggplot() +
        geom_area(aes(x=Med, y=CasUser)) +
        labs(x="Median Windspeed", y="Median Number of Casual Users") +
        facet_wrap(~seasoncond)
    
      
    ##Humidity
      #Registered users
        bike %>%
          dplyr::group_by(seasoncond, hum) %>%
          dplyr::summarise(Med = median(hum), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
        
        ggplot() +
          geom_area(aes(x=Med, y=RegUser)) +
          labs(x="Median Humidity", y="Median Number of Registered Users") +
          facet_wrap(~seasoncond)
      
      #Casual users
        bike %>%
          dplyr::group_by(seasoncond, hum) %>%
          dplyr::summarise(Med = median(hum), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
        
        ggplot() +
          geom_area(aes(x=Med, y=CasUser)) +
          labs(x="Median Humidity", y="Median Number of Casual Users") +
          facet_wrap(~seasoncond)
  
