setwd("E:/NUS/EBAC/EBA5001 Management of Business Analytics Project/1 Data Analytics Process and Best Practices/Assignment/Assignment 2/Task 2")

pacman::p_load(tidyverse, reshape, lubridate, ggplot2, lattice, dplyr)

bike <- read.csv('bikes_2011.csv', check.names=T)

#Change data type
bike %>%
  summary() #%>%
##???  factorize <- c(season, mnth, weekday, holiday, workingday, weathersit) %>%
##???  sapply(colname(bike), factor)

#Bike ridership over a year


#Correlational analysis before scatterplot (correlation heat map)
#http://www.r-graph-gallery.com/97-correlation-ellipses/
#Install package
  library(ellipse)
  library(RColorBrewer)

#Use of bike data
  cordata <- bike[, !(colnames(bike)=='dteday')]
  data <- cor(cordata)
  data

# Build a Pannel of 100 colors with Rcolor Brewer
  my_colors <- brewer.pal(5, "Spectral")
  my_colors=colorRampPalette(my_colors)(100)

# Order the correlation matrix
  ord <- order(data[1, ])
  data_ord = data[ord, ord]
  plotcorr(data_ord , col=my_colors[data_ord*50+50] , mar=c(1,1,1,1)  )
  
  #Findings: temp (0.45) and atemp (0.44), hr (0.41) are weakly positively correlated to total users (cnt)
  #Findings: temp (0.47) and atemp (0.47) are more strongly positively correlated to causual users

  #Findings: Seasonality (weather patterns) have no correlation to ridership  
  

#seasonality and casual??? - To continue


#Relationship between workingday and ridership??? - To continue
#bike %>%
#  group_by(mnth, workingday) %>%
#  summarise(median=n()) %>%
#  ggplot(aes(x = factor(mnth), y = median, colour=factor(workingday))) +
#  geom_point()
  #stat_smooth(method='lm')
  #geom_bar(stat='identity')
  #facet_grid(~weathersit) 
  #theme_light()

  
###Days with highest ridership)
#Days (Days in the week, Working day) that have the most riders
  bike %>%
    group_by(weekday) %>%
    summarise(median=median(cnt)) %>%
    arrange(desc(median))
  #Findings: Friday is the most popular day with most riders (182006)
  #Findings: Friday is the most popular working day with most riders (182006)
  #Note: Sunday is 0 in weekday
  
#Days (Days in the week, Working day) that have the most casual riders
  bike %>%
    group_by(weekday) %>%
    summarise(median=median(casual)) %>%
    arrange(desc(median))
  #Findings: Sunday is the most popular day with most casual riders (59603)
  #Findings: Monday is the most popular working day with most casual riders (31560)
  #Note: Sunday is 0 in weekday
  
#Days (Days in the week, Working day) that have the most registered riders
  bike %>%
    group_by(weekday) %>%
    summarise(median=median(registered)) %>%
    arrange(desc(median))
  #Findings: Tuesday is the most popular day with most registered riders (156349)
  #Findings: Tueday is the most popular working day with most riders (156349)
  #Note: Sunday is 0 in weekday
  
  
#Holiday that has the most riders - To continue

  
#Most ridership - peak hours of working day / holidays - To continue

  
  
#Circular barchart 
#https://www.r-graph-gallery.com/295-basic-circular-barplot/

#Hour based on weekday, weekend

  
#Clock plot
#http://www.r-graph-gallery.com/49-clock-plot/  
  # Data
  bikeTemp <- bike %>%
    #filter(holiday=="0" & workingday=="0") %>%
    filter(workingday=="0") %>%
    group_by(hr) %>%
    summarise(Users = median(cnt), CasUser = median(casual), RegUser = median(registered)) 

  #convert summary into list
    #x <- c(15540, 9558, 6661, 3441, 1817, 5157, 20801, 56930, 95467, 59486, 47511, 56598, 69198, 69339, 66482, 68590, 85622, 127632, 116985, 85802, 62818, 48875, 37677, 25116)
    x<-bikeTemp$Users
    
    # Clock plot function
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
    
    # Use the function on the created data
    clock.plot(x, main = "Total ridership based on hour during non-working days")

  
#Time Series plot - To continue: use 4 weeks for binning --> justify (because months is variable)
#http://www.r-graph-gallery.com/105-plotting-time-series/
  
#Define plot titles
  lab.temp <- "Actual temperature / °C"
  lab.atemp <- "Feels like temperature / °C"
  lab.hum <- "Humidity"
  lab.windspeed <- "Windspeed"
  lab.cnt <- "Ridership"

#Custom strip function:
#(NB the colour used is the default lattice strip background colour)
  my.strip <- function(which.given, which.panel, ...) {
    strip.labels <- c(lab.temp, lab.atemp, lab.hum, lab.windspeed, lab.cnt)
    panel.rect(0, 0, 1, 1, col="#ffe5cc", border=1)
    panel.text(x=0.5, y=0.5, adj=c(0.5, 0.55), cex=0.95,
               lab=strip.labels[which.panel[which.given]])
  }

#Define X axis date range:
  bike$dteday <- as.Date(bike$dteday)
  xlim <- range(bike$dteday)

#Define annual quarters for plot grid line markers:
  d <- seq(from=as.Date("2011-01-01"), to=as.Date("2012-01-01"), by=365/4)

#--Define colours for raw & smoothed data:
  col.raw <- "#377EB8" #colset[2] } see note above
  col.smo <- "#E41A1C" #colset[1] }
  col.lm <- "grey20"

#--Create multipanel plot:
  xyplot(temp + atemp + hum + windspeed + cnt ~ dteday, data = bike, 
         scales=list(y="free", rot=0), xlim=xlim,
         strip=my.strip, outer=TRUE, layout=c(1, 5, 1), xlab="Date", ylab="",
         panel=function(x, y, ...) {panel.grid(h=-1, v=0)    # plot default horizontal gridlines
           panel.abline(v=d, col="grey90") # custom vertical gridlines
           panel.xyplot(x, y, ..., type="l", col=col.raw, lwd=0.5) # raw data
           panel.loess(x, y, ..., col=col.smo, span=0.14, lwd=0.5) # smoothed data
           panel.abline(h=median(y, na.rm=TRUE), lty=2, col=col.lm, lwd=1) # median value
         },
         key=list(text=list(c("raw data", "smoothed curve", "median value")),
                  title="Weather",
                  col=c(col.raw, col.smo, col.lm), lty=c(1, 1, 2),
                  columns=2, cex=0.95,
                  lines=TRUE
         ),
  )


###To complete
#ridership and feels like --> implications: focus on geographical reasoning, climate change: further increase in temperature and ridership

#users to bike ratio (number of bikes per ride)

#R mark down 
  
-----------------------------

##New code    
library(RCurl)
  bikeURL <- getURL("https://raw.githubusercontent.com/idc9/stor390/master/data/bikes_2011.csv")
  bike <- read.csv(text=bikeURL,header=TRUE)
  
  
    

#Optimal average temperature, hr, workingday, reg, cas
  
#Registered users
bike %>%
  mutate(workingday = recode_factor(workingday, '0' = "Non-working day", '1' = "Working day")) %>%
  group_by(workingday, hr) %>%
  summarise(avgTemp = mean(atemp), RegUser=median(registered), CasUser=median(casual)) %>%

  ggplot() +
  geom_line(aes(x=hr, y=RegUser)) +
  labs(x="Hour of day", y="Number of Registered Users")+
  facet_wrap(.~workingday) +
  geom_line(aes(x=hr, y=avgTemp*500000), colour="blue") +
  geom_text(x=10,y=100000,label="Number of Users") +
  geom_text(x=10,y=190000,aes(label="Average Temperature")) +
  scale_y_continuous(sec.axis = sec_axis(~./500000, name = "Average Temperature"))

#Casual Users
bike %>%
  mutate(workingday = recode_factor(workingday, '0' = "Non-working day", '1' = "Working day")) %>%
  group_by(workingday, hr) %>%
  summarise(avgTemp = mean(atemp), RegUser=median(registered), CasUser=median(casual)) %>%
  
  ggplot() +
  geom_line(aes(x=hr, y=CasUser)) +
  labs(x="Hour of day", y="Number of Casual Users")+
  facet_wrap(.~workingday) +
  geom_line(aes(x=hr, y=avgTemp*500000), colour="blue") +
  geom_text(x=10,y=100000,label="Number of Users") +
  geom_text(x=10,y=190000,aes(label="Average Temperature")) +
  scale_y_continuous(sec.axis = sec_axis(~./500000, name = "Average Temperature"))


#Find out peak period for hour, weekday for casual and registered users
#Assumption: Holidays have negligible effect on the overall trend throughout the year
  bike$weekday[bike$weekday=="0"] <- "7"
  #bike$weekday[bike$weekday=="1"] <- "Mon"
  #bike$weekday[bike$weekday=="2"] <- "Tue"
  #bike$weekday[bike$weekday=="3"] <- "Wed"  
  #bike$weekday[bike$weekday=="4"] <- "Thu"
  #bike$weekday[bike$weekday=="5"] <- "Fri"
  #bike$weekday[bike$weekday=="6"] <- "Sat"

  bike %>%
    filter(holiday=="0") %>%
    group_by(weekday, hr) %>%
    summarise(casUser = median(casual), regUser = median(registered)) %>%
  
    ggplot() +
    aes(x=hr, y=casUser, colour=as.factor(weekday)) +
    geom_line() +
    labs(x="Time of day", y="Number of Casual Riders") +
    scale_color_brewer(palette="Paired") +
    scale_colour_discrete(name = "Day of Week", labels= c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun"))

  
  bike %>%
    filter(holiday=="0") %>%
    group_by(weekday, hr) %>%
    summarise(casUser = median(casual), regUser = median(registered)) %>%
    
    ggplot() +
    aes(x=hr, y=regUser, colour=as.factor(weekday)) +
    geom_line() +
    labs(x="Time of day", y="Number of Registered Riders") +
    scale_color_brewer(palette="Paired") +
    scale_colour_discrete(name = "Day of Week", labels= c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun"))

  
  
  
#####NEW
##Temperature
      
  #Regular users
  bike %>%
    group_by(weathersit, atemp) %>%
    summarise(Avg = median(atemp), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
    
    ggplot() +
    geom_area(aes(x=Avg, y=RegUser)) + 
    labs(x="Median Feeling Temperature", y="Median number of Regular Users") +
    facet_wrap(~weathersit)
  
  #Casual users
  bike %>%
    group_by(weathersit, atemp) %>%
    summarise(Avg = median(atemp), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
    
    ggplot() +
    geom_area(aes(x=Avg, y=CasUser)) + 
    labs(x="Median Feeling Temperature", y="Median number of Casual Users") +
    facet_wrap(~weathersit)
  
#Windspeed
  #Regular users
  bike %>%
    group_by(weathersit, windspeed) %>%
    summarise(Avg = median(windspeed), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
    
    ggplot() +
    geom_area(aes(x=Avg, y=RegUser)) + 
    labs(x="Median Windspeed", y="Median number of Regular Users") +
    facet_wrap(~weathersit)
  
  #Casual users
  bike %>%
    group_by(weathersit, windspeed) %>%
    summarise(Avg = median(windspeed), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
    
    ggplot() +
    geom_area(aes(x=Avg, y=CasUser)) + 
    labs(x="Median Windspeed", y="Median number of Casual Users") +
    facet_wrap(~weathersit)
  
#Humidity
  #Regular users
  bike %>%
    group_by(weathersit, hum) %>%
    summarise(Avg = mean(hum), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
    
    ggplot() +
    geom_area(aes(x=Avg, y=RegUser)) + 
    labs(x="Median Humidity", y="Median number of Regular Users") +
    facet_wrap(~weathersit)
  
  #Casual users
  bike %>%
    group_by(weathersit, hum) %>%
    summarise(Avg = mean(hum), Users = median(cnt), RegUser = median(registered), CasUser = median(casual)) %>%
    
    ggplot() +
    geom_area(aes(x=Avg, y=CasUser)) + 
    labs(x="Median Humidity", y="Median number of Casual Users") +
    facet_wrap(~weathersit)

#Average ridership for working weekdays
  bike %>%
    filter(holiday=="0" & workingday=="1") %>%
    group_by(workingday) %>%
    summarise(Users = sum(cnt)/5, casUser = sum(casual)/5, regUser = sum(registered)/5)
  
#Average ridership for weekends
  bike %>%
    filter(holiday=="0" & workingday=="0") %>%
    group_by(workingday) %>%
    summarise(Users = sum(cnt)/2, casUser = sum(casual)/2, regUser = sum(registered)/2)
  
  