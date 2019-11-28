#Set working directory
#setwd("C:/Users/Shenc/Documents/NUS EBAC/EBA5001/Assignment 1/Data")
setwd("E:/NUS/EBAC/EBA5001 Management of Business Analytics Project/1 Data Analytics Process and Best Practices/Assignment/Assignment 1")

#Install and load packages (including package to read .xlsx file)
  #pacman::p_load(install=TRUE, ggplot2, tidyverse, dplyr, tidyr, lattice, lubridate, lettercase, reshape2, plyr, rebus, IRdisplay, data.table, stringr, caret, plotly, editrules, gdata, stringi, tibble, openxlsx)
  
  library(base)
  library(ggplot2)
  library(tidyverse)
  library(plyr)
  library(gdata)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(lattice)
  library(caret)
  library(plotly)
  library(tibble)
  library(openxlsx)
  library(rebus)  
  
#Read .xlsx files 
  mbr1 <- readxl::read_xlsx("DABP_D2_Assignment_Data.xlsx", sheet="MemberData", col_names=TRUE)
  ses1 <- readxl::read_xlsx("DABP_D2_Assignment_Data.xlsx", sheet="SessionData", col_names=TRUE)


---------------

### 1. Analyse raw data structure ###
  
#Data check and analyze MemberData
  class(mbr1) #class of data object
  dim(mbr1) #dimensions of data
  names(mbr1) #column names
  glimpse(mbr1) #similar to str() but display more information
  summary(mbr1)

#Data check and analyze SessionData
  class(ses1) #class of data object
  dim(ses1) #dimensions of data
  names(ses1) #column names
  glimpse(ses1) #similar to str() but display more information
  summary(ses1)

#Convert to dataframe
  mbr <- as.data.frame(mbr1)
  ses <- as.data.frame(ses1)


---------------
    
### 2. Data cleaning ###
  
#2.1 Identify data issues
#Check for missing values in MemberData
  any(is.na(mbr))
  #is.na(mbr)
  sum(is.na(mbr))
  colnames(mbr)[colSums(is.na(mbr))>0]
  #Findings: Missing values in "Workphone", "Address", "Gender", "Height", "Weight", "FitnessPointsDec18"

#Check for missing values in SessionData
  any(is.na(ses)) 
  #is.na(ses)
  sum(is.na(ses))
  colnames(ses)[colSums(is.na(ses))>0]
  #Findings: Missing values in "Location", "SessionTime", "FitnessChampion"


#Check for 0 or more (*) spaces (//s) between beginning (^) and end ($) of string
  any(grepl("^//s*$",mbr)) 
  any(grepl("^//s*$",ses)) 
  #Findings: No space before or after an input in each cell

#Generate histogram to check total number of points up till Nov for outliers
  hist(mbr$FitnessPointsUpToNov18, breaks = 10)
  #Findings: May have an outlier with 800+ points

#Check for duplicate member names
  mbr[duplicated(mbr$MemberName)|duplicated(mbr$MemberName, fromLast=TRUE),]
  #Findings: 2 duplicated Member Names: "Cox, Anastacia Felicia" and "Varma, Sneha"
  mbr[duplicated(mbr$MemberName)&duplicated(mbr$DOB)&duplicated(mbr$MemberSince),"MemberName"]
  #Findings: 1 Member "Cox, Anastacia Felicia" is likely to be a duplicate entry because of same name, date of birth and MemberSince (these are never changing variable) 
  
  
#2.2 Clean and verify
#Variable 1 - to clean: MemberName (mbr)
#analysis & action - to remove row 110 as it is a duplicate of row 12. The two entries has identical profile. We will keep the row with the higher fitness points up to Nov
  mbr_1 <- mbr[-110,] #remove row 110
  mbr_1[duplicated(mbr_1$MemberName)|duplicated(mbr_1$MemberName, fromLast=TRUE),] #verify if duplicates are removed
  dim(mbr_1)
  #Findings: rows 36 and 80 are believed to be 2 individual person due to completely different profile picture 
  

#Variable 2 - to verify: MemberMobilePhone (ses)
#analysis & action - duplicated member in row 110 has two records of mobile number which results in duplication in fitness session
  temp1 <- ses %>% #create a temp table that merges the duplicated member's phone numer so as to find duplicated sessions
    mutate(., MemberMobilePhone=  gsub(84715874, 94352342, ses$MemberMobilePhone)) %>% #merge member's phone numbers
    mutate(., SessionDate = as.character(SessionDate)) %>% #convert SessionDate to character clas
    mutate(., key = paste(SessionDate, SessionType, MemberMobilePhone)) #exclude "location" and "sessiontime" in key due to missing values
  temp1[duplicated(temp1$key), ] #find duplicated sessions attended by member using 2 phone numbers
  
  ses_1 <- ses[-c(545,747,894,1088,1208,1362,1713),] #remove duplicated rows
  ses_1[duplicated(ses_1$key), ]  #verify if duplicates are removed 
  

#Variable 3 - to clean and verify: FitnessPointsDec18 (mbr)
#analysis & action - to verify fitness points for dec by cross referencing to mbr phone number for class participation
  mbr_2 <- mbr_1 %>%
    mutate(., FitnessPointsDec18 = ifelse(is.na( mbr_1$FitnessPointsDec18), 0,mbr_1$FitnessPointsDec18))#convert "NA" values to 0
  match_mobile <- match_df(mbr_2, ses_1, on = "MemberMobilePhone") #match MemberData and SessionDate based on phone number to find out who attended what classes in Dec
  #comments: all members participated in various classes in Dec
  temp2 <- temp1 %>%
    select(SessionType, MemberMobilePhone) #create new table with required variables
  temp3 <- ddply(temp2, .(SessionType, MemberMobilePhone), nrow) #temp table to find out how many sessions per type did each member attend
  
  dec_points <- merge(x = temp3, y = mbr_2, all.x = TRUE) #add fitness points for Dec
  dec_points_wide <- spread(dec_points, SessionType, V1, fill = 0) #create separate columns for kickboxing and yoga session count
  
#yoga and kickboxing points table per session 
  pts_tbl <- dec_points_wide %>%
    mutate(., yoga_pts = ifelse(KickBoxing == 0, FitnessPointsDec18/Yoga, 0)) %>% #calculate points per number of session for yoga class for members who attended yoga classes only
    mutate(., kb_pts = ifelse(Yoga == 0, FitnessPointsDec18/KickBoxing, 0)) #calculate points per number of session for kickboxing class for members who attended kickboxing classes only
  #temp table to pick up members with Dec yoga point > 0
  temp4 <- pts_tbl %>%
    filter(yoga_pts > 0) %>%
    select(Yoga, yoga_pts) %>%
    arrange(Yoga) %>%
    mutate(., yoga_points = yoga_pts)
  yoga_points_tbl <- unique(temp4) #number of fitness points given per number of yoga sessions attended
  #temp table to pick up members with Dec kickboxing point > 0
  temp5 <- pts_tbl %>%
    filter(kb_pts > 0) %>%
    select(KickBoxing, kb_pts) %>%
    arrange(KickBoxing) %>%
    mutate(., kb_points = kb_pts)
  kb_points_tbl <- unique(temp5) #number of fitness points given per number of kickboxing sessions attended  
  
#Dec point check
  #temp table to exclude columns yoga_pts and kb_pts
  temp6 <- pts_tbl %>% 
    select(., -c(yoga_pts,kb_pts)) 
  dec_pt <- cbind(temp6, yoga_points_tbl[match(temp6$Yoga, yoga_points_tbl$Yoga), -1],
                  kb_points_tbl[match(temp6$KickBoxing, kb_points_tbl$KickBoxing), -1])
  #temp table to select columns and clean up Dec points for yoga and kickboxing
  temp7 <- dec_pt %>%
    select(MemberName, MemberMobilePhone, DOB, Gender, Height, Weight, MemberSince, FitnessPointsUpToNov18, FitnessPointsDec18, Yoga, KickBoxing, yoga_points, kb_points) %>%
    mutate(., yoga_points = ifelse(is.na(dec_pt$yoga_points), 0, dec_pt$yoga_points)) %>% #change NA values to 0
    mutate(., kb_points = ifelse(is.na(dec_pt$kb_points), 0, dec_pt$kb_points)) %>% #change NA values to 0
    mutate(., yoga_ttl = dec_pt$yoga_points*dec_pt$Yoga) %>% #calculate Dec yoga points per number of sessions attended
    mutate(., kb_ttl = dec_pt$kb_points*dec_pt$KickBoxing) #calculate Dec kickboxing points per number of sessions attended
  
  mbr_3 <- temp7 %>%
    mutate(., yoga_ttl = ifelse(is.na(temp7$yoga_ttl), 0, temp7$yoga_ttl)) %>% #change NA values to 0
    mutate(., kb_ttl = ifelse(is.na(temp7$kb_ttl), 0, temp7$kb_ttl)) %>% #change NA values to 0
    mutate(., dec_clean = kb_ttl + yoga_ttl) %>% #calculate total dec fitness points per member
    mutate(., MemberSince = ymd(temp7$MemberSince)) %>% #convert Membersince to date class
    mutate(., DOB = ymd(temp7$DOB)) #convert DOB to date class
  
  dim(mbr_3) #mbr_3 includes info in original mbr.csv (except 1 duplicate), and 1) number of yoga/kickboxing session per mbr 
  #in Dec, 2) yoga/kickboxing points per number of sessions (more sessions = bonus points), 3) cleaned dec points
  
#Variable 4 - to clean: Location
#analysis & action - locational data is important as we want to find out the popularity of each class per location
#where is the champion at a certain date and time
  temp8 <- ses_1 %>% 
    filter(!is.na(Location), !is.na(SessionTime)) %>% #exclude lines with NA location and sessiontime
    select(SessionDate, FitnessChampion, SessionTime, Location) %>% 
    mutate(., key = paste(SessionDate, SessionTime, FitnessChampion)) %>% #create key for vlookup
    arrange(SessionDate)
  champion_tbl <- unique(temp8) #remove duplicate rows, keep only unique keys
  
#find missing location (9 records) based on champion_tbl
  temp9 <- ses_1 %>% 
    mutate(., key = paste(SessionDate, SessionTime, FitnessChampion)) #temp table to create key to match to champion tbl
  #where are the missing locations
  missing_loc <- ses_1 %>%
    filter(is.na(Location )) %>%
    select(SessionDate, FitnessChampion, SessionTime) %>%
    mutate(., key = paste(SessionDate, SessionTime, FitnessChampion)) %>%
    arrange(SessionDate)
  loc_tbl <- merge(missing_loc,champion_tbl, id= "key", all.x = TRUE) #find missing locations based on champion tbl
  temp10 <- loc_tbl %>%
    mutate(., Location1 = Location) %>% #change location column names to prepare table to merge with main ses table
    select(., -c(Location))
  
#replace missing location
  temp11 <- merge(temp9, temp10, id = "key", all.x = TRUE) #merge missing location columns with main table
  temp12 <- temp11 %>%
    mutate(., Location = ifelse(is.na(temp11$Location), "NA", temp11$Location)) #replace blanks in original location columns with NA for identification in the next step
  ses_2 <- temp12 %>%
    mutate(., Location = ifelse(temp12$Location == "NA", paste(temp12$Location1), temp12$Location)) %>% #fill in missing location with vlookup result
    select(Location, SessionDate, SessionTime, FitnessChampion, SessionType, MemberMobilePhone) %>% #select columns from original ses tables
    filter(!is.na(Location )) #remove location that is still missing. if done correctly, there should not be any missing locations by this time
  
  sum(is.na(ses_2$Location)) 
  dim(ses_2)
  
#Variable 5 - to clean: SessionTime
#analysis & action - SessionTime will be used to calculate the number of sessions per location per date
#where is the champion at a certain date, time and location
  temp12 <- ses_2 %>% 
    filter(!is.na(SessionTime), !is.na(FitnessChampion)) %>% #exclude lines with NA champion name and sessiontime
    select(SessionDate, FitnessChampion, SessionTime, Location) 
  temp12.1 <- temp12 %>%
    mutate(., key = paste(temp12$SessionDate, temp12$Location, temp12$FitnessChampion)) %>% #create key for vlookup
    arrange(SessionDate)
  temp13 <- temp12.1 %>%
    select(key, SessionTime)
  champion_tbl2 <- unique(temp13) #remove duplicate rows, keep only unique keys
  
#find missing sessiontime (20 records) based on champion_tbl2
  temp14 <- ses_2 %>% 
    mutate(., key = paste(SessionDate, Location, FitnessChampion)) #temp table to create key to match to champion tbl
  missing_st <- ses_2 %>%
    filter(is.na(SessionTime )) %>%
    select(SessionDate, FitnessChampion, Location) %>%
    mutate(., key = paste(SessionDate, Location, FitnessChampion)) %>%
    arrange(SessionDate)
  st_tbl <- merge(missing_st,champion_tbl2, id= "key", all.x = TRUE) #find missing session time based on champion tbl 2
  temp15 <- st_tbl %>%
    mutate(., SessionTime1 = SessionTime) %>% #change location column names to prepare table to merge with main ses table
    select(., -c(SessionTime))

#replace missing sessiontime
  temp16 <- merge(temp14, temp15, id = "key", all.x=TRUE) #merge missing location columns with main table
  temp17 <- temp16 %>%
    mutate(., SessionTime = ifelse(is.na(temp16$SessionTime), "NA", temp16$SessionTime)) #replace blanks in original location columns with NA for identification in the next step
  temp18 <- temp17 %>%
    mutate(., SessionTime = ifelse(temp17$SessionTime == "NA", paste(temp17$SessionTime1), temp17$SessionTime)) %>%
    select(Location, SessionDate, SessionTime, FitnessChampion, SessionType, MemberMobilePhone) %>%
    mutate(., SessionDate = ymd(SessionDate)) %>%
    filter(!is.na(SessionTime ))
  ses_3 <- unique(temp18)
  
  sum(is.na(ses_3$SessionTime))
  dim(ses_3) #ses_3 is the cleaned version of ses without blank location

#Visualise correlation of Height and Weight and Gender to impute Gender
  mbr_3 %>%
    filter(!is.na(Weight) & Weight >0 & !is.na(Height) & Height >0) %>%
    ggplot()+
    aes(x = Height, y=Weight, colour=Gender) + 
    labs (x="Height", y="Weight") +
    geom_point() + 
    stat_smooth(method="lm")
  #Finding: Relationship between Height, Weight, Gender is not clear-cut due to the spread of the points. Hence, we cannot impute values for Gender
  
   
#write.xlsx(mbr_3, "Member.xlsx", sheetName = "MemberData", col.names = TRUE, row.names = TRUE, append = FALSE)
#write.xlsx(ses_3, "Session.xlsx", sheetName = "SessionData", col.names = TRUE, row.names = TRUE, append = TRUE)

    
###3. Data Engineering###
#3.1 Create new columns to derive additional data
#Create new column to add up points for the year
  mbr_3$TotalFitnessPoints <- as.numeric(mbr_3$FitnessPointsUpToNov18) + as.numeric(mbr_3$dec_clean)
  
#Create new column to calculate Age as of 31 Dec 2018
  mbr_3$Age <- round(interval(mbr_3$DOB,"2018-12-31")/dyears(1),0)
  
#Create new column to calculate number of days of membership as of 31 Dec 2018
  mbr_3$LengthMemMth <- interval(mbr_3$MemberSince,"2018-12-31") %/% months(1)
  
#Create new column to calculate activeness of member (points divided by length of membership in months) in Dec
  mbr_3$AvgPtsPerMth <- round((mbr_3$TotalFitnessPoints / mbr_3$LengthMemMth),0)
  
#Create new column to calculate activeness of member (number of sessions divided by length of membership in months) in Dec 
  mbr_3$AvgSesPerMth <- round((mbr_3$Yoga + mbr_3$KickBoxing)/mbr_3$LengthMemMth,0)
  
#Create new column to calculate activeness of member for Yoga (number of sessions divided by length of membership in months) in Dec
  mbr_3$AvgYogaSesPerMth <- round((mbr_3$Yoga)/mbr_3$LengthMemMth,0)
  
#Create new column to calculate activeness of member for KickBoxing (number of sessions divided by length of membership in months) in Dec
  mbr_3$AvgKBSesPerMth <- round((mbr_3$KickBoxing)/mbr_3$LengthMemMth,0)
  
  
#Create new column to convert Date to Day of Week
  ses_3$SessionDay <- as.factor(format(ses_3$SessionDate, "%w"))
  #ses_3$SessionDay <- weekdays(ses_3$SessionDate, abbreviate = TRUE)
  
  
###4. Member Profiling###
#Member demographics: Age, Gender
  summary(mbr_3)
  
  table(mbr_3$Age, mbr_3$Gender)
  
  mbr_3 %>%
    group_by(Age) %>%
    summarise(count=n())%>%
    top_n(1, count) 
  #Findings: Mode of age is 38 (13)
  
  mbr_3 %>%
    filter(!is.na(Gender)) %>%
    group_by(Gender,Age) %>%
    summarise(count=n())%>%
    top_n(1, count)   
  #Findings: Mode age of females is 35(7), males is 34 (8)
  
  mbr_3 %>%
    filter(!is.na(Gender)) %>%
    group_by(Gender) %>%
    summarise(median(Age)) 
  #Findings: Median age of females is 37, males is 35
  
#Visualize distribution of Age and Gender for overall
  mbr_3 %>%
    ggplot()+
    aes(x = Age) + 
    labs (x="Age", y="Count") +
    geom_histogram() +
    facet_wrap(~Gender)

#Figure out bin size
    hist(mbr_3$Age, breaks = 3)
    hist(mbr_3$Age, breaks = 4)
    hist(mbr_3$Age, breaks = 5)
    hist(mbr_3$Age, breaks = 6)
    hist(mbr_3$Age, breaks = 7)
    hist(mbr_3$Age, breaks = 8)
    hist(mbr_3$Age, breaks = 9)
    hist(mbr_3$Age, breaks = 10)
    hist(mbr_3$Age, breaks = 11)
    hist(mbr_3$Age, breaks = 12)
    hist(mbr_3$Age, breaks = 13)
  
##Create bins for Age Categories
  mbr_3$AgeCat <- cut(mbr_3$Age, breaks = c(-Inf, 25, 30, 35, 40, 45, Inf), labels=c("<25", "25-29", "30-34", "35-40", "40-44", ">45"))

#Visualize distribution of Age and Gender for overall
  mbr_3 %>%
    ggplot()+
    aes(x = AgeCat) + 
    labs (x="Age Category", y="Count") +
    geom_bar() +
    facet_wrap(~Gender)
  
#Visualize Average Number of Sessions per Month by Gender for Overall
  mbr_3 %>%
    ggplot()+
    aes(x = AgeCat, y = AvgSesPerMth) + 
    labs(x="Age Category", y= "Average Number of Yoga and KickBoxing Sessions Per Month") +
    geom_boxplot() +
    facet_wrap(~Gender) 
  #Findings: Based on median values, females of ages 30-34, 35-40 and >50 attend more sessions (average of 2 sessions per month); males of ages of above 25 attend more sessions (average of 2 sessions per month)
  
#Visualize Average Number of Sessions per Month by Gender for KickBoxing
  mbr_3 %>%
    filter(KickBoxing>0) %>%
    ggplot()+
    aes(x = AgeCat, y = AvgSesPerMth) + 
    labs(x="Age Category", y= "Average Number of KickBoxing Sessions Per Month") +
    geom_boxplot() +
    facet_wrap(~Gender)  
  #Findings: Based on median values, same observation as overall, females of ages 30-34, 35-40 and >50 attend more sessions (average of 2 sessions per month); males of ages of above 25 attend more sessions (average of 2 sessions per month)
  
  
#Visualize Average Number of Sessions per Month by Gender for Yoga
  mbr_3 %>%
    #filter(KickBoxing>0) %>%
    #filter(Yoga>0) %>%
    ggplot()+
    aes(x = AgeCat, y = AvgSesPerMth) + 
    labs(x="Age Category", y= "Average Number of Yoga Sessions Per Month") +
    geom_boxplot() +
    facet_wrap(~Gender) 
  #Findings: Based on median values, same observation as overall, females of ages 30-34, 35-40 and >50 attend more sessions (average of 2 sessions per month); males of ages of above 25 attend more sessions (average of 2 sessions per month)
  

#Peak period based on day in Dec
  ses_3 %>%
    group_by(SessionType, SessionDay) %>%
    summarise(count=n()) %>%
    top_n(1,count)
  #Findings: Peak period for Yoga is Wednesday (261) and KickBoxing is Sunday (125)
  
  merge(ses_3,mbr_3, key="MemberMobilePhone") %>%
    ggplot() + 
    aes(x=SessionDay, fill=Gender) +
    geom_bar()+
    labs(x="Days of Week", y="Count") +
    facet_wrap(~SessionType) +
    scale_x_discrete(labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
  
  
#Peak period based on day and time in Dec
  ses_3 %>%
    group_by(SessionType, SessionDay, SessionTime) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  #Findings: Peak period for KickBoxing is Sunday 15:00-16:00 (125) and Yoga is Saturday 14:00-15:00 (95)
  
  
#Popular location
  ses_3 %>%
    group_by(SessionType, Location) %>%
    summarise(count=n()) %>%
    arrange(desc(count)) %>%
    top_n(1,count)
  #Findings: Peak period for Yoga is Hougang Mall (281) and Kickboxing is Punggol 21 (212)
  
  
#Most active age of users by Points
  aggregate(TotalFitnessPoints ~ Age+Gender, data=mbr_3, sum) %>%
    arrange(desc(TotalFitnessPoints))
  #Findings: Most active group of males based on points is 31 (2110 points) and females is 35 (2137)
  
  
#Most active users by Length of Membership
  aggregate(TotalFitnessPoints ~ Gender+LengthMemMth, data=mbr_3, sum) %>%
    arrange(desc(TotalFitnessPoints))
  #Findings: Most active group of males based on points has length of membership of 6 months (5062), while that of females is 7 months (4394)
  
  mbr_3 %>%
    ggplot() +
    aes(x=LengthMemMth, y=TotalFitnessPoints)+
    labs(x="Length of membership", y="Total Fitness Points") + 
    geom_point() +
    facet_wrap(~Gender) +
    stat_smooth(method="lm")
  #Findings: There is a positive correlation between length of membership and points earned
  

##Based on age
  #Most active age of users by Average Points Per Month
  aggregate(AvgPtsPerMth ~ Age+Gender, data=mbr_3, sum) %>%
    arrange(desc(AvgPtsPerMth))
  #Findings: Most active age of males is 21 (376) and females is 38 (30)
  
#Most active age of users by Sum of Sessions in Dec for Yoga
  aggregate(Yoga ~ Age+Gender, data=mbr_3, sum) %>%
    arrange(desc(Yoga))
  #Findings: Yoga is most popular in females of age 35 (42) and males of age 31 (43)
  
#Most active age of users by Sum of Sessions in Dec for KickBoxing
  aggregate(KickBoxing ~ Age+Gender, data=mbr_3, sum) %>%
    arrange(desc(KickBoxing))
  #Findings: Kickboxing is most popular in males of age 34 (37) and females of age 35 (29)
  
  
#Based on age category (binning)
#Breakdown of Gender according to Age Categories
  table(mbr_3$AgeCat, mbr_3$Gender)
  
#Most active age category of users by Sum of Average Points Per Month for Overall
  aggregate(AvgPtsPerMth ~ AgeCat+Gender, data = mbr_3, sum) %>%
    arrange(desc(AvgPtsPerMth))
  #Findings: Most active age group is females of age 35-40 (1493) and males of age <25 (1049)
  
#Most active age category of users by Sum of Average Points Per Month for Yoga
  aggregate(Yoga ~ AgeCat+Gender, data = mbr_3, sum) %>%
    arrange(desc(Yoga))
  #Findings: Yoga is most popular in females of age 35-40 (153) and males of age <25 (109)
  
#Most active age category of users by Sum of Average Points Per Month for KickBoxing
  aggregate(KickBoxing ~ AgeCat+Gender, data = mbr_3, sum) %>%
    arrange(desc(KickBoxing))
  #Findings: KickBoxing is most popular in males of age <25 (82) and females of age 35-40 (74)
  
#Most popular activity for different age categories based on Number of sessions
  AgeCat <- merge(aggregate(Yoga~AgeCat+Gender,data=mbr_3, sum), aggregate(KickBoxing~AgeCat+Gender,data=mbr_3, sum), key="AgeCat"&"Gender") 
  AgeCat$PopAct <- ifelse(AgeCat$Yoga>AgeCat$KickBoxing, "Yoga", "KickBoxing")
  AgeCat
  #Findings: Yoga is the most popular activity for all ages and gender


##Based on length of membership
#Most active users based on Length of Membership and TotalFitnessPoints
  aggregate(TotalFitnessPoints ~ LengthMemMth+Gender, data=mbr_3, sum) %>%
    arrange(desc(TotalFitnessPoints))
  #Findings: Most active members are males of 6 months (5062) and females is of 7 months (4394)
  
#Most active users based on Length of Membership and TotalFitnessPoints
  aggregate(TotalFitnessPoints ~ LengthMemMth+Age, data=mbr_3, sum) %>%
    arrange(desc(TotalFitnessPoints))
  #Findings: Most active members are those of age 35 as a member of 8 months (1185)
  
#Most active age of users by Sum of Sessions in Dec for Yoga
  aggregate(Yoga ~ LengthMemMth+Gender+Age, data=mbr_3, sum) %>%
    arrange(desc(Yoga))
  #Findings: Yoga is most popular in females of age 35 as a member of 8 months (21) and males of age 31 as a member of 4 months (18)
  
#Most active age of users by Sum of Sessions in Dec for KickBoxing
  aggregate(KickBoxing ~ LengthMemMth+Gender+Age, data=mbr_3, sum) %>%
    arrange(desc(KickBoxing))
  #Findings: KickBoxing is most popular in males of age 31 a member of 8 months (14) and females of age 32 as a member of 1 month (12)
  

#Top 10% member by Points, Gender based on Points
  mbr_Pts <- mbr_3 %>%
    arrange(desc(TotalFitnessPoints)) %>%
    top_frac(0.1, TotalFitnessPoints)
  summary(mbr_Pts)
  #Findings: Top male member based on Points is Zhao, Alfred (900) and female is Lee, Emma (757)
  #Findings: Top 10% members have median age of 34, with 9 month membership, attended median of 9 Yoga and 3 KickBoxing sessions, earned median total points of 631
  
  merge(ses_3, mbr_Pts, key="MemberMobilePhone") %>%
    group_by(Location, SessionType) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  #Findings: For top 10% members based on Points, Hougang Mall is the most popular location for Yoga (59), and KickBoxing (32)
  
  merge(ses_3, mbr_Pts, key="MemberMobilePhone") %>%
    group_by(SessionType, SessionDay, SessionTime) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  #Findings: For top 10% members based on Points, Mon 12:00-13:00 is the most popular time for Yoga (19); Sun 15:00-16:00 is the most popular time for KickBoxing (17)
  
  
#Top 10% member by Points, Gender based on Average Points per Month (Length of Membership)
  mbr_AvgPts<-mbr_3 %>%
    arrange(desc(AvgPtsPerMth)) %>%
    top_frac(0.1, AvgPtsPerMth)
  summary(mbr_AvgPts)
  #Findings: Top male member based on Average Points per Month (Length of Membership) is Ray, Ram (168) and female is Jha, Debrita (154)
  #Findings: Top 10% members have median age of 38, with 2 month membership, attended median of 9 Yoga and 3.5 KickBoxing sessions, earned median total points of 186, average points of 107 per month
  
  merge(ses_3, mbr_AvgPts, key="MemberMobilePhone") %>%
    group_by(Location, SessionType) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  #Findings: For top 10% members based on Average Points per Month, West Coast is the most popular location for Yoga (48), and Punggol 21 Community Club for KickBoxing (19)
  
  merge(ses_3, mbr_AvgPts, key="MemberMobilePhone") %>%
    group_by(SessionType, SessionDay, SessionTime) %>%
    summarise(count=n()) %>%
    arrange(desc(count)) %>%
    head(20)
  #Findings: For top 10% members based on Average Points per Month, Sat 14:00-15:00 is the most popular time for Yoga (20); Mon 13:00-14:00 is the most popular time for KickBoxing (9) 
  
  
#Top 10% member by Points, Gender based on Total number of sessions in Dec
  mbr_TotalSes<-mbr_3 %>%
    mutate(TotalSes=Yoga+KickBoxing) %>%
    arrange(desc(TotalSes)) %>%
    top_frac(0.1, TotalSes)
  summary(mbr_TotalSes)
  #Findings: Top female members based on Total number of sessions in Dec are Deol, Tanu (15); Reddy, Manisha (15); 	Liu, Elizabeth (15); Ahuja, Tanvi (15); 
  #Findings: Top male members are Lee, Danial (15); Ray, Ram (15)
  #Findings: Top 10% members have median age of 38, with 6 month membership, attended median of 9 Yoga and 5 KickBoxing sessions, earned median total points of 405.5, average points of 83 per month
  
  merge(ses_3, mbr_TotalSes, key="MemberMobilePhone") %>%
    group_by(Location, SessionType) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  #Findings: For top 10% members based on Total number of sessions in Dec, Hougang Mall is the most popular location for Yoga (70), and KickBoxing (40)
  
  merge(ses_3, mbr_TotalSes, key="MemberMobilePhone") %>%
    group_by(SessionType, SessionDay, SessionTime) %>%
    summarise(count=n()) %>%
    arrange(desc(count)) %>%
    head(20)
  #Findings: For top 10% members based on Total number of sessions in Dec, Sat 14:00-15:00 is the most popular time for Yoga (31); Sun 15:00-16:00 is the most popular time for KickBoxing (25) 
  
  
#Top 10% member by Points, Gender based on Length of Membership
  mbr_LengthMem <- mbr_3 %>%
    arrange(desc(LengthMemMth)) %>%
    top_frac(0.1, LengthMemMth) %>%
    arrange(desc(TotalFitnessPoints))
  summary(mbr_Pts)
  #Findings: Top male member based on length of membership is Zhao, Alfred (900) and female is Lee, Emma (757)
  #Findings: Top 10% members based on length of membership have median age of 34, with 9 month membership, attended median of 9 Yoga and 3 KickBoxing sessions, earned median total points of 631
  #Findings: Similar profile as that based on Points only
  
  merge(ses_3, mbr_Pts, key="MemberMobilePhone") %>%
    group_by(Location, SessionType) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  #Findings: For top 10% members based on length of membership, Hougang Mall is the most popular location for Yoga (59), and KickBoxing (32)
  
  merge(ses_3, mbr_Pts, key="MemberMobilePhone") %>%
    group_by(SessionType, SessionDay, SessionTime) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  #Findings: For top 10% members based on length of membership, Mon 12:00-13:00 is the most popular time for Yoga (19); Sun 15:00-16:00 is the most popular time for KickBoxing (17)
  
  
#Check Activeness of members based on Average Sessions Per Mth Up Till Nov (Hence Length of Membership up till Nov is Length of Membership up till Dec minus 1)
  #Comments: Those who join in Nov will have 0 months of membership which will lead to Inf
  mbr_3$AvgPtsNov <- ifelse (mbr_3$LengthMemMth > 1, round(mbr_3$FitnessPointsUpToNov18 / (mbr_3$LengthMemMth - 1), 0), mbr_3$FitnessPointsUpToNov18)
  
  #Comments: Fluctuation (standard deviation) should be for the whole period till Nov, rather than just Dec. 
  #Number of points earned in Dec should be within the range (i.e. standard deviation) of Average Points Per Mth up to Nov
  mbr_3$Active <- ifelse (mbr_3$dec_clean > (mbr_3$AvgPtsNov - sd(mbr_3$AvgPtsNov)), "Consistently active", "Not consistently active")
  mbr_3$Active <- ifelse (mbr_3$dec_clean > (mbr_3$AvgPtsNov + sd(mbr_3$AvgPtsNov)), "Consistently active", mbr_3$Active)
  
  mbr_3 %>%
    ggplot() + 
    geom_bar() +
    aes(x= LengthMemMth, fill=factor(Active)) +
    labs(x="Length of Membership", y="Count", fill="Activeness") +
    facet_wrap(~Gender)
  #Findings: Most members of 6 months and above are consistently active
  
  
#Top 10% member by Points, Gender based on Activeness
  mbr_Active <- mbr_3 %>%
    filter(Active=="Consistently active") %>%
    top_frac(0.1, TotalFitnessPoints) %>%
    arrange(desc(TotalFitnessPoints))
  summary(mbr_Active)
  #Findings: Top male member based on length of membership is Zhao, Alfred (900) and female is Lee, Emma (757)
  #Findings: Top 10% members based on length of membership have median age of 30.5, with 9.5 month membership, attended median of 8.5 Yoga and 3 KickBoxing sessions, earned median total points of 608
  
  merge(ses_3, mbr_Active, key="MemberMobilePhone") %>%
    group_by(Location, SessionType) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  #Findings: For top 10% members based on length of membership, Hougang Mall is the most popular location for Yoga (59), and KickBoxing (32)
  
  merge(ses_3, mbr_Active, key="MemberMobilePhone") %>%
    group_by(SessionType, SessionDay, SessionTime) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  #Findings: For top 10% members based on length of membership, Sun 15:00-16:00 is the most popular time for KickBoxing (15); Mon 12:00-13:00 is the most popular time for Yoga (15)
  
  write.xlsx(mbr_3, "Member.xlsx", sheetName = "MemberData", col.names = TRUE, row.names = TRUE, append = FALSE)
  write.xlsx(ses_3, "Session.xlsx", sheetName = "SessionData", col.names = TRUE, row.names = TRUE, append = TRUE)
  