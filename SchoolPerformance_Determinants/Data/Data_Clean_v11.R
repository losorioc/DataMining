```{r pressure, echo=FALSE}


#################################   Data cleaning   #############################

#Stacking datamframes with ID variable
high <- as.data.frame(read.csv("http://www.andrew.cmu.edu/user/achoulde/95791/projects/Project%20C/high.csv", header = TRUE))
high$level <- as.factor("HIGH")

mid<- as.data.frame(read.csv("http://www.andrew.cmu.edu/user/achoulde/95791/projects/Project%20C/middle.csv", header = TRUE))
mid$level <- as.factor("MID")

elem<- as.data.frame(read.csv("http://www.andrew.cmu.edu/user/achoulde/95791/projects/Project%20C/elementary.csv", header = TRUE))
elem$level <- as.factor("ELEMENTARY")

school <- rbind(high,mid,elem)

#removing rows with ? in API
school <- school[!(school$API == "?"),]

#changes ? to NA in numeric variables and changes variable types, and dropping used factors
school <- school %>% 
  mutate(AVG_ED = replace(AVG_ED, AVG_ED == "?", NA),
         VALID = replace(VALID, VALID == "?", NA)) %>%
  mutate(VALID = as.numeric(VALID),
         AVG_ED = as.numeric(AVG_ED), NUMTEACH = as.numeric(NUMTEACH),
         FULL_PCT = as.numeric(FULL_PCT), EMER_PCT = as.numeric(EMER_PCT),
         WVR_PCT = as.numeric(WVR_PCT), YRS_TEACH = as.numeric(YRS_TEACH),
         YRONE_TCH = as.numeric(YRONE_TCH), YRTWO_TCH = as.numeric(YRTWO_TCH)) %>%
  droplevels

#loop that checks student to teacher ratio. If ratio is higher than 40:1, it replaces NUMTEACH with NA 
for (i in 1:nrow(school)){
  school$NUMTEACH[i] <- ifelse((school$VALID[i]/school$NUMTEACH[i]) >= 40, NA, school$NUMTEACH[i])
}

#training and test data sets for all schools
set.seed(56)
school.train <- sample(1:nrow(school), 6111)
school.test <- school[-school.train,]
school.train <-school[school.train,]



##################  Data frame for each school level  (droppig unused levels) #################3#########


#Data frame for each school level
school.high <- filter(school, level == "HIGH")

school.mid <- filter(school, level == "MID")

school.ele <- filter(school, level == "ELEMENTARY")


#training and test data sets for each school level
set.seed(56)
school.high.train <- sample(1:nrow(school.high), 996)
school.high.test <- school.high[-school.high.train,]
school.high.train <-school.high[school.high.train,]

set.seed(56)
school.mid.train <- sample(1:nrow(school.mid), 946)
school.mid.test <- school.mid[-school.mid.train,]
school.mid.train <-school.mid[school.mid.train,]

set.seed(56)
school.ele.train <- sample(1:nrow(school.ele), 4169)
school.ele.test <- school.ele[-school.ele.train,]
school.ele.train <-school.ele[school.ele.train,]




#####################     Recreating data farmes with only dummy variables         ###############################


#function for making dummy variables for each factor (might be an easier approach but this was no trouble)
school.dummy <- as.data.frame(model.matrix(~CHARTER + AA_SIG + AS_SIG + HI_SIG + WH_SIG + SD_SIG + YR_RND + ACS_CORE + API + level, data = school))

#Calling functions. loops were avoided due to renaming concerns.
APIHIGH <- ifelse(school.dummy$API == 1, 0 , 1)
school.dummy <- cbind(school.dummy,APIHIGH)
school.dummy$APILow <-NULL

YR_RNDUnk <- ifelse(school.dummy$YR_RNDNo == 1 | school.dummy$YR_RNDYes == 1, 0, 1)
school.dummy<- cbind(school.dummy, YR_RNDUnk)
school.dummy$YR_RNDNo <- NULL

levelHIGH <- ifelse(school.dummy$levelMID == 1 | school.dummy$levelELEMENTARY == 1 , 0, 1)
school.dummy <- cbind(school.dummy, levelHIGH)

#new dataframe with dummies added.
school.dummy <- as.data.frame(cbind(school, school.dummy))

#traing and test data set for school.dummy
set.seed(56)
school.dummy.train <- sample(1:nrow(school.dummy), 6111)
school.dummy.test <- school.dummy[-school.dummy.train,]
school.dummy.train <-school.dummy[school.dummy.train,]


##################    Table with ONLY dummies (i.e. factors removed)    ####################


school.DO <-school.dummy %>% 
  dplyr::select(VALID, AVG_ED:YRTWO_TCH, `(Intercept)`:ACS_CORE43, APIHIGH:YR_RNDUnk)

#training and test data set for school.DO
set.seed(56)
school.DO.train <- sample(1:nrow(school.DO), 6111)
school.DO.test <- school.DO[-school.DO.train,]
school.DO.train <-school.DO[school.DO.train,]



###################  Making dummy only frame for school high.  #######################
school.high.DO <- school$level
school.high.DO <- cbind(school.DO, school.high.DO)
school.high.DO <- school.high.DO %>% filter(school.high.DO == "HIGH") %>%
  dplyr::select(VALID:ACS_CORE43, APIHIGH:YR_RNDUnk)

#training and test data for high school dummy frame.
set.seed(56)
school.high.DO.train <- sample(1:nrow(school.high.DO), 996)
school.high.DO.test <- school.high.DO[-school.high.DO.train,]
school.high.DO.train <-school.high.DO[school.high.DO.train,]

#undersampleing for high school dummy frame
set.seed(956)
y.high <- as.factor(school.high.DO$APIHIGH)
x.high<- dplyr::select(school.high.DO, VALID:ACS_CORE43, YR_RNDUnk)

undersample.high <- downSample(x = x.high,
                               y = y.high)



#####################   Dummy only for middle school + training, testing sets   #################


school.mid.DO <- school$level
school.mid.DO <- cbind(school.DO,school.mid.DO)
school.mid.DO <- school.mid.DO %>% filter(school.mid.DO == "MID") %>%
  dplyr::select(VALID:ACS_CORE43, APIHIGH)

set.seed(56)
school.mid.DO.train <- sample(1:nrow(school.mid.DO), 946)
school.mid.DO.test  <- school.mid.DO[-school.mid.DO.train,]
school.mid.DO.train <-school.mid.DO[school.mid.DO.train,]



#####################  DUmmy for elementary  ###########################


school.ele.DO <- school$level
school.ele.DO <- cbind(school.DO, school.ele.DO)
school.ele.DO <- school.ele.DO %>% filter(school.ele.DO == "ELEMENTARY") %>%
  dplyr::select(VALID:ACS_CORE43, APIHIGH:YR_RNDUnk)

set.seed(56)
school.ele.DO.train <- sample(1:nrow(school.ele.DO), 4168)
school.ele.DO.test <- school.ele.DO[-school.ele.DO.train,]
school.ele.DO.train <-school.ele.DO[school.ele.DO.train,]


##################    Data frame for exploring ######

school.ex <- school %>%
  mutate(ACS_CORE = replace(ACS_CORE, ACS_CORE == "?", NA)) %>%
  mutate(ACS_CORE = as.numeric(ACS_CORE))




######################### Creating data frames for LASSO where Y is APIHIGH and X is all others  ########################


# school.DO.glmnet <- na.omit(school.DO)
# Y <- school.DO.glmnet$APIHIGH
# X <- school.DO.glmnet
# X$APIHigh <-NULL
# X <- as.matrix(X)

#dropping one grade level from school.DO 
# school$level <-NULL
# school.train$level <- NULL
# school.test$level <- NULL
# rm(elem,high,mid)

