---
title: "Determinants of school performance in California"
author: "Lina Osorio, James Duguid, Peter DuPuis, Salvador Ayala"
date: "12/15/2017"
output: 
  html_document:
    theme: spacelab
    highlight: tango
    code_folding: hide
    toc: true
    toc_float: true
    toc_depth: 3
    fig_width: 10
    fig_height: 7
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```


```{r echo=FALSE, message=FALSE, warning=FALSE}

# Load libraries
library(ggplot2)
library(dplyr)
library(knitr)
library(leaps)
library(glmnet)
library(caret)
library(data.table)
library(randomForest)
library(grid)
library(klaR)
library(ggfortify)
library(plyr)
library(arules)
library(arulesViz)
library(FactoMineR)
# Disable scientific notation
options(scipen=999)
# Color palette taken from some homework
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
``` 

#Introduction

The purpose of this project is to evaluate the main determinants of school performance in California, analyzing in depth variables such as school levels, ethnic factors, and teachers profile. Our original dataset has 17 potential explanatory variables and the binarized variable API score used as a proxy of school success which is the variable we want to explain.

Our analysis found that the average education of a school's parents, the ethnic makes up of the school, and experience of a school's teaching staff are the most important variables in predicting a school's API performance. We found that these variables are important predictors regardless of grade level. These finding suggests there is potential racial segregation across schools. Moreover, we found that schools with more experience teaching staffs are predicted to have high API scores, supporting the idea of reallocating teachers to ensure all schools has an adequately experienced teaching force.

This document has four main sections. First, a description of data cleaning process and data exploration. Second, a comparison of the results of various variable selection methodologies to decide which are the key determinants of schools' success in California. Third, an evaluation of the importance of variables such as school levels, ethnic factors, and teachers' profile. Finally, conclusions and recommendations will be presented.



#Data cleaning

The original data was retrieved as three separate .CSV files, one file for elementary school, middle school, and high schools in California. These files were merged into one master data set (school) that contained all grade levels. The data was then reviewed to abnormalities that may exist, such large amounts of NAs, potential data entry errors, and any imbalances with our outcome variable of interest (API score). The following changes were made to our data:

-	API: any observations missing our outcome variable of interest were dropped (i.e. any school that reported a ? for API score). This resulted in 1235 observations being dropped. Imputation methods were considered but ultimately avoided due to concerns around imprecision. Moreover, there remains a sufficiently large amount of observations for our analysis.

-	Teacher Number (NUM_TECH): Some schools showed unusual student-teacher ratios. For any school with a student-teacher ratio more than 40:1, values were treated as entry errors and replaced with NAs. In total, there are 2237 NAs for this variable.

-	Average Core Courses (ACS): Due to the high number of ?, this variable was transformed into a factor variable. This would allow us to keep it in our analysis without introducing more NAs.

-	Average Education of Parents(AVG_ED)/ Number of students (VALID): In order to treat these variables as numeric variables, all ? changed to NAs. However, ?tended to occur in observations where API scores were also missing, causing few additional variables to be excluded.

In addition to these changes, a data frame that transformed factors into dummy variables was used in some of our analysis. This data frame (school.DO) was crated using the model matrix function, with the lowest factor being dropped. For example, the column where AA_SIG = HIGH was kept while the column for AA_SIG = LOW was dropped. These drops were necessary to avoid collinearity in some of our modeling. When the factor was not binary, then the column ? level was dropped.

We used as a proxy of school performance the binarized Academic Performance Index (API) score. This variable ranked as high performance those schools on the top 50-percentile of API results, and as low performance the rest of institutions. This variable also had some missing values that are classified like that for three main reasons: 1) the school didn't have a valid 2008 base API 2) the school faced significant changes in some students 3) the API was classified as invalid for other reason. We decided to drop the API missing values because are not comparable with the other schools. 

Source https://www.cde.ca.gov/ta/ac/ay/glossary09a.asp#gs2

```{r message=FALSE, warning=FALSE, cache=TRUE}


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
school$level <-NULL
school.train$level <- NULL
school.test$level <- NULL
# rm(elem,high,mid)
```

#Data exploration

An essential part of our analysis is evaluating which are the variables that better explain schools' performance. A first approximation to validate if our data is balanced is checking if the number of elementary, middle, and high schools are similar when we compare schools with high and low API score. Resampling our data into the training and the test groups from a balanced dataset will help to get more robust classifications. 

The original dataset has a lot of categorical variables that make difficult a descriptive analysis of correlation, for this reason, we divided the analysis into categorical and numeric variables. 

The first graph shows the count of school level by API score classification (1 if low, and 2 if high).

```{r message=FALSE, warning=FALSE, cache=TRUE}
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

# Code adapted from http://stat545.com/block020_multiple-plots-on-a-page.html
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

```



```{r message=FALSE, warning=FALSE, cache=TRUE}

ggplot(school.ex, aes(x = as.numeric(API), fill = level)) + geom_histogram(binwidth = .4)

```

The next plots show the relationship between categorical variables and API. We can see that most of non-charter schools have a low API, and the same for those classified as year-round schools. On the other hand, most of the schools that have a significative number of Hispanic or African American students have a lower performance, and the opposite happens in those schools that are classified as schools with a significantly high level of white and Asian students.

```{r message=FALSE, warning=FALSE, cache=TRUE}

#### multiplot is not working in my pc... these graphs should go in multiplot


A <- ggplot(data = filter(school.ex, CHARTER != "?"), aes(x = API, fill = CHARTER), size = 0.5) + 
     geom_histogram(binwidth = .4, stat = "count") +
     guides(fill=guide_legend(title="Charter")) +
     scale_alpha(guide = 'none')+
     theme(axis.text = element_text(size = 8), axis.title=element_text(size=8))

B <- ggplot(data = filter(school.ex, AA_SIG != "?"), aes(x = API, fill = AA_SIG)) + 
     geom_histogram(binwidth = .4, stat = "count") +
     guides(fill=guide_legend(title="African American")) +
     scale_alpha(guide = 'none')+
     theme(axis.text = element_text(size = 8), axis.title=element_text(size=8))

C <- ggplot(data = filter(school.ex, AS_SIG != "?"), aes(x = API, fill = AS_SIG)) + 
     geom_histogram(binwidth = .4, stat = "count") +
     guides(fill=guide_legend(title="Asian")) +
     scale_alpha(guide = 'none')+
     theme(axis.text = element_text(size = 8), axis.title=element_text(size=8))

D <- ggplot(data = filter(school.ex, HI_SIG != "?"), aes(x = API, fill = HI_SIG)) + 
     geom_histogram(binwidth = .4, stat = "count") +
     guides(fill=guide_legend(title="Hispanic")) +
     scale_alpha(guide = 'none')+
     theme(axis.text = element_text(size = 8), axis.title=element_text(size=8))

E <- ggplot(data = filter(school.ex, WH_SIG != "?"), aes(x = API, fill = WH_SIG)) + 
     geom_histogram(binwidth = .4, stat = "count") +
     guides(fill=guide_legend(title="White")) +
     scale_alpha(guide = 'none')+
     theme(axis.text = element_text(size = 8), axis.title=element_text(size=8))

Z <- ggplot(data = filter(school.ex, HI_SIG != "?"), aes(x = API, fill = SD_SIG)) + 
     geom_histogram(binwidth = .4, stat = "count") +
     guides(fill=guide_legend(title="Soc.disad")) +
     scale_alpha(guide = 'none') +
     theme(axis.text = element_text(size = 8), axis.title=element_text(size=8))

G <- ggplot(data = filter(school.ex, YR_RND != "?"), aes(x = API, fill = YR_RND)) + 
     geom_histogram(binwidth = .4, stat = "count") +
     guides(fill=guide_legend(title="Yr-round school")) +
     scale_alpha(guide = 'none') +
     theme(axis.text = element_text(size = 8), axis.title=element_text(size=8))

multiplot(A, B, C, D, E, Z, G, cols=2)

```

The graph below shows the correlation analysis between the non-factor variables. We can see that there is a low cross-correlation between the variables. The pairs of variables with the higher level of correlation are parent's level of education with API, and teacher's years of experience with teachers that completed the preparation program.

```{r message=FALSE, warning=FALSE, cache=TRUE}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = pmax(1, cex.cor * r))
}

# Use panel.cor to display correlations in lower panel.
# Here I've set transparency to 0.15 to better display the trends
# in the data

school.ex.cor <- na.omit(school.ex)
pairs(school.ex.cor[,c("API", "AVG_ED", "YRS_TEACH", "VALID", "NUMTEACH", "FULL_PCT", "WVR_PCT", "EMER_PCT", "ACS_CORE")], 
      pch = 16, lower.panel = panel.cor,
      col = rgb(0,0,0,0.15))
```

Finally, we did the graph above to combine the frequency histograms of parent's average education level by school's API level, and we can see that schools attended by children from educated parents have a higher probability of having a high API score.

```{r message=FALSE, warning=FALSE, cache=TRUE}

ggplot(school.ex, aes(x = AVG_ED, fill = API, na.rm = TRUE)) + geom_histogram(binwidth = 10)

```


#Question 1: Subset selection all variables

To determine which variables are the key determinants of school's performance we need to choose among a set of 17 original variables. Our data exploration gave us some intuition regarding which variables might be more relevant. For example, we saw that schools with teachers with more years of experience are teaching in schools with high performance. Also, that schools with a significative number of Hispanic students, or students in socioeconomic disadvantage are more frequent in low performed schools, and that variables associated with the student, for example, level of education of their parents. In this section, we are comparing the level of accuracy and complexity of different.  
Our approximation is comparing the performance of different subset selection methodologies. We approached lasso, forward subsect selection, and random forest. First, we applied these methods to our training dataset that we selected randomly from the complete dataset. We set the training subset as the 80% of our total number of observations. After this, we cross-validated the different models using k-fold cross-validation and compare the models level of accuracy. 


##Subset selection with lasso

The first variable selection method that we used was Lasso. This This method has two different model selection criteria: minimum lambda and the 1-SE. The first criteria indicates which variables kept being relevant predictors after controlling the overall model complexity by tuning lambda. Using the 1-SE rule we also got the same predictors than lambda-min

```{r message=FALSE, warning=FALSE, cache=TRUE}

#############################################################
## Base code to apply regsubset analysis to all data       ##
## This code is converted into functions in the following  ##
## chunk that conduct same analysis to Elementary,         ##
## Middle School and High School                           ##
#############################################################

# Clean the data for Lasso
school.glmnet <- na.omit(school.DO.train)
Y <- school.glmnet$APIHIGH
print(str(Y))
X <- school.glmnet
X$APIHIGH <-NULL
X <- as.matrix(X)
 
# Run lasso with binomial family
school.lasso <- glmnet(x= X, y = Y, family = "binomial")
plot.glmnet(school.lasso, label = TRUE)

# Conduct cross validations with k=10 and plot MSE
school.lasso.cv <- cv.glmnet(x= X, y = Y, family="binomial", nfolds = 10)
plot(school.lasso.cv)

# Get model of lambda min
lambda.min.cv <- school.lasso.cv$lambda.min
model.lambda.min <- coef(school.lasso.cv, s = "lambda.min")
subset.lambda.min <- dimnames(coef(school.lasso.cv, s = "lambda.min"))[[1]]


# Get model of lambda 1se
lambda.min.1se <- school.lasso.cv$lambda.1se
model.lambda.1se <- coef(school.lasso.cv, s = "lambda.1se")
subset.lambda.1se <- dimnames(coef(school.lasso.cv, s = "lambda.1se"))[[1]]


```

Analyzing lasso results, we got the same variables using the two decision criteria lambda-in and 1-SE. Additionally, when we evaluate the model, lasso performs poorly on prediction. For that reason, we are not using, because is not giving us relevant information to select the key determinants of school performance.

```{r message=FALSE, warning=FALSE, cache=TRUE}
#########################################################################
## Prediction using lambda min and lambda 1se with logistic regression ##
#########################################################################

# Predict class using lambda min
lambda.min.predict <- predict(school.lasso.cv, newx = X[1:nrow(school.DO.test),], s = "lambda.min", type = "class")
school.DO.test$lambda.min.pred <- lambda.min.predict

# Display confusion matrix derived from prediction of class with lasso min
conf.matrix.lambda.min <- confusionMatrix(data = school.DO.test$lambda.min.pred, reference = school.DO.test$APIHIGH)
conf.matrix.lambda.min

# Predict class using lambda 1se
lambda.1se.predict <- predict(school.lasso.cv, newx = X[1:nrow(school.DO.test),], s = "lambda.1se", type = "class")
school.DO.test$lambda.1se.predict <- lambda.1se.predict
# Display confusion matrix derived from prediction of class with lasso 1se
conf.matrix.lambda.1se <- confusionMatrix(data = school.DO.test$lambda.1se.pred, reference = school.DO.test$APIHIGH)
conf.matrix.lambda.1se

```

## Forward stepwise selection with BIC and CP

We  applied forward stepwise model selection to our dataset including the original variables and their dummies transformations. This method selects the model with the lowest residuals sum of squares (RSS) for every number of possible predictors. It starts with the intercept and adds predictors once at a time, until including all of them. We constructed different graphs to compare the decision criteria to select the best model. The options are adjusted r-squared, AIC, and BIC.

```{r message=FALSE, warning=FALSE, cache=TRUE}

#############################################################
## Base code to apply regsubset analysis to all data       ##
## This code is converted into functions in the following  ##
## chunk that conduct same analysis to Elementary,         ##
## Middle School and High School                           ##
#############################################################

# Run regsubset with "exhaustive" method
school.subset <- regsubsets(API ~ .,
                               data = school.train,
                               nbest = 1, nvmax = NULL,
                               method = "forward",  really.big = TRUE)
summary.exhaustive <- summary(school.subset)
outmat.exhaustive <- as.data.frame(summary.exhaustive$outmat)

# Get summary of regsubset and create data frame
r2.complexity <- data.frame(seq(1:length(summary.exhaustive$adjr2)))
r2.complexity$ADJR2 <- summary.exhaustive$adjr2
r2.complexity$RSS <- summary.exhaustive$rss
r2.complexity$CP <- summary.exhaustive$cp
r2.complexity$BIC <- summary.exhaustive$bic
colnames(r2.complexity) <- c("Complexity", "AR.squared", "RSS", "CP", "BIC")

```

```{r message=FALSE, warning=FALSE, cache=TRUE}
# Plot RSS vs. Complexity
ggplot(r2.complexity) +
  geom_point(aes(y = RSS, x = Complexity)) +
  labs(title = "\nRSS as a function of model complexity\n")
```

This graph presented above shows the selection results. We can see that the RSS decreases with the number of predictors, which is an expected outcome.

```{r message=FALSE, warning=FALSE, cache=TRUE}
# Plot R2 vs. Complexity

AR <- r2.complexity$Complexity[r2.complexity$AR.squared==max(r2.complexity$AR.squared)]
ggplot(r2.complexity) +
  geom_point(aes(y = AR.squared, x = Complexity)) +
  labs(title = "\nAdjusted R-squared as a function of model complexity\n") +
geom_point(data = r2.complexity[AR,], aes(y = AR.squared, x = Complexity), col = "red", size = 4)
```

The adjusted r-squared shows that if we include all the variables in the model, the most complex model will explain at maximum `r round(max(r2.complexity$AR.squared), 2)` of the binary API and correspond to the model with `r r2.complexity$Complexity[r2.complexity$AR.squared==max(r2.complexity$AR.squared)]` variables.


```{r message=FALSE, warning=FALSE, cache=TRUE}
# Get index of min CP
best.subset.size.cp <- which.min(r2.complexity$CP)
# Plot CP vs. Complexity. Code still needs to highlight minimum CP
ggplot(r2.complexity) +
  geom_point(aes(y = CP, x = Complexity)) +
  labs(title = "\nCP as a function of model complexity\n") + 
  xlim(0, 50) + geom_point(data = r2.complexity[best.subset.size.cp,], aes(y = CP, x = Complexity), col = "red", size = 4)

```

The minimum AIC estimator corresponds to a level of model complexity with `r r2.complexity$Complexity[r2.complexity$CP==min(r2.complexity$CP)]` variables.

```{r message=FALSE, warning=FALSE, cache=TRUE}
# Get index of min BIC
best.subset.size.bic <- which.min(r2.complexity$BIC)

# Plot BIC vs. Complexity.  Code still needs to highlight minimum BIC
ggplot() +
  geom_point(data = r2.complexity, aes(y = BIC, x = Complexity)) +
  labs(title = "\nBIC as a function of model complexity\n") + 
  xlim(0, 50) + geom_point(data = r2.complexity[best.subset.size.bic,], aes(y = BIC, x = Complexity), col = "red", size = 4)

```

On the other hand, the model chosen by BIC criteria corresponds to a complexity of `r r2.complexity$Complexity[r2.complexity$BIC==min(r2.complexity$BIC)]` variables.

The coefficients selected by BIC and AIC are shown in the following graphs:

```{r message=FALSE, warning=FALSE, cache=TRUE}
# Use best model size to get coefficients and plot

plotSubsetCoeff <- function (subset.object, num.predictors, metric = "") {
  coef <- coef(subset.object, id = num.predictors)
  # print(kable(as.data.frame(coef), digits = 3, col.names = "Coefficient"))
  names.coef <- names(coef)
  qplot(coef, x = names.coef, y = coef, geom = "point") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(hjust = 0.5)) +
    theme(text = element_text(size=12)) +
    labs(title = paste("\nBest subset according to", metric, "\n"), x = "Predictors",
                        y = "Coefficient") +
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
}

# Call the function to select the best subset according to BIC
plotSubsetCoeff(school.subset, best.subset.size.bic, "BIC")

# Call the function to select the best subset according to BIC
plotSubsetCoeff(school.subset, best.subset.size.cp, "CP")

# Get final models
coefs.subset.bic <- data.frame(coef(school.subset, id = best.subset.size.bic))
subset.regsubset.bic <- dimnames(coefs.subset.bic)[[1]]
coefs.subset.cp <- data.frame(coef(school.subset, id = best.subset.size.cp))
subset.regsubset.cp <- dimnames(coefs.subset.cp)[[1]]

# Print final models
kable(subset.regsubset.bic, col.names = "Subset Forward BIC")
kable(subset.regsubset.cp, col.names = "Subset Forward CP")

```


### K-Fold Cross validation

As we explained before, we did not include in our analysis the lasso method because we considered that it does not contribute to our discussion regarding the key determinants of school success because the two criteria selected the maximum number of variables. So, we compared Forward stepwise selection with randome forests using K-Fold cross validation. Our results in terms of accuracy are shown in the next table:   

```{r message=FALSE, warning=FALSE, cache=TRUE}
##################################
## Cross validation using forward BIC ##
##################################

# Get data frames with subset selected by BIC
school.bic.train <- dplyr::select(school.DO.train, APIHIGH, AA_SIGYes, AS_SIGYes, WH_SIGYes, SD_SIGYes,
                                  ACS_CORE6, ACS_CORE8, ACS_CORE43, EMER_PCT, ACS_CORE3)
school.bic.test <- dplyr::select(school.DO.test, APIHIGH, AA_SIGYes, AS_SIGYes, WH_SIGYes, SD_SIGYes,
                                 ACS_CORE6, ACS_CORE8, ACS_CORE43, EMER_PCT, ACS_CORE3)

# Get cross-validated misclassification rate
k.cv <- 10
bic.accuracy <- numeric(k.cv)

for (fold in 1:k.cv) {
  index.fold <- sample(nrow(school.bic.train), nrow(school.bic.train)/k.cv)
  bic.train.test  <- school.mid.DO[-index.fold,]
  bic.train.train <-school.mid.DO[index.fold,]
  log.bic.fit <- glm(APIHIGH ~., family = "binomial"(link='logit'), data = bic.train.train)
  log.bic.predict <-  predict(log.bic.fit, type = "response", bic.train.test)
  bic.train.test$log.bic.predict <- log.bic.predict
  bic.train.test <- dplyr::filter(bic.train.test, log.bic.predict != "NA")
  pred.bic <- numeric(nrow(bic.train.test))
  for (row in 1:nrow(bic.train.test)) {
    if (bic.train.test$log.bic.predict[row] > 0.5) {
      pred <- 1
    } else {
      pred <- 0
    }
    pred.bic[row] <- pred
  }
  bic.train.test$pred.bic <- pred.bic
  
  conf.matrix.bic <- confusionMatrix(data = bic.train.test$pred.bic,
                                   reference = bic.train.test$APIHIGH)
  result <- conf.matrix.bic$overall[1]
  bic.accuracy[fold] <- result
}
bic.accuracy <- round(mean(bic.accuracy), 4)
bic.miss.error <- 1 - bic.accuracy
```


```{r message=FALSE, warning=FALSE, cache=TRUE}
##################################
## Cross Validation using forward CP ##
##################################

# Get data frames with subset selected by CP
school.cp.train <- dplyr::select(school.DO.train, APIHIGH, AA_SIGYes, AS_SIGYes, WH_SIGYes, SD_SIGYes, AVG_ED, FULL_PCT, EMER_PCT,
                                  ACS_CORE1, ACS_CORE13, ACS_CORE15, ACS_CORE16, ACS_CORE2, ACS_CORE6, ACS_CORE8, ACS_CORE9,
                                  ACS_CORE42, ACS_CORE43, ACS_CORE3, ACS_CORE38)
school.cp.test <- dplyr::select(school.DO.test, APIHIGH, AA_SIGYes, AS_SIGYes, WH_SIGYes, SD_SIGYes, AVG_ED, FULL_PCT, EMER_PCT,
                                  ACS_CORE1, ACS_CORE13, ACS_CORE15, ACS_CORE16, ACS_CORE2, ACS_CORE6, ACS_CORE8, ACS_CORE9,
                                  ACS_CORE42, ACS_CORE43, ACS_CORE3, ACS_CORE38)


# Get cross-validated misclassification rate
k.cv <- 10
cp.accuracy <- numeric(k.cv)

for (fold in 1:k.cv) {
  index.fold <- sample(nrow(school.cp.train), nrow(school.cp.train)/k.cv)
  cp.train.test  <- school.mid.DO[-index.fold,]
  cp.train.train <-school.mid.DO[index.fold,]
  log.cp.fit <- glm(APIHIGH ~., family = "binomial"(link='logit'), data = cp.train.train)
  log.cp.predict <-  predict(log.cp.fit, type = "response", cp.train.test)
  cp.train.test$log.cp.predict <- log.cp.predict
  cp.train.test <- dplyr::filter(cp.train.test, log.cp.predict != "NA")
  pred.cp <- numeric(nrow(cp.train.test))
  for (row in 1:nrow(cp.train.test)) {
    if (cp.train.test$log.cp.predict[row] > 0.5) {
      pred <- 1
    } else {
      pred <- 0
    }
    pred.cp[row] <- pred
  }
  cp.train.test$pred.cp <- pred.cp
  
  conf.matrix.cp <- confusionMatrix(data = cp.train.test$pred.cp,
                                   reference = cp.train.test$APIHIGH)
  result <- conf.matrix.cp$overall[1]
  cp.accuracy[fold] <- result
}
cp.accuracy <- round(mean(cp.accuracy), 4)
cp.miss.error <- 1 - cp.accuracy
```



```{r message=FALSE, warning=FALSE, cache=TRUE}
#####################################
## Cross validation Random forest ##
###################

# Get cross-validated misclassification rate
k.cv <- 10
rf.accuracy.list <- numeric(k.cv)

for (fold in 1:k.cv) {
  index.fold <- sample(nrow(school.DO.train), nrow(school.DO.train)/k.cv)
  rf.train.test  <- school.DO.train[-index.fold,]
  rf.train.train <-school.DO.train[index.fold,]
  rf.fit <- randomForest(as.factor(APIHIGH) ~ .,
                            data = school.DO.train[,-10],
                            mtry = 8, ntree = 100,
                            importance = TRUE, na.action = na.omit)
  rf.pred <-  predict(rf.fit, rf.train.test, type = "response")
  
  conf.matrix.rf <- confusionMatrix(data = rf.pred, reference = rf.train.test$APIHIGH)
    result <- conf.matrix.rf$overall[1]
  rf.accuracy.list[fold] <- result
}
rf.accuracy <- round(mean(rf.accuracy.list), 4)
rf.miss.rate <- 1 - rf.accuracy

```


```{r message=FALSE, warning=FALSE, cache=TRUE}
################################
## Random forest fit ####
########################

school.rf <- randomForest(as.factor(APIHIGH) ~ ., data = school.DO.train[,-10], mtry = 8, ntree = 100, importance = TRUE, na.action = na.omit)
rf.test.preds <-  predict(school.rf, school.DO.test, type = "response")
rf.conf.matrix <- confusionMatrix(data = rf.test.preds, reference = school.DO.test$APIHIGH)
importance.table <- importance(school.rf)
importance.df <- as.data.frame(importance.table)
importance.df <- cbind(importance.df, variable = rownames(importance.df))
importance.df <- arrange(importance.df, desc(MeanDecreaseAccuracy))
importance.df <- importance.df[1:10, ]
importance.names <- importance.df$variable

```



```{r message=FALSE, warning=FALSE, cache=TRUE}
#####################################
## Comparison of cross validated ##
#####################################

# Get overall accuracy for all methods
# bic.accuracy
# cp.accuracy
# rf.accuracy

# Create data frame
models <- c("ForwardCP", "ForwardBIC", "RandomForest")
num.predictors <- c(length(subset.regsubset.cp), length(subset.regsubset.bic), length(importance.table[,1]))
accuracy <- c(cp.accuracy, bic.accuracy, rf.accuracy)
misclassification <- c(1-cp.accuracy, 1-bic.accuracy, 1-rf.accuracy)

comparison.table <- data.frame(models, num.predictors, accuracy, misclassification)
kable(comparison.table, col.names = c("Method", "Number of Predictors", "Overall Accuracy", "Missclassification Rate"), align = "c")

```
Our results table shows the comparison in terms of overall accuracy of the cross-validation process for the stepwise and the random forest models. Based on the accuracy lebel we selected the random forest model because it has 0.945 accuracy rate which is much higher than the 0.74 rate of accuracy we got from the BIC and AIC criteria.

##Performance of final model

After selecting random forest as the model with the highest level of accuracy in the training subset, we predicted the test subsample data for the 10 most relevant variables based on the random forest results object. We decided 10 as the number of variables because was the minimum number of variables that we got when comparing with the other variable selection methods we cross-validate. The results showed a level of accuracy of 0.8616.

```{r message=FALSE, warning=FALSE, cache=TRUE}

varImpPlot(school.rf, n.var = 10, cex = 0.75)
rf.conf.matrix

```

##Conclusion part 1
- We compared the performance of a logistic regression utilizing a subset selected with forward stepwise selection using the criteria BIC and AIC against the performance of random forest. We found that the performance of random forest was better in terms of accuracy. Therefore, we decided to use random forest to select the subset of predictors that are more important to predict API High. 
- Given that the number of predictors selected by logistic regressiOn using forward with BIC was 10, we decided to select the top 10 variables with higher importance according to random forest.
- Therefore, the subset of variables that is most important to predict API High is given by the top 10 variables identified by random forests. These variables are the following `r importance.names`.


#Question 2: Top variables at school level
Intro. Why we are only using random forest. 

##Comparison of most important predictor by school level
```{r message=FALSE, warning=FALSE, cache=TRUE}

#########################################################
## Helper function to re-run analysis on school levels ##
#########################################################

topTenRandomForest <- function(train.data, test.data){
  temp.rf <- randomForest(as.factor(APIHIGH) ~ ., data = train.data[,-10],
                                mtry = 8, ntree = 100, importance = TRUE, na.action = na.omit)
  temp.importance <- importance(temp.rf)
  importance.df <- as.data.frame(temp.importance)
  importance.df <- cbind(importance.df, variable = rownames(importance.df))
  importance.df <- arrange(importance.df, desc(MeanDecreaseAccuracy))
  importance.df <- importance.df[1:10, ]
  top.ten <- importance.df$variable
  top.ten
}

```


```{r message=FALSE, warning=FALSE, cache=TRUE}
# Get top ten variables by school level
top.ten.ele <- topTenRandomForest(school.ele.DO.train, school.ele.DO.test)
top.ten.mid <- topTenRandomForest(school.mid.DO.train, school.mid.DO.test)
top.ten.high <- topTenRandomForest(school.high.DO.train, school.high.DO.test)

# Create index of variables
index.rf.comparison <- seq(1:10)
comp.rf.school.level <- data.frame(index.rf.comparison, top.ten.ele, top.ten.mid, top.ten.high)

kable(comp.rf.school.level, col.names = c("Variable", "Top10.Elementary", "Top10.MidSchool",
                                          "Top10.HighSchool"), align = "c")

```

##Comparison of variables and coefficients

Following our comparison of each model in question 1, we have decided to exclusively use random forest to identify important prediction variables for each grade level. As mentioned elsewhere, this decision was made due to the accuracy of the random forest when compared to other models. Some of key findings include the following:

-	All grade levels share a number of important variables for predicting API score. The following variables appear in the top ten for each grade level: AVG_ED, WH_SIGYes, SD_SIGYes, AS_SIGYes, NUMTEACH, FULL_PCT, YRONE_TCH, YRS_TEACH.

-	The average education of the school's parents is the most important variable when predicting a school's AIP score, according to our random forest model. This is true across all grade levels.

-	Across all grade levels, race is an important predictor of a school's API score, according to our random forest model. All variables associated with a significant population of a certain race (i.e. WH_SIGYes for a school with a high population of white students) appear in all rankings, though their order vary slightly depending on the grade level.

-	Variables related to school's teaching staff are important in predicting a school's API scores. The percentage of full time teachers, the number of new teachers (i.e. teachers with only one of experience), and the overall experience of a school's teachers appear in the top ten for each grade level, though their order varies. 

-	VALID, which indicates the number of students tested, is an important predictor only for elementary schools. Similarly, ACS_CORES, which is the number of core course offered at school, is only important in predicting API scores for high schools.

##Conclusion: 

-	The average education of a school's parents, the racial make up the school, and the experience of the teaching staff appear to be the best predictors of a school's API score, regardless of grade level.


```{r message=FALSE, warning=FALSE, cache=TRUE}
# Create table with top 10 variables and coefficients pero school level

```



##Question 3: Ethnic factors
The question is: Are ethnic factors significant predictors of school performance? Is their importance consistent across school levels?

From the models produced for question 1 and 2, we see that race is an important predictor for a school's API score. Variables denoting a school has having a significant population of students that are African American, White, and Hispanic were present in our random forest output of variable importance. This was true for all grade levels, as well as at each individual grade level. We believe that race is an important predictor regardless of grade level. However, we furthered explored this relationship through other methods to determine the extent to which race was an important predictor. 


##Principal components analysis

Our first approach was applying a Multiple Correspondence Analysis. This analysis works similarly to a Principal Component Analysis but is better suited for race factor variables included in our data. [FIG.] shows the Multiple Correspondence Analysis graphic, with variables that are on opposite sides of the axis are contrasting. Schools where the population of Asian Americans and Whites are significant appear to contrast with school where the population of African American and Hispanic significant. This plot suggests that there may be segregation occurring between schools, especially between the White population and the African Americans/Hispanics population. 

```{r message=FALSE, warning=FALSE, cache=TRUE}
#MCA for race
mca1.1 <- school.train[,c("AA_SIG", "AS_SIG", "WH_SIG", "HI_SIG")]
mca1 <- apply(mca1.1, 2, function(x) nlevels(as.factor(x)))
mca2 <- MCA(mca1.1, graph = FALSE)

mca1_vars_df <- data.frame(mca2$var$coord, Variable = rep(names(mca1), mca1))
mca1_obs_df = data.frame(mca2$ind$coord)

#plot for MCA
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA Plot for Race") +
  scale_colour_discrete(name = "Variable")


# Exploitation of MCA and code adaptation are from : http://www.gastonsanchez.com/visually-enforced/how-to/2012/10/13/MCA-in-R/

```


##Overlaid histograms

We then plotted our predicted probability of a school being labeled as high performing using our random forest model from question 1. [fig.] shows the random forest's predicted probability between school with significant white populations (1) and schools without a significant White population (0). According to our random forest model, schools with a significant White population were more likely to be classified having a "high API score." The opposite is true for schools without a significant White population. We then overlaid a histogram for the random forest predicted probability of scoring a high API scores for schools with a high population of economically disadvantaged students and compared with those schools that were not economically disadvantaged. The output suggests that even where there is a high economically disadvantaged population, schools with significate population of White students were more likely to have a high API score, according to our random forest output. 

```{r message=FALSE, warning=FALSE, cache=TRUE}

rf.test.preds <- predict(school.rf, school.DO.test, type = "prob")

rf.test.probs <- cbind(rf.test.preds, school.DO.test)
rf.test.probs <- na.omit(rf.test.probs)
rf.test.probs$class <- ifelse(rf.test.probs$`0` >= 0.5, 0, 1)
rf.test.probs$`0` <- NULL

ggplot(rf.test.probs, aes(x = `1`, fill = as.factor(SD_SIGYes))) +
  geom_histogram(aes(alpha = 0.5)) +
  facet_wrap(~WH_SIGYes) +
  labs(title = paste("\nDistribution of Performance by Race and Class\n"), x = "Probability from Random Forest",
       y = "Count")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=12)) + 
  theme(legend.position="bottom") +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  guides(fill=guide_legend(title="Economic Disadvantage")) +
  scale_alpha(guide = 'none')
  
  

```


##Association rules

Lastly, we applied an association rule to each grade level to confirm that this pattern of racial segregation and API scores were accurate at each grade level. [fig] maps an association rule between race, economic standing, and API score for each level. For example, [fig] contains a cluster confirm our findings: schools with a significant population of African Americans, Hispanics, or Economical Disadvantaged students were associated with a low API scores. Conversely, schools with significant populations of Whites, Asian Americans, and not Economically Disadvantaged students were associated with high API scores. These results were similar at each grade level.

```{r message=FALSE, warning=FALSE, cache=TRUE,results= FALSE}
#association rules for students
#code taken from https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/
race.df <- data.frame(white = as.factor(school.DO.train$WH_SIGYes),
                      asian = as.factor(school.DO.train$AS_SIGYes),
                      hispanic = as.factor(school.DO.train$HI_SIGYes),
                      black = as.factor(school.DO.train$AA_SIGYes),
                      poor = as.factor(school.DO.train$SD_SIGYes),
                      performance = as.factor(school.DO.train$APIHIGH))
rules <- apriori(race.df)
rules <- apriori(race.df,
                parameter = list(minlen = 2, supp = 0.005, conf = 0.8),
                appearance = list(rhs = c("performance=0", "performance=1"),
                    default = "lhs"))
rules.sorted <- sort(rules, by = "lift")
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1
rules.pruned <- rules.sorted[!redundant]

race.df.h <- data.frame(white = as.factor(school.high.DO.train$WH_SIGYes),
                      asian = as.factor(school.high.DO.train$AS_SIGYes),
                      hispanic = as.factor(school.high.DO.train$HI_SIGYes),
                      black = as.factor(school.high.DO.train$AA_SIGYes),
                      poor = as.factor(school.high.DO.train$SD_SIGYes),
                      performance = as.factor(school.high.DO.train$APIHIGH))
rulesh <- apriori(race.df.h)
rulesh <- apriori(race.df.h,
                parameter = list(minlen = 2, supp = 0.005, conf = 0.8),
                appearance = list(rhs = c("performance=0", "performance=1"),
                    default = "lhs"))
rules.sortedh <- sort(rulesh, by = "lift")
subset.matrixh <- is.subset(rules.sortedh, rules.sortedh)
subset.matrixh[lower.tri(subset.matrixh, diag=T)] <- NA
redundanth <- colSums(subset.matrixh, na.rm = T) >= 1
rules.prunedh <- rules.sorted[!redundanth]

race.df.m <- data.frame(white = as.factor(school.mid.DO.train$WH_SIGYes),
                      asian = as.factor(school.mid.DO.train$AS_SIGYes),
                      hispanic = as.factor(school.mid.DO.train$HI_SIGYes),
                      black = as.factor(school.mid.DO.train$AA_SIGYes),
                      poor = as.factor(school.mid.DO.train$SD_SIGYes),
                      performance = as.factor(school.mid.DO.train$APIHIGH))
rulesm <- apriori(race.df.m)
rulesm <- apriori(race.df.m,
                parameter = list(minlen = 2, supp = 0.005, conf = 0.8),
                appearance = list(rhs = c("performance=0", "performance=1"),
                    default = "lhs"))
rules.sortedm <- sort(rulesm, by = "lift")
subset.matrixm <- is.subset(rules.sortedm, rules.sortedm)
subset.matrixm[lower.tri(subset.matrixm, diag=T)] <- NA
redundantm <- colSums(subset.matrixm, na.rm = T) >= 1
rules.prunedm <- rules.sorted[!redundantm]

race.df.e <- data.frame(white = as.factor(school.ele.DO.train$WH_SIGYes),
                      asian = as.factor(school.ele.DO.train$AS_SIGYes),
                      hispanic = as.factor(school.ele.DO.train$HI_SIGYes),
                      black = as.factor(school.ele.DO.train$AA_SIGYes),
                      poor = as.factor(school.ele.DO.train$SD_SIGYes),
                      performance = as.factor(school.ele.DO.train$APIHIGH))
rulese <- apriori(race.df.e)
rulese <- apriori(race.df.e,
                parameter = list(minlen = 2, supp = 0.005, conf = 0.8),
                appearance = list(rhs = c("performance=0", "performance=1"),
                    default = "lhs"))
rules.sortede <- sort(rulese, by = "lift")
subset.matrixe <- is.subset(rules.sortede, rules.sortede)
subset.matrixe[lower.tri(subset.matrixe, diag=T)] <- NA
redundante <- colSums(subset.matrixe, na.rm = T) >= 1
rules.prunede <- rules.sorted[!redundante]
```

```{r message=FALSE, warning=FALSE, cache=TRUE}
r<-plot(rules, method = "graph", control = list(itemLabels = TRUE),main="Association RuleS for All Grade Levels") 
rhi<-plot(rulesh, method = "graph", control = list(itemLabels = TRUE),main="Association RuleS for High School")
rm<-plot(rulesm, method = "graph", control = list(itemLabels = TRUE),main="Association Rules for Middle School")
re <-plot(rulese, method = "graph", control = list(itemLabels = TRUE),main="Association Rules for Elementary School")
```
##Conclusion: 

Our random forest models from question 1 and 2 provide evidences that the racial make up of a school is a significant predictor to school performance. Variables that demarked that a school had a significant population of Whites, Hispanic, Asian Americans, and African American appeared in all random forest models. Moreover, our Multiple Correspondence Analysis and Association Rule analysis suggests that there is a level of racial segregation occurring across schools, with African and Hispanics segregated from their White peers. Such findings appear to confirm concerns that minority populations in American may be subject to segregation into lower performing and lower quality schools.


# Question 4: Reallocating teacher
The questions is: Could discrepancies in performance be reduced by actively reallocating teachers?

## PCA analysis for variables associated to teachers

```{r message=FALSE, warning=FALSE, cache=TRUE}

df.teachers <- na.omit(data.frame(full.training = school.DO$FULL_PCT, num.teachers = school.DO$NUMTEACH, emer.cred = school.DO$EMER_PCT, yrs.teach = school.DO$YRS_TEACH, teach.waiver = school.DO$WVR_PCT, school.DO$APIHIGH))
 
df <- df.teachers[c(1,2,3,4,5)]
 
autoplot(prcomp(df), data = df.teachers, colour = 'school.DO.APIHIGH', loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, alpha=0.3)

```

We selected from our dataset the features that by definition are associated with teachers: 

* EMER_PCT: percentage of teacher with emergency credentials
* WVR_PCT: Percentage of teacher with waiver
* YRS_TEACH: The average number of years that all teachers have been instructing students 
* YRONE_TCH: Number of first-year teacher
* YRTWO_TCH: Number of second-year teacher 
* NUMTEACH: The number of teachers at the school
* FULL_PCT: Percentage of teachers who have completed a teacher preparation program

The PCA result shows that among the seven variables, the loading vector places approximately equal weight on number of teachers and teachers' years of experience. The two variables are very far from each other which means that both variables are not correlated with each other. So, we can discuss both variables separately based on the following analysis based on the results from the random forest results.

##Partial plot analysis
To answer the question of whether teachers could actively be reallocated to improve school performance, we wanted to examine how the variables describing a school's teachers were associated with that school's performance. Our approach was to use partial dependence plots based on our random forest model to visualize these relationships. We thought that if the likelihood of being classified as a high performing school leveled out after a certain value of one of these variables, it would suggest diminishing returns (or diminishing harms) to that variable. For example, if a partial dependence plot showed that the likelihood of being classified as a high performing school increased as the percentage of teachers who had completed a full training increased, but leveled off after a certain percentage - for example 30% - we might be able to say that it would be possible to reallocate teachers from schools with above 30% of this variables to schools that had below 30% and the performance would not necessarily suffer if the same total number of teachers was maintained. The plots below show the log probability that a school will be classified as being a high performing school (`APIHIGH` = 1) in relation to the different variables that describe teachers.

```{r message=FALSE, warning=FALSE, cache=TRUE}

#Variables about teachers
par(mfrow = c(3,3))
partialPlot(school.rf, na.omit(school.DO.train[,-10]), x.var = EMER_PCT, which.class = 1, plot=TRUE, rug = TRUE, main = "EMER_PCT")
partialPlot(school.rf, na.omit(school.DO.train[,-10]), x.var = WVR_PCT, which.class = 1, plot=TRUE, rug = TRUE, main = "WVR_PCT")
partialPlot(school.rf, na.omit(school.DO.train[,-10]), x.var = YRONE_TCH, which.class = 1, plot=TRUE, rug = TRUE, main = "YRONE_TCH")
partialPlot(school.rf, na.omit(school.DO.train[,-10]), x.var = YRTWO_TCH, which.class = 1, plot=TRUE, rug = TRUE, main = "YRTWO_TCH")
partialPlot(school.rf, na.omit(school.DO.train[,-10]), x.var = NUMTEACH, which.class = 1, rug = TRUE, main = "NUMTEACH")
partialPlot(school.rf, na.omit(school.DO.train[,-10]), x.var = FULL_PCT, which.class = 1, rug = TRUE, main = "FULL_PCT")
partialPlot(school.rf, na.omit(school.DO.train[,-10]), x.var = YRS_TEACH, which.class = 1, plot=TRUE, rug = TRUE, main = "YRS_TEACH")

```

As the partial dependence plots above demonstrate, we do not necessarily see the leveling off patterns we were looking for when assessing the extent to which teachers could be actively reassigned. Even though the plots for the variables `EMER_PCT`, `YRONE_TCH`, `YRTWO_TCH`, and `NUMTEACH` appear to level off after a certain point, the rug plots overlaid onto these partial dependence plots indicate that this leveling off occurs outside the range of most of our data. For this reason, we cannot say with much confidence that there are diminishing returns or diminishing harms associated with a these variables after a given cutoff. However, if a policy-maker wanted to established a minimum likelihood of achieving high performance as defined in this project, using this types of plots could help to ensure that each school had the number or percentage of teachers with these characteristics to ensure these conditions for success.

##Conclusion

The results of this project mostly confirm expectations we had when started. The average education of parents, the racial makeup of the schools, and the experience of the teaching faculty all proved to be highly important predictors of the success of schools (based on whether they fell above the 50th percentile of API scores). However, there were some limitations to our methods that mean we are less confident in our findings than we would like.
First, the variables used to predict school performance, as well as the outcome variable itself, are not very detailed. They are mostly dichotomous or categorical variables when we would have preferred to have used continuous variables. For example, rather than simply knowing if a school had a "significant" Hispanic population, we would have liked to have an actual percentage. Similarly, rather than simply knowing whether a school was above the 50th percentile of scores does not seem sufficient; we would have like to have known the actual scores. This would have allowed us to make more precise differentiations between schools. Using series of dummy variables instead of continuous variables might have changed the way our models work and their performance. For example, we were expecting the Lasso model we implemented to perform much better and be more informative for our purposes than it was. One unanswered question we have is what the performance and usefulness of the model would have been if we had used different formulations of the variables we had.
Second, and related to the issue above, is the amount of missing data in our set. During our cleaning process we noted that many of the observations simply had a `?` for certain variables. Not using this missing values probably affected our ability to draw good conclusions from our data. To give a concrete example of how this problem and the problem above relate, we had so many missing values for the number of core courses variable that we had to create dummies for each number of core courses (43 in total).
For future investigations, we would like to fill in the gaps mentioned above, but also find more information on charter schools and core courses. We think that these aspects of a school might provide interesting nuance to our understanding of school performance.
