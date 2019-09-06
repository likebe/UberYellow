# Thesis Analysis

library("xlsx")
my_data <- read.xlsx(file.choose(), 1)
attach(my_data)
head(my_data)

# make all factors numeric:
for (col in 1:ncol(data)){
  as.numeric(as.character(data[col,]))
}

# add a uber ratio variable, in which uberRatio = #uber/(#uber + #yellow)
my_data$UberRatio <- as.numeric(as.character(my_data$Uber))/(as.numeric(as.character(my_data$Uber)) + as.numeric(as.character(my_data$Yellow)))

# add a totalCrime variable that adds all the crimes
my_data$TotalCrime <- as.numeric(as.character(my_data$Robbery)) + 
  as.numeric(as.character(my_data$Weapons)) +
  as.numeric(as.character(my_data$Rape)) +
  as.numeric(as.character(my_data$Drugs)) +
  as.numeric(as.character(my_data$Intoxicated)) +
  as.numeric(as.character(my_data$Harrassment)) +
  as.numeric(as.character(my_data$Kidnapping)) +
  as.numeric(as.character(my_data$Trespass))

# dealing with missing data (can also be done in excel)
# see which rows are incomplete
incomplete_rows <- my_data[!complete.cases(my_data),]

# data without missing values
data <- na.omit(my_data, invert = FALSE)
head(data)

# without considering socio-economic factors
myvars <- c("Robbery", "Weapons", "Rape", "Drugs", "Intoxicated", "Uber", "Yellow", "Harrassment"
            , "Kidnapping", "Trespass")

# consider the shape of UberRatio/Uber/Yellow
hist(data$UberRatio)
data$Uber <- as.numeric(as.character(data$Uber))
data$Yellow <- as.numeric(as.character(data$Yellow))
hist(data$Uber)
hist(data$Yellow)

# recode the age variables
data$Lessthan18 <- data$Year0 + data$Year1 + data$Year2 + data$Year3
data$Between18and35 <- data$Year4 + data$Year5
data$Between35and65 <- data$Year6 + data$Year7 + data$Year8
data$Above65 <- data$Year9 + data$Year10 + data$Year11

# convert data from factors to numerics
data$MedianHouseholdIncome <- as.numeric(as.character(data$MedianHouseholdIncome))
data$AvgComToWork <- as.numeric(as.character(data$AvgComToWork))
data$Gini <- as.numeric(as.character(data$Gini))
data$HighSchoolDropOutRate <- as.numeric(as.character(data$HighSchoolDropOutRate))
data$AvgFamilyIncome <- as.numeric(as.character(data$AvgFamilyIncome))
data$MedianFamilyIncome <- as.numeric(as.character(data$MedianFamilyIncome))
data$TotalPop <- as.numeric(as.character(data$TotalPop))
data$PopDensity <- as.numeric(as.character(data$PopDensity))
data$AmerIndian.Alaska <- as.numeric(as.character(data$AmerIndian.Alaska))
data$PacificIslanders <- as.numeric(as.character(data$PacificIslanders))
data$OtherRaceAlone <- as.numeric(as.character(data$OtherRaceAlone))
data$TwoOrMoreRaces <- as.numeric(as.character(data$TwoOrMoreRaces))
data$PopulationOlderThan25 <- as.numeric(as.character(data$PopulationOlderThan25))
data$LessThanHighSchool <- as.numeric(as.character(data$LessThanHighSchool))
data$HighSchool <- as.numeric(as.character(data$HighSchool))
data$SomeCollege <- as.numeric(as.character(data$SomeCollege))
data$Master <- as.numeric(as.character(data$Master))
data$ProfSchool <- as.numeric(as.character(data$ProfSchool))
data$Doctorate <- as.numeric(as.character(data$Doctorate))
data$DropOut <- as.numeric(as.character(data$DropOut))
data$Remained <- as.numeric(as.character(data$Remained))
data$Household.Income <- as.numeric(as.character(data$Household.Income))
data$Robbery <- as.numeric(as.character(data$Robbery)) 
data$Weapons <-  as.numeric(as.character(data$Weapons))
data$Rape <- as.numeric(as.character(data$Rape))
data$Drugs <-  as.numeric(as.character(data$Drugs))
data$Intoxicated <-  as.numeric(as.character(data$Intoxicated))
data$Harrassment <-  as.numeric(as.character(data$Harrassment))
data$Kidnapping <- as.numeric(as.character(data$Kidnapping))
data$Trespass <- as.numeric(as.character(data$Trespass))
data$Yellow <- as.numeric(as.character(data$Yellow))

# check whether there are still non-numeric columns
for (col in 1:ncol(data)){
  print(is.numeric(data[,col]))
}

# correlation table
cor(data)

# check missing values
incomplete_row <- data[!complete.cases(data),]
data <- na.omit(data)

# model comparison: zero-inflated Poisson vs. regular Poission using the full model that includes:
# Uber, TotalCrime, Income, Age (between18-35), Edu(highschooldropoutrate), 
# AvgCommuteTowork, Races (black, asian)

##### FOR UBER DATA FIRST #################################################################
# Regular Poission Model
my.glm1.uber <-glm(data$Uber ~ data$TotalCrime + data$Lessthan18 
                   + data$AvgComToWork + data$AsianAlone +
                     data$Gini + data$HighSchoolDropOutRate, family = poisson, data = data)
summary(my.glm1.uber)

###########################################################################################
###########################################################################################
###########################################################################################
# Standard Poisson Model Regression Table
my.glm1.standard.uber <-glm(data$Uber ~ data$TotalCrime + data$Above65 
                            + data$AvgComToWork + data$Bachelor + data$TotalPop +
                              data$Residents.Mobility +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm1.standard.uber)


data$totalpickup <- data$Uber + data$Yellow
mean(data$AvgComToWork)
sd(data$AvgComToWork)
UberPickup <- data$Uber
hist(UberPickup)
YellowPickup <- data$Yellow
hist(YellowPickup)

# -residents mobility
my.glm2.standard.uber <-glm(data$Uber ~ data$TotalCrime + data$Above65 
                            + data$AvgComToWork + data$Bachelor + data$TotalPop +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm2.standard.uber)

# - avg commute to work
my.glm3.standard.uber <-glm(data$Uber ~ data$TotalCrime + data$Above65 
                           + data$Bachelor + data$TotalPop +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm3.standard.uber)

# - education
my.glm4.standard.uber <-glm(data$Uber ~ data$TotalCrime + data$Above65 
                             + data$TotalPop +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm4.standard.uber)

# - age
my.glm5.standard.uber <-glm(data$Uber ~ data$TotalCrime 
                            + data$TotalPop +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm5.standard.uber)

# - income
my.glm6.standard.uber <-glm(data$Uber ~ data$TotalCrime 
                            + data$TotalPop, family = poisson, data = data)

summary(my.glm6.standard.uber)

# - population
my.glm7.standard.uber <-glm(data$Uber ~ data$TotalCrime 
                            , family = poisson, data = data)

summary(my.glm7.standard.uber)

library(sjPlot)
library(snakecase)
tab_model(my.glm7.standard.uber ,my.glm6.standard.uber, my.glm5.standard.uber, my.glm4.standard.uber, 
          my.glm3.standard.uber, my.glm2.standard.uber, my.glm1.standard.uber, transform = NULL, digits = 6)

######################################################################################
################################## END OF THE STANDARD POISSON MODEL #################
######################################################################################


###########################################################################################
##############################  Manually ZERO-INFLATED Model ############################
###########################################################################################


# Classify into two groups
# the Uber/Yellow positive pickup group
positive_Uber_pickups <- data[ which(data$Uber > 0), ]
positive_Yellow_pickups <- data[ which(data$Yellow > 0), ]

# the Uber/Yellow zero pickup group
Uber_zero_pickup <- data[ which(data$Uber == 0), ]
Yellow_zero_pickup <- data[ which(data$Yellow == 0), ]

###################################
# Positive Poisson Regression Table
###################################
# Uber
my.glm1.positive.uber <-glm(positive_Uber_pickups$Uber ~ positive_Uber_pickups$TotalCrime + positive_Uber_pickups$Above65 
                            + positive_Uber_pickups$AvgComToWork + positive_Uber_pickups$Bachelor + positive_Uber_pickups$TotalPop +
                              positive_Uber_pickups$Residents.Mobility +
                              positive_Uber_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm1.positive.uber)

# Yellow
my.glm1.positive.yellow <-glm(positive_Yellow_pickups$Yellow ~ positive_Yellow_pickups$TotalCrime + positive_Yellow_pickups$Above65 
                            + positive_Yellow_pickups$AvgComToWork + positive_Yellow_pickups$Bachelor + positive_Yellow_pickups$TotalPop +
                              positive_Yellow_pickups$Residents.Mobility +
                              positive_Yellow_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm1.positive.yellow)

library(plm)
phtest(my.glm1.positive.uber, my.glm1.positive.yellow)


# -residents mobility
# Uber
my.glm2.positive.uber <-glm(positive_Uber_pickups$Uber ~ positive_Uber_pickups$TotalCrime + positive_Uber_pickups$Above65 
                            + positive_Uber_pickups$AvgComToWork + positive_Uber_pickups$Bachelor + positive_Uber_pickups$TotalPop +
                              positive_Uber_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm2.positive.uber)

# Yellow
my.glm2.positive.yellow <-glm(positive_Yellow_pickups$Yellow ~ positive_Yellow_pickups$TotalCrime + positive_Yellow_pickups$Above65 
                              + positive_Yellow_pickups$AvgComToWork + positive_Yellow_pickups$Bachelor + positive_Yellow_pickups$TotalPop +
                                positive_Yellow_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm2.positive.yellow)

# - avg commute to work
# Uber
my.glm3.positive.uber <-glm(positive_Uber_pickups$Uber ~ positive_Uber_pickups$TotalCrime + positive_Uber_pickups$Above65 
                            + positive_Uber_pickups$Bachelor + positive_Uber_pickups$TotalPop +
                              positive_Uber_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm3.positive.uber)

# Yellow
my.glm3.positive.yellow <-glm(positive_Yellow_pickups$Yellow ~ positive_Yellow_pickups$TotalCrime + positive_Yellow_pickups$Above65 
                              + positive_Yellow_pickups$Bachelor + positive_Yellow_pickups$TotalPop +
                                positive_Yellow_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm3.positive.yellow)


# - education
# Uber
my.glm4.positive.uber <-glm(positive_Uber_pickups$Uber ~ positive_Uber_pickups$TotalCrime + positive_Uber_pickups$Above65 
                            + positive_Uber_pickups$TotalPop +
                              positive_Uber_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm4.positive.uber)

# Yellow
my.glm4.positive.yellow <-glm(positive_Yellow_pickups$Yellow ~ positive_Yellow_pickups$TotalCrime + positive_Yellow_pickups$Above65 
                              + positive_Yellow_pickups$TotalPop +
                                positive_Yellow_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm4.positive.yellow)

# - age
# Uber
my.glm5.positive.uber <-glm(positive_Uber_pickups$Uber ~ positive_Uber_pickups$TotalCrime 
                            + positive_Uber_pickups$TotalPop +
                              positive_Uber_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm5.positive.uber)

# Yellow
my.glm5.positive.yellow <-glm(positive_Yellow_pickups$Yellow ~ positive_Yellow_pickups$TotalCrime 
                              + positive_Yellow_pickups$TotalPop +
                                positive_Yellow_pickups$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm5.positive.yellow)

# - income
# Uber
my.glm6.positive.uber <-glm(positive_Uber_pickups$Uber ~ positive_Uber_pickups$TotalCrime 
                            + positive_Uber_pickups$TotalPop , family = poisson, data = data)

summary(my.glm6.positive.uber)

# Yellow
my.glm6.positive.yellow <-glm(positive_Yellow_pickups$Yellow ~ positive_Yellow_pickups$TotalCrime 
                              + positive_Yellow_pickups$TotalPop, family = poisson, data = data)

summary(my.glm6.positive.yellow)

# - population
# Uber
my.glm7.positive.uber <-glm(positive_Uber_pickups$Uber ~ positive_Uber_pickups$TotalCrime , family = poisson, data = data)

summary(my.glm7.positive.uber)

# Yellow
my.glm7.positive.yellow <-glm(positive_Yellow_pickups$Yellow ~ positive_Yellow_pickups$TotalCrime, family = poisson, data = data)

summary(my.glm7.positive.yellow)

library(sjPlot)
library(snakecase)
tab_model(my.glm7.positive.uber ,my.glm6.positive.uber, my.glm5.positive.uber, my.glm4.positive.uber, 
          my.glm3.positive.uber, my.glm2.positive.uber, my.glm1.positive.uber, transform = NULL, digits = 6)

tab_model(my.glm7.positive.yellow ,my.glm6.positive.yellow, my.glm5.positive.yellow, my.glm4.positive.yellow, 
          my.glm3.positive.yellow, my.glm2.positive.yellow, my.glm1.positive.yellow, transform = NULL, digits = 6)


#####################################
###### zeroes Poisson model #########
#####################################

# probit model manipulation
# add two binary variables
data$uber_zero[data$Uber == 0] <- 1
data$uber_zero[data$Uber > 0] <- 0
sum(data$uber_zero)

data$yellow_zero[data$Yellow == 0] <- 1
data$yellow_zero[data$Yellow > 0] <- 0
sum(data$yellow_zero)

#############
#############
# probit uber
my.glm1.probit.uber <-glm(data$uber_zero ~ data$TotalCrime + data$Above65 
                          + data$AvgComToWork + data$Bachelor + data$TotalPop +
                            data$Residents.Mobility +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm1.probit.uber)

# - residents mobility
my.glm2.probit.uber <-glm(data$uber_zero ~ data$TotalCrime + data$Above65 
                          + data$AvgComToWork + data$Bachelor + data$TotalPop +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm2.probit.uber)

# - avg commute to work
my.glm3.probit.uber <-glm(data$uber_zero ~ data$TotalCrime + data$Above65 
                          + data$Bachelor + data$TotalPop +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm3.probit.uber)

# - education
my.glm4.probit.uber <-glm(data$uber_zero ~ data$TotalCrime + data$Above65 
                          + data$TotalPop +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm4.probit.uber)

# - age
my.glm5.probit.uber <-glm(data$uber_zero ~ data$TotalCrime 
                          + data$TotalPop +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm5.probit.uber)

# - income
my.glm6.probit.uber <-glm(data$uber_zero ~ data$TotalCrime 
                          + data$TotalPop, family = binomial(link = "probit"), data = data)

summary(my.glm6.probit.uber)

# - population
my.glm7.probit.uber <-glm(data$uber_zero ~ data$TotalCrime, family = binomial(link = "probit"), data = data)

summary(my.glm7.probit.uber)

###############
###############
# probit yellow
my.glm1.probit.yellow <-glm(data$yellow_zero ~ data$TotalCrime + data$Above65 
                          + data$AvgComToWork + data$Bachelor + data$TotalPop +
                            data$Residents.Mobility +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm1.probit.yellow)

# - residents mobility
my.glm2.probit.yellow <-glm(data$yellow_zero ~ data$TotalCrime + data$Above65 
                          + data$AvgComToWork + data$Bachelor + data$TotalPop +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm2.probit.yellow)

# - avg commute to work
my.glm3.probit.yellow <-glm(data$yellow_zero ~ data$TotalCrime + data$Above65 
                          + data$Bachelor + data$TotalPop +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm3.probit.yellow)

# - education
my.glm4.probit.yellow <-glm(data$yellow_zero ~ data$TotalCrime + data$Above65 
                          + data$TotalPop +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm4.probit.yellow)

# - age
my.glm5.probit.yellow <-glm(data$yellow_zero ~ data$TotalCrime 
                          + data$TotalPop +
                            data$MedianHouseholdIncome, family = binomial(link = "probit"), data = data)

summary(my.glm5.probit.yellow)

# - income
my.glm6.probit.yellow <-glm(data$yellow_zero ~ data$TotalCrime 
                          + data$TotalPop, family = binomial(link = "probit"), data = data)

summary(my.glm6.probit.yellow)

# - population
my.glm7.probit.yellow <-glm(data$yellow_zero ~ data$TotalCrime, family = binomial(link = "probit"), data = data)

summary(my.glm7.probit.yellow)

tab_model(my.glm7.probit.uber, my.glm6.probit.uber, my.glm5.probit.uber, my.glm4.probit.uber,
          my.glm3.probit.uber, my.glm2.probit.uber, my.glm1.probit.uber,transform = NULL, digits = 6)

tab_model(my.glm7.probit.yellow, my.glm6.probit.yellow, my.glm5.probit.yellow, my.glm4.probit.yellow,
          my.glm3.probit.yellow, my.glm2.probit.yellow, my.glm1.probit.yellow,transform = NULL, digits = 6)
summary(my.glm1.positive.pool)

### Get the Regression Tables ###
library(sjPlot)
library(snakecase)
tab_model(my.glm7.probit.uber ,my.glm6.probit.uber, my.glm5.probit.uber, my.glm4.probit.uber, 
          my.glm3.probit.uber, my.glm2.probit.uber, my.glm1.probit.uber, transform = NULL, digits = 6)

tab_model(my.glm7.positive.yellow ,my.glm6.positive.yellow, my.glm5.positive.yellow, my.glm4.positive.yellow, 
          my.glm3.positive.yellow, my.glm2.positive.yellow, my.glm1.positive.yellow, transform = NULL, digits = 6)



#############################################################################################
################################## END OF THE Manually ZERO-INFLATED  MODEL #################
#############################################################################################












# quasipoisson model
my.glm1.quasi.uber <-glm(data$Uber ~ data$TotalCrime + data$Lessthan18 
                         + data$AvgComToWork + data$AsianAlone +
                           data$Gini + data$HighSchoolDropOutRate, family = quasipoisson, data = data)
summary(my.glm1.quasi.uber)
plot(my.glm1.quasi.uber)

library(car)
vif(my.glm1.uber)

# Zero-inflated Poisson Model
library(pscl)
my.glm1.uber.zeroinfl <- zeroinfl(data$Uber ~ data$TotalCrime + data$Lessthan18 
                             + data$AvgComToWork + data$AsianAlone +
                               data$Gini + data$HighSchoolDropOutRate, 
                             data = data, dist = "poisson")
summary(my.glm1.uber.zeroinfl)

# -race component
my.glm2.uber.zeroinfl <- zeroinfl(data$Uber ~ data$TotalCrime + data$Lessthan18 
                                  + data$AvgComToWork +
                                    data$Gini + data$HighSchoolDropOutRate, 
                                  data = data, dist = "poisson")
summary(my.glm2.uber.zeroinfl)

# -commute to work
my.glm3.uber.zeroinfl <- zeroinfl(data$Uber ~ data$TotalCrime + data$Lessthan18 
                                  + data$Gini + data$HighSchoolDropOutRate, 
                                  data = data, dist = "poisson")
summary(my.glm3.uber.zeroinfl)

# - education
my.glm4.uber.zeroinfl <- zeroinfl(data$Uber ~ data$TotalCrime + data$Gini + data$Lessthan18 
                                  , data = data, dist = "poisson")
summary(my.glm4.uber.zeroinfl)

# - age
my.glm5.uber.zeroinfl <- zeroinfl(data$Uber ~ data$TotalCrime + data$Gini, 
                                  data = data, dist = "poisson")
summary(my.glm5.uber.zeroinfl)

# -Gini
my.glm6.uber.zeroinfl <- zeroinfl(data$Uber ~ data$TotalCrime, 
                                  data = data, dist = "poisson")
summary(my.glm6.uber.zeroinfl)


library(sjPlot)
library(snakecase)
tab_model(my.glm6.uber.zeroinfl, my.glm5.uber.zeroinfl, my.glm4.uber.zeroinfl, 
          my.glm3.uber.zeroinfl, my.glm2.uber.zeroinfl, my.glm1.uber.zeroinfl, transform = NULL)



# hurdle poisson
modelHurdle <- hurdle(data$Uber ~ data$TotalCrime + data$Lessthan18 
                      + data$AvgComToWork + data$AsianAlone +
                        data$Gini + data$HighSchoolDropOutRate,
                      data = data)
summary(modelHurdle)

# using the Vuong test to compare the zero-inflated model with the regular one
vuong(my.glm1.uber, my.glm1.uber.zeroinfl)
vuong(my.glm1.uber, modelHurdle)
vuong(my.glm1.uber.zeroinfl, modelHurdle)

######### FOR YELLOW CAB TEST ##########################################################
# Regular Poission Model
my.glm1.yellow <-glm(data$Yellow ~ data$TotalCrime + data$Lessthan18 
              + data$AvgComToWork + data$AsianAlone +
                data$Gini + data$HighSchoolDropOutRate, family = poisson, data = data)
summary(my.glm1.yellow)

######################################################### Standard #################################################
######################################################### Poisson ##################################################
######################################################### For ######################################################
######################################################### Yellow ###################################################
my.glm1.standard.yellow <-glm(data$Yellow ~ data$TotalCrime + data$Above65 
                            + data$AvgComToWork + data$Bachelor + data$TotalPop +
                              data$Residents.Mobility +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm1.standard.yellow)

# -residents mobility
my.glm2.standard.yellow <-glm(data$Yellow ~ data$TotalCrime + data$Above65 
                            + data$AvgComToWork + data$Bachelor + data$TotalPop +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm2.standard.yellow)

# - avg commute to work
my.glm3.standard.yellow <-glm(data$Yellow ~ data$TotalCrime + data$Above65 
                            + data$Bachelor + data$TotalPop +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm3.standard.yellow)

# - education
my.glm4.standard.yellow <-glm(data$Yellow ~ data$TotalCrime + data$Above65 
                            + data$TotalPop +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm4.standard.yellow)

# - age
my.glm5.standard.yellow <-glm(data$Yellow ~ data$TotalCrime 
                            + data$TotalPop +
                              data$MedianHouseholdIncome, family = poisson, data = data)

summary(my.glm5.standard.yellow)

# - income
my.glm6.standard.yellow <-glm(data$Yellow ~ data$TotalCrime 
                            + data$TotalPop, family = poisson, data = data)

summary(my.glm6.standard.yellow)

# - population
my.glm7.standard.yellow <-glm(data$Yellow ~ data$TotalCrime 
                            , family = poisson, data = data)

summary(my.glm7.standard.yellow)

library(sjPlot)
library(snakecase)
tab_model(my.glm7.standard.yellow ,my.glm6.standard.yellow, my.glm5.standard.yellow, my.glm4.standard.yellow, 
          my.glm3.standard.yellow, my.glm2.standard.yellow, my.glm1.standard.yellow, transform = NULL, digits = 6)


######################################################################## END OF STANDARD ###########################################
######################################################################## POISSON FOR ###############################################
######################################################################## YELLOW ####################################################


####################################################################################################################################
####################################### POOLING DATA WITH A UBER DUMMY #############################################################
####################################################################################################################################

# duplicate the data (top is yellow and bot is Uber)

rep_data <- data
rep_data <- do.call("rbind", replicate(2, rep_data, simplify = FALSE))

# create the uber dummy variable
rep_data$UberDummy <- 1
rep_data[(1:1957),59] <- 0

# create the interaction term
rep_data$UberXTotalCrime <- rep_data$UberDummy*rep_data$TotalCrime

# create the pickup variable that records yellow pickups for the top half and uber pickups for the bottom half
rep_data$pickup <- 1
rep_data[(1:1957), 61] <- rep_data$Yellow[1:1957]
rep_data[(1958:3914), 61] <- rep_data$Uber[1958:3914]
View(rep_data)



# Standard Poisson for Positive Values
####################################################################################################################################

# Classify into two groups
# the positive pickup group
rep_positive_pickups <- rep_data[ which(rep_data$pickup > 0), ]

# the zero pickup group
rep_zero_pickup <- rep_data[ which(rep_data$pickup == 0), ]

###################################
# Positive Pooling Poisson Regression Table
###################################
my.glm1.positive.pool <- glm(rep_positive_pickups$pickup ~rep_positive_pickups$TotalCrime + rep_positive_pickups$UberXTotalCrime + 
                               rep_positive_pickups$UberDummy +
                              rep_positive_pickups$Above65 
                            + rep_positive_pickups$AvgComToWork + rep_positive_pickups$Bachelor + rep_positive_pickups$TotalPop +
                              rep_positive_pickups$Residents.Mobility +
                              rep_positive_pickups$MedianHouseholdIncome, family = poisson, data = rep_positive_pickups)

summary(my.glm1.positive.pool)

# -residents mobility
my.glm2.positive.pool <- glm(rep_positive_pickups$pickup ~ rep_positive_pickups$TotalCrime + rep_positive_pickups$UberXTotalCrime + 
                               rep_positive_pickups$UberDummy +
                               rep_positive_pickups$Above65 
                             + rep_positive_pickups$AvgComToWork + rep_positive_pickups$Bachelor + rep_positive_pickups$TotalPop +
                               rep_positive_pickups$MedianHouseholdIncome, family = poisson, data = rep_positive_pickups)

summary(my.glm2.positive.pool)

# - avg commute to work
my.glm3.positive.pool <- glm(rep_positive_pickups$pickup ~ rep_positive_pickups$TotalCrime + rep_positive_pickups$UberXTotalCrime + 
                               rep_positive_pickups$UberDummy +
                               rep_positive_pickups$Above65 
                             + rep_positive_pickups$Bachelor + rep_positive_pickups$TotalPop +
                               rep_positive_pickups$MedianHouseholdIncome, family = poisson, data = rep_positive_pickups)

summary(my.glm3.positive.pool)


# - education
# Uber
my.glm4.positive.pool <- glm(rep_positive_pickups$pickup ~ rep_positive_pickups$TotalCrime + rep_positive_pickups$UberXTotalCrime + 
                               rep_positive_pickups$UberDummy +
                               rep_positive_pickups$Above65 
                             + rep_positive_pickups$TotalPop +
                               rep_positive_pickups$MedianHouseholdIncome, family = poisson, data = rep_positive_pickups)

summary(my.glm4.positive.pool)

# - age
my.glm5.positive.pool <- glm(rep_positive_pickups$pickup ~ rep_positive_pickups$TotalCrime + rep_positive_pickups$UberXTotalCrime
                             +  rep_positive_pickups$UberDummy + rep_positive_pickups$TotalPop +
                               rep_positive_pickups$MedianHouseholdIncome, family = poisson, data = rep_positive_pickups)

summary(my.glm5.positive.pool)

# - income
my.glm6.positive.pool <- glm(rep_positive_pickups$pickup ~ rep_positive_pickups$TotalCrime + rep_positive_pickups$UberXTotalCrime
                             +  rep_positive_pickups$UberDummy + rep_positive_pickups$TotalPop, family = poisson, data = rep_positive_pickups)

summary(my.glm6.positive.pool)

# - population
my.glm7.positive.pool <- glm(rep_positive_pickups$pickup ~ rep_positive_pickups$TotalCrime + rep_positive_pickups$UberXTotalCrime +  rep_positive_pickups$UberDummy
                             , family = poisson, data = rep_positive_pickups)

summary(my.glm7.positive.pool)

library(sjPlot)
library(snakecase)
tab_model(my.glm7.positive.pool ,my.glm6.positive.pool, my.glm5.positive.pool, my.glm4.positive.pool, 
          my.glm3.positive.pool, my.glm2.positive.pool, my.glm1.positive.pool, transform = NULL, digits = 6)

###################################
# Probit Regression for Pooled Data
###################################

# probit model manipulation
# add two binary variables
rep_data$zero[rep_data$pickup == 0] <- 1
rep_data$zero[rep_data$pickup > 0] <- 0


#############
#############
# probit 
my.glm1.probit.pool <-glm(rep_data$zero ~ rep_data$TotalCrime + rep_data$UberXTotalCrime + rep_data$UberDummy + rep_data$Above65 
                          + rep_data$AvgComToWork + rep_data$Bachelor + rep_data$TotalPop +
                            rep_data$Residents.Mobility +
                            rep_data$MedianHouseholdIncome, family = binomial(link = "probit"), data = rep_data)

summary(my.glm1.probit.pool)

# - residents mobility
my.glm2.probit.pool <-glm(rep_data$zero ~ rep_data$TotalCrime + rep_data$UberXTotalCrime + rep_data$UberDummy + rep_data$Above65 
                          + rep_data$AvgComToWork + rep_data$Bachelor + rep_data$TotalPop +
                            rep_data$MedianHouseholdIncome, family = binomial(link = "probit"), data = rep_data)

summary(my.glm2.probit.pool)

# - avg commute to work
my.glm3.probit.pool <-glm(rep_data$zero ~ rep_data$TotalCrime + rep_data$UberXTotalCrime + rep_data$UberDummy + rep_data$Above65 
                          + rep_data$Bachelor + rep_data$TotalPop +
                            rep_data$MedianHouseholdIncome, family = binomial(link = "probit"), data = rep_data)

summary(my.glm3.probit.pool)

# - education
my.glm4.probit.pool <-glm(rep_data$zero ~ rep_data$TotalCrime + rep_data$UberXTotalCrime + rep_data$UberDummy + rep_data$Above65 
                          + rep_data$TotalPop +
                            rep_data$MedianHouseholdIncome, family = binomial(link = "probit"), data = rep_data)

summary(my.glm4.probit.pool)

# - age
my.glm5.probit.pool <-glm(rep_data$zero ~ rep_data$TotalCrime + rep_data$UberXTotalCrime + rep_data$UberDummy
                          + rep_data$TotalPop +
                            rep_data$MedianHouseholdIncome, family = binomial(link = "probit"), data = rep_data)

summary(my.glm5.probit.pool)

# - income
my.glm6.probit.pool <-glm(rep_data$zero ~ rep_data$TotalCrime + rep_data$UberXTotalCrime + rep_data$UberDummy
                          + rep_data$TotalPop, family = binomial(link = "probit"), data = rep_data)

summary(my.glm6.probit.pool)

# - population
my.glm7.probit.pool <-glm(rep_data$zero ~ rep_data$TotalCrime + rep_data$UberXTotalCrime + rep_data$UberDummy
                          , family = binomial(link = "probit"), data = rep_data)

summary(my.glm7.probit.pool)


library(sjPlot)
library(snakecase)
tab_model(my.glm7.probit.pool ,my.glm6.probit.pool, my.glm5.probit.pool, my.glm4.probit.pool, 
          my.glm3.probit.pool, my.glm2.probit.pool, my.glm1.probit.pool, transform = NULL, digits = 6)


##################################################### END OF POOLING DATA ANALYSIS ##################################################
#####################################################################################################################################
#####################################################################################################################################

 
library(car)

# Zero-inflated Poisson Model
library(pscl)
my.glm1.yellow.zeroinfl <- zeroinfl(data$Yellow ~ data$TotalCrime + data$Lessthan18 
                             + data$AvgComToWork + data$AsianAlone +
                               data$Gini + data$HighSchoolDropOutRate, 
                             data = data, dist = "poisson")
summary(my.glm1.yellow.zeroinfl)


my.glm1.yellow.zeroinfl2 <-zeroinfl(data$Yellow ~ data$TotalCrime + data$Above65 + data$AvgComToWork +
                                    data$Bachelor + data$TotalPop + data$Residents.Mobility + 
                                    data$MedianHouseholdIncome, 
                                    data = data, dist = "poisson")


my.glm1.yellow.zeroinfl3 <-zeroinfl(data$Yellow ~ data$TotalCrime | data$TotalCrime + data$TotalPop, 
                                    data = data, dist = "poisson")

summary(my.glm1.yellow.zeroinfl3)

my.glm1.uber.zeroinfl3 <-zeroinfl(data$Uber ~ data$TotalCrime + data$MedianHouseholdIncome | data$TotalCrime + data$TotalPop, 
                                    data = data, dist = "poisson")

summary(my.glm1.uber.zeroinfl3)





# -race component
my.glm2.yellow.zeroinfl <- zeroinfl(data$Yellow ~ data$TotalCrime + data$Lessthan18 
                                  + data$AvgComToWork +
                                    data$Gini + data$HighSchoolDropOutRate, 
                                  data = data, dist = "poisson")
summary(my.glm2.yellow.zeroinfl)

# -commute to work
my.glm3.yellow.zeroinfl <- zeroinfl(data$Yellow ~ data$TotalCrime + data$Lessthan18 
                                  + data$Gini + data$HighSchoolDropOutRate, 
                                  data = data, dist = "poisson")
summary(my.glm3.yellow.zeroinfl)

# - education
my.glm4.yellow.zeroinfl <- zeroinfl(data$Yellow ~ data$TotalCrime + data$Gini + data$Lessthan18 
                                  , data = data, dist = "poisson")
summary(my.glm4.yellow.zeroinfl)

# - age
my.glm5.yellow.zeroinfl <- zeroinfl(data$Yellow ~ data$TotalCrime + data$Gini, 
                                  data = data, dist = "poisson")
summary(my.glm5.yellow.zeroinfl)

# -Gini
my.glm6.yellow.zeroinfl <- zeroinfl(data$Yellow ~ data$TotalCrime, 
                                  data = data, dist = "poisson")
summary(my.glm6.yellow.zeroinfl)

# test
my.test.zeroinfl <- zeroinfl(data$Yellow ~ data$TotalCrime + data$AvgFamilyIncome, 
                                    data = data, dist = "poisson")
cor.test(data$MedianFamilyIncome, data$Yellow)

min.model = zeroinfl(data$Yellow ~ data$TotalCrime)
fwd.model = step(min.model, direction='forward', scope=(~ data$TotalCrime + data$WhiteAlone + data$BlackOrAfriAmeri
                                                        + data$AsianAlone + data$Bachelor + data$HighSchoolDropOutRate
                                                        + data$Gini + data$HighSchool + data$DropOut + data$MedianHouseholdIncome
                                                        + data$MedianFamilyIncome + data$AvgFamilyIncome + data$Residents.Mobility
                                                        + data$SomeCollege + data$Lessthan18 + data$Between18and35 + data$Between35and65))


min.model = zeroinfl(data$Yellow ~ data$TotalCrime)
fwd.model = step(min.model, direction='forward', scope=(~ data$TotalCrime + data$Lessthan18 
                                                        + data$AvgComToWork + data$AsianAlone +
                                                          data$Gini + data$HighSchoolDropOutRate))


my.glm <- glm(data$Uber ~ data$TotalCrime + data$BlackOrAfriAmeri
      + data$AsianAlone + data$HighSchoolDropOutRate
      + data$Gini
      + data$MedianFamilyIncome +  data$Lessthan18 + data$Between18and35, family = poisson)

vif(my.glm)

library(sjPlot)
library(snakecase)
tab_model(my.glm6.yellow.zeroinfl, my.glm5.yellow.zeroinfl, my.glm4.yellow.zeroinfl, 
          my.glm3.yellow.zeroinfl, my.glm2.yellow.zeroinfl, my.glm1.yellow.zeroinfl, transform = NULL)





# hurdle poisson
modelHurdle <- hurdle(data$Yellow ~ data$TotalCrime + data$Lessthan18 
                      + data$AvgComToWork + data$AsianAlone +
                        data$Gini + data$HighSchoolDropOutRate,
                      data = data)
summary(modelHurdle)

# using the Vuong test to compare the zero-inflated model with the regular one
vuong(my.glm1.yellow, my.glm1.yellow.zeroinfl)
vuong(my.glm1.yellow, modelHurdle)
vuong(my.glm1.yellow.zeroinfl, modelHurdle)

####################################################################################

# # of zeros
counterYellow <- 0
for (i in data$Yellow){
  if (i == 0){
    counterYellow = counterYellow + 1
  }
}
print(counterYellow)

counterUber <- 0
for (i in data$Uber){
  if (i == 0){
    counterUber = counterUber + 1
  }
}
print(counterUber)

plot(data$Uber)
plot(data$Yellow)

######################################################################################################
########################################### OLS Regressions with density instead of count##########################################
######################################################################################################

##########################################################################################
## OLS FOR Uber density
##########################################################################################

# Create density variable
data$UberDen <- data$Uber/data$TotalPop
data$YellowDen <- data$Yellow/data$TotalPop
data$CrimeDen <- data$TotalCrime/data$TotalPop

my.lm1.ols.uber <-lm(data$UberDen ~ data$CrimeDen + data$Above65 
                            + data$AvgComToWork + data$Bachelor+
                              data$Residents.Mobility +
                              data$MedianHouseholdIncome, data = data)

summary(my.lm1.ols.uber)

# -residents mobility
my.lm2.ols.uber <-lm(data$UberDen ~ data$CrimeDen + data$Above65 
                     + data$AvgComToWork + data$Bachelor+
                       data$MedianHouseholdIncome, data = data)

summary(my.lm2.ols.uber)

# - avg commute to work
my.lm3.ols.uber <-lm(data$UberDen ~ data$CrimeDen + data$Above65 
                     + data$Bachelor+
                       data$MedianHouseholdIncome, data = data)

summary(my.lm3.ols.uber)

# - education
my.lm4.ols.uber <-lm(data$UberDen ~ data$CrimeDen + data$Above65 
                     +data$MedianHouseholdIncome, data = data)

summary(my.lm4.ols.uber)

# - age
my.lm5.ols.uber <-lm(data$UberDen ~ data$CrimeDen 
                     +data$MedianHouseholdIncome, data = data)

summary(my.lm5.ols.uber)


# - income
my.lm6.ols.uber <-lm(data$UberDen ~ data$CrimeDen , data = data)

summary(my.lm6.ols.uber)

library(sjPlot)
library(snakecase)
tab_model(my.lm6.ols.uber, my.lm5.ols.uber, my.lm4.ols.uber, 
          my.lm3.ols.uber, my.lm2.ols.uber, my.lm1.ols.uber, transform = NULL, digits = 6)


##########################################################################################
## OLS FOR YELLOW
##########################################################################################

# OLS FOR yellow density
my.lm1.ols.yellow <-lm(data$YellowDen ~ data$CrimeDen + data$Above65 
                     + data$AvgComToWork + data$Bachelor +
                       data$Residents.Mobility +
                       data$MedianHouseholdIncome, data = data)

summary(my.lm1.ols.yellow)

# -residents mobility
my.lm2.ols.yellow <-lm(data$YellowDen ~ data$CrimeDen + data$Above65 
                       + data$AvgComToWork + data$Bachelor +
                         data$MedianHouseholdIncome, data = data)

summary(my.lm2.ols.yellow)

# - avg commute to work
my.lm3.ols.yellow <-lm(data$YellowDen ~ data$CrimeDen + data$Above65 
                       +data$Bachelor +
                         data$MedianHouseholdIncome, data = data)

summary(my.lm3.ols.yellow)

# - education
my.lm4.ols.yellow <-lm(data$YellowDen ~ data$CrimeDen + data$Above65 
                       +data$MedianHouseholdIncome, data = data)

summary(my.lm4.ols.yellow)

# - age
my.lm5.ols.yellow <-lm(data$YellowDen ~ data$CrimeDen
                       +data$MedianHouseholdIncome, data = data)

summary(my.lm5.ols.yellow)

# - income
my.lm6.ols.yellow <-lm(data$YellowDen ~ data$CrimeDen, data = data)

summary(my.lm6.ols.yellow)

library(sjPlot)
library(snakecase)
tab_model(my.lm6.ols.yellow, my.lm5.ols.yellow, my.lm4.ols.yellow, 
          my.lm3.ols.yellow, my.lm2.ols.yellow, my.lm1.ols.yellow, transform = NULL, digits = 6)

##########################################################################################
## OLS FOR POOLED ANALYSIS
##########################################################################################

rep_data$PickupDen <- rep_data$pickup/rep_data$TotalPop
rep_data$CrimeDen <- rep_data$TotalCrime/rep_data$TotalPop
rep_data$CrimeDenXUber <- rep_data$CrimeDen*rep_data$UberDummy

my.lm1.pool <- lm(rep_data$PickupDen ~rep_data$CrimeDen + rep_data$CrimeDenXUber + 
                    rep_data$UberDummy +
                    rep_data$Above65 + rep_data$AvgComToWork + rep_data$Bachelor+
                    rep_data$Residents.Mobility +
                    rep_data$MedianHouseholdIncome, data = rep_data)

summary(my.lm1.pool)

# -residents mobility
my.lm2.pool <- lm(rep_data$PickupDen ~rep_data$CrimeDen + rep_data$CrimeDenXUber + 
                    rep_data$UberDummy +
                    rep_data$Above65 + rep_data$AvgComToWork + rep_data$Bachelor+
                    rep_data$MedianHouseholdIncome, data = rep_data)

summary(my.lm2.pool)

# - avg commute to work
my.lm3.pool <- lm(rep_data$PickupDen ~rep_data$CrimeDen + rep_data$CrimeDenXUber + 
                    rep_data$UberDummy +
                    rep_data$Above65 + rep_data$Bachelor+
                    rep_data$MedianHouseholdIncome, data = rep_data)

summary(my.lm3.pool)


# - education
my.lm4.pool <- lm(rep_data$PickupDen ~rep_data$CrimeDen + rep_data$CrimeDenXUber + 
                    rep_data$UberDummy +
                    rep_data$Above65 +
                    rep_data$MedianHouseholdIncome, data = rep_data)

summary(my.lm4.pool)

# - age
my.lm5.pool <- lm(rep_data$PickupDen ~rep_data$CrimeDen + rep_data$CrimeDenXUber + 
                    rep_data$UberDummy +
                    rep_data$MedianHouseholdIncome, data = rep_data)

summary(my.lm5.pool)

# - income
my.lm6.pool <- lm(rep_data$PickupDen ~rep_data$CrimeDen + rep_data$CrimeDenXUber + 
                    rep_data$UberDummy, data = rep_data)

summary(my.lm6.pool)

library(sjPlot)
library(snakecase)
tab_model(my.lm6.pool, my.lm5.pool, my.lm4.pool, 
          my.lm3.pool, my.lm2.pool, my.lm1.pool, transform = NULL, digits = 6)



###### End of OLS Regressions ##########################
########################################################
########################################################


#################### Proportion Model ##################
########################################################
########################################################

require(devtools)
install_github("f1kidd/fmlogit")

library(fmlogit)
data$YellowRatio <- 1-data$UberRatio
Y.df <- data.frame(cbind(data$UberRatio, data$YellowRatio))
colnames(Y.df) <- c("Uber Ratio", "Yellow Ratio")
X.df <- data.frame(cbind(data$TotalCrime, data$Above65, data$AvgComToWork, 
                         data$Bachelor, data$TotalPop,data$Residents.Mobility,
                         data$MedianHouseholdIncome))
Z.df <- data.frame(cbind(data$TotalCrime, data$Lessthan18))
colnames(X.df) <- c("TotalCrime", "Above65","AvgComToWork", "Bachelor","TotalPop", "ResidentsMobility","MedianHouseholdIncome")
colnames(Z.df) <- c("TotalCrime", "lessthan18")
my.fmlogit1 <- fmlogit(Y.df, X.df)
Y2.df <- data.frame(cbind(data$YellowRatio, data$UberRatio))
my.fmlogit2 <- fmlogit(Y2.df, X.df)

?fmlogit

#