
#########################################################################
#                                                                       #
#  Merging/joining data in R                                            #
#                                                                       #
#########################################################################

########### objects/packages needed ###########
library(tidyverse)
library(dslabs)
###############################################

#######################
#  Merging in base R  #
#######################

#  The merge() function can be used to combine or merge data together   #
#  in various ways. The merge() function takes at least three           #
#  inputs -- the two data sets to be merged and the variable(s) by      #
#  which the data should be matched.                                    #

## View both data
head(temp_carbon)
head(historic_co2)

## Subset historic CO2 data to years A.D.
historic1 <- historic_co2[historic_co2$year > 0,]

## Determine number of rows in each data
dim(temp_carbon)
dim(historic1)

## Merge temp and historic data
merge1 <- merge(temp_carbon, historic1, by="year")


#  By default, the merge() function will result in data that contains   #
#  one row for each match.                                              #
#                                                                       #
#  To merge data with different numbers of observations and retain all  #
#  of the observations from both original data, the all option can be   #
#  specified as TRUE. A missing value, NA, will be filled in whereever  #
#  information is not available.                                        #

## Merge temp and historic data: keep all rows
merge2 <- merge(temp_carbon, historic1, by="year", all=TRUE)

#  The options all.x and all.y include only the non-matching            #
#  observations from the first or second data, respectively.            #

## Merge temp and historic data: keep all temp rows
merge3 <- merge(temp_carbon, historic1, by="year", all.x=TRUE)
dim(temp_carbon)

## Merge temp and historic data: keep all historic rows
merge4 <- merge(temp_carbon, historic1, by="year", all.y=TRUE)
dim(historic1)

## Subset to ice cores values
historic2 <- historic1[historic1$source=="Ice Cores",]

# Merge temp and ice cores historic data: keep all temp rows
merge5 <- merge(temp_carbon, historic2, by="year", all.x=TRUE)
dim(temp_carbon)


#  If the variable(s) by which a match should be defined have           #
#  different names in the two data sets, they can be specified in a     #
#  vector given for the by.x and by.y options.                          #

## Change the name of historic year
historic3 <- historic1
names(historic3)[1] <- "YEAR"

## Merge temp and historic data: keep matched rows
merge6 <- merge(temp_carbon, historic3, by.x="year", by.y="YEAR")



##############################
#  Joining in the tidyverse  #
##############################

#  The tidyverse package dplyr contains functions that are used for     #
#  joining data together in various ways. Each joining function takes   #
#  at least three inputs -- the two data sets to be joined and the      #
#  variable(s) by which the data should be joined.                      #
#                                                                       #
#  If the variable(s) by which the data should be joined have the same  #
#  name in the two input data, these variables can be listed in a       #
#  vector for the by option.                                            #
#                                                                       #
#  The inner_join() function joins information from both input data.    #
#  Only observations that are in both input data are kept in the        #
#  resulting joined data.                                               #

## Join temp and historic data
join1 <- inner_join(temp_carbon, historic1, by="year")


#  The full_join() function joins information from both input data.     #
#  All observations are kept in the resulting joined data, including    #
#  those that are only in the first input data, those that are only in  #
#  the second input data, and those that are in both.                   #

## Join temp and historic data: keep all rows
join2 <- full_join(temp_carbon, historic1, by="year")


#  The left_join() function joins information from the second input     #
#  data to the observations in the first input data. Only observations  #
#  that are in the first input data are kept in the resulting joined    #
#  data.                                                                #

## Join temp and historic data: keep all temp rows
join3 <- left_join(temp_carbon, historic1, by="year")


#  The right_join() function joins information from the first input     #
#  data to the observations in the second input data. Only              #
#  observations that are in the second input data are kept in the       #
#  resulting joined data.                                               #

## Join temp and historic data: keep all historic rows
join4 <- right_join(temp_carbon, historic1, by="year")


#  If the variables have different names across the input data, the by  #
#  option should contain information for how the variables correspond.  #

## Join temp and historic data
join6 <- inner_join(temp_carbon, historic3, by=c("year"="YEAR"))


# Depending on your datasets, you may not need to merge. it depends on what
# variables you end up looking at that determines what you are able to start
# with

#########################################################################


