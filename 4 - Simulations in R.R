
#########################################################################
#                                                                       #
#  Simulations in R                                                     #
#                                                                       #
#########################################################################

########### objects/packages needed ###########
library(ggplot2)
# OR library(tidyverse)
###############################################

#  One of the powerful abilities of R is quick computation, which       #
#  allows users to simulate outcomes and results. Many statistical      #
#  results are based on the idea of repeated sampling, which is a rare  #
#  occurrence in real life. However, R gives users the ability to       #
#  simulate repeated sampling and to view the results.                  #

##################
#  Random seeds  #
##################

#  When using any random generating function in R, evaluating the same  #
#  command multiple times will yield different results. Often, it is    #
#  important to be able to reproduce results. To generate the same      #
#  random values each time a command is run, the random generation      #
#  seed can be set by using the set.seed() function. There is one       #
#  required input -- the seed. It is common practice to use a relevant  #
#  value (lucky number, birth date, address, etc.) to protect against   #
#  searching for an optimal sample.                                     #
#                                                                       #
#  Seeds operate in a defined order, so once the seed is set, the next  #
#  random generation commands will use the next seeds in that order.    #
#  Thus, setting the seed once at the beginning of a program will give  #
#  the same random values each time the program is run, but each        #
#  command within the program will generate different results. To       #
#  yield the same values each time a random generation command is run,  #
#  the set.seed() function must be rerun prior to each command.         #

## Without a set seed
rnorm(10, mean=2.5, sd=0.5)
rnorm(10, mean=2.5, sd=0.5)

## With a set seed specified once
set.seed(5)
rnorm(10, mean=2.5, sd=0.5)
rnorm(10, mean=2.5, sd=0.5)

set.seed(5)
rnorm(10, mean=2.5, sd=0.5)
rnorm(10, mean=2.5, sd=0.5)

## With a set seed specified each time
set.seed(5)
rnorm(10, mean=2.5, sd=0.5)

set.seed(5)
rnorm(10, mean=2.5, sd=0.5)

#############################
#  Monte Carlo simulations  #
#############################

#  Monte Carlo simulation is the process of using repeated sampling     #
#  to determine some behavior or characteristic. This type of           #
#  simulation can be used in many different ways.                       #
#                                                                       #
#  Many applications of Monte Carlo sampling will use the population    #
#  or an appropriate pseudo-population to generate an estimated         #
#  sampling distribution of a relevant statistic that can then be       #
#  explored. One example in this category is to demonstrate and         #
#  understand the Central Limit Theorem.                                #
#                                                                       #
#  Suppose that we have a population from which we plan to take a       #
#  random sample and use the Central Limit Theorem to test the mean.    #
#  The distribution of our population is unknown, but we think that it  #
#  is similar to the chi-squared distribution with four degrees of      #
#  freedom. One important question is how large of a random sample is   #
#  needed from the population for the Central Limit Theorem to hold.    #
#  This question can be answered using a Monte Carlo simulation that    #
#  estimates the sampling distribution of the mean at various sample    #
#  sizes for evaluation of approximate normality.                       #
ztest <- function(x){
  samples <- rnorm(x, 99.5, 4.8)
  samples
  zstat <- (mean(samples) - 99.5)/(4.8/sqrt(x))
  pvalue <- pnorm(-abs(zstat)) * 2
  reject_null <- (pvalue < 0.05)
  reject_null
}

prop_reject <- sum(replicate(10000, ztest(51)))/10000
prop_reject

ztest2 <- function(size){
  samp <- rnorm(size, 99.5, 4.8)
  z <- (mean(samp) - 99.5)/(sd(samp)/sqrt(size))
  pvalue <- pnorm(-abs(z))*2
  reject_null <- pvalue < 0.05
  reject_null
}

prop_reject <-
  sum(replicate(10000, ztest2(9)))/10000
prop_reject

## First plot the chi-squared population distribution
Xdata2 <- data.frame(X=c(0,20))
ggplot(Xdata2, aes(x=X)) + stat_function(fun=dchisq, args=list(df=4))

## Determine the number of repeated samples to draw and parameter values
K <- 10000
a <- 4

## Draw 10,000 samples of size 5 from the population distribution
samps <- replicate(K, rchisq(5,a))
samps[,1:10]

## Determine the sample mean from each random sample
means5 <- apply(samps,2,mean)

## Create a QQ plot of the resulting sample means
mean_data <- data.frame(X=means5)
ggplot(mean_data, aes(sample=X)) + stat_qq() + stat_qq_line() + labs(title="n=5")

#  If we want to repeat this process with other sample sizes, we can    #
#  write a function instead of writing the same code several times.     #
ztest <- function(x){
  samples <- rchisq(x,3)
  samples
  zstat <- (mean(samples) - 3 )/(2.45/sqrt(x))
  pvalue <- pnorm(-abs(zstat)) * 2
  reject_null <- (pvalue < 0.05)
  reject_null
}

prop_reject <- sum(replicate(10000, ztest(51)))/10000
prop_reject
## Repeat this process with sample sizes of 15, 30, 45, 60, and 75
sizes <- c(15,30,45,60,75)
samp.means <- lapply(sizes, function(size) replicate(K, mean(rchisq(size,a))))

qq.means <- function(x){
  mean_data <- data.frame(X=samp.means[[which(sizes==x)]])
  qqmean <-ggplot(mean_data, aes(sample=X)) + stat_qq() + stat_qq_line() + 
    labs(title=paste("n=",x,sep=""))
  print(qqmean)
}

lapply(sizes, qq.means)

#  Once we have determined which sample size yields a sampling          #
#  distribution that is sufficiently approximately normal, we can       #
#  verify the parameters of the sampling distribution. The population   #
#  mean and variance of chi-squared distributions are the df and 2*df,  #
#  respectively.                                                        #

norm.size <- 45
means <- samp.means[[which(sizes==norm.size)]]

## Estimate the mean of the sampling distribution (which is 4)
mean(means)

## Estimate the standard devation of the sampling distribution
true_sd <- sqrt(2*4/norm.size)
true_sd
sd(means)

##################### end of section 1 ##################################

########### objects/packages needed ###########
library(ggplot2)
# OR library(tidyverse)
###############################################


###################
#  Bootstrapping  #
###################

#  Bootstrapping uses similar ideas as Monte Carlo simulations, but     #
#  instead of drawing samples from a given or pseudo- population,       #
#  repeated samples are taken from the available sampled data.          #
#  Bootstrapping uses with replacement sampling to create several       #
#  simulated samples that are the same size as the original sampled     #
#  data.                                                                #

#  Like with Monte Carlo simulations, there are many applications of    #
#  bootstrapping that evaluate the estimated sampling distribution      #
#  of the parameter of interest determined from the bootstrap samples.  #
#                                                                       #
#  It is important to know that there are assumptions that need to      #
#  hold for bootstrapping to work. One of these assumptions is that     #
#  the sampled data represent an independent, representative sample     #
#  from the population. Also, bootstrapping will not work as expected   #
#  if the underlying population has very heavy tails.                   #
#                                                                       #
#  Suppose that for budget purposes we are only able to draw a sample   #
#  of 7 from the unknown population distribution. In this case, the     #
#  sample size is too small to be able to reasonably use the Central    #
#  Limit Theorem. Thus, the sampling distribution is unknown. In this   #
#  case, bootstrapping can be used to estimate the variation in the     #
#  sampling distribution.                                               #

# Daily 4.2

A<-rchisq(7, df=4)

A_mean<- mean(A)
B<-10000
## Draw the bootstrap samples
Aboot_samp <- replicate(B, sample(A, replace=T))
Aboot_samp[,1:5]

## Determine the sample mean from each bootstrap sample
Aboot_means <- apply(Aboot_samp,2,mean)

## Plot the sampling distribution
Ameans_df <- data.frame(Aboot_means)
ggplot(Ameans_df, aes(x=Aboot_means)) + geom_histogram(binwidth=1.5,fill="white",color="black")

## Estimate the mean and standard deviation of the sampling distribution
mean(Aboot_means)
sd(Aboot_means)

## Determine the 95% bootstrap confidence interval of the sample mean
boot_A<-function(){
  A<-rchisq(7, df=4)
  A_mean<- mean(A)
  B<-10000
  ## Draw the bootstrap samples
  Aboot_samp <- replicate(B, sample(A, replace=T))
  Aboot_means <- apply(Aboot_samp,2,mean)
  Aboot_err <- Aboot_means - A_mean
  Aboot_err_sort <- sort(Aboot_err)
  p2.5 <- B*0.025 #location of a
  p97.5 <- B*0.975 #location of b
  Aboot_ci <- A_mean - Aboot_err_sort[c(p97.5,p2.5)]
  truth <- F
  if (Aboot_ci[1]< 4 && Aboot_ci[2]>4){truth = T} else{truth = F}
  truth
}
cis<-replicate(100, boot_A())
head(cis)
sum(cis)







## Draw the sample of 7 from the unknown distribution
samp_data <- c(1.77, 10.48, 4.30, 0.96, 2.67, 2.76, 2.02)



## Plot the sample
samp_df <- data.frame(samp_data)
ggplot(samp_df, aes(x=samp_data)) + geom_histogram(binwidth=1.5,fill="white",color="black")

## Find the sample mean
samp_mean <- mean(samp_data)

## Determine the number of bootstrap samples
B<-10000

## Draw the bootstrap samples
boot_samp <- replicate(B, sample(samp_data, replace=T))
boot_samp[,1:5]

## Determine the sample mean from each bootstrap sample
boot_means <- apply(boot_samp,2,mean)

## Plot the sampling distribution
means_df <- data.frame(boot_means)
ggplot(means_df, aes(x=boot_means)) + geom_histogram(binwidth=1.5,fill="white",color="black")

## Estimate the mean and standard deviation of the sampling distribution
mean(boot_means)
sd(boot_means)

## Determine the 95% bootstrap confidence interval of the sample mean
boot_err <- boot_means - samp_mean
boot_err_sort <- sort(boot_err)
p2.5 <- B*0.025 #location of a
p97.5 <- B*0.975 #location of b
boot_ci <- samp_mean - boot_err_sort[c(p97.5,p2.5)]
boot_ci

#  Bootstrapping can also be used to estimate the variability of        #
#  parameters whose sampling distribution is difficult or impossible    #
#  to determine theoretically.                                          #

## Determine the median of the sampled data
samp_med <- median(samp_data)

## Determine the sample median from each bootstrap sample
boot_meds <- apply(boot_samp,2,median)

## Determine the 95% bootstrap confidence interval
boot_meds_err <- boot_meds - samp_med
boot_meds_err_sort <- sort(boot_meds_err)
boot_med_ci <- samp_med - boot_meds_err_sort[c(p97.5,p2.5)]
boot_med_ci

#  Note that the bootstrap confidence intervals are not symmetric       #
#  around the point estimate. In certain cases, the standard deviation  # 
#  of the bootstrapped estimates can be used in place of the standard   #
#  error in the usual normal confidence interval equation if you are    #
#  confident that approximate normality holds.                          #

##################### end of section 2 ##################################

########### objects/packages needed ###########
library(ggplot2)
# OR library(tidyverse)
samp_data <- c(1.77, 10.48, 4.30, 0.96, 2.67, 2.76, 2.02)
samp_mean <- mean(samp_data)
B<-10000
boot_means <- replicate(B, mean(sample(samp_data, replace=T)))
###############################################


###############################
#  Bootstrapping (continued)  #
###############################

#  Suppose that we would like to use the sample of 7 observations       #
#  from the unknown population to test if the mean of the unknown       #
#  population is greater than 3. In this case, bootstrapping can be     #
#  used to estimate the sampling distribution of the test statistic     #
#  under the null hypothesis.                                           #

## Determine the test statistic 
samp_mean 

## Determine the sampling distribution of the test statistic under the null hypothesis
mu0 <- 3
boot_means_null <- boot_means - mean(boot_means) + mu0

## Plot the null hypothesis sampling distribution
means_df <- data.frame(boot_means_null)
ggplot(means_df, aes(x=boot_means_null)) + geom_histogram(binwidth=1.5,fill="white",color="black")

## Determine the p-value (right-tailed)
sum(boot_means_null >= samp_mean)/B

#  Suppose that we would like to use the sample of 7 observations       #
#  from the unknown population and a second sample of 8 observations    #
#  from another unknown population to test if the means of the two      #
#  unknown populations are the same. Again, bootstrapping can be used   #
#  to estimate the sampling distribution of the appropriate test        #
#  statistic under the null hypothesis.                                 #

## Draw the second sample and calculate its sample mean
samp_data2 <- c(4.68, 4.83, 4.29, 1.42, 7.81, 5.20, 3.97, 1.59)
samp_mean2 <- mean(samp_data2)

## Determine the test statistic 
samp_diff <- samp_mean - samp_mean2
samp_diff

## Determine the sampling distribution of the test statistic under the null hypothesis
rand.test <- function(x){
  rand_comb <- sample( c(samp_data, samp_data2) )
  bmean1 <- mean(rand_comb[1:x])
  bmean2 <- mean(rand_comb[(x+1):(length(samp_data)+length(samp_data2))])
  bmean1 - bmean2
}

boot_diffs_null <- replicate(B, rand.test(length(samp_data)))

## Plot the null hypothesis sampling distribution
diffs_df <- data.frame(boot_diffs_null)
ggplot(diffs_df, aes(x=boot_diffs_null)) + geom_histogram(binwidth=1,fill="white",color="black")

## Determine the p-value (two-tailed)
sum(boot_diffs_null <= samp_diff | boot_diffs_null >= 2*mean(boot_diffs_null) - samp_diff)/B

##################### end of section 3 ##################################


venue_data <- c(11.3, 21.6, 16.3, 12.9, 12.8, 18.8, 17.1, 14.7)

venue_std<- sd(venue_data)
B<- 10000
std0 <- 2
boot_stds <- replicate(B, mean(sample(venue_data, replace=T)))
boot_stds_null <- boot_stds - mean(boot_stds) +  std0

## Plot the null hypothesis sampling distribution
##means_df <- data.frame(boot_means_null)
##ggplot(means_df, aes(x=boot_means_null)) + geom_histogram(binwidth=1.5,fill="white",color="black")

## Determine the p-value (right-tailed)
right_side<-sum(boot_stds_null >= std0 - venue_std)
left_side<- sum(boot_stds_null <= std0)
(left_side + right_side) / B

sum(boot_stds_null <= venue_std | boot_stds_null >= 2*mean(boot_stds_null) - venue_std)/B

#################
new_data <- c(15.9, 14.4, 10.9, 19.7, 11.5, 16.4, 11.6, 14.5)
new_std <- sd(new_data)
samp_diff <-venue_std -new_std


rand.test <- function(x){
  rand_comb <- sample( c(venue_data, new_data) )
  bsd1 <- sd(rand_comb[1:x])
  bsd2 <- sd(rand_comb[(x+1):(length(venue_data)+length(new_data))])
  bsd1 - bsd2
}

boot_stds_null <- replicate(B, rand.test(length(venue_data)))
sum(boot_stds_null <= samp_diff)/B



# From 4.4 
# Q 8 works if the Test statistic is to the left of the mean, if it is to the right,
# we need to swap the signs on each input
# only in the 2 sided test we need neg abs value around test statistic when calculating
# the p value
# Power- correctly rejecting the null hypothesis (kinda opposite of empirical type one error)
# When looking at empirical power, the null matters- change the mu in the test statistic to
# something is not the actual mean to make the alternative hypothesis true


