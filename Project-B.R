# Project-B.R

# This is the file where we do some further reparation of the data in 
# preparation for our analysis

# and where we prepare some trivial models to determine what the baseline performance
# is and how to measure it.

# We have two meassures of success - number of calls converted to term deposits
# and number of calls made 

# the objective is the retain the maximum number of conversos while 
# making the fewest number of calls.

# The extremes or making very few calls at a good conversion rate 
# but only picking up a few deposits is not particularly desireable

# A better measure might be how many calls we can skip 
# while keeping the missed deposts rate well below the 
# unfiltered conversion rate of 11.27%

# read the dataframe that we had saved at the end of project-A.R 
bankFull <- read.table("BankFull.dataframe")
str(bankFull)


# Performance 
# population 41188
# yes : 4640 = 11.265%
# no : 36548 = 88.735%



# all data prep moved to Project-A.R



# Here's where we write out the prepared data frame before analysis
write.table(bankFull,"BankFull1.dataframe")


# Model Performance 

# Here we are gong to measure the performance of our classification.

# Creating to basic benchmark models
# 1 : a random model
# Set the Call (Y/N) to Y for 80% randomly of our data set 
# measure the performance - anything wose than  this a complete failure

# 2 : a manual model based on the characteristic with the most obvious
# skew.
# This is better than random and is the real baseline we need to exceed
# to sow we are doing something useful.

# From examining the data we can get a pretty good result by excluding 
# customers with an unknown credit status and a telephone (rather than  cellular)
# this looses us 4233 calls but only 152 accounts (3.6% convversion  compared to 11.3%)

# excluding all telephone customers looses 15044 calls but only 787 contracts (5.2%)
# this gives two useful ends to the results range :
# loose a lot of calls (36%) but loose only 17% of the accounts 
# - which is the a low cost option 
# or loose a few calls (10.3%) but loose only 3.3% of the accounts

# Our model output is expressed in terms of a vector called Call 
# which has the same number of elements as the original data (41188)
# all customers are set to 'yes' and then customers to be excldued are set to 'no'

Call <- rep('call', dim(bankFull)[1])
Call <- factor(Call, levels=c('skip', 'call'))

# set up our random index to exclude 20% of customers
index <- sample (1:dim(bankFull)[1]
                 , dim(bankFull)[1]*0.2
                 , replace = FALSE
                 )

Call [index] <- 'skip'

# OK that's our basic random model working 
# let's look at the performance
# expecting that it will be trivial

# tabular

# table(bankFull$y[Call=='yes'], bankFull$y[Call=='no'])
table(bankFull$y)
table(bankFull$y[Call=='call'])
table(bankFull$y[Call=='skip'])
table(bankFull$y, Call)


table(bankFull$y, Call)[2,1] / (table(bankFull$y, Call)[1,1] + table(bankFull$y, Call)[2,1])
table(bankFull$y, Call)[2,2] / (table(bankFull$y, Call)[1,2] + table(bankFull$y, Call)[2,2])

# OK so that's pretty clear - random selection of choosing 80% custoemr to call
# gives no benefit - i.e. we loose the same proportion of business as we make

# Just for the same of completness lets graph it :

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(5,5,4,1))

barplot(table(bankFull$y)
        , col=c('lightblue', 'pink')
        , ylim=c(0,40000)
        , ylab = "\nNumber of Calls"
        , xlab = "Call Outcome : term deposit\n"
        , main = "Call outcomes for calls made and calls skipped\n randomly chosen 20% of calls skipped"
)
barplot(table(bankFull$y[Call=='skip'])
        , col=c('blue', 'red')
        , add=TRUE
)
legend("topright"
       , col = c('white', 'lightblue','pink', 'white', 'blue', 'red', 'white')
       , legend = c(''
                    , 'Call Made - Fail'
                    , 'Call Made - Success'
                    , ''
                    , 'Call Skipped - saved call'
                    , 'Call Skipped - missed deposit'
                    , ''
                    )
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)


# next models - based on observed imbalance in 
# credit status: unknown 
# and 
# call type:telephone

Call <- rep('call', dim(bankFull)[1])
Call <- factor(Call, levels=c('skip', 'call'))

# Skip all calls to customers with telephones 
# they have a lowere success rate
Call[bankFull$contact=='telephone'] <- 'skip'

# basic tabular results 
table(bankFull$y)
table(bankFull$y[Call=='call'])
table(bankFull$y[Call=='skip'])
table(bankFull$y, Call)


table(bankFull$y, Call)[2,1] / (table(bankFull$y, Call)[1,1] + table(bankFull$y, Call)[2,1])
table(bankFull$y, Call)[2,2] / (table(bankFull$y, Call)[1,2] + table(bankFull$y, Call)[2,2])

# OK conversion rate increases to 14.7% 
# - skipped calls would have converted to 787 deposits - at 5.2% conversion
# relatively efficient

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(5,5,6,1))

barplot(table(bankFull$y)
        , col=c('lightblue', 'pink')
        , ylim=c(0,40000)
        , ylab = "\nNumber of Calls"
        , xlab = "Call Outcome : term deposit\n"
        , main = 
"Call outcomes for calls made and calls skipped
Based on skipping calls to customers with a landline
which have a lower conversion rate"
)
barplot(table(bankFull$y[Call=='skip'])
        , col=c('blue', 'red')
        , add=TRUE
)
legend("topright"
       , col = c('white', 'lightblue','pink', 'white', 'blue', 'red', 'white')
       , legend = c(''
                    , 'Call Made - Fail'
                    , 'Call Made - Success'
                    , ''
                    , 'Call Skipped - saved call'
                    , 'Call Skipped - missed deposit'
                    , ''
       )
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)


# calls to customers with unknown or poor credit history

Call <- rep('call', dim(bankFull)[1])
Call <- factor(Call, levels=c('skip', 'call'))

# Skip all calls to customers with unknown or poor credit history
# they have a lower success rate
Call[(bankFull$default=='unknown' & bankFull$contact=='telephone')] <- 'skip'

# basic tabular results 
table(bankFull$y)
table(bankFull$y[Call=='call'])
table(bankFull$y[Call=='skip'])
table(bankFull$y, Call)


table(bankFull$y, Call)[2,1] / (table(bankFull$y, Call)[1,1] + table(bankFull$y, Call)[2,1])
table(bankFull$y, Call)[2,2] / (table(bankFull$y, Call)[1,2] + table(bankFull$y, Call)[2,2])

# OK conversion rate increases to 12.1% 
# - skipped calls would have converted to 152 deposits - at 3.6% conversion
# so made more calls but the excluded group were even more poorly performing


# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(5,5,6,1))

barplot(table(bankFull$y)
        , col=c('lightblue', 'pink')
        , ylim=c(0,40000)
        , ylab = "\nNumber of Calls"
        , xlab = "Call Outcome : term deposit\n"
        , main = 
 "Call outcomes for calls made and calls skipped
Based on skipping calls to customers with a landline
and with an unknown credit history
which have an even lower conversion rate"
)
barplot(table(bankFull$y[Call=='skip'])
        , col=c('blue', 'red')
        , add=TRUE
)
legend("topright"
       , col = c('white', 'lightblue','pink', 'white', 'blue', 'red', 'white')
       , legend = c(''
                    , 'Call Made - Fail'
                    , 'Call Made - Success'
                    , ''
                    , 'Call Skipped - saved call'
                    , 'Call Skipped - missed deposit'
                    , ''
       )
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)

# so we have established two performance extremes based on observed data 
# excluding a lot of customers with telephones gives many fewer calls but 
# gives a >14% conversion on calls made
# excluding fewer customers based on telephone and unknown credit score
# gives a 12.1% conversion but looses very few potential deposits

# The analysis for the best answer will need to be a useful answer
# which sits in the general range of these simple answers
























