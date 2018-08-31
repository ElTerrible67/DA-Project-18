# Project-A.R
# This is the R file where we do the initial exploration of the data set

# LIBRARIES 

# plyr and dplyr (plyr first is advised)
if (!require(plyr)) {
  install.packages("plyr")
  library(plyr)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
#
# The source is : https://archive.ics.uci.edu/ml/machine-learning-databases/00222/
# the data is a outcome of a large scale bank marketing exercise 
# of over 41,000 individual phone calls to customers to sell a term deposit product.
#
# The data includes the outcome 'yes' or 'no' based on the customer taking uo the term discount
# product within a set periof after the campaign.
#
# The data also includes the duration of the 
# phone call - which correlates highly with the outcome and is not known before the customer
# is called so it is removed from the data as it is not relevant to the choice of 
# a customer for a marketing call.

# The purpose of the exercise is to improve the bank's call/success ratio by identifying
# customers who have a low probability of taking up the offer and not including them in the 
# customer to be called list in future campaigns.

# Telephone marketing is a low success activity - the customer's key objective is to 
# identify a way of making a reduction in the call pool size without a significant 
# reduction in the number of successes - a very small pool with a high success rate
# is not a good solution - a slightly reduced pool which includes all or almot all of the 
# sucesses is a better solution as the Bank's business is getting customers to deposit money 
# but at a reduced number of calls per success than in the campaign being examined.

# Initial data treatmen
#
# the data arrives as semi-colon seperated data - an unusual format 
# the choice is to convert outside R or inside

# Read file and convert while reading
# use the read.csv routine but apply it to data read with readLines and with
# gsub providing replacement of semicolons with commas
#
# textConnection makes the result of the gsub on readLines look like a file 
# to enable us to use read.csv on it 

bankFull <- read.csv (textConnection(gsub(";", ",", readLines("C:\\Users\\Ciara\\Desktop\\RStudioProjects\\DAProject18\\bank-additional-full.csv"))) 
                      , header = TRUE
                      , na.strings=c("")
                      , stringsAsFactors = TRUE
                      )


# let's look at what we have :
dim(bankFull)
# 41188 rows - we expected 41188 - success.
str(bankFull)
glimpse(bankFull)
summary(bankFull)

install.packages("dataQualityR")
library(dataQualityR)

data(bankFull)
num.file <- paste(tempdir(), "/dq_num.csv", sep= "")
cat.file <- paste(tempdir(), "/dq_cat.csv", sep= "")
checkDataQuality(data= crx, out.file.num= num.file, out.file.cat= cat.file)
#num.file <- paste(tempdir(), "/dqr_num.csv", sep= "")
#cat.file <- paste(tempdir(), "/dqr_cat.csv", sep= "")
#checkDataQuality(data= bankFull, out.file.num= num.file, out.file.cat= cat.file)

DataQualityReport <- read.csv (textConnection(gsub(";", ",", readLines("C:\\Users\\Ciara\\AppData\\Local\\Temp\\Rtmp2j0xMU\\dqr_num.csv"))) 
                      , header = TRUE
                      , na.strings=c("")
                      , stringsAsFactors = TRUE
)


# The data include some economic indicators which are the same for many rows 
# these have no bearing on the inclusion of the customer in a call list so we will drop
# these from the analysis data 
# 
# However this data could be analysed and used by the bank to determine 
# when to incread its marketing activity and when to reduce it - if there are 
# corelations between general economic indicators and the success of the campaig
#
# dropping 'economic' columns : emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed
#
bankFull <- bankFull[, -c(16,17,18,19,20)]
# confirm we did what we expected 
str(bankFull)

# we know that the call duration closely corelates to the outcome
# but it is not available before the customer is called so we need to drop it
# Also the number of contacts performed during this campaign is not available 
# as an initial selection inidcator so we have to drop it as well. 
bankFull <- bankFull[, -c(11, 12)]
# confirm we did what we expected 
str(bankFull)

# OK now we have our basic data 
# it's not too difficult to reproduce from scratch so no need to store an interim dataset.

# let's check for NA values 
sapply(bankFull, function(x) sum(is.na(x)))
# none - good job 

# let's look at the quality of the data that we have 
# and the individual corelation between the data and the outcome 

# outcome overall :
# hist (bankFull$y)

# Age 
# let's plot a histogram of age and add a plot of 
# age of successful calls 

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
par(mar=c(5,4,4,2))

hist (bankFull$age
      , breaks = 40 
      , col='lightblue'
      , main = "Call outcome by Age"
      , xlab = "Age Range"
      , ylab = "Number of Calls"
      , ylim = c(0,4000)
      )
hist (bankFull$age[bankFull$y=="yes"]
      , breaks = 40
      , col='salmon'
      , add=TRUE
      )
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
       )
        
# OK - Age doesn't seem to corelate significantly 

# JOB 
# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(8,4,4,2))


table(bankFull$job)
barplot (table(bankFull$job)
         , col='lightblue'
         , main = "Call outcome by Job"
         , xlab = ""
         , ylab = "Number of Calls"
         , ylim = c(0,12000)
         , las=2
         # , add=FALSE
         )
barplot (table(bankFull$job[bankFull$y=="yes"])
         , col='salmon'
         , las=2
         , add=TRUE
         
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
abline(h=0)

# OK job doesn't corelate closely

# marital 

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(5,4,4,2))


table(bankFull$marital)
barplot (table(bankFull$marital)
         , col='lightblue'
         , main = "Call outcome by Marital Status"
         , xlab = ""
         , ylab = "Number of Calls"
         , ylim = c(0,25000)
         , las=2
         # , add=FALSE
)
barplot (table(bankFull$marital[bankFull$y=="yes"])
         , col='salmon'
         , las=2
         , add=TRUE
         
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
abline(h=0)

# OK marital status doesn't corelate too closely

# education 

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(10,4,4,2))


table(bankFull$education)
barplot (table(bankFull$education)
         , col='lightblue'
         , main = "Call outcome by Education Level"
         , xlab = ""
         , ylab = "Number of Calls"
         , ylim = c(0,12000)
         , las=2
         # , add=FALSE
)
barplot (table(bankFull$education[bankFull$y=="yes"])
         , col='salmon'
         , las=2
         , add=TRUE
         
)
legend("topleft"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
# abline(h=0)

# OK educational status doesn't corelate too closely

# but it is clear that we need to refactor this so that we have 
# the education levels in order of achievement.

# credit default 

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(5,4,4,2))


table(bankFull$default)
table(bankFull$default[bankFull$y=="yes"])
table(bankFull$default,bankFull$y)
barplot (table(bankFull$default)
         , col='lightblue'
         , main = "Call outcome by Credit Default Status"
         , xlab = ""
         , ylab = "Number of Calls"
         , ylim = c(0,35000)
         , las=2
         # , add=FALSE
)
barplot (table(bankFull$default[bankFull$y=="yes"])
         , col='salmon'
         , las=2
         , add=TRUE
         
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
# abline(h=0)

# OK credit default status doesn't corelate too closely
# but frankly I'd exclude all credit deafult = yes from any 
# additional marketing as the sale may succeed but the product will
# not be likely to be profitable

# Housing Status

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(7,5,4,2))


table(bankFull$housing)
table(bankFull$housing,bankFull$y)
barplot (table(bankFull$housing)
         , col='lightblue'
         , main = "Call outcome by Housing Status\n"
         , xlab = ""
         , ylab = "Number of Calls\n"
         , ylim = c(0,25000)
         , las=2
         # , add=FALSE
)
barplot (table(bankFull$housing[bankFull$y=="yes"])
         , col='salmon'
         , las=2
         , add=TRUE
         
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
# abline(h=0)

# OK housing status doesn't corelate too closely

# Loan Status

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(5,5,4,2))


table(bankFull$loan)
table(bankFull$loan,bankFull$y)
barplot (table(bankFull$loan)
         , col='lightblue'
         , main = "Call outcome by Loan Status"
         , xlab = ""
         , ylab = "Number of Calls\n"
         , ylim = c(0,35000)
         , las=2
         # , add=FALSE
)
barplot (table(bankFull$loan[bankFull$y=="yes"])
         , col='salmon'
         , las=2
         , add=TRUE
         
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
# abline(h=0)

# OK loan status doesn't corelate too closely

# Contact Method

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(7,6,4,2))


table(bankFull$contact)
table(bankFull$contact,bankFull$y)
table(bankFull$contact,bankFull$default,bankFull$y)
barplot (table(bankFull$contact)
         , col='lightblue'
         , main = "Call outcome by Contact Method"
         , xlab = "Contact Method"
         , ylab = "Number of Calls\n"
         , ylim = c(0,28000)
         # , las=2
         # , add=FALSE
)
barplot (table(bankFull$contact[bankFull$y=="yes"])
         , col='salmon'
         #, las=2
         , add=TRUE
         
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
# abline(h=0)

# OK contact method doesn't corelate too closely
# but there does seem to be a better oucome for cellular than telephone
# (presumanly landline) 

# Month

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(7,6,4,2))


table(bankFull$month)
barplot (table(bankFull$month)
         , col='lightblue'
         , main = "Call outcome by Last Contact Month"
         , xlab = "Month"
         , ylab = "Number of Calls\n"
         , ylim = c(0,15000)
         # , las=2
         # , add=FALSE
)
barplot (table(bankFull$month[bankFull$y=="yes"])
         , col='salmon'
         #, las=2
         , add=TRUE
         
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
# abline(h=0)

# this is interesting = there is a clearly better outcome for calls made 
# in periods when there were few calls than in thse when there were many 
# 
# I think that what is happening here is that customers who are enageged with
# the bank make subsequent calls and therefore these later calls appear to 
# correlate with sucessful outcomes
# but this may be a false indicator as it may be data that is unavailable
# when selecting customers for calling


# Day of Week

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(7,6,4,2))


table(bankFull$day_of_week)
barplot (table(bankFull$day_of_week)
         , col='lightblue'
         , main = "Call outcome by Last Contact Day of Week"
         , xlab = "Day of Week"
         , ylab = "Number of Calls\n"
         , ylim = c(0,10000)
         # , las=2
         # , add=FALSE
)
barplot (table(bankFull$day_of_week[bankFull$y=="yes"])
         , col='salmon'
         #, las=2
         , add=TRUE
         
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
# abline(h=0)

# OK Day of Week has no significant correlation

# Previous Campaign

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
# margins: par(mar = c( bottom, left , top , right ))
par(mar=c(7,6,4,2))


table(bankFull$poutcome)
barplot (table(bankFull$poutcome)
         , col='lightblue'
         , main = "Call outcome by Previous Campaign Outcome"
         , xlab = "Previous Campaign Outcome"
         , ylab = "Number of Calls\n"
         , ylim = c(0,35000)
         # , las=2
         # , add=FALSE
)
barplot (table(bankFull$poutcome[bankFull$y=="yes"])
         , col='salmon'
         #, las=2
         , add=TRUE
         
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)
# abline(h=0)

# OK there is a clear indication that a successful outcome in a 
# previous campaign is a good indicator of a successful outcome here
# unfortunately there are very few positive previous outcomes !

# Number of contacts 
# this look like data which is not available 
# when selecting the customer as it is generated 
# during the campaign 

# better to remove it 

# pdays

# number of days passed after last contact on a previous campaign
# 999 means no previous campaing
# look at these seperately 

# numeric data - go back to histogram

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
par(mar=c(5,4,4,2))

hist (bankFull$pdays[bankFull$pdays!=999]
      , breaks = 10 
      , col='lightblue'
      , main = "Call outcome by number of days between contacts \n on a previous campaign"
      , xlab = "number of days between contacts"
      , ylab = "Number of Calls"
      , ylim = c(0,600)
)
hist (bankFull$pdays[(bankFull$y=="yes") & (bankFull$pdays!=999)]
      , breaks = 10
      , col='salmon'
      , add=TRUE
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)

# OK there is a clear indication that any previous
# contact is a good thing - but no particular 
# corelation with the number of days
# we may already have this data from the previous campaing
# indicator - so it may just be noise.



# previous

# number of previous contacts before this campaign 
# 0 means no contact
# let's ignore the 0 values they will overwhelm the other data 

# revert back to 1,1 - just in case we were doing something else
par(mfrow=c(1,1))
par(mar=c(5,4,4,2))

hist (bankFull$previous[bankFull$previous!=0]
      , breaks = 5 
      , col='lightblue'
      , main = "Call outcome by number of previous contacts"
      , xlab = "number of previous contacts"
      , ylab = "Number of Calls"
      , ylim = c(0,6000)
)
hist (bankFull$previous[(bankFull$y=="yes") & (bankFull$previous!=0)]
      , breaks = 5
      , col='salmon'
      , add=TRUE
)
legend("topright"
       , col = c('salmon', 'lightblue')
       , legend = c('Success    ', 'All Calls    ')
       , pch = 15 # 15 is a small filled square
       , pt.cex = 2
)

# OK there is already a clear indication that any previous
# contact is a good thing .
# Here we are seeing that higher number of prrevious contacts
# indicate better outcomes.

# write out the data frame so we don't have to rerun this component unless we
# change it


write.table(bankFull,"BankFull.dataframe")













