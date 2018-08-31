# project-D.R

# Random Forest (same as Project-C - excedpt that C uses a neural net)

# read the dataframe that we had saved at the end of project-B.R 
bankFull <- read.table("BankFull1.dataframe")
str(bankFull)

# adding a numerical representation of the outcome 0.0 (no) - 1.0 (yes)
# This is a trick to help train some models to give us a value between the two limits
# which we can use as a way of selecting a larger pool of custoemrs to call
# by choosing a low threshold - for this exercise we want to eliminate poor customers
# not only call the very good prospects.
str(bankFull)
bankFull$ynum <- 0.0
bankFull$ynum[bankFull$y=='yes'] <- 1.0
str(bankFull)
summary(bankFull)

# let's divide our data into a training and testing set
# we'll choose a 25% training set - not too large for computive intensive models
# but large enough to avoid soem problems with sparse values and overfitting

index <- sample(  dim(bankFull)[1]
                , (dim(bankFull)*0.25)[1]
                , replace = TRUE
                )
bankTrain <- bankFull[index,]
bankTest <- bankFull[-index,]



# Random Forest Prediction 
# using nnet (single hidden layer)
if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}



bankForest <- randomForest(ynum~age+job+marital+education+default+housing+loan
                           +contact+month+day_of_week+pdays+previous+poutcome
                , data = bankTrain
                , ntree=2000
                )


predicted <- predict(bankForest, newdata=bankTest)

summary (predicted)
str(predicted)
hist(predicted, breaks=100)

# OK let's derive our Call/Skip values from the nn predictions
# the key variable is the threhold we choose for skip

threshold = 0.1

Call <- rep('call', dim(bankTest)[1])
Call <- factor(Call, levels=c('skip', 'call'))
Call [predicted<threshold] <- 'skip'
table(Call)
table(Call, bankTest$y)

# call Success rate
print ('Call Success Rate')
print(table(Call, bankTest$y)[2,2]/ (table(Call, bankTest$y)[2,1]+table(Call, bankTest$y)[2,2]))

# OK the Random Forest works reasonably well - 
# pick your threshold of acceptability 
# and the output will give you a list of customers to call
# the higher the threshold the fewer the customers called but the 
# greater the conversion rate - so chosing the number is driven by the 
# business value of the deposit account vs the business cost of making the calls

# The random forest is more reliable than the neural net which sometimes returns
# useless results. There is a much better continuity of values albeit skewed toward the
# the lower end.

# Again the exercise is not to predict the actual yes but intead to select a pool 
# of customers to call with a given pool size or a given expected conversion rate.

# The business decision is to determine the threshold
# set up our output vectors
callsForest=c(0:40)  # number of calls
conversionForest=c(0:40) # expected converion % to success
salesForest=c(0:40) # percentage of 'available business achieved

# going to pull out some results for different thresholds
for (i in c(0:40)) {
  threshold <- (i/40)
  Call <- rep('call', dim(bankTest)[1])
  Call <- factor(Call, levels=c('skip', 'call'))
  Call [predicted<threshold] <- 'skip'
  callsForest[i+1] <- table(Call, bankTest$y)[2,1]+table(Call, bankTest$y)[2,2]
  conversionForest[i+1] <- table(Call, bankTest$y)[2,2]/ (table(Call, bankTest$y)[2,1]+table(Call, bankTest$y)[2,2]) 
  salesForest[i+1] <- table(Call, bankTest$y)[2,2] / (table(Call, bankTest$y)[1,2]+table(Call, bankTest$y)[2,2])
  }

# now plot the results
plot (callsForest, salesForest
      , type='l'
      , main = "Conversion rate of calls to business \n based on random forest classification of most likely customers"
      , xlab = "Number of calls made"
      , ylab = "Propotion of calls converted to business"
      , ylim = c(0,1)
      , lwd = 2
      , col='green'
      )
abline(v=0, col='grey')
lines (callsForest, conversionForest
       , lwd = 2
       , col = 'blue')
abline(h=0.42, col='grey', lty=2)
abline(h=1, col='grey')
abline(h=0, col='grey')
abline(v=3600, col='grey', lty=2)
abline(h=0.11, col='grey', lty=5)
#abline(v=0, col='grey')
legend("right"
       , col = c('white', 'blue', 'green')
       , legend = c('Call Effectiveness '
                    , 'Overall Conversion Rate'
                    , 'Proportion of opportunity realised'
       )
       , pch = 1 # 15 is a small filled square
       , pt.cex = 1
)










































































