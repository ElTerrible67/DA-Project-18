# project-C.R

# here is where we do that actual analysis of the data

# What we are looking for is a way to identify a group within the 
# customers which includes as many as possible of the yes answers while
# eliminating as many as possible of the no answers.

# Or the other way aroudn - find a way to idendify a pool of customers 
# with a very small or zero number of yes answers to exclude them from calling.

# as per previous comments the objective is to retain a larger pool of customers
# and only eliminate those with a high probabliity of being a no - rather than to 
# find a smalle pool with a good percentage of yes answers.

# So this means we are looking for methods which will give is a probability of being a yes
# or a no rather than a smple answer answer so that we can set the benchmark sufficiently 
# low to include a large pool of customers in the campaign while excluding the very low 
# probability candidates.

# As there is no factor with a good correlation with the result we will be working with 
# marginal probabilities so in general we will use a large pool for a training data 
# rather than a small one to avoid any overfitting.
# So 25% train 75% test for example rather than 10%-90% 

# read the dataframe that we had saved at the end of project-B.R 
bankFull <- read.table("BankFull1.dataframe")
str(bankFull)

# OK let's look at some tree based solutions

# load the rpart package
if (!require(rpart)) {
  install.packages("rpart")
  library(rpart)
}

# divide data into train and test 
# not now - lets just throw all the data a a tree
# to see how it looks.

# let's look at a regression tree
# with complexity set to very fine to see what makes a 
# difference.

regTree <- rpart(y~age+job+marital+education+default+housing+loan
                 +contact+month+day_of_week+pdays+previous+poutcome
                 , method = "anova"
                 , data = bankFull
                 , control=rpart.control(minsplit=10
                                         ,cp=0.001
                                         ,maxcompete=10)
                 )

str(regTree)
printcp(regTree)
plotcp(regTree)
rsq.rpart(regTree) # same as printcp()
print(regTree)
summary(regTree)
plot(regTree)
text(regTree)

# This doesn't really give us anything useful - even with the 
# complexity set very fine (i.e splits which make very little 
# difference will still be calcualted)

# This isn't really giving us a good approach
# what we want is to generate a probability function
# for each row and then adjust the 'include' probability
# until we get a appropriate subset of the data.

# adding a numerical representation of the outcome 0.0 (no) - 0.999 (yes)
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



# Neural Net Prediction 
# using nnet (single hidden layer)
if (!require(nnet)) {
  install.packages("nnet")
  library(nnet)
}

# going to deliberately apply weights to the successful calls
# so the neural net will give them more consideration
bankTrainWeights <- bankTrain$ynum
bankTrainWeights[bankTrainWeights==1] <- 10
bankTrainWeights[bankTrainWeights==0] <- 1

bankNet <- nnet(y~age+job+marital+education+default+housing+loan
                +contact+month+day_of_week+pdays+previous+poutcome
                , data = bankTrain
                , size=20
                , weights=bankTrainWeights
                , maxit=500
                )


predicted <- predict(bankNet, newdata=bankTest)

summary (predicted)
str(predicted)
hist(predicted, breaks=100)

# OK let's derive our Call/Skip values from the nn predictions
# the key variable is the threhold we choose for skip

threshold = 0.5

Call <- rep('call', dim(bankTest)[1])
Call <- factor(Call, levels=c('skip', 'call'))
Call [predicted<threshold] <- 'skip'
table(Call)
table(Call, bankTest$y)

# call Success rate
print ('Call Success Rate')
print(table(Call, bankTest$y)[2,2]/ (table(Call, bankTest$y)[2,1]+table(Call, bankTest$y)[2,2]))

# OK the nn works reasonably well - 
# pick your threshold of acceptability 
# and the output will give you a list of customers to call
# the higher the threshold the fewer the customers called but the 
# greater the conversion rate - so chosing the number is driven by the 
# business value of the deposit account vs the business cost of making the calls

# there is a significant bump in the histogram of the nn output 
# trying different value to try and eliminate this 

# below is a duplicate of above - but used for trying different settings 
# to explore the answers space - particularly to try and eliminate
# the spike that occurs in the predicted values.

# going to deliberately apply weights to the successful calls
# so the neural net will give them more consideration
# the weights chosen reflect the yes/no balance in the data
bankTrainWeights <- bankTrain$ynum
bankTrainWeights[bankTrainWeights==1] <- 8
bankTrainWeights[bankTrainWeights==0] <- 1

bankNet <- nnet(y~age+job+marital+education+default+housing+loan
                +contact+month+day_of_week+pdays+previous+poutcome
                , data = bankTrain
                , size=15
                , weights=bankTrainWeights
                , maxit=500
                , skip=TRUE
)


predicted <- predict(bankNet, newdata=bankTest)

# summary (predicted)
# str(predicted)
hist(predicted, breaks=200, xlim = c(0,1))

# OK let's derive our Call/Skip values from the nn predictions
# the key variable is the threhold we choose for skip

threshold = 0.40

Call <- rep('call', dim(bankTest)[1])
Call <- factor(Call, levels=c('skip', 'call'))
Call [predicted<threshold] <- 'skip'
table(Call)
table(Call, bankTest$y)

# call Success rate
print ('Call Success Rate')
print(table(Call, bankTest$y)[2,2]/ (table(Call, bankTest$y)[2,1]+table(Call, bankTest$y)[2,2]))

# a smaller hidden layer (~ 5 nodes) - seems to give a better spread of values
#

# actually the results are very variable 
# what we want is a even distribution of values which gives a 
# range of choices for the threshold - but on some runs the 
# data tends to all cluster around a small range 

# it's not clear how to control the bahavious of the nn to get a nice output histogram
# it looks like the best way is probably to just look at the range of values for the 
# training data and rerun it if there is a poor spread - once a good spread of 
# values is obtained then there is a clear gradient form 0 to 1 of poorer
# outcomes to better outcomes - so just pick the target or the budget and 
# let the nn predictor give you the custoemr list.

# setting skip=TRUE - gives a much better result
# with far fewer poor results with just a spike in the histogram
















































































