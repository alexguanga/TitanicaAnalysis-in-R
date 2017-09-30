# This CODE was used from a YouTube course on R and Data Science
# The link to the tutorial is https://www.youtube.com/channel/UCRhUp6SYaJ7zme4Bjwt28DQ
# As you'll see, lots of the comments are used as reminders for myself

#========================================================================================
# Load raw data
test <- read.csv("../test.csv", header=TRUE)
train <- read.csv("../train.csv", header=TRUE)

#========================================================================================

# Add a survived variable to the test data set to allow for combinbing data sets
# The data.frame is an object (like Python)

# 'rep' means to replicate the string "None" for 'nrow' times 
# (which is the total # of times in test)
# Lastly, we do this for all the rows and column in the test data set
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived) 

# Factos is a data type that structures it in categorize
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Info. on the R data types
str(data.combined)

# Create table of all the possible results (either 1 or 0 -> survived and didn't survived)
table(data.combined$Survived) 

# Distribution among survivability among Pclass
table(data.combined$Pclass)

#========================================================================================
##
## Data Visualization
##
#========================================================================================

# Load up ggplot 
library(ggplot2)

# Our intuition tells us that rich people will have a higher survivability
# let's look at the information to either prove or disprove our hypothesis

train$Pclass <- as.factor(train$Pclass)

# Pclass is the x-axis, color coordinate this on if they survived or not.
ggplot(train, aes(x=Pclass, fill=factor(Survived))) +
  geom_bar(width=0.5) +
  xlab("PClass") +
  ylab("Total Count") +
  labs(fill="Survived")


# Changing names from factors to strings
train$Name <- as.character(train$Name)

# Only get the info. for the first rows
head(train$Name)

# Unique names, but it seems there are some duplicates. There are 1307 names but 1309 observations
data.combined$Name <- as.character(data.combined$Name)
length(unique(data.combined$Name))

# Finding the issue with the duplicate values

# The 'which' is like the 'WHERE' clause in SQL, we only get the data that are duplicates in the Name column
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
print(dup.names)

# Layman: From the data.combined, I only want the values which are in dup.names
data.combined[which(data.combined$Name %in% dup.names),]

#========================================================================================
##
## Data Manipulation
##
#========================================================================================

# Upload string manipulation library
library(stringr)

# store names that have the word "Miss. " in it
misses <- data.combined[which(str_detect(data.combined$Name, "Miss. ")),]

# Format [row, column]. INDEXES START AT 1 not 0!
misses[1:5,1:7]

# Store for 'Mrs.'
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs. ")),]
mrses[1:5, 1:7]

# Store males
males <- data.combined[which(train$Sex == "male"),]
males[1:5, 1:7]

# Function returns the title of the name
extractTitle <- function(name) {
  name <- as.character(name)
  if (length(grep("Miss. ", name)) > 0){
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0){
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0){
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0){
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL 

# Uses the 'extractTitle' function to store the titles
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)

#========================================================================================
##
## Data Visualization
##
#========================================================================================

# Note: The range from 1:891 is only used bc we don't know the survivability of the other obs.

ggplot(data.combined[1:891,], aes(x=Title, fill=factor(Survived))) +
  geom_bar(width=0.5) + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill="Survived")

# Result: That woman and children survived more often, especially among middle and upper class
# A lot of men died from the lower and middle class (upper class were split even)

# Terminology -> We believe that 'Master': Young boys & 'Miss': Young girls


# Looking at the sex among different classes
ggplot(data.combined[1:891,], aes(x=Sex, fill=factor(Survived))) +
  geom_bar(width=0.5) + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill="Survived")

# Looking at age over the entire data set
summary(data.combined$Age)

# Looking at age over the train data set
summary(data.combined[1:891, "Age"]) 
# Result: there are 263 NA's (or missing values), 177 come from train dataset


ggplot(data.combined[1:891,], aes(x=Age, fill=factor(Survived))) +
  geom_histogram(binwidth=10) + 
  facet_wrap(~Sex + Pclass) + 
  xlab("Age") +
  ylab("Total Count") + 
  labs(fill='Survived')

# Validate that "Master. " is a good proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age) 
# Result: Seem positive. The max is 14.5 age and min is a 0.330

# Investigate information on "Miss." 
misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)
# Result: Not as good as master. Max is 63 and min is 0.17 


# Plotting the 'misses' using their Pclass and age
ggplot(misses[misses$Survived != "None",], aes(x=Age, fill=factor(Survived))) +
         facet_wrap(~Pclass) +
         geom_histogram(binwidth=10) +
         ggtitle("Age for Miss. by Pclass") +
         xlab("Age") +
         ylab("Total Count")


# Calculating 'misses' that are traveling alone 
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))
# Result: misses who are traveling tend to be young adults 


# Info on the sibsp variable
summary(data.combined$SibSp)
# Find the different number of Sibsp possible
length(unique(data.combined$SibSp))
# Change this data type to factor
data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x=SibSp, fill=factor(Survived))) +
  facet_wrap(~Pclass + Title) +
  geom_bar(width=1) +
  ggtitle("Pclass, title") +
  xlab("Sibsb") +
  ylab("Total Count") +
  ylim(0,300) + # Interval for the y-axis
  labs(fill='Survived')


# Info on the parch variable
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x=Parch, fill=factor(Survived))) +
  facet_wrap(~Pclass + Title) +
  geom_bar(width=1) +
  ggtitle("Pclass, title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) + # Interval for the y-axis
  labs(fill='Survived')

#========================================================================================
##
## Feature Enginering
##
#========================================================================================

# Create new variable called family size
# We are combining it one list after the other. 

# Ex. It stores all the 'Sibsb' variable in one big list and 'Parsh' in another large list
# then, we can combine both since they are indexed the same 
temp.sibsp <- c(train$SibSp, test$SibSp) 
temp.parsh <- c(train$Parch, test$Parch)
data.combined$FamilySize <- as.factor(temp.sibsp + temp.parsh + 1)

ggplot(data.combined[1:891,], aes(x=FamilySize, fill=factor(Survived))) +
  facet_wrap(~Pclass + Title) +
  geom_bar(width=1) +
  ggtitle("Pclass, title") +
  xlab("Family Size") +
  ylab("Total Count") +
  ylim(0,300) + 
  labs(fill='Survived')


# Info on the Ticket variable
str(data.combined$Ticket) # 929 factor levels, probably not a factor level

# Converting Ticket variable to string
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20] 
# Ticket variable looks a bit unstructured

# Looking at the first character in the Ticket variable
substr(data.combined$Ticket, 1, 1) 

# Smart way to check if there are any empty variable
ticket.var.char <- ifelse(data.combined$Title == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.var.char) 

# Creating new variable to the data.combined dataset
data.combined$Tick.FirstChar <- as.factor(ticket.var.char)

# High-level plot of the data
ggplot(data.combined[1:891, ], aes(x=Tick.FirstChar, fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Survival by Ticket Fare's first character") +
  xlab("Ticket fare's first character") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill="Survived")

# Adding facet-wrap or categorizing the data by Pclass
ggplot(data.combined[1:891, ], aes(x=Tick.FirstChar, fill=factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("PClass") +
  xlab("Ticket fare's first character") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill="Survived")

# Adding Title variable to the facet-wrap
ggplot(data.combined[1:891, ], aes(x=Tick.FirstChar, fill=factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("PClass, Title") +
  xlab("Ticket fare's first character") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill="Survived")

# Recap: The ticket variable doesn't seem to be providing much info, so we probably wouldn't 
# use that variable in the algorithm


# Info on the Fare variable
str(data.combined$Fare)
summary(data.combined$Fare)
length(unique(data.combined$Fare))

# Can't convert fare to factor, large number of observations. 
# Treat as numeric and visualize it
ggplot(data.combined, aes(x=Fare)) +
  geom_histogram(binwidth=5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200) 

# Let's see if fare has any predictive power
ggplot(data.combined[1:891, ], aes(x=Fare, fill=factor(Survived))) +
  geom_histogram(binwidth=5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("PClass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill="Survived")

# Recap: There's not much info that the fare can provide that we don't know already
# we don't want to add too much variables, bc then we will be overfitting


# Info on any Cab variable
str(data.combined$Cabin) 

# Cabin's is a factor level, changed that bc there are 187 levels
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100] # Note: There are a lot of blank of info on Cabin

# Replace empty cabin level with 'U', so the U variable will be passed onto all the 
# empty rows under the Cabin column
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- 'U'
data.combined$Cabin[1:100]

# Look at the first ch of cabin
length(unique(substr(data.combined$Cabin, 1, 1))) # Returns 9 unique values, could use as factors

# Create new var for the first ch of cabin
data.combined$Cabin.FirstChar <- as.factor(substr(data.combined$Cabin, 1, 1))
str(data.combined$Cabin.FirstChar)
levels(data.combined$Cabin.FirstChar)

# High-level plot: for each level of a level - make a graph
ggplot(data.combined[1:891, ], aes(x=Cabin.FirstChar, fill=factor(Survived))) +
  geom_bar() +
  ggtitle("PClass, Title") +
  xlab("Ticket cabin's first character") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill="Survived")

# Including PClass
ggplot(data.combined[1:891, ], aes(x=Cabin.FirstChar, fill=factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("PClass") +
  xlab("Ticket cabin's first character") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill="Survived")

# Including PClass and Title
ggplot(data.combined[1:891, ], aes(x=Cabin.FirstChar, fill=factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("PClass, Title") +
  xlab("Ticket cabin's first character") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill="Survived")

# What about people with multiple cabins?
# We use str_detect to check if there are any spaces, if there is, then we know there are multiple cabins
data.combined$MultipleCabins <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891, ], aes(x=MultipleCabins, fill=factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("PClass, Title") +
  xlab("Multiple Cabins") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill="Survived")
# Recap: Not much insight cabins provides


# Info. on embarked
str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891, ], aes(x=Embarked, fill=factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("PClass, Title") +
  xlab("Embarjed") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill="Survived")
# Recap: Not much info can be given by embarked


# Summry: Pclass, Title (combines age and sex into this), maybe we'll use sibsp and parch

#========================================================================================
#
# Exploratory Modeling
#
#========================================================================================

library(randomForest)

# Creating models, will be using random forest to build
rf.data.1 <- data.combined[1:891, c("Pclass", "Title")] # Only need the Pclass and Title columns
randomforest.label <- as.factor(train$Survived) 

set.seed(1234) # Setting the random num to a specific number 

# importance refers to recieving info on the importance of the parameters
rf.model.1 <- randomForest(x=rf.data.1, y=randomforest.label, importance=TRUE, ntree=1000)
rf.model.1

varImpPlot(rf.model.1) 

# Results - Confusion matrix:
#    0   1   class.error
# 0 538  11  0.02003643
# 1 174 168  0.50877193
# Layman: when somebody died (0 to the right of the matrix) it predicted 538 would die
# but 11 times they would suvived (~ 98% accurate)
# However, when somebody survived (1 to the right of the matrix) it predicted 174 would die 
# and 168 would survive (~50 percent accuracy)


# Train a Random Forest model with the parameters Pclass, title, and sibsb
rf.data.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]
set.seed(1234)

rf.model.2 <- randomForest(x=rf.data.2, y=randomforest.label, importance=TRUE, ntree=1000)
rf.model.2
varImpPlot(rf.model.2) # Results changed from 0.02003643 -> 0.1129326 and 0.50877193 -> 0.3333333

# Train a Random Forest with the parameters Pclass, title, and Parch
rf.data.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]
set.seed(1234)

rf.model.3 <- randomForest(x=rf.data.3, y=randomforest.label, importance=TRUE, ntree=1000)
rf.model.3
varImpPlot(rf.model.3)


# Train a Random Forest with the parameters Pclass, title, and SibSp, Parch
rf.data.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)

rf.model.4 <- randomForest(x=rf.data.4, y=randomforest.label, importance=TRUE, ntree=1000)
rf.model.4
varImpPlot(rf.model.4)


# Train a Random Forest with the parameters Pclass, title, and FamilySize
rf.data.5 <- data.combined[1:891, c("Pclass", "Title", "FamilySize")]

set.seed(1234)

rf.model.5 <- randomForest(x=rf.data.5, y=randomforest.label, importance=TRUE, ntree=1000)
rf.model.5
varImpPlot(rf.model.5)
# Familysize incorporates sibsp and parch which oculd be a reaosn that it helped the model
# better than using them sibsp and parch together


# Train a Random Forest with the parameters Pclass, title, sibsp, FamilySize
rf.data.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "FamilySize")]

set.seed(1234)

rf.model.6 <- randomForest(x=rf.data.6, y=randomforest.label, importance=TRUE, ntree=1000)
rf.model.6
varImpPlot(rf.model.6)
# Note: The order of the independent variables can influence the error rate


# Train a Random Forest with the parameters Pclass, title, parch, FamilySize
rf.data.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "FamilySize")]

set.seed(1234)

rf.model.7 <- randomForest(x=rf.data.7, y=randomforest.label, importance=TRUE, ntree=1000)
rf.model.7
varImpPlot(rf.model.7)

#========================================================================================
#
# Submission (First One)
#
#========================================================================================

# We need to use the test set to figure out how accurate are data actually is. This is 
# important since there's a possiblity that our models can be overfitting. 
# Let's start with a submission of our 5th model to Kaggle, since it did better than the other models

# Subset our test record and strategy
test.submit.df <- data.combined[892:1309, c("Pclass", "Title", "FamilySize")]

# Make prediction with our model and the test dataset
rf.5.predicts <- predict(rf.model.5, test.submit.df)
table(rf.5.predicts) 

# Create df that indicates the testing passenger suvival
submit.df <- data.frame(Passenger.ID = rep(892:1309), Survived = rf.5.predicts)

# Create csv file
write.csv(submit.df, file='RandomF_SUB_20170621.csv', row.names=FALSE)


#========================================================================================
#
# Cross Validation
#
#========================================================================================

# Our model scores 0.79426 in Kaggle. It's different that our 0.8159
# We have to dive into cross validation in hopes to be more accurate
library(caret)
library(doSNOW)


# Doing a 10-fold CV, repeated 10 times
set.seed(2348)
cv.10.folds <- createMultiFolds(randomforest.label, k=10, times=10) 

# Check stratification
table(randomforest.label)
table(randomforest.label[cv.10.folds[[3]]])


# Set up a caret's traincontrol object per above
# Let's try 10-fold CV repeated 10 times
train.ctrl.1 <- trainControl(method="repeatedcv", number=10, repeats=10, 
                             index=cv.10.folds)


# Set up a doSNOW package for multi-core packaging. This is helpful as we're going 
# to be training a lot of trees. 
cluster <- makeCluster(6, type="SOCK")
registerDoSNOW(cluster)

# Set up for reproducibility and train
# randomforest.train.5 are independent vars that were used in the random forest model
set.seed(34324)
rf.5.cv.1 <- train(x = rf.data.5, y = randomforest.label, method = "rf", tuneLength = 3,
                        ntree = 500, trControl = train.ctrl.1)

# Properly shut down the cluster
stopCluster(cluster)
rf.5.cv.1


# The above is slightly more pessimisitc than the rf.5 prediction, but not pessimistic enough. 

# Let's try 5-fold CV repeated 10 times
set.seed(5983)
cv.5.folds <- createMultiFolds(randomforest.label, k=5, times=10)
train.ctrl.2 <- trainControl(method="repeatedcv", number=5, repeats=10, 
                             index=cv.5.folds)

cluster <- makeCluster(6, type="SOCK")
registerDoSNOW(cluster)

rf.5.cv.2 <- train(x = rf.data.5, y = randomforest.label, method = "rf", tuneLength = 3,
                        ntree = 500, trControl = train.ctrl.2)
# Shutdown cluster
stopCluster(cluster)

# Check results
rf.5.cv.2


# Try 3-fold CV repeated 10 times.
set.seed(37596)
cv.3.folds <- createMultiFolds(randomforest.label, k=3, times=10)
train.ctrl.3 <- trainControl(method="repeatedcv", number=3, repeats=10, 
                             index=cv.3.folds)

cluster <- makeCluster(6, type="SOCK")
registerDoSNOW(cluster)

rf.5.cv.3 <- train(x = rf.data.5, y = randomforest.label, method = "rf", tuneLength = 3,
                        ntree = 64, trControl = train.ctrl.3)

# Shutdown cluster
stopCluster(cluster)

# Check results
rf.5.cv.3


#========================================================================================
#
# Exploratory Modeling 2
#
#========================================================================================

# Let's use a single decision tree to better understand what's going on with our 
# features. Obviously Random Forest are far more powerful than single trees, but single
# trees have the advantage of being easier to understand. 

library(rpart)
library(rpart.plot)

# Note: From the previous cross-validation, we'll use 3-fold CV 10 times

# Create utility function
# Note: our method is NOT rf but rpart now for decision trees 

rpart.cv <- function(seed, training, labels, ctrl) {
  cluster <- makeCluster(6, type="SOCK")
  registerDoSNOW(cluster)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 3, 
                    trControl = ctrl)
  
  # Shutdown cluster
  stopCluster(cluster)
  
  return (rpart.cv)
}

# Grab features
features <- c("Pclass", "Title", "FamilySize")

rpart.data.1 <- data.combined[1:891, features]

# Run CV with the train.ctrl.3 (which is 3-fold CV 10 times)
rpart.1.cv.1 <- rpart.cv(94622, rpart.data.1 , randomforest.label, train.ctrl.3)
rpart.1.cv.1


prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# The plot bring out some interesting lines of investigation. Namely:
#      1 - Titles of "Mr." and "Other" are predicted to perish at an 
#          overall accuracy rate of 83.2 %.
#      2 - Titles of "Master.", "Miss.", & "Mrs." in 1st & 2nd class
#          are predicted to survive at an overall accuracy rate of 94.9%.
#      3 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes equal to 5, 6, 8, & 11 are predicted to perish
#          with 100% accuracy.
#      4 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes not equal to 5, 6, 8, or 11 are predicted to 
#          survive with 59.6% accuracy.


# 1 - Looking at the first bullet point 
table(data.combined$Title)

# Parse out last name and title
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1:10]

last.names <- sapply(name.splits, "[", 1)

# Add last.name to the data frame
data.combined$LastName <- last.names

# Parsing title
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# Why is there a title with the?
data.combined[which(titles == "the"),]

# Restructing the way that titles are in the data combined
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles %in% c("Mme.")] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Major.", "Capt.")] <- "Officer"
table(titles)

# Create new title variable
data.combined$Updated.Title <- as.factor(titles)

# Visualize the new version of title
ggplot(data.combined[1:891,], aes(x=Updated.Title, fill=Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Survival Rate Categorizes on Updated Titles")

# Reorganize titles based on the ggplot visualization
indexes <- which(data.combined$Updated.Title == "Lady.")
data.combined$Updated.Title[indexes] <- "Mrs."

indexes <- which(data.combined$Updated.Title == "Dr." |
                   data.combined$Updated.Title == "Rev." |
                   data.combined$Updated.Title == "Officer" |
                   data.combined$Updated.Title == "Sir.")
data.combined$Updated.Title[indexes] <- "Mr."

# Visualize the new version of title (with titles reorganized)
ggplot(data.combined[1:891,], aes(x=Updated.Title, fill=Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Survival Rate Categorizes on Updated Titles")


# Grab features to fit into the decison trees
features <- c("Pclass", "Updated.Title", "FamilySize")
rpart.data.2 <- data.combined[1:891, features]

# Rebuild the rpart tree with the updated info we now have on titles
rpart.2.cv.1 <- rpart.cv(94622, rpart.data.2, randomforest.label, train.ctrl.3)

prp(rpart.2.cv.1$finalModel, type=0, extra=1, under=TRUE)
# Result: We still don't have much classification on the Mr. We know that men in first 
# class prob. had a higher survivability than those in third class

# Understanding man from the "1" Pclass
indexes.mr.firstclass <- which(data.combined$Updated.Title == "Mr." & data.combined$Pclass == "1")
firstclass.mr.df <- data.combined[indexes.mr.firstclass, ]

summary(firstclass.mr.df)
# Result: There's one female, but all this should be males

# Correctly categorize the female doctor
firstclass.mr.df[firstclass.mr.df$Sex == "female",]

# Fixing all instances where females are included in the Mr subclass
indexes <- which(data.combined$Updated.Title == "Mr." & data.combined$Sex == "female")
data.combined$Updated.Title[indexes] <- "Mrs."

# Are there any other slipups?
length(which(data.combined$Sex == "female" &
               (data.combined$Updated.Title == "Master." |
               data.combined$Updated.Title == "Mr.")))

# Refresh dataframe
indexes.mr.firstclass <- which(data.combined$Updated.Title == "Mr." & data.combined$Pclass == "1")
firstclass.mr.df <- data.combined[indexes.mr.firstclass, ]

# Look at the first class Mr.
summary(firstclass.mr.df[firstclass.mr.df$Survived == "1", ])
View(firstclass.mr.df[firstclass.mr.df$Survived == "1", ])

# Looking at high fares
indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])

# Visualize survival rate of "Mr." by fares
ggplot(firstclass.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by Fare")


# Do some feature enginering based on the passengers with the same tickets
# Setting the following info. to 0
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)


for (i in 1:length(tickets)) {
  current.tick <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.tick)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$Ticket.Party.Size <- ticket.party.size
data.combined$Avg.Fare <- avg.fare

# Refresh 1st class "Mr" dataframe
firstclass.mr.df <- data.combined[indexes.mr.firstclass, ]
summary(firstclass.mr.df)


# Visualize new features
ggplot(firstclass.mr.df[firstclass.mr.df$Survived != "None",], aes(x = Ticket.Party.Size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rate 1st Class by the ticket party size")

ggplot(firstclass.mr.df[firstclass.mr.df$Survived != "None",], aes(x = Avg.Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rate 1st class by the average fare")


# After looking at the previous two ggplots, we can make a hypothesis that ticket.party.size
# is highly correlated with avg.fare
summary(data.combined$Avg.Fare)

# One missing value, take a look
data.combined[is.na(data.combined$Avg.Fare), ]

# Records of similar passengers and summarize avg.fares of the missing value
indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." & FamilySize == "1" &
                                       Ticket != "1044"))
similar.na.passengers <- data.combined[indexes, ]
summary(similar.na.passengers$Avg.Fare)

# Use median since close to mean and a little higher than mean
data.combined[1044, "Avg.Fare"] <- 7.840

# Leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("Ticket.Party.Size", "Avg.Fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postprod.data.combined <- predict(preProc, preproc.data.combined)

# Check if there's any correlation between 
cor(postprod.data.combined$Ticket.Party.Size, postprod.data.combined$Avg.Fare)
# Result: The correlation was quite low - 0.09428625

# Check the correlation only on 1st class 
indexes <- which(data.combined$Pclass == "1")
cor(postprod.data.combined$Ticket.Party.Size[indexes], 
    postprod.data.combined$Avg.Fare[indexes])
# Result: A higher correlation but still fairly low - 0.2576249


# Let's check the new updates on the feature enginnering
features <- c("Pclass", "Updated.Title", "FamilySize", "Ticket.Party.Size", "Avg.Fare")
rpart.data.3 <- data.combined[1:891, features]

# Run CV 
rpart.3.cv.1 <- rpart.cv(94622, rpart.data.3, randomforest.label, train.ctrl.3)

prp(rpart.3.cv.1$finalModel, type=0, extra=1, under=TRUE)



#========================================================================================
#
# Submitting models to Kaggle
#
#========================================================================================

# Subset of the test set
test.submit.df <- data.combined[892:1309, features]

# Make prediction
rpart3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart3.preds)

submit.df <- data.frame(PassengerID = rep(892:1309), Survived = rpart3.preds)
write.csv(submit.df, file = "RPART_SUB_20170714_1.csv", row.names = FALSE)



# --------------------------- Random forest ---------------------------

features <- c("Pclass", "Updated.Title", "Ticket.Party.Size")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = randomforest.label, ntree=1000)

test.submit.df <- data.combined[892:1309, features]

# Make prediction
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

#========================================================================================
#
# Extra information
#
#========================================================================================

# If we want to improve our model, a good place to start is focusing on where it
# gets things wrong!

library(infotheo)

# These are discrete variables
mutinformation(randomforest.label, data.combined$Pclass[1:891])
mutinformation(randomforest.label, data.combined$Sex[1:891])
mutinformation(randomforest.label, data.combined$SibSp[1:891])
mutinformation(randomforest.label, data.combined$Parch[1:891])
mutinformation(randomforest.label, data.combined$Embarked[1:891])
mutinformation(randomforest.label, data.combined$Title[1:891])
mutinformation(randomforest.label, data.combined$FamilySize[1:891])
mutinformation(randomforest.label, data.combined$Tick.FirstChar[1:891])
mutinformation(randomforest.label, data.combined$Cabin[1:891])
mutinformation(randomforest.label, data.combined$Updated.Title[1:891])
mutinformation(randomforest.label, data.combined$Ticket.Party.Size[1:891])

# Not discrete variables
mutinformation(randomforest.label, discretize(data.combined$Fare[1:891]))
mutinformation(randomforest.label, discretize(data.combined$Avg.Fare[1:891]))



library(Rtsne)

most.correct <- data.combined[data.combined$Updated.Title != "Mr.",]
indexes <- which(most.correct$Survived != "None")

tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("Visualization of Features")
# tsne.1$Y is a matrix of x and y components
# Look for where there are similar group of dots

condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))
condinformation(randomforest.label, data.combined[1:891, c("Updated.Title", "Pclass")])


misters <- data.combined[data.combined$Updated.Title == "Mr.",]
indexes <- which(misters$Survived != "None")
tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = misters$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("Visualization of Features")
# Predominatly we see large of orange data
# It's difficult understanding the green dots within this amount of orange clusters

condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$Survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("Visualization of Features")

condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the variable to our data.combined dataframe
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]