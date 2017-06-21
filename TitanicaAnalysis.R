
# Load raw data
test <- read.csv("test.csv", header=TRUE)
train <- read.csv("train.csv", header=TRUE)

# Add a survived variable to the test data set to allow for combinbing data sets
# The data.frame is an objecte like Python
# 'rep' means to replicate the string "None" for 'nrow' times (which is the total # of times in test)
# Lastly, we do this for all the rows and column in the test data set
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived) 

# factors is a data type that structures it in categorize
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Info. on the R data types
str(data.combined)

# Create table of all the possible variables and the amnt. to each variable 
table(data.combined$Survived) 

# Distribution among classes, third class has the most amnt ppl while the second class has the least amt of ppl
table(data.combined$Pclass)

# load up ggplot 
library(ggplot2)

# Hypothesis - Rich people survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
# Train is the file, Pclass is the x-axis, color coordinate this on if they survived or not.
# Geom is the bar graph, the rest are labels
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
# The which is like a 'WHERE' clause in SQL, we only get the data that are duplicates in the Name column
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
print(dup.names)

# Look at the data from these duplicates values
# Layman: From the data.combined, I only want the values which are in dup.names
data.combined[which(data.combined$Name %in% dup.names),]

# Cool string manipulation
library(stringr)

# Like 'Miss.', load up all the strings that have the word "Miss. " in it
misses <- data.combined[which(str_detect(data.combined$Name, "Miss. ")),]

# Format [row, column]. INDEXES START AT 1 not 0!
misses[1:5,1:7]

# Using 'Mrs.' now
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs. ")),]
mrses[1:5, 1:7]

# Check males to see if any patters
males <- data.combined[which(train$Sex == "male"),]
males[1:5, 1:7]

# Expand relationship btw survival and pclass by adding new variable 'Title'
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

titles <- NULL # An empty variable 

# The i is the row, and the name is the column
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)

# We only want info for survived in the train set, must only use that
# The test dataset is used so we can try to test our result with that dataset
ggplot(data.combined[1:891,], aes(x=Title, fill=factor(Survived))) +
  geom_bar(width=0.5) + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill="Survived")

# Graph above tell us that women and children first, especially among middle and upper class
# A lot of men died from the lower and middle class (upper class were split even)

# Master: Young boys & Miss: Young girls

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
summary(data.combined[1:891, "Age"]) # Only looking at the train dataset
ggplot(data.combined[1:891,], aes(x=Age, fill=factor(Survived))) +
  geom_histogram(binwidth=10) + 
  facet_wrap(~Sex + Pclass) + 
  xlab("Age") +
  ylab("Total Count") + 
  labs(fill='Survived')
# Result: there are 263 NA's (or missing values), 177 come from train dataset


# Validate that "Master. " is a good proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)
# Result: Seem positive. The max is 14.5 age and min is a 0.330


# Investigate information on "Miss." 
misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)
# Result: Not quite positive. Max is 63 and min is 0.17 

# Only getting info. from the table where Survived is not equal to None
ggplot(misses[misses$Survived != "None",], aes(x=Age, fill=factor(Survived))) +
         facet_wrap(~Pclass) +
         geom_histogram(binwidth=10) +
         ggtitle("Age for Miss. by Pclass") +
         xlab("Age") +
         ylab("Total Count")


# Calculating misses that are traveling alone 
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





