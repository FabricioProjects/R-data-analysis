# load dataframes
train <- read.csv("E:/Dev/share_win/csv_files/train.csv", header = TRUE)
test <- read.csv("E:/Dev/share_win/csv_files/test.csv", header = TRUE)

# drop column "PassengerId"
test$PassengerId <- NULL
train$PassengerId <- NULL

# add a column named "Survived" with all rowns named "none" as a new dataframe 
test.survived <- data.frame(Survived = rep("none", nrow(test)), test[,])

# combined two dataframes (appended)
data.combined <- rbind(train, test.survived)

# structure of data (data types)
str(data.combined)

# changing data types of columns
str(data.combined$Pclass)
data.combined$Pclass <- as.factor(data.combined$Pclass) # changing int type to factor type
str(data.combined$Pclass)  # just to see the difference
data.combined$Survived <- as.factor(data.combined$Survived)  # changing char type to factor type
str(data.combined$Survived)

# count of factors levels
table(data.combined$Survived)
table(data.combined$Pclass)

# install and load up a visualization library
# install.packages("ggplot2")
library(ggplot2)

# hypothesis: rich folks survived at higher rates
train$Pclass <- as.factor(train$Pclass) 

ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
       stat_count(width = 0.5) +
       xlab("Pclass") +
       ylab("Total Count") +
       labs(fill = "Survived")

# "Name" works better as a string
head(as.character(train$Name))

# unique names of "train" and "test" datasets
length(unique(as.character(data.combined$Name)))
length(unique(data.combined$Name))                # no data type check

# looking for duplicate strings (character)
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# finding the duplicate names in the "data.combined".
data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)
# lets detect the string pattern "Miss." in the dataframe 
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
# show only the range of first 5 raws but all columns
misses[1:5,]

# lets detect the string pattern "Mrs." (married women) in the dataframe 
mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrs[1:5,]

# there is a high survive rates betwen males too?
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

# creating a new function (extractTitle) to put a new variable called "Title" in the data.combined
# the function have just one argument (name)
extractTitle <- function(name)                 # function name
{       
  name <- as.character(name)                   # data type
  
  if (length(grep("Miss.",name)) > 0) 
     { return("Miss.") }
  else if (length(grep("Master.",name)) > 0) 
          { return("Master.") }
  else if (length(grep("Mrs.",name)) > 0) 
          { return("Mrs.") }
  else if (length(grep("Mr.",name)) > 0) 
          { return("Mr.") }
  else { return(" Other")}
}

titles <- NULL         # defining 
i      <- integer()    # defining 
for(i in 1:nrow(data.combined))   # loop on all rows
   {
    titles <- c(titles, extractTitle(data.combined[i,'Name']))  # aloca as variaveis da coluna "titles"
   }

data.combined$Title <- as.factor(titles) # creating the new columun

head(data.combined)  # checking

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# further investigation 
table(data.combined$Sex) 

ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# basic statistics about age (numeric data type)
summary(data.combined$Age)           # NA's = missing values
summary(data.combined[1:891,"Age"])  # NA's = missing values

ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  stat_count(width = 1) +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")

# closer look to "Master." (boys) statistics 
which(data.combined$Title == "Master.")
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

# closer look to "Miss."
miss <- data.combined[which(data.combined$Title == "Miss."),]
summary(miss$Age)

ggplot(miss[miss$Survived != "none",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  stat_count(width = 6) +
  ggtitle("Age for Miss. by Pclass") +
  xlab("Age") +
  ylab("Total Count") + 
  ylim(0,6)               # axis limits
  
# misses without parents
misses.alone <- miss[which(miss$SibSp == 0 & miss$Parch ==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5)) # how many young

# creating a column called "family size" in data.combined
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$Familysize <- as.factor(temp.sibsp + temp.parch +1)

ggplot(data.combined[1:891,], aes(x = Familysize, fill = Survived)) +
#  geom_histogram(stat = "count") +
  stat_count(width = 0.8) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass,Title") +
  xlab("Familysize") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# many factor lvls in "Ticket" so its better define it as string
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

# se o valor é "", substitui por " ". caso não, pega o primeiro caracter até o primeiro (1,1)
ticket.first.char <- ifelse(data.combined$Ticket == ""," ", substr(data.combined$Ticket,1,1))
unique(ticket.first.char)

# adiciona outra coluna ao dataframe "data.combined"
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# investigating "Fare"
summary(data.combined$Fare)
length(unique(data.combined$Fare)) 

ggplot(data.combined, aes(x = Fare)) +
  stat_count(width = 2) +
  ggtitle("Combined Fare Distribution ") +
  xlab("Fare") +
  ylab("Total Count") 
  
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  stat_count(width = 4) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass,Title") +
  xlab("Fare") +
  ylab("Total Count") +
  labs(fill = "Survived")  

# investigating "Cabin"
str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]
# replacing empty "Cabin" values for "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
levels(cabin.first.char)
# add new column "cabin.first.char"
data.combined$cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_histogram(stat = "count") +
#  stat_count(width = 1) +
  facet_wrap(~Pclass) +
  ggtitle("By cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

# multiple cabine
# yes -> there is space in the "data.combined$Cabin" string. Otherwise, "no"
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "),"yes","no"))

#=========================================
# RANDOM FOREST                          #
#=========================================

# install.packages("randomForest")
library(randomForest)

# get data to use in the model 1
rf.train.1 <- data.combined[1:891, c("Pclass","Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234) # unique seed
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1) # graphical interpretation

# get data to use in the model 2
rf.train.2 <- data.combined[1:891, c("Pclass","Title", "Familysize")]

set.seed(1234) # need to use the same seed to confront model 1
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, proximity = T, ntree = 1000)
rf.2
# visualização da importancia das variaveis
varImpPlot(rf.2)

# rf.2$confusion
# rf.2$err.rate
# rf.2$votes
# str(rf.2)

# visualização da saturação do numero de arvores
plot(rf.2) + 
  legend("topright", colnames(rf.2$err.rate),col=1:3,cex=0.7,fill=1:3)
rf.2                  # aproximação do modelo
table(train$Survived) # numero real

# visualização da proximidade
MDSplot(rf.2, data.combined$Survived)

# Pie Chart with Percentages
lbls <- c("Dead", "Live")
pct <- round(table(train$Survived)/sum(table(train$Survived))*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(table(train$Survived),labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Survived") 






