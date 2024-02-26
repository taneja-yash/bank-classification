bank.df <- read.csv('bank.csv') #import the csv file
head(bank.df) #look at top 6 rows of the data


library(ggplot2)

#Histograms for numerical variables
hist(bank.df$age, 
     main = "Histogram - Age",xlab = "Age",ylab = "Frequency",col = "gray",border = "black",
     xlim = c(min(bank.df$age), max(bank.df$age)))

hist(bank.df$duration, 
     main = "Histogram - Duration",xlab = "Age",ylab = "Frequency",col = "gray",border = "black",
     xlim = c(min(bank.df$duration), max(bank.df$duration)))

#boxplots for numerical variables
boxplot(bank.df$age,main = "Boxplot - Age",ylab = "Age",
        col = c("skyblue", "lightgreen", "lightcoral"),border = "black")

boxplot(bank.df$duration,main = "Boxplot - Age",ylab = "Age",
        col = c("skyblue", "lightgreen", "lightcoral"),border = "black")

#Countplots for categorical variables
ggplot(bank.df, aes(x = job, fill = job))+
  geom_bar()+
  labs(title = "Job Type Counts", x = "Job Type", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = marital, fill = marital))+
  geom_bar()+
  labs(title = "Marital Status Counts", x = "Marital Status", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = education, fill = education))+
  geom_bar()+
  labs(title = "Education Counts", x = "Education", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = default, fill = default))+
  geom_bar()+
  labs(title = "Default Counts", x = "Default", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = housing, fill = housing))+
  geom_bar()+
  labs(title = "Housing Counts", x = "Housing", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = loan, fill = loan))+
  geom_bar()+
  labs(title = "Loan Counts", x = "Loan", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = contact, fill = contact))+
  geom_bar()+
  labs(title = "Contact Type Counts", x = "Contact Type", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = month, fill = month))+
  geom_bar()+
  labs(title = "Month Counts", x = "Month", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = day_of_week, fill = day_of_week))+
  geom_bar()+
  labs(title = "Day of Week Counts", x = "Day of Week", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = poutcome, fill = poutcome))+
  geom_bar()+
  labs(title = "Outcome Counts", x = "Outcome", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

ggplot(bank.df, aes(x = y, fill = y))+
  geom_bar()+
  labs(title = "Result Counts", x = "Result", y = "Count")+
  theme_gray() +
  scale_fill_brewer(palette = "Paired")

barplot(table(bank.df$job), horiz = TRUE, col = "gray",
        main = "Bar Plot - Job Types", xlab = "Frequency", ylab = "Job")

barplot(table(bank.df$marital), horiz = TRUE, col = "gray",
        main = "Bar Plot - Marital Status", xlab = "Frequency", ylab = "Marital Status")

barplot(table(bank.df$education), horiz = TRUE, col = "gray",
        main = "Bar Plot - Education Level", xlab = "Frequency", ylab = "Education Level")

barplot(table(bank.df$housing), horiz = TRUE, col = "gray",
        main = "Bar Plot - Housing", xlab = "Frequency", ylab = "Housing")

barplot(table(bank.df$loan), horiz = TRUE, col = "gray",
        main = "Bar Plot - Loan", xlab = "Frequency", ylab = "Has a loan")

barplot(table(bank.df$contact), horiz = TRUE, col = "gray",
        main = "Bar Plot - Contact Types", xlab = "Frequency", ylab = "Contact Types")

barplot(table(bank.df$month), horiz = TRUE, col = "gray",
        main = "Bar Plot - Month", xlab = "Frequency", ylab = "Month")

barplot(table(bank.df$day_of_week), horiz = TRUE, col = "gray",
        main = "Bar Plot - Day of the Week", xlab = "Frequency", ylab = "Day of the Week")

barplot(table(bank.df$campaign), horiz = TRUE, col = "gray",
        main = "Bar Plot - Campaign Types", xlab = "Frequency", ylab = "Campaign Types")

barplot(table(bank.df$previous), horiz = TRUE, col = "gray",
        main = "Bar Plot - Previous Contact", xlab = "Frequency", ylab = "Previous Contact")

barplot(table(bank.df$y), horiz = TRUE, col = "gray",
        main = "Bar Plot - Result", xlab = "Frequency", ylab = "Result")


#Finding percentage of records that have result = No
percentage.no = round((sum(bank.df$y == 'no')/nrow(bank.df))*100, 2)

print(paste("Percentage of records where result = No: ", percentage.no, "%", sep = ""))
print(paste("Percentage of records where result = Yes: ", 100 - percentage.no, "%", sep = ""))

#There is a need to balance out the Yes and No records


#Replacing 'unknown' values in our data to NA so that we can remove them
bank.df$job = ifelse(bank.df$job == 'unknown', NA, bank.df$job)
bank.df$marital = ifelse(bank.df$marital == 'unknown', NA, bank.df$marital)
bank.df$age = ifelse(bank.df$age == 'unknown', NA, bank.df$age)
bank.df$education = ifelse(bank.df$education == 'unknown', NA, bank.df$education)
bank.df$default = ifelse(bank.df$default == 'unknown', NA, bank.df$default)
bank.df$housing = ifelse(bank.df$housing == 'unknown', NA, bank.df$housing)
bank.df$loan = ifelse(bank.df$loan == 'unknown', NA, bank.df$loan)
bank.df$contact = ifelse(bank.df$contact == 'unknown', NA, bank.df$contact)
bank.df$month = ifelse(bank.df$month == 'unknown', NA, bank.df$month)
bank.df$day_of_week = ifelse(bank.df$day_of_week == 'unknown', NA, bank.df$day_of_week)
bank.df$duration = ifelse(bank.df$duration == 'unknown', NA, bank.df$duration)
bank.df$campaign = ifelse(bank.df$campaign == 'unknown', NA, bank.df$campaign)
bank.df$pdays = ifelse(bank.df$pdays == 'unknown', NA, bank.df$pdays)
bank.df$previous = ifelse(bank.df$previous == 'unknown', NA, bank.df$previous)
bank.df$poutcome = ifelse(bank.df$poutcome == 'unknown', NA, bank.df$poutcome)
bank.df$y = ifelse(bank.df$y == 'unknown', NA, bank.df$y)

#Checking the first 6 rows of the dataframe
head(bank.df)

#Checking number of NA values
sum(is.na(bank.df))
sum(is.na(bank.df$default))

#dropping the default column as it has around 7k NA values
bank.df = bank.df[, !(names(bank.df) %in% c('default'))]
head(bank.df)

#writing a function to find mode to replace the NA values
mode_calculation = function(x){
  unique.x = unique(x)
  unique.x[which.max(tabulate(match(x, unique.x)))]
}


#replace NA values by the column mode
mode.job = mode_calculation(bank.df$job)
bank.df$job = ifelse(is.na(bank.df$job), mode.job, bank.df$job)

mode.marital = mode_calculation(bank.df$marital)
bank.df$marital = ifelse(is.na(bank.df$marital), mode.marital, bank.df$marital)

mode.education = mode_calculation(bank.df$education)
bank.df$education = ifelse(is.na(bank.df$education), mode.education, bank.df$education)

mode.housing = mode_calculation(bank.df$housing)
bank.df$housing = ifelse(is.na(bank.df$housing), mode.housing, bank.df$housing)

mode.loan = mode_calculation(bank.df$loan)
bank.df$loan = ifelse(is.na(bank.df$loan), mode.loan, bank.df$loan)

#checking for NA values to see if there are any left
sum(is.na(bank.df))

library(GGally)
#creating pairplot for the columns
ggpairs(bank.df)

#Creating a new column to store 0 if pdays = 999, i.e., the customer has never been contacted
bank.df$pdays_yes = ifelse(bank.df$pdays == 999, 0, 1)
#dropping pdays
bank.df = bank.df[, !(names(bank.df) %in% c('pdays'))]
head(bank.df)

#creating model matrix
new.df = model.matrix(~.-y-1, data = bank.df)

#changing the last column values from no to 0 and yes to 1
bank.df$y = ifelse(bank.df$y == 'no', 0, 1)
new.df <- cbind(new.df, result = bank.df$y)


#changing new.df back to a dataframe
new.df = as.data.frame(new.df)

#Changing the format of values as '-' interrrupts further code
names(new.df)[names(new.df) == "jobblue-collar"] = "jobblue_collar"
names(new.df)[names(new.df) == "jobself-employed"] = "jobself_employed"

library(ROSE)

#oversampling the data to have almost equal representation of records with result 0 and 1
oversampled.df = ovun.sample(result ~ ., data = new.df, method = "over", N = 53000, seed = 1234)$data

#Again checking for percentage of records having 0 and 1 as their results
new.percentage = round((sum(oversampled.df$result == 0)/nrow(oversampled.df))*100, 2)

print(paste("Percentage of records where result = No: ", new.percentage, "%", sep = ""))
print(paste("Percentage of records where result = Yes: ", 100 - new.percentage, "%", sep = ""))

set.seed(1234) #set the seed so that results do not change everytime the code is run

#taking out indices to include in the training data
train.indices = sample(nrow(oversampled.df), 0.8 * nrow(oversampled.df))

#creating the training data and the test data
train.data = oversampled.df[train.indices, ]
test.data = oversampled.df[-train.indices, ]

x.train = train.data[, -ncol(train.data)]
y.train = train.data[, ncol(train.data)]

x.test = test.data[, -ncol(test.data)]
y.test = test.data[, ncol(test.data)]

y.train = as.factor(y.train)
y.test = as.factor(y.test)

#Classification:

# 1. Decision Tree
library(rpart) #import the library

dt = rpart(y.train ~ ., data = x.train ,method = "class") #train the model

#print the decision tree
printcp(dt)
print(dt)

#plot the decision tree
plot(dt)
text(dt, use.n = T, all = T)

#making predictions using decision tree and finding accuracy of the model
dt.pred = predict(dt, newdata = x.test, type = "class")

dt.acc = round(mean(dt.pred == y.test)*100, 2)
cat("Accuracy of the Decision Tree = " , dt.acc, "%")

#confusion matrix
library(caret)
dt.conf.mat = confusionMatrix(table(y.test, dt.pred))
print(dt.conf.mat)

dt.prec = round(dt.conf.mat$byClass["Pos Pred Value"]*100, 2)
dt.sens = round(dt.conf.mat$byClass["Sensitivity"]*100, 2)


#ROC curve
library(pROC)
dt.roc = roc(y.test, dt.pred)
plot(dt.roc, main = "ROC Curve",col = "red", lwd = 2,legacy.axes = TRUE)

# 2. Random Forest

library(randomForest)

rf = randomForest(y.train ~ ., data = x.train, ntree = 5)

print(rf)

rf.pred = predict(rf, x.test)

rf.acc = round(mean(rf.pred == y.test)*100, 2)
cat("Accuracy of the Random Forest = " , rf.acc, "%")

rf.conf.mat = confusionMatrix(table(y.test, rf.pred))
print(rf.conf.mat)

rf.prec = round(rf.conf.mat$byClass["Pos Pred Value"]*100, 2)
rf.sens = round(rf.conf.mat$byClass["Sensitivity"]*100, 2)

rf.roc = roc(y.test, rf.pred)
plot(rf.roc, main = "ROC Curve",col = "red", lwd = 2,legacy.axes = TRUE)

# 3. Logistic Regression

logr = glm(y.train ~ ., data = x.train, family = binomial)
summary(logr)

logr.pred = predict(logr, x.test, type = "response")
logr.pred = ifelse(logr.pred >= 0.5, 1, 0)

logr.acc = round(mean(logr.pred == y.test)*100, 2)
cat("Accuracy of the Logistic Regression Model = ", logr.acc, "%")

logr.conf.mat = confusionMatrix(table(y.test, logr.pred))
print(logr.conf.mat)

logr.prec = round(logr.conf.mat$byClass["Pos Pred Value"]*100, 2)
logr.sens = round(logr.conf.mat$byClass["Sensitivity"]*100, 2)

logr.roc = roc(y.test, logr.pred)
plot(logr.roc, main = "ROC Curve",col = "red", lwd = 2,legacy.axes = TRUE)

# 4. K-Nearest Neighbors

library(class)

knn.model = knn(train = x.train, test = x.test, cl = y.train, k = 3)

knn.acc = round(mean(knn.model == y.test)*100, 2)
cat("Accuracy of the KNN Model = ", knn.acc, "%")

knn.conf.mat = confusionMatrix(table(y.test, knn.model))
print(knn.conf.mat)

knn.prec = round(knn.conf.mat$byClass["Pos Pred Value"]*100, 2)
knn.sens = round(knn.conf.mat$byClass["Sensitivity"]*100, 2)

knn.roc = roc(y.test, knn.model)
plot(knn.roc, main = "ROC Curve",col = "red", lwd = 2,legacy.axes = TRUE)

# 5. Support Vector Machine

library(e1071)
svm.model = svm(y.train ~ ., data = x.train)

svm.pred = predict(svm.model, x.test)

svm.acc = round(mean(svm.pred == y.test)*100, 2)

cat("Accuracy of the SVM Model = ", svm.acc, "%")

svm.conf.mat = confusionMatrix(table(y.test, svm.pred))
print(svm.conf.mat)

svm.prec = round(svm.conf.mat$byClass["Pos Pred Value"]*100, 2)
svm.sens = round(svm.conf.mat$byClass["Sensitivity"]*100, 2)

svm.roc = roc(y.test, svm.pred)
plot(svm.roc, main = "ROC Curve",col = "red", lwd = 2,legacy.axes = TRUE)

#creating a dataframe storing accuracies of the models and plotting it to compare the models
model.names = c("Decision Tree", "Random Forest", "Logistic Regression", "K-Nearest Neighbors", "Support Vector Machines")
acc.scores = c(dt.acc, rf.acc, logr.acc, knn.acc, svm.acc)
prec.scores = c(dt.prec, rf.prec, logr.prec, knn.prec, svm.prec)
prec.scores = unname(as.list(prec.scores))
sens.scores = c(dt.sens, rf.sens, logr.sens, knn.sens, svm.sens)
sens.scores = unname(as.list(sens.scores))


acc.scores.df = as.data.frame(cbind(model.names, acc.scores, prec.scores, sens.scores))
acc.scores.df 

ggplot(acc.scores.df, aes(x = model.names, y = acc.scores, fill = model.names)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Accuracy Scores", x = "Model Name", y = "Accuracy Score") +
  theme_minimal()

dt.pred <- as.numeric(as.character(dt.pred))
rf.pred <- as.numeric(as.character(rf.pred))
logr.pred <- as.numeric(as.character(logr.pred))
knn.model <- as.numeric(as.character(knn.model))
svm.pred <- as.numeric(as.character(svm.pred))

#using ensemble learning to combine the models
ensemble.pred = ifelse((dt.pred + rf.pred + logr.pred + knn.model + svm.pred) > 4, 1, 0)

ensemble.acc = mean(ensemble.pred == y.test) * 100

cat("Accuracy of the ensemble model = ", round(ensemble.acc, 2), "%")
