library(readr)
mydata <- read_csv("General/New folder (3)/Material/YEAR 3/New Approach/Datasets/DataFinal.csv")
View(mydata)

library(bnlearn)
install.packages("caTools")
library(caTools)

library(caret)
# normalizing data
data <- preProcess(as.data.frame(mydata), method=c("range"))

mydata <- predict(data, as.data.frame(mydata))
mydata

#splitting dataset into train and test data
data_split = sample.split(Y = mydata$Date, SplitRatio = 0.8)

#subsetting into initial Train data
train = mydata[data_split,]

#subsetting into Test data
test = mydata[!data_split,]

#splitting dataset into train and validation data
train_split = sample.split(Y = train$Date, SplitRatio = 0.8)

#subsetting into Train data
train_final = train[train_split,]

#subsetting into Validation data
Validation = train[!train_split,]

train_final <- train_final[,-1]


#grow=shrink algorithm
t <- gs(train_final)
plot(t)

#iamb
t2 <- iamb.fdr(train_final)
plot(t2)

#testing if both graphs are equal or not
all.equal(t,t2)

#number of tests used by each algorithm
ntests(t)
ntests(t2)

#storing the test log in a text file
sink(file = "General/New folder (3)/Material/YEAR 3/New Approach/Scripts/log.txt")
t = gs(train_final, debug = TRUE)
sink()

#try un-optimized and non-strict algorithm version
t3 <- gs(train_final, optimized = FALSE, strict = FALSE, debug = FALSE)
