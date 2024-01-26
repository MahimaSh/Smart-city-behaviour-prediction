library(readr)
mydata <- read_csv("General/New folder (3)/Material/YEAR 3/New Approach/Datasets/DataFinal.csv")
View(mydata)

library(bnlearn)
library(caTools)

library(caret)
# normalizing data
data <- preProcess(as.data.frame(mydata), method=c("range"))

newdata <- predict(data, as.data.frame(mydata))
newdata

#splitting dataset into train and test data
data_split = sample.split(Y = newdata$Date, SplitRatio = 0.8)

#subsetting into initial Train data
train = newdata[data_split,]

#subsetting into Test data
test.set = newdata[!data_split,]

#splitting dataset into train and validation data
train_split = sample.split(Y = train$Date, SplitRatio = 0.8)

#subsetting into Train data
train.set = train[train_split,]

#subsetting into Validation data
validation.set = train[!train_split,]

validation.set <- validation.set[,-1]
train.set <- train.set[,-1]

#k-fold cross validation

#handling missing data
#incomplete.data = train_final
#for (col in seq(ncol(incomplete.data))){
#  incomplete.data[sample(nrow(incomplete.data), 2442), col] = NA
#}
dag = structural.em(train.set)   
dag
plot(dag)
names(dag)
info = structural.em(train.set, return.all = TRUE, maximize = "tabu", maximize.args = list(tabu = 50, max.tabu = 50), fit = "mle-g", fit.args = list(iss = 1), impute = "exact", max.iter = 3)
names(info)
info$dag
head(info$imputed)
info$fitted

dfitted = bn.fit(dag, train.set)
library(arules)



categories.BH <- discretize(validation.set$`Number of Bicycle Hires`, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
pred = predict(dfitted, node = "Number of Bicycle Hires", data = validation.set, method = "exact")
#predicting bike hires
head(pred)
head(validation.set$`Number of Bicycle Hires`)
cor(validation.set$`Number of Bicycle Hires`, pred, use = "complete.obs")

predicted.BH <- discretize(pred, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
head(categories.BH)
head(predicted.BH)
cmatrix.BH <- confusionMatrix(data=predicted.BH, reference = categories.BH)
cmatrix.BH


#Bus journeys
categories.BUS <- discretize(validation.set$`Bus journeys (m)`, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
pred = predict(dfitted, node = "Bus journeys (m)", data = validation.set, method = "exact")
head(pred)
head(validation.set$`Bus journeys (m)`)
cor(validation.set$`Bus journeys (m)`, pred, use = "complete.obs")

predicted.BUS <- discretize(pred, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
head(categories.BUS)
head(predicted.BUS)
cmatrix.BUS <- confusionMatrix(data=predicted.BUS, reference = categories.BUS)
cmatrix.BUS

#Underground journeys
categories.UG <- discretize(validation.set$`Underground journeys (m)`, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
pred = predict(dfitted, node = "Underground journeys (m)", data = validation.set, method = "exact")
head(pred)
head(validation.set$`Underground journeys (m)`)
cor(validation.set$`Underground journeys (m)`, pred, use = "complete.obs")

predicted.UG <- discretize(pred, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
head(categories.UG)
head(predicted.UG)
cmatrix.UG <- confusionMatrix(data=predicted.UG, reference = categories.UG)
cmatrix.UG

#Overground journeys
categories.OG <- discretize(validation.set$`Overground Journeys (m)`, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
pred = predict(dfitted, node = "Overground Journeys (m)", data = validation.set, method = "exact")
head(pred)
head(validation.set$`Overground Journeys (m)`)
cor(validation.set$`Overground Journeys (m)`, pred, use = "complete.obs")

predicted.OG <- discretize(pred, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
head(categories.OG)
head(predicted.OG)
cmatrix.OG <- confusionMatrix(data=predicted.OG, reference = categories.OG)
cmatrix.OG

#CO - pollution element
categories.CO <- discretize(validation.set$CO, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
pred = predict(dfitted, node = "CO", data = validation.set, method = "exact")
head(pred)
head(validation.set$CO)
cor(validation.set$CO, pred, use = "complete.obs")

predicted.CO <- discretize(pred, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
head(categories.CO)
head(predicted.CO)
cmatrix.CO <- confusionMatrix(data=predicted.CO, reference = categories.CO)
cmatrix.CO

categories.PM <- discretize(validation.set$PM10, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
pred = predict(dfitted, node = "PM10", data = validation.set, method = "exact")
head(pred)
head(validation.set$PM10)
cor(validation.set$PM10, pred, use = "complete.obs")

predicted.PM <- discretize(pred, breaks = 4, labels = c("Low","Medium-Low","Medium-High","High"))
head(categories.PM)
head(predicted.PM)
cmatrix.PM <- confusionMatrix(data=predicted.PM, reference = categories.PM)
cmatrix.PM

sink(file = "General/New folder (3)/Material/YEAR 3/New Approach/Scripts/Confusion-Matrix.txt")
print("Bicycle Hire")
cmatrix.BH
print("Bus Journeys")
cmatrix.BUS
print("Underground Journeys")
cmatrix.UG
print("Overground Journeys")
cmatrix.OG
print("CO - Pollution")
cmatrix.CO
print("PM10 - Pollution")
cmatrix.PM
sink()