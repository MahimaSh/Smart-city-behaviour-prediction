library(readr)
mydata <- read_csv("General/New folder (3)/Material/YEAR 3/New Approach/Datasets/DataFinal.csv")
View(mydata)
colnames(mydata) = make.names(colnames(mydata))
library(bnlearn)
library(caTools)
library(arules)
library(parallel)
library(caret)

# normalizing data
data <- preProcess(as.data.frame(mydata), method=c("range"))

newdata <- predict(data, as.data.frame(mydata))
newdata = newdata[,-1]
data2 = newdata
cl = makeCluster(2)

dag = structural.em(newdata)   
dag
plot(dag)
names(dag)


info = structural.em(newdata, return.all = TRUE, maximize = "tabu", maximize.args = list(tabu = 50, 
                  max.tabu = 50), fit = "mle-g", fit.args = list(iss = 1), impute = "exact", max.iter = 3)
names(info)
info$dag
head(info$imputed)
info$fitted

#creating bnfit object from network build from missing data
dfitted = bn.fit(dag, newdata)
data2 = impute(dfitted, data = data2, cluster = cl)
head(data2)

data.final=data2[sample(3817,3817),] 
training.set = data.final[1:3100, ]
testing.set = data.final[3101:3817, ]

#learning the network structure of the data
dag1 = hc(data.final)
plot(dag1)
fitted = bn.fit(dag1, training.set)


categories.BH <- discretize(testing.set$Number.of.Bicycle.Hires, method = "interval", breaks = 3, 
                            labels = c("A.Low","B.Medium","C.High"))
pred.BH = predict(fitted, node = "Number.of.Bicycle.Hires", data = testing.set, method = "exact")
#predicting bike hires
head(pred.BH)
head(testing.set$Number.of.Bicycle.Hires)
cor(testing.set$Number.of.Bicycle.Hires, pred.BH, use = "complete.obs")

predicted.BH <- discretize(pred.BH, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.BH)
head(predicted.BH)
cmatrix.BH <- confusionMatrix(data=predicted.BH, reference = categories.BH)
cmatrix.BH


#Bus journeys
categories.BUS <- discretize(testing.set$Bus.journeys..m., method = "interval", breaks = 3, 
                             labels = c("A.Low","B.Medium","C.High"))
pred.BUS = predict(fitted, node = "Bus.journeys..m.", data = testing.set, method = "exact")
head(pred.BUS)
head(testing.set$Bus.journeys..m.)
cor(testing.set$Bus.journeys..m., pred.BUS, use = "complete.obs")

predicted.BUS <- discretize(pred.BUS, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.BUS)
head(predicted.BUS)
cmatrix.BUS <- confusionMatrix(data=predicted.BUS, reference = categories.BUS)
cmatrix.BUS

#Underground journeys
categories.UG <- discretize(testing.set$Underground.journeys..m., method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred.UG = predict(fitted, node = "Underground.journeys..m.", data = testing.set, method = "exact")
head(pred.UG)
head(testing.set$Underground.journeys..m.)
cor(testing.set$Underground.journeys..m., pred.UG, use = "complete.obs")

predicted.UG <- discretize(pred.UG, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.UG)
head(predicted.UG)
cmatrix.UG <- confusionMatrix(data=predicted.UG, reference = categories.UG)
cmatrix.UG

#Overground journeys
categories.OG <- discretize(testing.set$Overground.Journeys..m., method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred.OG = predict(fitted, node = "Overground.Journeys..m.", data = testing.set, method = "exact")
head(pred.OG)
head(testing.set$Overground.Journeys..m.)
cor(testing.set$Overground.Journeys..m., pred.OG, use = "complete.obs")

predicted.OG <- discretize(pred.OG, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.OG)
head(predicted.OG)
cmatrix.OG <- confusionMatrix(data=predicted.OG, reference = categories.OG)
cmatrix.OG

#CO - pollution element
categories.CO <- discretize(testing.set$CO, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred.CO = predict(fitted, node = "CO", data = testing.set, method = "exact")
head(pred.CO)
head(testing.set$CO)
cor(testing.set$CO, pred.CO, use = "complete.obs")

predicted.CO <- discretize(pred.CO, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.CO)
head(predicted.CO)
cmatrix.CO <- confusionMatrix(data=predicted.CO, reference = categories.CO)
cmatrix.CO

#PM10 - pollution element
categories.PM <- discretize(testing.set$PM10, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred.PM = predict(fitted, node = "PM10", data = testing.set, method = "exact")
head(pred.PM)
head(testing.set$PM10)
cor(testing.set$PM10, pred.PM, use = "complete.obs")

predicted.PM <- discretize(pred.PM, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.PM)
head(predicted.PM)
cmatrix.PM <- confusionMatrix(data=predicted.PM, reference = categories.PM)
cmatrix.PM

plot(x=pred.BH, y= testing.set$Number.of.Bicycle.Hires,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Number of Bicycle Hires')
abline(a=0, b=1)
plot(x=pred.BUS, y= testing.set$Bus.journeys..m.,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Bus Journeys')
abline(a=0, b=1)
plot(x=pred.UG, y= testing.set$Underground.journeys..m.,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Underground Journeys')
abline(a=0, b=1)
plot(x=pred.OG, y= testing.set$Overground.Journeys..m.,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Overground Journeys')
abline(a=0, b=1)
plot(x=pred.CO, y= testing.set$CO,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='CO - Pollution element')
abline(a=0, b=1)
plot(x=pred.PM, y= testing.set$PM10,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='PM10 - Pollution element')
abline(a=0, b=1)

Actual.data <- data.frame(Number_of_Bicycle_Hires = categories.BH,
                          Bus_Journeys = categories.BUS,
                          Underground_Journeys = categories.UG,
                          Overground_Jouneys = categories.OG,
                          CO_Pollution = categories.CO,
                          PM10_Pollution = categories.PM)
Predicted.data <- data.frame(Number_of_Bicycle_Hires = predicted.BH,
                             Bus_Journeys = predicted.BUS,
                             Underground_Journeys = predicted.UG,
                             Overground_Jouneys = predicted.OG,
                             CO_Pollution = predicted.CO,
                             PM10_Pollution = predicted.PM)
write.csv(Actual.data, file = "General/New folder (3)/Material/YEAR 3/New Approach/Smart-City-Project/RealValues.csv", row.names = FALSE)
write.csv(Predicted.data, file = "General/New folder (3)/Material/YEAR 3/New Approach/Smart-City-Project/Predictions.csv", row.names = FALSE)

sink(file = "General/New folder (3)/Material/YEAR 3/New Approach/Scripts/Confusion Matrix.txt")
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

sink(file = "General/New folder (3)/Material/YEAR 3/New Approach/Scripts/Log-Likelihood.txt")
log_likelihood <- logLik(dfitted, testing.set, by.sample = FALSE, na.rm = TRUE, debug = TRUE)
sink()

write.csv(data.final, "General/New folder (3)/Material/YEAR 3/New Approach/Datasets/CompleteData.csv", row.names=FALSE)

#k-fold cross validation
set.seed(123)  # Set seed for reproducibility
folds <- createFolds(data.final$Number.of.Bicycle.Hires, k = 10, list = TRUE)
accuracies <- c()
precisions <- c()
recalls <- c()
f1_scores <- c()
sensitivity_scores <- c()
rsq <- function (x, y) cor(x, y) ^ 2

for (fold in seq_along(folds)) {
  train_index <- unlist(folds[-fold])
  test_index <- unlist(folds[fold])
  
  bn_fit <- bn.fit(dag1, data = data.final[train_index, ])
  predicted_values <- predict(bn_fit, node = "Number.of.Bicycle.Hires", data = data.final[test_index, ], method = "exact")
  true_values <- data.final$PM10[test_index]
  
  
  true <- discretize(true_values, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
  predicted <- discretize(predicted_values, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
  
  # Calculate evaluation metrics
  accuracy <- mean(predicted == true)
  matrix <- confusionMatrix(data = predicted, reference = true)
  sensitivity <- matrix$byClass[1]
  precision <- matrix$byClass[5]
  recall <- matrix$byClass[6]
  f1 <- matrix$byClass[7]
  
  # Store evaluation metrics
  accuracies <- c(accuracies, accuracy)
  precisions <- c(precisions, precision)
  recalls <- c(recalls, recall)
  f1_scores <- c(f1_scores, f1)
  sensitivity_scores <- c(sensitivity_scores, sensitivity)
}
# Calculate average evaluation metrics
avg_accuracy <- mean(accuracies)
avg_precision <- mean(precisions)
avg_recall <- mean(recalls)
avg_f1 <- mean(f1_scores)

print(paste("Average Accuracy:", avg_accuracy))
print(paste("Average Precision:", avg_precision))
print(paste("Average Recall:", avg_recall))
print(paste("Average F1 Score:", avg_f1))


rd <- data.frame(data.final$cloud_cover, data.final$sunshine, data.final$mean_temp,
                           data.final$pressure, data.final$Number.of.Bicycle.Hires, data.final$CO, data.final$NO,
                           data.final$O3, data.final$PM10, data.final$Bus.journeys..m., data.final$Underground.journeys..m.,
                           data.final$DLR.Journeys..m., data.final$Tram.Journeys..m., data.final$Overground.Journeys..m.)
colnames(rd) <- c('cloud_cover', 'sunshine', 'mean_temp', 'pressure', 'Number.of.Bicycle.Hires',
                            'CO', 'NO', 'O3', 'PM10', 'Bus.journeys..m.', 'Underground.journeys..m.', 'DLR.Journeys..m.', 
                            'Tram.Journeys..m.', 'Overground.Journeys..m.')

validation <- data.final[3801:3817, ]

evidence.data <- testing.set
eventlist = evidence.data[ ,11]
test_list = list()
expected_output <- numeric(nrow(evidence.data))

for(i in 1:nrow(evidence.data)){
  test_list = list(
    cloud_cover = evidence.data$cloud_cover[i],
    sunshine = evidence.data$sunshine[i],
    pressure = evidence.data$pressure[i],
    Number.of.Bicycle.Hires = evidence.data$Number.of.Bicycle.Hires[i],
    Bus.journeys..m. = evidence.data$Bus.journeys..m.[i],
    Tram.Journeys..m. = evidence.data$Tram.Journeys..m.[i],
    Overground.Journeys..m. = evidence.data$Overground.Journeys..m.[i]
  )
  results <- cpdist(fitted, nodes = 'CO', test_list, method = "lw", debug = FALSE, n=100)
  expected_output[i] <- sum(unlist(results) * attr(results, "weights")) / sum(attr(results, "weights"))
}
print(expected_output)

