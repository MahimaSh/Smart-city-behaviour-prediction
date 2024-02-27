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
info = structural.em(newdata, return.all = TRUE, maximize = "tabu", maximize.args = list(tabu = 50, max.tabu = 50), fit = "mle-g", fit.args = list(iss = 1), impute = "exact", max.iter = 3)
names(info)
info$dag
head(info$imputed)
info$fitted

dfitted = bn.fit(dag, newdata)

data2 = impute(dfitted, data = data2, cluster = cl)
head(data2)

data.final=data2[sample(3817,3817),] 
training.set = data.final[1:3100, ]
validation.set = data.final[3101:3817, ]


dag = hc(data.final)
plot(dag)
fitted = bn.fit(dag, training.set)


categories.BH <- discretize(validation.set$Number.of.Bicycle.Hires, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred = predict(fitted, node = "Number.of.Bicycle.Hires", data = validation.set, method = "exact")
#predicting bike hires
head(pred)
head(validation.set$Number.of.Bicycle.Hires)
cor(validation.set$Number.of.Bicycle.Hires, pred, use = "complete.obs")

predicted.BH <- discretize(pred, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.BH)
head(predicted.BH)
cmatrix.BH <- confusionMatrix(data=predicted.BH, reference = categories.BH)
cmatrix.BH


#Bus journeys
categories.BUS <- discretize(validation.set$Bus.journeys..m., method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred = predict(fitted, node = "Bus.journeys..m.", data = validation.set, method = "exact")
head(pred)
head(validation.set$Bus.journeys..m.)
cor(validation.set$Bus.journeys..m., pred, use = "complete.obs")

predicted.BUS <- discretize(pred, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.BUS)
head(predicted.BUS)
cmatrix.BUS <- confusionMatrix(data=predicted.BUS, reference = categories.BUS)
cmatrix.BUS

#Underground journeys
categories.UG <- discretize(validation.set$Underground.journeys..m., method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred = predict(fitted, node = "Underground.journeys..m.", data = validation.set, method = "exact")
head(pred)
head(validation.set$Underground.journeys..m.)
cor(validation.set$Underground.journeys..m., pred, use = "complete.obs")

predicted.UG <- discretize(pred, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.UG)
head(predicted.UG)
cmatrix.UG <- confusionMatrix(data=predicted.UG, reference = categories.UG)
cmatrix.UG

#Overground journeys
categories.OG <- discretize(validation.set$Overground.Journeys..m., method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred = predict(fitted, node = "Overground.Journeys..m.", data = validation.set, method = "exact")
head(pred)
head(validation.set$Overground.Journeys..m.)
cor(validation.set$Overground.Journeys..m., pred, use = "complete.obs")

predicted.OG <- discretize(pred, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.OG)
head(predicted.OG)
cmatrix.OG <- confusionMatrix(data=predicted.OG, reference = categories.OG)
cmatrix.OG

#CO - pollution element
categories.CO <- discretize(validation.set$CO, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred = predict(fitted, node = "CO", data = validation.set, method = "exact")
head(pred)
head(validation.set$CO)
cor(validation.set$CO, pred, use = "complete.obs")

predicted.CO <- discretize(pred, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.CO)
head(predicted.CO)
cmatrix.CO <- confusionMatrix(data=predicted.CO, reference = categories.CO)
cmatrix.CO

#PM10 - pollution element
categories.PM <- discretize(validation.set$PM10, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
pred = predict(fitted, node = "PM10", data = validation.set, method = "exact")
head(pred)
head(validation.set$PM10)
cor(validation.set$PM10, pred, use = "complete.obs")

predicted.PM <- discretize(pred, method = "interval", breaks = 3, labels = c("A.Low","B.Medium","C.High"))
head(categories.PM)
head(predicted.PM)
cmatrix.PM <- confusionMatrix(data=predicted.PM, reference = categories.PM)
cmatrix.PM


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
log_likelihood <- logLik(dfitted, validation.set, by.sample = FALSE, na.rm = TRUE, debug = TRUE)
sink()

write.csv(data.final, "General/New folder (3)/Material/YEAR 3/New Approach/Datasets/CompleteData.csv", row.names=FALSE)


#query
evidence.data <- validation.set[, names(fitted)]
#eventlist <- validation.set[ ,11]
results <- numeric(nrow(evidence.data))

for(i in 1:nrow(evidence.data)){
  evid = evidence.data[i, ]
  evid = paste("(", paste(names(evidence.data), "==", evid, collapse=" & "), ")")
  #evid = paste("(", names(evidence.data), " == '", as.character(evidence.data[i, ]), "')", collapse = " & ")
  results[i] <- cpdist(fitted, 'CO', eval(parse(text = evid)), method = "lw", debug = TRUE)
}
print(results)


rd <- data.frame(data.final$cloud_cover, data.final$sunshine, data.final$mean_temp,
                           data.final$pressure, data.final$Number.of.Bicycle.Hires, data.final$CO, data.final$NO,
                           data.final$O3, data.final$PM10, data.final$Bus.journeys..m., data.final$Underground.journeys..m.,
                           data.final$DLR.Journeys..m., data.final$Tram.Journeys..m., data.final$Overground.Journeys..m.)
colnames(rd) <- c('cloud_cover', 'sunshine', 'mean_temp', 'pressure', 'Number.of.Bicycle.Hires',
                            'CO', 'NO', 'O3', 'PM10', 'Bus.journeys..m.', 'Underground.journeys..m.', 'DLR.Journeys..m.', 
                            'Tram.Journeys..m.', 'Overground.Journeys..m.')

validation <- data.final[3801:3817, ]

evidence.data <- validation
#eventlist <- validation.set[ ,11]
#results <- numeric(nrow(evidence.data))
eventlist = evidence[ ,11]
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
