library(readr)
library(ggplot2)
library(corrplot)
cd <- read_csv("General/New folder (3)/Material/YEAR 3/New Approach/Datasets/CompleteData.csv")
View(cd)
colnames(cd)



variables <- colnames(cd)

for(i in variables){
  cd[[i]] <- discretize(cd[[i]], method = "interval", breaks = 3, labels = c("Low","Medium","High"))
}

#nlevels(cd$cloud_cover)
#graph = hc(cd)
#fitted2 = bn.fit(hc(cd), cd)

reduced_data <- data.frame(cd$cloud_cover, cd$sunshine, cd$mean_temp,
                           cd$pressure, cd$Number.of.Bicycle.Hires, cd$CO, cd$NO,
                           cd$O3, cd$PM10, cd$Bus.journeys..m., cd$Underground.journeys..m.,
                           cd$DLR.Journeys..m., cd$Tram.Journeys..m., cd$Overground.Journeys..m.)
colnames(reduced_data) <- c('cloud_cover', 'sunshine', 'mean_temp', 'pressure', 'Number.of.Bicycle.Hires',
                            'CO', 'NO', 'O3', 'PM10', 'Bus.journeys..m.', 'Underground.journeys..m.', 'DLR.Journeys..m.', 
                            'Tram.Journeys..m.', 'Overground.Journeys..m.')

m <- cor(reduced_data[-6], reduced_data$CO)
corrplot(m, method = "circle")


#prop.table(table())
cpquery(fitted, (CO == "Medium"), evidence = list(Bus.journeys..m. = "C.High"), method = "lw", n = 1000, debug = TRUE)

