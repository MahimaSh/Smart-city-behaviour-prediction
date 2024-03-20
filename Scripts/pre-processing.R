library(readxl)

#loading the 4 datasets
Weather <- read_excel("General/New folder (3)/Material/YEAR 3/New Approach/Datasets/Weather.xlsx")
TFL <- read_excel("General/New folder (3)/Material/YEAR 3/New Approach/Datasets/tfl-journeys-type.xlsx", sheet = "Journeys")
Pollution <- read_excel("General/New folder (3)/Material/YEAR 3/New Approach/Datasets/Pollution 2010 - 2020.xlsx")
Bicycle_hire <- read_excel("General/New folder (3)/Material/YEAR 3/New Approach/Datasets/Bicycle hire 2010-2023.xlsx", sheet = "Data")

mat = matrix(ncol = 0, nrow = 0) 
df=data.frame(mat)
Bicycle_hire$Day <- as.Date(Bicycle_hire$Day, '%d/%m/%Y')
df = subset(Bicycle_hire, Bicycle_hire$Day >= as.Date('2010-07-30') & Bicycle_hire$Day <= as.Date('2021-01-09'))
duplicated(df) #identifying dupliacte values
df<-df[!duplicated(df$Day), ] #removing duplicate values

colnames(df) #checking column names and index value
names(df)[1] <- "Date"  #renaming 'Day' to 'Date'
weather_bh_data = merge(x=Weather, y=df, by = 'Date', all.y = TRUE)   #merging both data based on extracted data from bike hires

colnames(weather_bh_data)
weather_bh_data <- weather_bh_data[-c(12:20)] #removed irrelevant data such as average year hires
names(weather_bh_data)[11] <- "Number of Bicycle Hires" #renaming column name
names(Pollution)[3] <- "Date"

second_data = merge(x=weather_bh_data, y=Pollution, by = 'Date', all.x = TRUE)

subset_CO <- subset(Pollution, subset = (Species=="CO"))    #extracting subsets based on specie type
subset_NO <- subset(Pollution, subset = (Species=="NO"))
subset_NO2 <- subset(Pollution, subset = (Species=="NO2"))
subset_O3 <- subset(Pollution, subset = (Species=="O3"))
subset_PM10 <- subset(Pollution, subset = (Species=="PM10"))
subset_SO2 <- subset(Pollution, subset = (Species=="SO2"))
names(subset_CO)[2] <- "CO"   #renaming column value as specie's name
names(subset_NO)[2] <- "NO"
names(subset_NO2)[2] <- "NO2"
names(subset_O3)[2] <- "O3"
names(subset_PM10)[2] <- "PM10"
names(subset_SO2)[2] <- "SO2"

subset_CO <- subset_CO[-c(1,2,5,6)]   #dropping columns that are not useful such as measurement unit
subset_NO <- subset_NO[-c(1,2,5,6)]
subset_NO2 <- subset_NO2[-c(1,2,5,6)]
subset_O3 <- subset_O3[-c(1,2,5,6)]
subset_PM10 <- subset_PM10[-c(1,2,5,6)]
subset_SO2 <- subset_SO2[-c(1,2,5,6)]

#combining the previous merged dataset to the lates ones
second_data = merge(x=weather_bh_data, y=subset_CO, by = 'Date', all.x = TRUE)    
second_data = merge(x=second_data, y=subset_NO, by = 'Date', all.x = TRUE)
second_data = merge(x=second_data, y=subset_NO2, by = 'Date', all.x = TRUE)
second_data = merge(x=second_data, y=subset_O3, by = 'Date', all.x = TRUE)
second_data = merge(x=second_data, y=subset_PM10, by = 'Date', all.x = TRUE)
second_data = merge(x=second_data, y=subset_SO2, by = 'Date', all.x = TRUE)

colnames(second_data)
names(second_data)[12] <- "CO"
names(second_data)[13] <- "NO"
names(second_data)[14] <- "NO2"
names(second_data)[15] <- "O3"
names(second_data)[16] <- "PM10"
names(second_data)[17] <- "SO2"
install.packages('splitstackshape')
library('splitstackshape')
library(data.table)
TFL_new <- expandRows(TFL, count = 3, count.is.col = TRUE, drop= TRUE)
TFL_new$`Period beginning` <- as.Date(TFL_new$`Period beginning`, '%d/%m/%Y')
TFL_new$`Period ending` <- as.Date(TFL_new$`Period ending`, '%d/%m/%Y')
TFL_new = subset(TFL_new, TFL_new$`Period beginning` > as.Date('2010-07-24') & TFL_new$`Period ending` < as.Date('2021-01-10'))

colnames(TFL_new)

TFL_new <- TFL_new[-c(1,2,3,4)]
TFL_new <- rbind(TFL_new, TFL_new[rep(3817, 1), ])

second_data<- cbind(second_data, TFL_new)
colnames(second_data)

#exporting the dataset to csv file
write.csv(second_data, "General/New folder (3)/Material/YEAR 3/New Approach/Datasets/DataFinal.csv", row.names=FALSE)
