###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the Processed_data folder

## ---- packages --------
#load needed packages. make sure they are installed.
require(dplyr) #for data processing/cleaning
require(tidyr) #for data processing/cleaning
require(skimr) #for nice visualization of data 
require(ggplot2) # for bar plots

## ---- loaddata --------
data_location <- "../../Data/Raw_data/penguins_raw_dirty.csv"
data_path <- "../../Data/Raw_data/"

rawdata <- read.csv(data_location, check.names=FALSE)

# make and view data dictionary
dictionary <- read.csv(paste(data_path, "datadictionary.csv", sep=""))
print(dictionary)


## ---- exploredata --------

# look at the data
skimr::skim(rawdata)

## ---- cleandata1 --------
d1 <- rawdata

#Fixing species name typos:
d1$Species <- sub("gTin", "guin", d1$Species)
d1$Species <- sub("gufn", "guin", d1$Species)
d1$Species <- sub("PeOg", "Peng", d1$Species)
d1$Species <- sub("deKie", "delie", d1$Species)
d1$Species <- sub("eMPe", "e Pe", d1$Species)
d1$Species <- sub("Ven", "Gen", d1$Species)
d1$Species <- sub("Pen", "pen", d1$Species)
unique(d1$Species)

## ---- shortennames -------

# Shortening species names to common name only (this is a convoluted way to do it but only way I could get rid of the parentheses)
d1$Species <- gsub("[^[:alnum:]]", "", d1$Species)
d1$Species <- gsub("penguinPygoscelisadeliae", "", d1$Species)
d1$Species <- gsub("penguinPygoscelispapua", "", d1$Species)
d1$Species <- gsub("penguinPygoscelisantarctica", "", d1$Species)
unique(d1$Species)
## ---- cleandata2 --------

cl <- d1$`Culmen Length (mm)` 
# find cl=="missing and replace "missing" with NA
cl[ cl == "missing" ] <- NA  
# coerce to numeric
cl <- as.numeric(cl)  
d1$`Culmen Length (mm)` <- cl

## ---- cleandata3.1 --------
d2 <- d1 

# find too large (non NA) Culmen Lengths and divide them by 10 to fix data entry error 
cl[ !is.na(cl) & cl>300 ]
cl[ !is.na(cl) & cl>300 ] <- cl[ !is.na(cl) & cl>300 ]/10  

d2$`Culmen Length (mm)` <- cl

## ---- cleandata4 --------
# Get rid of masses which are too tiny to be adults by replacing them with NA and then dropping

d3 <- d2
mm <- d3$`Body Mass (g)`

mm[ mm < 100 ] <- NA
nas <- which( is.na(mm) )

d3 <- d3[ -nas, ]

## ---- cleandata5 --------
# Make Species, Sex, Island categorical variables
d3$Species <- as.factor(d3$Species)
d3$Sex <- as.factor(d3$Sex)
d3$Island <- as.factor(d3$Island)  
	skimr::skim(d3)

## ---- exploratoryplots --------

# Make bivariate plots for continous data to check for errors and distribution of points
plot(d3$`Body Mass (g)`, d3$`Culmen Length (mm)`)
plot(d3$`Body Mass (g)`, d3$`Culmen Depth (mm)`)
plot(d3$`Body Mass (g)`, d3$`Flipper Length (mm)`)
plot(d3$`Delta 15 N (o/oo)`, d3$`Delta 13 C (o/oo)`)
# Make histograms 
hist(d3$`Body Mass (g)`)
hist(d3$`Culmen Length (mm)`)
hist(d3$`Culmen Depth (mm)`)
hist(d3$`Flipper Length (mm)`)
hist(d3$`Delta 15 N (o/oo)`)
hist(d3$`Delta 13 C (o/oo)`)

# Make bar plots of counts for categorical variables
ggplot(d3, aes(x=Species))+geom_bar()
ggplot(d3, aes(x=Island))+geom_bar()
ggplot(d3, aes(x=Sex))+geom_bar()

## ---- finalizedata --------

# removing useless columns studyName, Stage
d4 <- subset(d3, select = -c(studyName, Stage))

## ---- savedata --------
processeddata <- d4      # change if you did more steps

# location to save file
save_data_location <- "../../Data/Processed_data/processeddata.rds"
saveRDS(processeddata, file = save_data_location)

save_data_location_csv <- "../../Data/Processed_data/processeddata.csv"
write.csv(processeddata, file = save_data_location_csv, row.names=FALSE)

## ---- finallook --------

print(d4)