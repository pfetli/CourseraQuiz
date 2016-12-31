## === Gettind and Cleaning Data
## Practical Assignement 
## Week 3

## === Question 1
# The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# and load the data into R. The code book, describing the variable names is here:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# Create a logical vector that identifies the households on greater than 10 acres 
# who sold more than $10,000 worth of agriculture products. Assign that logical vector to the 
# variable agricultureLogical. Apply the which() function like this to identify the rows of the 
# data frame where the logical vector is TRUE.
# which(agricultureLogical)
# What are the first 3 values that result?

# Download and save file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
f <- file.path(getwd(), "communities.csv")
download.file(fileUrl, destfile=f)
dateDownloaded <- date() # "Sun Dec 25 12:18:30 2016"

# Load data
dat.com <- read.csv(f)
head(dat.com)
str(dat.com)

# Indentify the households (which)
agricultureLogical <- dat.com$ACR == 3 & dat.com$AGS == 6
which(agricultureLogical)[1:3]

## === Question 2
# Using the jpeg package read in the following picture of your instructor into R
# https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg
# Use the parameter native=TRUE. What are the 30th and 80th quantiles of the resulting 
# data? (some Linux systems may produce an answer 638 different for the 30th quantile)
# install.packages("jpeg")
library(jpeg)

# Download and save file 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
f <- file.path(getwd(), "jeff.jpg")
download.file(url, f, mode = "wb")

# Load data
img.jpg <- readJPEG(f, native = TRUE)

# Get quantile
quantile(img.jpg, probs = c(0.3, 0.8))

## === Question 3
# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
# Load the educational data from this data set:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
# Match the data based on the country shortcode. How many of the IDs match? Sort the 
# data frame in descending order by GDP rank (so United States is last). What is the 
# 13th country in the resulting data frame?
# Original data sources:
#   http://data.worldbank.org/data-catalog/GDP-ranking-table
# http://data.worldbank.org/data-catalog/ed-stats

library(dplyr)

# Download, save and read files
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
f <- file.path(getwd(), "FGDP.csv")
download.file(fileUrl, destfile=f)
dat.FGDP <- read.csv(f,skip = 4, nrows = 215)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
f <- file.path(getwd(), "country.csv")
download.file(fileUrl, destfile=f)
dat.Country <- read.csv(f)

dateDownloaded <- date() # "Sun Dec 25 12:46:22 2016"

# Create Subset and rename variables
names(dat.FGDP)
sub.FGDP <- dat.FGDP %>%
  select(X,X.1,X.3,X.4) %>%
  filter(X != "") %>%
  mutate(inFGDP = 1)
colnames(sub.FGDP) <- c("CountryCode", "rankingGDP","Long.Name", "gdp","inFGDP")

names(dat.Country)
sub.country <- dat.Country %>%
  select(CountryCode,Income.Group) %>%
  mutate(inCountry = 1)

# Merge and count NAs
dat.merge <- merge(sub.FGDP,sub.country,by.x=c("CountryCode"),by.y=c("CountryCode"), 
                   all.x=TRUE,all.y=TRUE)
sum(!is.na(unique(dat.merge$rankingGDP)))

# Sort
dat.sort <- dat.merge %>%
  arrange(desc(rankingGDP))
dat.sort[13,]


## === Question 4
# What is the average GDP ranking for the "High income: OECD" and 
# "High income: nonOECD" group? 

mean.GDP <- dat.merge %>%
  group_by(Income.Group) %>%
  summarize(avg_GDP = mean(rankingGDP, na.rm=TRUE))
  
  
## === Question 5
# Cut the GDP ranking into 5 separate quantile groups. Make a table versus 
# Income.Group. How many countries are Lower middle income 
# but among the 38 nations with highest GDP?
# install.packages("Hmisc")
library(Hmisc)
quant.GDP <- dat.merge %>%
  mutate(breaks = cut2(rankingGDP,g=5))
table(quant.GDP$breaks, quant.GDP$Income.Group)


