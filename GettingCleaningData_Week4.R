## === Gettind and Cleaning Data
## Practical Assignement 
## Week 4

getwd()


## === Question 1
# The American Community Survey distributes downloadable data about United 
# States communities. Download the 2006 microdata survey about housing 
# for the state of Idaho using download.file() from here:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# and load the data into R. The code book, describing the variable names is here:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# Apply strsplit() to split all the names of the data frame on the characters "wgtp".
# What is the value of the 123 element of the resulting list?

# Download and save file "communities"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
f <- file.path(getwd(), "communities.csv")
download.file(fileUrl, destfile=f)
dateDownloaded <- date() # "Fri Dec 30 18:08:04 2016"

# Load data
dat.com <- read.csv(f)
head(dat.com)
str(dat.com)
names(dat.com)[123]

# split names on the character "wgtp"
dat.split <- strsplit(names(dat.com),"wgtp") 
dat.split[123]



## === Question 2
# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
# Remove the commas from the GDP numbers in millions of dollars and average them. 
# What is the average?
# Original data sources:
#   http://data.worldbank.org/data-catalog/GDP-ranking-table

# Download and save file "Gross Domestic Product"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
f <- file.path(getwd(), "GrossDomesticProduct.csv")
download.file(fileUrl, destfile=f)

# Load data
dat.gdp <- read.csv(f,skip = 4,nrows = 215)
str(dat.gdp)

# Remove commas and calculate average
dat.sub <- subset(dat.gdp,select=c(X,X.1,X.3,X.4))
colnames(dat.sub) <- c("country","ranking","countryNames","gdp")
dat.sub$gdpnumbers <- as.integer(gsub(",","",dat.sub$gdp))
mean(dat.sub$gdpnumbers, na.rm=TRUE)



## === Question 3
# In the data set from Question 2 what is a regular expression
# that would allow you to count the number of countries whose name 
# begins with "United"? Assume that the variable with the country 
# names in it is named countryNames. How many countries begin with United? 

isUnited <- grepl("^United", dat.sub$countryNames)
summary(isUnited)



## === Question 4
# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
# Load the educational data from this data set:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
# Match the data based on the country shortcode. Of the countries for which 
# the end of the fiscal year is available, how many end in June?
# Original data sources:
#   http://data.worldbank.org/data-catalog/GDP-ranking-table
# http://data.worldbank.org/data-catalog/ed-stats

# Download and save file "educational data"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
f <- file.path(getwd(), "FEDSTATS.csv")
download.file(fileUrl, destfile=f)

# Load data
dat.fedstats <- read.csv(f)
str(dat.fedstats)

# Match the two datasets and get the solution
dat.merge <- merge(dat.sub,dat.fedstats,by.x=c("country"),by.y=c("CountryCode"),
                   all.x=TRUE,all.y=TRUE)
table(dat.merge$Special.Notes)
dat.fiscalyear <- grepl("fiscal year",tolower(dat.merge$Special.Notes))
dat.june <- grepl("june", tolower(dat.merge$Special.Notes))
table(dat.fiscalyear,dat.june)



  
## === Question 5
# You can use the quantmod (http://www.quantmod.com/) package to get historical 
# stock prices for publicly traded companies on the NASDAQ and NYSE. Use the 
# following code to download data on Amazon's stock price and get the times the 
# data was sampled.

#install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
class(amzn)
sampleTimes = index(amzn)
class(sampleTimes)

# How many values were collected in 2012? How many values were collected 
# on Mondays in 2012?
library(lubridate)
addmargins(table(year(sampleTimes), weekdays(sampleTimes)))




