#-------------------------
# Spatial cluster detection using SpatialEpi R package
#-------------------------

install.packages("dplyr")
install.packages("ggplot2")
install.packages("SpatialEpi")
install.packages("stringr")
install.packages("olsrr")
install.packages("tidyverse")
install.packages("dynamac")
install.packages("TSstudio")

library(dplyr)
library(ggplot2)
library(SpatialEpi)
library(stringr)
library(olsrr)
library(tidyverse)
library(dynamac)
library(TSstudio)

# Generate consistant, random data:
# seed <- 105593
# set.seed(1*seed)
# x <- runif(100)
# set.seed(2*seed)
# y <- runif(100)
# set.seed(3*seed)
# f <- runif(100)

if (!dir.exists('data')) {
  dir.create('data')
}
download.file('https://occurrence-download.gbif.org/occurrence/download/request/0283362-200613084148143.zip', 'data/0283362-200613084148143.zip')
unzip('data/0283362-200613084148143.zip', overwrite = TRUE, exdir = 'data')
sightings <- read.csv(file = 'data/0283362-200613084148143.csv', header = TRUE, sep = '\t')

#head(sightings)
sightings$eventDateM <- substr(as.character(as.Date(sightings$eventDate)), 1, 7)
x <- as.character(str_split(round(sightings$decimalLatitude, 0), '.', n = 1))
y <- as.character(str_split(round(sightings$decimalLongitude, 0), '.', n = 1))
sightings$x <- x
sightings$y <- y
sightings$coords <- paste(x, y, sep = ',')


#as.data.frame(filter(sightings, sightings$countryCode == 'EC'))


sightingsEC = sightings %>% filter(sightings$countryCode == 'EC')
byMesEC = as.data.frame(count(sightingsEC, eventDateM, name = 'nEC'))
byMes = as.data.frame(count(sightings, eventDateM))
df = merge(byMes, byMesEC, by = 'eventDateM')


# required data sets
#df <- data.frame(x=sightings$decimalLatitude, y=sightings$decimalLongitude)
# pop data
#df$pop <- floor(f*1000)
# case data
#df$case_data <- rbinom(100,df$pop,.1)

# overall rate
orate <- sum(df$nEC)/sum(df$n)

# expected cases
#df$expected <- df$pop*orate
df$expected <- df$n*orate

coef(lm(nEC ~ n, data = df))
ggplot(data = df, mapping = aes(x = nEC,y = n)) + geom_point() + geom_abline()
ols_regress(nEC ~ n, data = df)

# Bivariate Linear Regression
reg <- lm(df$nEC ~ df$n)
summary(reg)

ols_plot_obs_fit(reg)
ols_plot_diagnostics(reg)

# Time Series Ecuador
tsEC = ts(df$nEC, start = c(2016), deltat = 1/12)
ts_plot(tsEC, Xtitle = "Fecha", Ytitle = "Observaciones", title = "Observaciones del Condor Andino en Ecuador (2016-17)")

# Time Series South America
tsSA = ts(df$n, start = c(2016), deltat = 1/12)
ts_plot(tsSA, Xtitle = "Fecha", Ytitle = "Observaciones", title = "Observaciones del Condor Andino en America Sur (2016-17)")
