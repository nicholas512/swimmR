library(lubridate)
library(here)

df <- read.csv(here('data', 'cleaned_data.csv'), stringsAsFactors = F)
df$date <- as.Date(df$date, "%Y-%m-%d")
df$DOY <- as.Date(df$date, "%Y-%m-%d")

#=============================================================================
#= Helpers
#===========
col.yr <- function(yr){
  switch(as.character(yr),
         '2014'='black',
         '2015'='red',
         '2016'='blue',
         '2017'='green',
         '2018'='grey',
         '2019'='orange')
} 


col.beach <- function(yr){
  switch(as.character(yr),
         'WBO'='black',
         'BRT'='red',
         'PEB'='blue',
         'PRB'='green',
         'MNY'='grey')
}  

#=============================================================================
#= Analysis
#===========

# histograms
dev.new()
par(mfrow=c(2,2))
hist(log10(df$ecoli[df$beach=='BRT']), breaks = seq(0.9, 3, .1)); abline(v=log10(200))
hist(log10(df$ecoli[df$beach=='WBO']), breaks = seq(0.9, 3, .1)); abline(v=log10(200))
hist(log10(df$ecoli[df$beach=='MNY']), breaks = seq(0.9, 3, .1)); abline(v=log10(200))
hist(log10(df$ecoli[df$beach=='PRB']), breaks = seq(0.9, 3, .1)); abline(v=log10(200))
hist(log10(df$ecoli[df$beach=='PEB']), breaks = seq(0.9, 3, .1)); abline(v=log10(200))

# basic stats
by(data = df$ecoli, INDICES = df$beach, FUN = median, na.rm=T)
by(data = df$ecoli, INDICES = df$beach, FUN = mean, na.rm=T)
by(data = df$ecoli, INDICES = df$beach, FUN = sd, na.rm=T)

# when is single value over 400 
df[df$status=='no swim' & df$ecoli < 200, ]
df[df$status=='no swim' & df$ecoli < 200 & !df$rain, ]

# beaches over time
for (beach in unique(df$beach)){
  dev.new()
  plot(df$DOY, log10(df$ecoli), type='n', xlab='Date', ylab='E. Coli', 
       main=paste0(beach))
  
  for (yr in unique(year(df$date))){
  
  dat <- df[year(df$date)==yr & df$beach ==beach,]
  lines(dat$DOY, log10(dat$ecoli), col=col.yr(yr))
  abline(h=log10(200))
  }
}

###
for (yr in unique(year(df$date))){
  dev.new()
  plot(df$DOY, log10(df$ecoli), type='n', xlab='Date', ylab='E. Coli', 
       main=paste0(yr))
  
  for (beach in unique(df$beach)){
    dat <- df[year(df$date)==yr & df$beach ==beach,]
    lines(dat$DOY, log10(dat$ecoli), col=col.beach(beach))
    abline(h=log10(200))
  }
}


