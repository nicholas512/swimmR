library(lubridate)
library(here)
library(waffle)

df <- read.csv(here('data', 'cleaned_data.csv'), stringsAsFactors = F)
df$date <- as.Date(df$date, "%Y-%m-%d")
df$DOY <- as.Date(df$date, "%Y-%m-%d")
df$status <- factor(df$status, levels = c('no swim', 'swim', 'closed'))
df$beach <- factor(df$beach, levels = c("BRT", "WBO", "PEB", "MNY",  "PRB"))
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

name.beach <- function(beachcode){
  switch(beachcode, 'WBO'='Westboro',
          'BRT'='Britannia',
          'PEB'='Petrie East Bay',
          'PRB'='Petrie River Beaches',
          'MNY'="Mooney's")
}

multistack.bar <- function(x=list(x), betweenspace = 2, withinspace=0, ...){
  # note that number of rows in each component matrix of the list
  # should be equal
  # if you have missing value use "NA", to make it complete 
  # number of column can differ
  #https://stackoverflow.com/questions/32997632/grouped-and-stacked-barplot-using-base-r
  mylist <- x
  space = list()
  space[[1]] <- c(rep(withinspace,ncol(mylist[[1]] )),betweenspace ) 
  
  for ( i in 2:length(mylist)){
    if(i == length(mylist)){
      space[[i]] <- c(rep(withinspace,ncol(mylist[[i]] )-1)) 
    } else{
      space[[i]] <- c(rep(withinspace,ncol(mylist[[i]] )-1),betweenspace ) 
    }
  }
  un.space <- c(unlist(space))
  newdata <- do.call("cbind", mylist)
  barplot(newdata, space= un.space,  ...)
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

# compare years, same beach
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
#  compare beaches over years
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

## Pies
dev.new()
par(mfrow=c(2,3))
for (beach in unique(df$beach)){
  dat <- df[df$beach == beach,]
  n.swim <- sum(dat$status == 'swim')
  n.noswim <- sum(dat$status == 'no swim' & !dat$rain)
  n.noswimrain <- sum(dat$status == 'no swim'& dat$rain)
  pie(c(n.swim, n.noswim, n.noswimrain), labels = c("Swim", "NSA", "NSA-R"),
      main=name.beach(beach))
}

## Waffles

par(mfrow=c(5,1))
for (beach in unique(df$beach)){
  dev.new()
  dat <- df[df$beach == beach,]
  n.swim <- sum(dat$status == 'swim')
  n.noswim <- sum(dat$status == 'no swim' & dat$rain)
  n.noswimrain <- sum(dat$status == 'no swim'& !dat$rain)
  plt = waffle(parts=c(swim=n.swim, `no swim`=n.noswim, `no swim (rain)`=n.noswimrain), 
               rows=8, main=beach)
  print(plt)
  }


## stacked bar
dev.new()
m = c(rep(1,5),2)
layout(m)
par(mar=c(0, 4.1, 4.1, 2.1))
yrlist <- unique(year(df$date))
cols <- c('red', 'lightblue', 'grey40'  )
L <- lapply(yrlist, function(x) acast(data = df[year(df$date)==x,], formula = status~beach, fun.aggregate = length, drop=F))
names(L) <- yrlist
multistack.bar(L, betweenspace = 1, las=2, col=cols,
               ylim=c(0, 80))
lapply(seq_along(yrlist), function(x) text(2.5 + 6*(x-1), 75, yrlist[x]))
#par(xpd=NULL)
par(mar=c(0,0,0,0))
plot.new()
legend('center', legend=levels(df$status), ncol = 3, fill=cols)       
