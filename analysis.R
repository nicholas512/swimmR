library(lubridate)
library(here)
library(waffle)
library(reshape2)

df <- read.csv(here::here('data', 'cleaned_data.csv'), stringsAsFactors = F)
df$date <- as.Date(df$date, "%Y-%m-%d")
df$DOY <- as.Date(df$DOY, "%Y-%m-%d")
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
par(mfrow=c(2,3))
#dens = list()
for (beach in unique(df$beach)){
  dat <- log10(df$ecoli[df$beach == beach])
  h <- hist(dat, breaks = seq(0.9, 3, .1),plot=F)
  h$counts <- h$counts / sum(h$counts)
  plot(h, main=name.beach(beach), 
       xlab='E. Coli count', xaxt='n', 
       ylab='', las=1, ylim=c(0, 0.15))
  abline(v=log10(200), col='red')
  #dens[[beach]] <- density(dat, na.rm=T, bw = 0.15)
  axis(side = 1, at = 1:3, labels=parse(text=paste0("10^", 1:3)))
}


# basic stats
data.frame(
median = round(t(rbind(by(data = df$ecoli, INDICES = df$beach, FUN = median, na.rm=T))),1),
mean = round(t(rbind(by(data = df$ecoli, INDICES = df$beach, FUN = mean, na.rm=T))),1),
sd = round(t(rbind(by(data = df$ecoli, INDICES = df$beach, FUN = sd, na.rm=T))),1)
)



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

smry <- acast(data = df, formula = beach~status, fun.aggregate = length, drop=F)
round(100*(smry[,c('no swim')] / apply(smry, 1, sum)), 0)




#### actual day-of
#df$dayof <- 0
dayof <- data.frame(date=df$date-1, beach=df$beach, dayof=df$ecoli)
df <- merge(df, dayof)
# if you go swimming on a swim day, what is mean E. Coli?
round(acast(data = df, formula = beach~status, fun.aggregate = mean, drop=F, value.var='dayof')[,-1], 0)
round(acast(data = df, formula = beach~status, fun.aggregate = mean, drop=F, value.var='ecoli')[,-1], 0)

# if you go swimming on a swim day, what is the chance the beach *should* be closed
acast(data = df, formula = beach~status, 
      fun.aggregate = function(x) sum(x>200), 
      drop=F, value.var='ecoli')[,-3]



y <- as.numeric(rbind(by(df$ecoli[df$beach=="WBO"], df$DOY[df$beach=="WBO"], mean)))
plot(unique(df$DOY[df$beach=="WBO"]), y)
