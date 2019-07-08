"
http://www.ottawapublichealth.ca/en/public-health-services/beach-water-quality-results.aspx

Water test results are not available immediately as they take at least 18 hours to process 
in the laboratory.  For this reason, no-swim advisories are issued based on water sample 
results from the previous day and knowledge of water quality at each beach in previous y
ears, and the impact of rainfall and bird activity on water quality.   
Ottawa Public Health will issue a no-swimming advisory if:

the geometric mean of 5 water samples taken on the previous day is greater 
than 200 E. coli per 100mL of water; 
or
a single water sample test result from the previous day is greater than 400 E. 
coli per 100mL ; 
or
there is a significant rainfall event.

"
library(openxlsx)
library(reshape2)
library(here)


datafile <- here('data',"shealthhealth-protection-divisionenvironmental-healthrecreational-waterbeaches2019-beachesresul.xlsx")

# read in and reformat
w19 <- read.xlsx(datafile, sheet=1)
w18 <- read.xlsx(datafile, sheet=3)
w17 <- read.xlsx(datafile, sheet=4)
w16 <- read.xlsx(datafile, sheet=5)
w15 <- read.xlsx(datafile, sheet=6)
w14 <- read.xlsx(datafile, sheet=7)

raw <- rbind(w19,w18,w17, w16,w15,w14)
names(raw) <- c("date", "BRT", "BRT_stat", "WBO", "WBO_stat", "MNY", "MNY_stat", "PEB", "PEB_stat", "PRB", "PRB_stat")
raw$date <- convertToDate(raw$date)


# tidy
df_count <- melt(raw[,c("date", "BRT", "WBO", "MNY",  "PEB",  "PRB")], id="date", 
                 value.name = "ecoli", variable.name = "beach")
df_status <- melt(raw[,c("date", "BRT_stat", "WBO_stat", "MNY_stat",  "PEB_stat",  "PRB_stat")], id="date", 
                 value.name = "status", variable.name = "beach")
df_status$beach <- gsub("_stat", "", df_status$beach)

df <-merge(df_count, df_status)


# standardize status
df$status <- trimws(df$status)
df$status <- tolower(df$status)

df$rain <- FALSE
df$rain[df$status %in% c("nsa - rain", "nsa-rain", "no swim (rainfall)")] <- TRUE

df$status[df$status %in% c("nsa - rain", "nsa-rain", "no swim (rainfall)")] <- 'no swim'
df$status[df$status == 'not open'] <- 'closed'
df$status[df$status == 'nsa'] <- 'no swim'

# ecoli to numeric
df$ecoli[df$ecoli=="Not Open"] <- NA
df$ecoli <- as.numeric(df$ecoli)

# day-of year
df$DOY <- format(df$date, format="%m-%d")
df$DOY <- as.Date(df$DOY, "%m-%d")

write.csv(df, here('data','cleaned_data.csv'), row.names = FALSE)

