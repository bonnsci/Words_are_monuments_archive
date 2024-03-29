## This script:
## 1) pre-processes the raw visitation rate data downloaded from NPS into one single df (saved in Data/visitationRateData)
## 2) calculates the average annual visitation rate for the past x years for each park
## 3) uses 2 above to calculate the "name-viewings" metric

## install packages
## library(rlist) ## needed for list.rbind 

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
detach_package("plyr", TRUE)
library(dplyr) #need to make sure this one is installed tho! 

########
## 1) pre-processes the raw visitation rate data downloaded from NPS into one single df

## get list of all csvs in the visitationRateData folder:
folder = "Data/visitationRateData/"
csvs = list.files(path=folder, pattern="*.csv", full.names=TRUE)
csvs

## for each csv, read, parse, and add to list

df_list <- list()

for(csv in csvs){
  
  ## Read just the data, remove commas, and convert column to numeric
  df_np <- read.csv(csv, skip = 3, header = T)
  df_np$RecreationVisitors  <- as.numeric(as.character(gsub(",", "", df_np$RecreationVisitors)))
  
  ## Add national park name as a new column and remove the " NP" that's part of the name in the csv
  np_name <- toString(read.csv(csv,nrows=2,header=F)[,1][2]) %>%
    { gsub(" NP", "", .) }
  
  df_np <- df_np %>% 
    mutate(np = np_name)
  
  df_list <- list.append(df_list, df_np)
}

## create single df 
df_visitRates <- list.rbind(df_list)

df_visitRates["TotalRecreationVisitors"] <- NULL

###########
## 2) calculate the average annual visitation rate for the past x years for each park

df_visitRate_avg <- df_visitRates %>%
  filter(Year >= 2009) %>%
  group_by(np) %>%
  summarize(avgVisitationRate = mean(RecreationVisitors, na.rm=TRUE))

## clean up the national park names
df_visitRate_avg <- df_visitRate_avg %>%
  dplyr::mutate_at(vars(np),  
            ~recode(.,  "Denali & PRES" = "Denali", 
                    "Wrangell-St. Elias & PRES" = "Wrangell-St. Elias", 
                    "Hawaii Volcanoes" = "Hawai'i Volcanoes"))


df_visitRate_avg$avgVisitationRate <- round(as.numeric(df_visitRate_avg$avgVisitationRate),0)

# optional: make your own copy 
# if you do, make sure to use this "..._mine.csv" filename or whatever
# you prefer in the following scripts, 
# as the code uses our file "Ave_Visitation_Rate_Parks.csv" provided in the archive 
write_csv(df_visitRate_avg, "Data/Generated/Ave_Visitation_Rate_Parks_mine.csv")
