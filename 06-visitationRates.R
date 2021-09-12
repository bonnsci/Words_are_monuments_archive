## This script:
## 1) pre-processes the raw visitation rate data downloaded from NPS into one single df (saved in Data/visitationRateData)
## 2) calculates the average annual visitation rate for the past x years for each park
## 3) uses 2 above to calculate the "name-viewings" metric
## 4) plots this as a barchart

## NOTE: must run script 02 (read-clean-data-RTR.R) FIRST

## install packages
## library(rlist) ## needed for list.rbind - Have moved all packages tot he set up code :) 
#detach(package:plyr) ## detach plyr because it screws with summarize function in dplyr | SB THis code doenst work for me so have made a work around
library(rlist)

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

write_csv(df_visitRate_avg, "Data/Generated/Ave_Visitation_Rate_Parks.csv")
###########
## 3) uses output of step 2 above and the df in script 02 to calculate the "name-viewings" metric
## make sure you run script 02 first
## UPDATE: just do
df <- read.csv("./Data/inputs/cleaned-data-2020-11-09.csv")

##check that national park names match
unique(df$np) %in% unique(df_visitRate_avg$np) #SB - getting an error here to fix:Error in df$np : object of type 'closure' is not subsettable 
# BM fixed error by loading the "df" above and getting Hawai'i and Wrangell-St. Elias punctuations consistent
# CHECK = good to go

# number of place names per park for SI Table
# num <- aggregate(place~np, data=df, FUN="length")

# Generate totals for Problematic classes
problem_tot <- df %>% 
  filter(!is.na(problem)) %>% 
  group_by(np, problem) %>% 
  dplyr::summarise(n = n())

## join visitation rate to problem totals
problem_tot <- left_join(problem_tot, df_visitRate_avg, by = c("np" = "np"))

## multiply number of problems per class per np with the avgVisitationRate
problem_tot <- problem_tot %>%
  mutate(name.viewings = n*avgVisitationRate/10^6)

## calculate the total name.viewings per problem class
problem_tot_allnp <- problem_tot %>%
  group_by(problem) %>%
  summarize(name.viewings = sum(name.viewings)/10^6)


## clean up the problem class names
problem_tot <- problem_tot %>%
  mutate_at(vars(problem),  
            ~recode(.,  "Named for person who directly or used power to perpetrate violence against a group" = "Is for a person who\ncommitted racist violence", 
                    "Name itself promotes racist ideas and/or violence against a group" = "Promotes racism",
                    "Western use of Indigenous name" = "Is western use",
                    "Named after person who supported racist ideas (but non-violent, not in power)" = "Is for a racist person",
                    "Other - truly does not fit any other classes" = "Other",
                    "No information - cannot find explanation" = "No info",
                    "Colonialism - non-violent person but gained from Indigenous removal" = "Memorializes\ncolonization",
                    "No - IPN, western built w/ WPN, or erasure as only problem" = "No"))


###########
## 4) plots this as a barchart

# ## change x-axis plotting order to match Script 03 --BM: we've abandoned this ordering (its subjective)
# problem_tot$problem <- factor(problem_tot$problem,levels = c("Memorializes\ncolonization", 
#                                                              "Promotes racism", 
#                                                              "Is for a racist person", 
#                                                              "Is for a person who\ncommitted racist violence",
#                                                              "Is western use"))

## need to change the colors of the stacked bars (national parks)

# from Kurt's spider plots
park.pal <- c("#466D53", "#D5AE63", "#E16509", "#376597",
              "#A4BED5", "#F7ECD8", "#698B22", "#4B4E55",
              "#CD4F39", "#8B668B", "#150718", "#F4A460",
              "#8B4513", "#ACC2CF", "#E8C533", "#DFDED3")


p1 <- problem_tot %>%
  filter(problem != "No") %>%
  filter(problem != "No info") %>%
  filter(problem != "Other") %>%
  ggplot(aes(x=reorder(problem, -name.viewings), y=name.viewings)) + # get bars in descending order
    geom_bar(aes(fill=np), stat="identity", width=0.7, show.legend=T) +
    xlab("Problem Class") +
    ylab("Average annual name exposure\n(name count x millions of visitors)") +
    scale_fill_manual(values=park.pal, name="National Park") +
    #scale_fill_manual(breaks=c("1", "0"), values=c("coral2", "gray80")) +
    theme(panel.grid.minor=element_blank(), 
          panel.grid.major=element_blank(),
          # panel.grid.major.x=element_line(color="gray80", size=0.4),
          axis.text.x=element_text(angle=-40, hjust=0, size=12),
          axis.text.y=element_text(size=12),
          plot.margin=margin(1,3,1,1, unit="cm"),
          axis.title.x=element_text(size=14, face="bold"),
          axis.title.y=element_text(size=14, face="bold"),
          panel.background = element_rect(fill = 'white') ,
          panel.border=element_rect(color="grey50", fill=NA, size=0.5),
    )	 

quartz(width=8, height=6) # might still need to adjust window proportions before saving
p1
quartz.save("./outputs/figs/bar_visitation_colorbypark.png", type="png", device=dev.cur(), dpi=300, bg="white")
dev.off()