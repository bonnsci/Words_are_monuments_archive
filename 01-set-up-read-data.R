#Code for analysis and figures for McGill et al 2021. Words are monuments.

#This code sets up the environment and reads in the data      
#From Sean Anderson: #use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
packages <-  c( "janitor", "here", 
                "reshape2", "data.table", "stringr", "googlesheets4", 
                "maptools", "maps",  "mapproj", "geosphere", "sf","rgeos","ggalt", "rgdal", "albersusa",  #mapping tools
                "cowplot",  "scatterpie",  "RColorBrewer", "viridis",  
                "tidyverse", "devtools",
                "rlist", "fmsb" ,"oce", "ggrepel",
                "WebPower", "gridExtra",  
                "betareg", "car", "emmeans", "gam", "ggpubr",  # new ones for running Jon's beta Regression
                "hrbrthemes", "lmtest", "multcompView", "MuMIn", 
                "psych", "rcompanion")


package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
library(dplyr) #need to make sure this one is installed tho! 
#saving files to folders with timestamps
st=format(Sys.time(), "%Y-%m-%d") #this gets the timestamp
rm(packages, package.check) #clean up the environment

#Read in the data
df <- read.csv(here::here("Data/inputs/cleaned-data-2020-11-09.csv"))
df_visitRate_avg <- read.csv(here::here("Data/Generated/Ave_Visitation_Rate_Parks.csv"))

##check that national park names match
unique(df$np) %in% unique(df_visitRate_avg$np) 

a <- df_visitRate_avg[df_visitRate_avg$np=="Denali & PRES",]
## clean up the national park names
df_visitRate_avg <- df_visitRate_avg %>%
  dplyr::mutate_at(vars(np),  
                   ~dplyr::recode(.,  "Denali & PRES" = "Denali", 
                           "Wrangell-St. Elias & PRES" = "Wrangell-St. Elias", 
                           "Hawaii Volcanoes" = "Hawai'i Volcanoes"))



##check that national park names match
unique(df$np) %in% unique(df_visitRate_avg$np) 

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
  dplyr::mutate_at(vars(problem),  
            ~dplyr::recode(.,  "Named for person who directly or used power to perpetrate violence against a group" = "Is for a person who\ncommitted racist violence", 
                    "Name itself promotes racist ideas and/or violence against a group" = "Promotes racism",
                    "Western use of Indigenous name" = "Is western use",
                    "Named after person who supported racist ideas (but non-violent, not in power)" = "Is for a racist person",
                    "Other - truly does not fit any other classes" = "Other",
                    "No information - cannot find explanation" = "No info",
                    "Colonialism - non-violent person but gained from Indigenous removal" = "Memorializes\ncolonization",
                    "No - IPN, western built w/ WPN, or erasure as only problem" = "No"))

