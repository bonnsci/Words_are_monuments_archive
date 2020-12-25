## This script creates spider plots and rose/target/planetary boundary plots for 
## efficiently displaying s "score card" for each park

rm(list = ls(all.names = TRUE)) 

library(here)
library(tidyr)
library(tidyverse)
library(oce)
library(fmsb)
library(dplyr)
library(RColorBrewer)
library(viridis)


df <- read.csv(here::here("Data", "inputs", "cleaned-data-2020-11-09.csv")) 
df <- df %>% 
  dplyr::select(np, erasure, problem, derog) %>% 
  mutate_at(vars(problem), ~dplyr::recode(.,  "Named for person who directly or used power to perpetrate violence against a group" = "Perpetrated", 
                                   "Name itself promotes racist ideas and/or violence against a group" = "Promotes",
                                   "Western use of Indigenous name" = "Western",
                                   "Named after person who supported racist ideas (but non-violent, not in power)" = "Ideas",
                                   "Other - truly does not fit any other classes" = "Other",
                                   "No information - cannot find explanation" = "No_info",
                                   "Colonialism - non-violent person but gained from Indigenous removal" = "Colonialism",
                                   "No - IPN, western built w/ WPN, or erasure as only problem" = "No"))

# calculate the proportion of erasure and derogatory in each park
df <- df %>% 
  dplyr::group_by(np) %>% 
  dplyr::mutate(num_names = n()) %>%
  dplyr::mutate(prop_probable  = sum(na.omit(erasure ==  "Potentially - probably had IPN"))/num_names) %>% 
  dplyr::mutate(prop_certain  = sum(na.omit(erasure ==  "Yes - know IPN"))/num_names) %>% 
  dplyr::mutate(prop_erasure = prop_probable + prop_certain) %>% 
  dplyr::mutate(prop_derog = sum(na.omit(!derog == "No info - not likely, no evidence"))/num_names) %>% 
  dplyr::select(-c(prop_certain, prop_probable)) %>% 
  ungroup()

# find maximum values of erasure and derogatory in order to scale
max_erasure <- max(df$prop_erasure)
max_derog <- max(df$prop_derog)

# add scaled variables for erasure and derogatory
df <- df %>% 
group_by(np) %>%
  mutate(Erasure = prop_erasure/max_erasure) %>% 
  mutate(Derogatory = prop_derog/max_derog) %>% 
  ungroup()

## Now, count problem categories by park and reshape to wide format with
## individual columns by problem category
df_problem <- df %>%
  count(np, problem) %>%
  spread(problem, n, fill=0) 

# create name count by park to calculate frequencies
df_counts <-  df %>% 
  group_by(np) %>% 
  summarise(names = n())

# join and calulate proportions
df_problem <- left_join(df_problem, df_counts, by = "np") %>%
  dplyr::select(np, Colonialism, Other, Perpetrated, Promotes, Ideas, Western, names)

df_problem <- df_problem %>% 
  group_by(np) %>% 
  mutate(prop_colonial = Colonialism / names) %>% 
  mutate(prop_other = Other / names) %>% 
  mutate(prop_perp = Perpetrated / names) %>% 
  mutate(prop_promotes = Promotes / names) %>% 
  mutate(prop_western = Western / names) %>% 
  mutate(prop_ideas = Ideas / names) %>% 
  ungroup()

# calculate max values across all parks (for each problem) to scale problem  
max_colonial <- max(df_problem$prop_colonial)
max_other <- max(df_problem$prop_other)
max_perp <- max(df_problem$prop_perp)
max_promotes <- max(df_problem$prop_promotes )
max_western <- max(df_problem$prop_western)
max_ideas <- max(df_problem$prop_ideas)

# scale pproportion of each problem cat by maximum across parks
df_problem <- df_problem %>% 
  group_by(np) %>% 
  mutate(Colonialism = prop_colonial / max_colonial) %>% 
  mutate(Other = prop_other / max_other ) %>% 
  mutate(Perp.Violence = prop_perp / max_perp ) %>% 
  mutate(Promotes.Racism = prop_promotes / max_promotes) %>% 
  mutate(Western.Use = prop_western / max_western) %>% 
  mutate(Racist.Views = prop_ideas / max_ideas) %>% 
  ungroup() %>% 
  dplyr::select(1,2,3,15:18)

df_scales <- df %>% 
  dplyr::select(np, Erasure, Derogatory) %>% 
  dplyr::group_by(np) %>% 
  slice(n =1)

df_scales <- left_join(df_problem, df_scales, by = "np") %>%
  dplyr::select(np, Derogatory, Western.Use, Erasure, Other,  
         Colonialism, Racist.Views, Perp.Violence, 
         Promotes.Racism)

# need park names for plots
parks <- df_scales$np

# drop park name for radarplot tibble formatting
df_radar <- df_scales %>% 
  dplyr::select(-1)

# Add min and max rows for spider format
df_radar <- rbind(rep(1,8) , rep(0,8) , df_radar)

## woof, that was some pretty clunky data wrangling but it got the job done ##

## (1) Create custom park pallete
## (2) Create custom pallete for parks
# install.packages("devtools")
# devtools::install_github("katiejolly/nationalparkcolors", force = T)
# library(nationalparkcolors)
# hex <- park_palette("MtRainier")
# unique(hex)

park.pal <- c("#466D53", "#D5AE63", "#E16509", "#376597",
              "#A4BED5", "#F7ECD8", "#698B22", "#4B4E55",
              "#CD4F39", "#8B668B", "#150718", "#F4A460",
              "#8B4513", "#ACC2CF", "#E8C533", "#DFDED3")

## (2) Create custom transparent pallete

trans.pal <- c("#466D53A6", "#D5AE63A6", "#E16509A6", "#376597A6",
              "#A4BED5A6", "#F7ECD8A6", "#698B22A6", "#4B4E55A6",
              "#CD4F39A6", "#8B668BA6", "#150718A6", "#F4A460A6",
              "#8B4513A6", "#ACC2CFA6", "#E8C533A6", "#DFDED3A6")

## Create 4x4 spider plot

par(mar=c(1,1,1,1))
par(mfrow=c(4,4))

for(i in 1:16){
  
  # Custom the radarChart
  radarchart(df_radar[c(1,2,i+2),], axistype=1, 
              
              #custom polygon
              pcol=park.pal[i] , pfcol=trans.pal[i] , plwd=2, plty=1 , 
              
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,.25), cglwd=0.8,
              
              #custom labels
              vlcex=0.8,
              
              #title
              title=parks[i]
  )
}

# ggsave("outputs/figs/RTR_spider_plot.pdf", 
#        width = 10, height = 10)
# Getting error so saving manually for now

###################################################################################
###################################################################################
########### New on 11/7/2020 (the day Biden and Harris elected) #################
###################################################################################
###################################################################################

# Drop "other"-- DONE
# Reorder categories -- DONE
# Remove the y-axis labels -- DONE
# Fewer lines? -- DONE
# Change category names -- DONE

df_election <- df_radar %>% 
  dplyr::select(-Other)



quartz(width=8, height=8)

par(mar=c(1,0,1,0))
par(mfrow=c(4,4))

for(i in 1:16){

radarchart(df_election[c(1,2,i+2),], axistype=0, 
           
           #custom polygon
           pcol=park.pal[i] , pfcol=trans.pal[i] , plwd=2, plty=1, seg = 3,
           
           #custom the grid
           cglcol="lightgrey", cglty=1, cglwd=0.8,
           
           #custom labels
           vlcex=.9, vlabels = c("Derogatory", "Appropriation", "Replacement",
                                  "Colonialism", "Supported\nRacism", "Perp.\nViolence",
                                  "Racist Word"),
           
           #title
           title=parks[i]
)
}


#quartz.save("./outputs/figs/bar_problem_totals_colorbypark_noIPN.png", type="png", device=dev.cur(), dpi=300, bg="white")
dev.off()

# the end

