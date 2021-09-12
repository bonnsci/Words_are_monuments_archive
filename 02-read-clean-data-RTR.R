#This script is used to 1) (if necessary) read in the data from the googledrive 
### and then 2) load the data for use, and clean up the entries for consistency
# SBB 30 Spet 2019 -taken from KEI rmd scripts
# BMM 04 June 2020
source(here::here("r.scripts", "00-set-up.R")) #load packages and set up 

###### ---PART 1 (optional) IMPORT THE RAW DATA FROM GOOGLE SHEETS  #### CAN SKIP TO NEXT STEP BELOW UNLESS YOU WANT TO DO A FRESH PULL FROM THE GOOGLE SHEET


# this requires  -- getting this set up in the 00 setup script but for now:
# install.packages("googlesheets4")
# library(googlesheets4)

# PROBABLY CAN SKIP THIS UNLESS NEED FRESH DOWNLOAD AS OF 9 JUNE 2021, JUST LOAD THE DATA BELOW IN PART 2#
# sheets_deauth()  # our sheet is readable by anyone with a link
place <- read_sheet("https://docs.google.com/spreadsheets/d/1cXH5qpc5th6cNpHa7gZ-372n4YXpIl1YUzi5cncYm0g/edit?usp=sharing", 
                    skip=1, range="'Data from maps'!2:2243")


# ######## test for surveyor = colonialism re-categorized
# sheets_deauth()  # our sheet is readable by anyone with a link
# place <- read_sheet("https://docs.google.com/spreadsheets/d/14eGm01WA7f5E0sa2z-IJHIq2t_xZMb_AJyF6-CYQX18/edit?usp=sharing", 
#                      skip=1, range="'Data from maps'!2:2243")

as.data.frame(names(place))


place <- place %>% 
  dplyr::select(2:7, 13:17, 19,22, 24)
# as.data.frame(names(dat))
colnames(place) <- c("ID", "np", "place", "feature", "type","is_natural" , "indig_or_wstrn", "is_oipn", "for_apeople", "is_transl", "class", "erasure", "problem", "derog")

#save it with date stamp 
#Can save the raw imported data with this code to keep a copy 
st=format(Sys.time(), "%Y-%m-%d") #this gets the timestamp (also in set up)
tablab <- "GS-imported-data-" # name of the output
fname <- paste("./Data/Generated/",tablab,st, ".csv", sep = "") #
fname #check its what you want
write.table(place, fname,  #change the file name for a new run
            sep=",", col.names= T, row.names=F)

#### ------ PART 2 (required) READ IN THE EXPORTED DATA AND CLEAN IT UP --------
# dat <- read.csv(paste("./Data/Generated/GS-imported-data-", st ,".csv", sep = ""), stringsAsFactors = FALSE) #read in data - NEED TO UPDATE IF YOU WANT THE LATEST VERSION
# update below with the latest date 
dat <- read.csv("./Data/Generated/GS-imported-data-2021-09-11.csv", stringsAsFactors = F)   

#  Need to merge duplicate class names
# look for duplicates
# levels(dat$np)
# levels(dat$feature) # need to merge Ranger station and Ranger Station, Meadow/Field and Meadow/field
# levels(dat$type)
# levels(dat$is_natural) # merge natural and Natural
# levels(dat$indig_or_wstrn) # merge western, Western, WEstern
# levels(dat$is_oipn)
# levels(dat$for_apeople)
# levels(dat$is_transl) # merge no, No, translation Translation
# levels(dat$class) # merge other Other, person Person, place Place, plant Plant
# levels(dat$erasure)
# levels(dat$problem)
# levels(dat$derog)
unique(dat$problem)
unique(dat$np)  # should only have 16 unless misspellings in data
# so we can use dplr
dat <- dplyr::as_tibble(dat)

# change the names
df <- dat %>%
  dplyr::mutate_at(vars(feature, is_natural, indig_or_wstrn, is_transl, class),   #"feature", "is_natural", "indig_or_wstrn", "is_transl", "class"
            ~dplyr::recode(.,  "Ranger station" = "Ranger Station",
                        "Meadow/field" = "Meadow/Field",
                        "natural" = "Natural",
                        "western" = "Western",
                        "WEstern" = "Western",
                        "no" = "No",
                        "translation" = "Translation",
                        "other" = "Other",
                        "person" = "Person",
                        "place" = "Place", 
                        "plant" = "Plant"
                        )) %>% 
  dplyr::mutate_at(vars(problem),  
            ~dplyr::recode(.,  "Colonialism - non-violent person but gained from Indigenous removal in initial settlement period prior to NP/NM status" = "Colonialism - non-violent person but gained from Indigenous removal",
                    "Western use of Indigenous name [7-8]"= "Western use of Indigenous name",
                    "Named for person who directly or used power to perpetrate violence against a group [5]" = "Named for person who directly or used power to perpetrate violence against a group",
                    "Name itself promotes racist ideas and/or violence against a group [2,4,6]" = "Name itself promotes racist ideas and/or violence against a group",
                    "Named after person who supported racist ideas (but non-violent, not in power) [3]" = "Named after person who supported racist ideas (but non-violent, not in power)",
                    "Relevant western use of Indigenous name"="Western use of Indigenous name")) %>%
  dplyr::mutate_at(vars(np),  
            ~dplyr::recode(.,  "Crater Lake National Park" = "Crater Lake", "Glacier National Park" = "Glacier", "Hawaii Volcano" = "Hawai'i Volcanoes", "Wrangell St Elias" = "Wrangell-St. Elias"))

                    
# check it worked
unique(df$problem)


### only for when you have downloaded new data
tablab <- "cleaned-data-" # name of the output
fname <- paste("./Data/inputs/",tablab,st, ".csv", sep = "") #
fname #check its what you want
write.table(df, fname,  #change the file name for a new run
            sep=",", col.names= T, row.names=F)


