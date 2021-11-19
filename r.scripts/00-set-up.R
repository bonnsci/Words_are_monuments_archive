#This code sets up the environment and reads in the data      
#From Sean Anderson:
#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
packages <-  c( "janitor", "here", "cowplot",
                "reshape2", "data.table", "stringr", "googlesheets4", 
                "maptools", "maps",  "mapproj", "geosphere", "sf","rgeos","ggalt", "rgdal",
                "ggsn", # for adding map scale bar and N arrow to maps
                "cowplot",  "scatterpie",  "RColorBrewer", "viridis",  
                "tidyverse", "devtools",
                "rlist", "fmsb" ,"oce", "ggrepel",
                "WebPower", "gridExtra",  
                "betareg", "car", "emmeans", "gam", "ggpubr", 
                "hrbrthemes", "lmtest", "multcompView", "MuMIn", 
                "psych", "rcompanion", "mgcv", "tidyr")


package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})    # Warning if this function finds a package that it cannot install 
# It stops, it does not go through the rest of the list
#note albersusa can be downloaded from github
# devtools::install_github("hrbrmstr/albersusa")
library(albersusa)


#saving files to folders with timestamps
st=format(Sys.time(), "%Y-%m-%d") #this gets the timestamp

rm(packages, package.check) #clean up the environment


# load the data: 
source(here::here("r.scripts", "00-set-up.R")) #load packages and set up 
df <- read_csv(here:here("./Data/Inputs/cleaned-data-2021-10-29.csv"))

