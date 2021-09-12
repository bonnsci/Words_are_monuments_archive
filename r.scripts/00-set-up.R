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
                "betareg", "car", "emmeans", "gam", "ggpubr",  # new ones for running Jon's beta Regression
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

# the data
df <- read.csv("./Data/inputs/cleaned-data-2021-09-11.csv") 


# ##########  packrat stuff not sure it works
# 
# .libPaths() # Check that the first path is to your ~/RTR/packrat/lib/x86_64-apple-darwin15.6.0/3.5.3
# #                                                                     /name will vary depending on your machine and R version.
# # if not run:
# packrat::init()  # will set project directory to within the cloned repo rather than your default library
# # packrat::on() # the above will put the session in packrat mode, but this command puts you in packrat mode if for some reason it is off
# 
# 
# 
# # in theory you should not have to install packages if you have pulled from the master
# 
# # load the libraries
# # run this
# pkgs <- unname(installed.packages(lib.loc = .libPaths()[1])[,1])
# pkgs <- pkgs[!pkgs %in% c("gdtools", "svglite", "systemfonts", "vdiffr", "leafpop")]
# 
# loadlib <- lapply(pkgs, FUN = function(x){    # this based on the Sean Anderson fn above
#   library(x, character.only = TRUE)
# })
# 
# ### the last attached package should be "zoo" if all worked correctly
# 
# rm(loadlib)
# # note if it gives a warning about not being able to install any packages, that also means any pkgs that come alphabetically after it were not installed.
# # so run 
# # remove.packages(c("problem package"))  # so it is not in the list in next step
# # will have to deal with this issue in the code when we get to calling on these packages!
# # not sure what the issue is.
# 
# # install these manually yourself they are big files # only have to do this once
# install.packages(c("rgdal", "sf", "gdtools", "svglite", "systemsfonts", "vdiffr"), dependencies=TRUE)
# 
# library(rgdal)
# library(sf)
# # library(svglite)
# # library(systemfonts)
# # library(vdiffr)
# # library(gdtools)
# 
# packrat::status()  # this will read through the project code and detect anywhere we call for a package
# # and tell us if we are missing any packages in our library that are called for
# 
# # if the above returns  "Up to date" then skip the next bit.
# # If instead the above says that there are pkgs not installed in your library.
# # check that you have pulled from master the latest RTR/packrat/src directory.
# # this has all the tarballs for the packages in the master /r.scripts
# # once you are in packrat mode (above) that should initialize R to make your unpacked RTR lib
# # there could be a package someone added to new bit of code but did not push the package to the repo
# #
# 
# # in case you need to add a package here's what you can use
# # install.packages(pkgs=c("RcppEigen"),
# #                 dependencies = TRUE)
# 
# # for something not in CRAN: 
# # install.packages("devtools")
# # devtools::install_github("hrbrmstr/albersusa")
# 
# # once you have added new packages, you'll want to share the src tarball with everyone
# # so that when we run the script that needs that package, we have it.
# # to do that run
# # packrat::snapshot()
# # and push your /src contents and packrat.lock file to the repo.
# 
# ##############################
# # go to script 04 unless you want to do a fresh download of the data from google sheets (most recent 3 July 2021)
# ##############################
