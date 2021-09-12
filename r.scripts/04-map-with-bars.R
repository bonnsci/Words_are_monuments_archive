# this code makes a map with the bar charts 
# add coordinates to the parks for plotting spatially

# library(usmap)  # these are already loaded if you ran 00-setup.R, but need to leave this here so packrat sees library(usmap)
# library(albersusa)
# library(ggsn)
# library(ggplot2)
# library(dplyr)

# read in df from "00-set-up.R"

# park centroids
NParks <- read_csv(here::here("Data", "maps", "National_Park_Service__Park_Unit_Centroids.csv")) %>% 
  dplyr::select(-GlobalID, -ORIG_FID , -DATE_EDIT, -OBJECTID, -UNIT_CODE, -GIS_Notes, -GNIS_ID,  -CREATED_BY) %>% 
  filter(UNIT_TYPE %in% "National Park") %>% 
  dplyr::rename(np = PARKNAME)

NParks$np <- substr(NParks$UNIT_NAME, 1, nchar(NParks$UNIT_NAME)-14)
NParks <- NParks[,-3]
NParks[16,7] <- "Denali"

# wrangle the data needed and join coords to parks, removing redundant columns

df.map <- df %>% 
  dplyr::group_by( np, indig_or_wstrn) %>% 
  dplyr::summarise(n = n()) %>% 
  arrange(desc(indig_or_wstrn)) %>% #, desc(percent)
  left_join(NParks) %>% #join the coords and data together
  pivot_wider(names_from = indig_or_wstrn, values_from = n, #pivot to wide format
              values_fill = list(n = 0)) %>% 
  dplyr::select(-REGION, -METADATA, -UNIT_TYPE, -STATE, -"NA") %>%
  ungroup()

# for I or W: manual hack to get Hawaii and Alaska parks in the right place for the map below that puts them south of CA
df.map[11, 2] = -101.01 #Hawai'i
df.map[11, 3] = 24.7 #Hawai'i
df.map[6, 2] = -113.18525 #Denali
df.map[6, 3] = 26.0 #Denali
df.map[14, 2] = -111.0 #Wrangell
df.map[14, 3] = 26.6 #Wrangell
df.map[12, 2] = -107.5 # Mesa Verde --nudging east so bar does not overlap with Canyonlands
df.map[2, 2] = -103.5  # Big Bend --nudging west so does not overlap with Hawai'i




#Step 1 BACKGROUND MAP 

quartz(width=8, height=5)

# NOTE on projections: I seem to be unable to plot geom_rect onto curvilinear projections, 
# the data just don't show up. So sticking with this crap projection for now.
p <- ggplot() + 
  geom_sf(data=usa_sf(), size=0.2, col="grey80", fill="gray95") +  # don't need to prepare map data, just call it here
  coord_sf(datum=sf::st_crs(4326)) +
  theme(
    panel.background = element_rect(fill = "gray100"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank())   #line(colour = "grey90", size = 0.125)) +
  #guides(fill=guide_legend(nrow = 2)) 

p

#Step 2 

df.map$nnames <- rowSums(df.map[,4:5])

##### for I or W calcs
# make a proportion Indigenous, prop. Western AKA non-Indigenous
df.map$prop.I <- df.map$Indigenous /df.map$nnames 
df.map$prop.W <- df.map$Western / df.map$nnames
# sum(df.map$nnames) # check = nrow(dat)  # sum = 2235 because there are 6 names we're not sure whether or not they are Indigenous

# melt / gather / now it's called pivot_longer so that ggplot reads the proportions all in one column
# and fills them by proportion type
df.map2 <-  df.map %>%
  pivot_longer(cols=c("prop.I", "prop.W")) %>%
  ungroup()
# name = proportion type, value = proportion value

#step 3 add bars to map  ###################### PLOT FOR PAPER

# For park name labels to show up where we want them on the map
parklabs <- df.map[,1:3]
parklabs[1,3] <- 45.5   # 44.4
parklabs[2,3] <- 31.3
parklabs[3,2] <- -117.5
parklabs[3,3] <- 42.7
parklabs[4,3] <- 45
parklabs[5,3] <- 42
parklabs[6,2] <- -117   # Denali
parklabs[6,3] <- 28
parklabs[7,2] <- -88      #Everglades
parklabs[7,3] <- 27
parklabs[8,3] <- 50.5 # glacier
parklabs[9,3] <- 36.5
parklabs[10,3] <- 37
parklabs[11,3] <- 25.2 # Hawai'i
parklabs[12,3] <- 39
parklabs[13,3] <- 48
parklabs[14,3] <- 28.4
parklabs[14,1] <- "Wrangell-\nSt. Elias"
parklabs[15,3] <- 45.5
parklabs[16,3] <- 38.8
parklabs[16,2] <- -120

# northSymbols() # To see the north arrow options

scale <- 5
width <- 2

colors <- c("Indigenous" = "#40B0A6", "Non-Indigenous"= "#E1BE6A")

p + 
  geom_rect(data=df.map, aes(xmin = X - width / 2,
                             xmax = X + width / 2,
                             ymin = Y,
                             ymax = Y + prop.W*scale,
                             fill="Non-Indigenous"),
            #col="gray60",
            size=0.4) +
  geom_rect(data=df.map, aes(xmin = X - width / 2,
                             xmax = X + width / 2,
                             ymin = Y + prop.W*scale,
                             ymax = Y + prop.W*scale + prop.I*scale,
                             fill="Indigenous"),
            #col="gray60",
            size=0.4) +
  scale_fill_manual(values=colors) +
  geom_text(data=parklabs, aes(x=X, y=Y, label=np), size=3, hjust=0) +
  labs(fill= "Place Names\nLanguage Group",
       x="Longitude",
       y="Latitude") +
  north(location="topright",scale=0.08, symbol=12, 
        x.min=-125, x.max=-65, y.min=20, y.max=55) +
  annotate(geom="text", label="N", x=-67.3, y=52) +
  scalebar(location="bottomright", dist=500, height=0.02, transform=TRUE,
           model="WGS84", dist_unit="km", st.dist =0.03, st.size=3, border.size=0.15,
           x.min=-125, x.max=-65, y.min=20, y.max=55,
           box.color = "gray30", box.fill = c("gray30", "white"), st.color="gray30") +
  theme(legend.position=c(0.88, 0.25),
        legend.direction="vertical",
        legend.background = element_rect(fill="gray95"),
        axis.text = element_text(size=6, color="gray30"),
        axis.title = element_text(size=8, color="gray30"),
        axis.ticks = element_line(size=0.2, color="gray30"))


quartz.save("./outputs/figs/map_bars_20210907.png", type="png", device=dev.cur(), dpi=300, bg="white")
dev.off()
