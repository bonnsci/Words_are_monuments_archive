############################## MAKE FIGURE 2: BAR CHART OF CATEGORY COUNTS STACKED WHERE COLOR=NUMBER BY PARK

# loaded df in script 00
# set up the data
df2 <- df[,c(1,2,7, 12:14)]

df2melt <- reshape2::melt(df2, id.vars=c("ID", "np"))
df2melt$value[df2melt$value %in% "Relevant western use of Indigenous name"] <- "Western use of Indigenous name"
count_per_park <- aggregate(data=df2melt, ID~variable+np+value, FUN=length)
# note column "ID" is now count
vals <- unique(count_per_park$value)
count_per_park <- count_per_park[count_per_park$value %in% vals[c(1, 3:5, 16:18)],]
# unique(count_per_park$value)
# add the "Relevant wester use of Indigenous name" to "Western use..."

count_per_park$dummy_color <- ifelse(count_per_park$value=="Indigenous", "1", "0")

# how many parks per problem type?  ## 
parkn <- aggregate(np~value, data=count_per_park, FUN=length)
parkn
count_per_park$value <- factor(count_per_park$value)
# levels(count_per_park$value)
# rename with (number) indicating no. of parks with that problem see parkn above.
count_per_park$value <- plyr::mapvalues(count_per_park$value, from=levels(count_per_park$value), to = 
                                          c("Memorializes colonialism (16)",  #"Memorializes colonization", 
                                            #"Indigenous (15)",
                                            "Racist Word (9)", # "Promotes racism", 
                                            "Supported Racism (6)", # "For a racist person", 
                                            "Perpetrated Violence (15)", # "Anti-Indigenous genocide perpetrator", 
                                            "Appropriation (16)", # "Western use",
                                            "Replacement (12)", # "Erasure",
                                            "Derogatory (4)"))

# to order df by total counts aggregate
sum_problems <- aggregate(ID~value, data=count_per_park, FUN=sum)
sum_problems <- sum_problems[order(sum_problems$ID, decreasing = T),]
sum_problems$dummy_order <- 1:nrow(sum_problems)
count_per_park$value <- ordered(count_per_park$value, levels=sum_problems$value)
#sum_problems$dummy_color <- c("0", rep("1", nrow(sum_problems)-1)) 


#### make the darn plot

# from Kurt's spider plots
park.pal <- c("#466D53", "#D5AE63", "#E16509", "#376597",
              "#A4BED5", "#F7ECD8", "#698B22", "#4B4E55",
              "#CD4F39", "#8B668B", "#150718", "#F4A460",
              "#8B4513", "#ACC2CF", "#E8C533", "#DFDED3")


### currently set up to exclude the IPN count bar###  ###### FIG 2A
p7 <- ggplot(data=count_per_park[!count_per_park$value=="Indigenous (15)",], aes(x=value, y=ID)) +
  geom_bar(aes(fill=np), stat="identity", width=0.7) +  # , show.legend=F
  xlab("Problem") +
  ylab("Count of Place Names") +
  scale_fill_manual(values=park.pal, name="National Park") +
  #scale_color_manual(values=c("gold", NA)) +
  geom_label(data=sum_problems[!sum_problems$value=="Indigenous (15)",], aes(x=value, y=ID+0, label=ID), 
            vjust=0, size=3, fontface="italic", color="gray20", label.size=0) +  # label.size=0 removes the border around the label
  scale_y_continuous(breaks=seq(0,270,50), expand=c(0,0), limits=c(0,280)) +
  # guides(fill=guide_legend(ncol=2)) +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0, size=10, color="black"),
        axis.text.y=element_text(size=10, color="black", margin=margin(l=15, unit="pt")),
        axis.ticks.x=element_blank(),
        plot.margin=margin(0.5,1,1,1, unit="cm"),   # 0.5,3,1,1,
        axis.title.x=element_text(size=12, face="bold"),
        axis.title.y=element_text(size=12, face="bold"),
        panel.background = element_rect(fill = 'white') ,
        # panel.border=element_rect(color="gray50", fill=NA, size=0.8),
        legend.position="right",  # "right"
        legend.background = element_rect(color=NA, fill=NA),
        legend.margin=margin(t=1, l=0.5, unit="cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size=8)
  )	 

quartz(width=6, height=4) # might still need to adjust window proportions before saving
p7
quartz.save("./outputs/figs/bar_problem_totals_colorbypark_noIPN09112021.png", type="png", device=dev.cur(), dpi=300, bg="white")
# dev.off()

p7_for_grid <- p7 +
  theme(panel.grid.major.y=element_blank(),
        #panel.grid.major.x = element_line(size=0.3, color="gray85"),
        legend.position="none",
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.margin = unit(c(0.5,0.2,00,0.45), "cm")
  )


# # as percent of total names
# tot <- sum_problems[!sum_problems$value %in% c("Indigenous (15)", "Derogatory (4)"),]
# a <- sum(tot$ID)
# a/nrow(df)  # 0.27


##### COUNT BY VISITATION RATE  #### FIG 2B
df_visitRate_avg <- read.csv("Data/Generated/Ave_Visitation_Rate_Parks.csv")

count_per_park <- left_join(count_per_park, df_visitRate_avg)

count_per_park$exposure <- count_per_park$ID * count_per_park$avgVisitationRate / 10^6

sum_visits <- aggregate(exposure~value, data=count_per_park, FUN=sum)
sum_visits$value <- ordered(sum_visits$value, levels=sum_problems$value) 


### visitation plot, y-axis right side up, not colored by park
p8b <- ggplot(data=sum_visits[!sum_visits$value=="Indigenous (15)",], aes(x=value, y=exposure)) +
  geom_bar(stat="identity", width=0.7) +  # , show.legend=F  for park colors use: aes(fill=np)=
  scale_y_continuous(limits=c(0,650), breaks=seq(0,600,100), expand=c(0,0)) +
  xlab("Problem") +
  ylab("Potential Place Name Exposure\n(in millions of visitor-views per year)") +
  scale_fill_manual(values=park.pal, name="National Park") +
  #scale_color_manual(values=c("gold", NA)) +
  geom_label(data=sum_visits[!sum_visits$value=="Indigenous (15)",], aes(x=value, y=exposure+0, label=round(exposure)), 
             vjust=0, size=3, fontface="italic", color="gray20", label.size=0) +  # label.size=0 removes the border around the label
  
  coord_cartesian(clip="off") +
  # geom_text(label="(Count of names * average\nannual visitors in millions)", # y-axis subtitle
  #           x=-0.55,
  #           y=200,
  #           angle=90, size=3.5, color="black", fontface="plain",
  #           check_overlap = TRUE) +  # so it does not overplot the text
  # guides(fill=guide_legend(ncol=2)) +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        #panel.grid.major.x=element_line(color="gray80", size=0.3),
        axis.text.x=element_text(angle=-30, hjust=0, size=10, color="black"),
        axis.text.y=element_text(size=10, color="black", margin=margin(l=10, unit="pt")),
        plot.margin=margin(0.5,5,0.5,1, unit="cm"),
        axis.title.x=element_text(size=12, face="bold"),
        axis.title.y=element_text(size=12, face="bold"),
        # axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = 'white') ,
        # panel.border=element_rect(color="grey50", fill=NA, size=0.5),
        plot.background = element_rect(fill = "transparent",colour = NA), # so that the y-axis "0" from p7 isn't cut off
        legend.position="right",
        axis.ticks.x=element_blank(),
        legend.background = element_rect(color=NA, fill=NA),
        legend.margin=margin(t=0.5, l=0.5, unit="cm")
  )	 

quartz(width=6, height=4) # might still need to adjust window proportions before saving
p8b
quartz.save("./outputs/figs/bar_visit_nocolor_noIPN_09112021.png", type="png", device=dev.cur(), dpi=300, bg="white")
dev.off()



p8_for_grid <- p8b +
  theme(panel.grid.major.y=element_blank(),
        plot.margin = unit(c(0.2,0.2,0.5,0.2), "cm"),
        legend.position="none")

#############  PUT FIG 2A AND 2B TOGETHER

leg <- cowplot::get_legend(p7) 

# for just plotting the two plots without a legend use rel_heights=c(0.72,1)

quartz(width=5, height=8)

plot_grid(p7_for_grid, leg, p8_for_grid, 
          ncol=2, 
          rel_heights = c(0.8,1,1), 
          rel_widths=c(1, 0.4, 1),
          labels=c("A", " ", "B"), 
          axis="bl")

quartz.save("./outputs/figs/bar_colorbypark_count_visit_2panels_wlegend_visit_aggregated20210911.png", type="png", device=dev.cur(), dpi=300, bg="white")
dev.off()


