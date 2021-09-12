## This script tests beta regression on name categories by longitude.
## For the significant regressions (western use / appropriation) we make a plot too.

## this .R file is from .Rmd file "RTR Analysis with Beta Regression.Rmd" 11-15-20
## used the following to convert to .R
## knitr::purl("RTR Analysis with Beta Regression.Rmd", documentation = 2)
## --BM

#' ---
#' title: "RTR Analysis with Beta Regression"
#' date: "10/25/2020"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
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
library(tidyr)
library(ggplot2)
library(ggrepel)

#' 
#' # Effect of geography on place name categorization
#' 
#' *The dataset.*
#' This dataset examines the effect of longitude on place names in the 16 U.S. National Parks. Below is histogram showcasing the proportion of different problem categories across U.S. National Parks. The data is clearly skewed to the right, following Poisson distribution. Give that the response variables are proportions, we elected to fit models to the data using regression that assumes the data follows the Beta distribution.
#' 
## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------


## Read in the data
df.map.long <- read.csv("./Data/Generated/df_map_long2021-09-11.csv", header = TRUE)

#' 
## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------

### Setup 5b Beta Distribution
# library(tidyverse)
# library(hrbrthemes)
# library(viridis)
# library(car) # diagnostics
# library(MuMIn) #model selection
# library(psych)
# library(betareg)
# library(lmtest)
# library(rcompanion)
# library(multcompView)
# library(emmeans)

# Beta distributions require non absolute 0 data.
df.map.long$percent_percent <- (df.map.long$percent)/100
df.map.long$percent_percent <- df.map.long$percent_percent + ifelse(df.map.long$percent_percent == 0, 0.000001, 0)

### note longitude is stored in X ####

#' 
#' # Beta Regression #
#' 
#' An analysis on the effect of longitude and problem category on the proportion of each problem cases across US National Parks using Beta Regression. 
#' 
#' **Description of Beta Regression for Rates and Proportions.**
#' Fit beta regression models for rates and proportions via maximum likelihood using a parametrization with mean (depending through a link function on the covariates) and precision parameter (called phi).
#' 
#' **Joint Test.**
#' Evaluate the significance of each predictor in the model with a joint test. The joint test produces an analysis-of-variance-like table based on linear functions of predictors in a model or emmGrid object. Specifically, the function constructs, for each combination of factors (or covariates reduced to two or more levels), a set of (interaction) contrasts via contrast, and then tests them using test with joint = TRUE. Optionally, one or more of the predictors may be used as by variable(s), so that separate tables of tests are produced for each combination of them.
#' 
#' **Examine the effect of longitude on each problem category independently.**
#' 
## ------------------------------------------------------------------------
# subset the different problem categories
unique(df.map.long$problem)
# Anti-Indigenous genocide perpetrator (AIGP)
AIGP <- subset(df.map.long, problem=="Anti-Indigenous genocide perpetrator")  #anti-indigenous genocide perpetrator
FRP <- subset(df.map.long, problem=="For a racist person") # for a racist person
MC <- subset(df.map.long, problem=="Memorializes colonization") #memorializes colonization
PR <- subset(df.map.long, problem== "Promotes racism")  #promotes racism 
WU <- subset(df.map.long, problem=="Western Use")  

#' 
#' ## Analysis of "Anti-Indigenous genocide perpetrator"
#' 
#' We are testing for the effect of longitude (X) on Anti-Indigenous genocide perpetrator (AIGP) using a beta regression model.
#' 
## ----message=F, warning=F------------------------------------------------
model_AIGP <- betareg(percent_percent~X,
                      data=AIGP, link = "log")

#' **Joint test.**
## ----message=F, warning=F------------------------------------------------

joint_tests(model_AIGP) # Not significant


#' 
#' **Likelihood Ratio Test.**
#' Evaluation with a Likelihood Ratio Test of Nested Models (LRT). The Likelihood-Ratio test (sometimes called the likelihood-ratio chi-squared test) is a hypothesis test that helps you choose the “best” model between two nested models. “Nested models” means that one is a special case of the other. 
## ---- warning=FALSE, message=FALSE---------------------------------------

lrtest(model_AIGP)

#' 
#' **Beta-coefficients and Pseudo R-squared.**
#' Summary on the fit of the model.
#' 
## ----warning=FALSE, message=FALSE----------------------------------------
summary(model_AIGP)


#' **95% Confidence Intervals of each predictor.**
#' 
## ---- warning=FALSE, message=FALSE---------------------------------------

confint(model_AIGP, level = 0.9)


#' 
#' **Plot of fitted values and residuals.**
#' 
## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------

plot(fitted(model_AIGP),
     residuals(model_AIGP),
     main = " Plot of model fitted values and residuals",
     xlab = "fitted values",
     ylab = "residuals")

#' 
#' Below is a figure that examines the effect of longitude on the proportion of place names that are named for an Anti-Indigenous genocide perpetrator.
## ----echo=F, warning=F, message=F----------------------------------------

# figure out the predictions and 95% CI
test.1 <- predict(model_AIGP, newdata = AIGP, interval = "confidence")

# Add predicted values to the dataset
AIGP$predlm = predict(model_AIGP, type = "response")
# apparently it's hard to do add 95% CI to Beta regression

# Plot AIGP
ggplot(data=AIGP, aes(x=X, y=percent_percent)) +
  geom_point(show.legend=T) +
  xlab("Longitude (west-east)") +
  ylab("Percent of Park Names") +
  facet_grid(rows=vars(problem), scales="free_y") +
  geom_line(aes(y=predlm), size =1) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'gray90') ,
    strip.text=element_text(size=12, face="bold"),
    strip.background = element_rect(fill=NA)
  )


#' 
#' **Conclusion on AIPN.**
#' Based on the log-likelihood ratio test, we failed to reject the null hypothesis and conclude that not enough evidence is available to suggest the null is false at the 95% confidence level. Specifically, longitude is not a significant predictor of the proportion of place names associated with an Anti-Indigenous genocide perpetrator (AIGP) across the US national parks studies (*X^2^*(1) = 3.038, *p* = 0.08134). However, we found that the proportion of place names associated with an AIGP decreases by 98% (95% CI = 99, 97).
#' 
#' ## Analysis of "For a racist person"
#' 
#' We are testing for the effect of longitude on For a racist person (FRP) using a beta regression model.
#' 
## ----message=F, warning=F------------------------------------------------
model_FRP <- betareg(percent_percent~X,
                     data=FRP, link = "log")

#' **Joint test.**
## ----message=F, warning=F------------------------------------------------

joint_tests(model_FRP) # Not significant


#' 
#' **Likelihood Ratio Test.**
#' Evaluation with a Likelihood Ratio Test of Nested Models (LRT). The Likelihood-Ratio test (sometimes called the likelihood-ratio chi-squared test) is a hypothesis test that helps you choose the “best” model between two nested models. “Nested models” means that one is a special case of the other. 
## ---- warning=FALSE, message=FALSE---------------------------------------

lrtest(model_FRP)

#' 
#' **Beta-coefficients and Pseudo R-squared.**
#' Summary on the fit of the model.
#' 
## ----warning=FALSE, message=FALSE----------------------------------------
summary(model_FRP)


#' **95% Confidence Intervals of each predictor.**
#' 
## ---- warning=FALSE, message=FALSE---------------------------------------

confint(model_FRP, level = 0.9)


#' 
#' **Plot of fitted values and residuals.**
#' 
## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------

plot(fitted(model_FRP),
     residuals(model_FRP),
     main = "Plot of model fitted values and residuals",
     xlab = "fitted values",
     ylab = "residuals")

#' 
#' Below is a figure that examines the effect of longitude on the proportion of place names that are named for a Racist Person.
## ----echo=F, warning=F, message=F----------------------------------------

# Add predicted values to the dataset
FRP$predlm = predict(model_FRP, type = "response")
# apparently it's hard to do add 95% CI to Beta regression

# Plot AIGP
ggplot(data=FRP, aes(x=X, y=percent_percent)) +
  geom_point(show.legend=T) +
  xlab("Longitude (west-east)") +
  ylab("Percent of Park Names") +
  facet_grid(rows=vars(problem), scales="free_y") +
  geom_line(aes(y=predlm), size =1) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'gray90') ,
    strip.text=element_text(size=12, face="bold"),
    strip.background = element_rect(fill=NA)
  )


#' 
#' **Conclusion on FRP.**
#' Based on the log-likelihood ratio test, we failed to reject the null hypothesis and conclude that not enough evidence is available to suggest the null is false at the 95% confidence level. Specifically, longitude is not a significant predictor of the proportion of place names associated with an For a racist person (FRP) across the US national parks studies (*X^2^*(1) = 0.03, *p* = 0.8729).
#' 
#' ## Analysis of "Memorializes colonization"
#' 
#' We are testing for the effect of longitude on Memorializes colonization (MC) using a beta regression model.
#' 
## ----message=F, warning=F------------------------------------------------
model_MC <- betareg(percent_percent~X,
                    data=MC, link = "log")

#' **Joint test.**
## ----message=F, warning=F------------------------------------------------

joint_tests(model_MC) # Significant


#' 
#' **Likelihood Ratio Test.**
#' Evaluation with a Likelihood Ratio Test of Nested Models (LRT). The Likelihood-Ratio test (sometimes called the likelihood-ratio chi-squared test) is a hypothesis test that helps you choose the “best” model between two nested models. “Nested models” means that one is a special case of the other. 
## ---- warning=FALSE, message=FALSE---------------------------------------

lrtest(model_MC)

#' 
#' **Beta-coefficients and Pseudo R-squared.**
#' Summary on the fit of the model.
#' 
## ----warning=FALSE, message=FALSE----------------------------------------
summary(model_MC)


#' **95% Confidence Intervals of each predictor.**
#' 
## ---- warning=FALSE, message=FALSE---------------------------------------

confint(model_MC, level = 0.9)


#' 
#' **Plot of fitted values and residuals.**
#' 
## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------

plot(fitted(model_MC),
     residuals(model_MC),
     main = "Plot of model fitted values and residuals",
     xlab = "fitted values",
     ylab = "residuals")

#' 
#' Below is a figure that examines the effect of longitude on the proportion of place names that are named for a Racist Person.
## ----echo=F, warning=F, message=F----------------------------------------

# Add predicted values to the dataset
MC$predlm = predict(model_MC, type = "response")
# apparently it's hard to do add 95% CI to Beta regression

# Plot MC
ggplot(data=MC, aes(x=X, y=percent_percent)) +
  geom_point(show.legend=T) +
  xlab("Longitude (west-east)") +
  ylab("Percent of Park Names") +
  facet_grid(rows=vars(problem), scales="free_y") +
  geom_line(aes(y=predlm), size =1) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'gray90') ,
    strip.text=element_text(size=12, face="bold"),
    strip.background = element_rect(fill=NA)
  )


#' 
#' **Conclusion on MC.**
#' Based on the log-likelihood ratio test, we rejected the null hypothesis and and conclude that the alternative hypothesis is true at the 95% confidence level. Specifically, we identify longitude as a significant predictor of the proportion of place names associated with the category "Memorializes colonization" (MC) across the US national parks studies (*X^2^*(1) = 2.36, *p* = 0.1141). Specifically, for each  1 degree increase in longitude the proportion of place names that memorializes colonization increases by 0.02 (95% CI = 0.01, 0.03 ).
#' 
## ------------------------------------------------------------------------

library(gam)
library(mgcv)

# use gam and mgcv functions to generate plots

mygam = gam(percent_percent ~ X, 
            family=betar(link="log"), 
            data = MC)

min <- min(MC$X)
max <- max(MC$X)
new.x <- expand.grid(X= seq(min, max, length.out = 1000))
new.y <- predict(mygam, newdata = new.x, se.fit = TRUE, type="response")
new.y <- data.frame(new.y)
addThese <- data.frame(new.x, new.y)
addThese <- rename(addThese, y = fit, SE = se.fit)
addThese <- mutate(addThese, lwr = y - 1.96 * SE, upr = y + 1.96 * SE) # calculating the 95% confidence interval

# Plot

# MC.plot <- ggplot(data=MC, aes(x=X, y=percent_percent)) +
#   geom_point(show.legend=T, size=0.6) +
#   xlab("Longitude (west-east)") +
#   ylab("Proportion of Park Names") +
#   # facet_grid(rows=vars(problem), scales="free_y") +
#   geom_line(data = addThese, aes(x=X, y=y)) + 
#   geom_line(data = addThese, aes(x=X, y=lwr), color = "#E1BE6A", linetype = "dashed") + 
#   geom_line(data = addThese, aes(x=X, y=upr), color = "#E1BE6A", linetype = "dashed") + 
#   xlim(min(MC$X - 0.5), max(MC$X + 0.5)) +
#   ylim(0, max(MC$percent_percent+0.02)) +
#   theme(
#     panel.grid.minor=element_blank(), 
#     panel.grid.major=element_blank() ,
#     panel.background = element_rect(fill = 'white', color="gray60") ,
#     strip.text=element_text(size=12, face="bold"),
#     strip.background = element_rect(fill=NA)
#   )
# MC.plot

#' 
#' ## Analysis of "Promotes racism"
#' 
#' We are testing for the effect of longitude on Promotes racism (PR) using a beta regression model.
#' 
## ----message=F, warning=F------------------------------------------------
model_PR <- betareg(percent_percent~X,
                    data=PR, link = "log")

#' **Joint test.**
## ----message=F, warning=F------------------------------------------------

joint_tests(model_PR) # Not significant


#' 
#' **Likelihood Ratio Test.**
#' Evaluation with a Likelihood Ratio Test of Nested Models (LRT). The Likelihood-Ratio test (sometimes called the likelihood-ratio chi-squared test) is a hypothesis test that helps you choose the “best” model between two nested models. “Nested models” means that one is a special case of the other. 
## ---- warning=FALSE, message=FALSE---------------------------------------

lrtest(model_PR)

#' 
#' **Beta-coefficients and Pseudo R-squared.**
#' Summary on the fit of the model.
#' 
## ----warning=FALSE, message=FALSE----------------------------------------
summary(model_PR)


#' **95% Confidence Intervals of each predictor.**
#' 
## ---- warning=FALSE, message=FALSE---------------------------------------

confint(model_PR, level = 0.9)


#' 
#' **Plot of fitted values and residuals.**
#' 
## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------

plot(fitted(model_PR),
     residuals(model_PR),
     main = "Plot of model fitted values and residuals",
     xlab = "fitted values",
     ylab = "residuals")

#' 
#' Below is a figure that examines the effect of longitude on the proportion of place names that are named for a Racist Person.
## ----echo=F, warning=F, message=F----------------------------------------

# Add predicted values to the dataset
PR$predlm = predict(model_PR, type = "response")
# apparently it's hard to do add 95% CI to Beta regression

# Plot AIGP
ggplot(data=PR, aes(x=X, y=percent_percent)) +
  geom_point(show.legend=T) +
  xlab("Longitude (west-east)") +
  ylab("Percent of Park Names") +
  facet_grid(rows=vars(problem), scales="free_y") +
  geom_line(aes(y=predlm), size =1) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'gray90') ,
    strip.text=element_text(size=12, face="bold"),
    strip.background = element_rect(fill=NA)
  )


#' 
#' **Conclusion on PR.**
#' Based on the log-likelihood ratio test, we failed to reject the null hypothesis and conclude that not enough evidence is available to suggest the null is false at the 95% confidence level. Specifically, we found that longitude is not a significant predictor of the proportion of place names associated with the category "Promotes Racisim" (PR) across the US national parks studies (*X^2^*(1) = 0.6546, *p* = 0.4185).
#' 
#' ## Analysis of "Western use"
#' 
#' We are testing for the effect of longitude on Western use (WU) using a beta regression model.
#' 
## ----message=F, warning=F------------------------------------------------
model_WU <- betareg(percent_percent~X,
                    data=WU, link = "log")

#' **Joint test.**
## ----message=F, warning=F------------------------------------------------

joint_tests(model_WU) # Significant


#' 
#' **Likelihood Ratio Test.**
#' Evaluation with a Likelihood Ratio Test of Nested Models (LRT). The Likelihood-Ratio test (sometimes called the likelihood-ratio chi-squared test) is a hypothesis test that helps you choose the “best” model between two nested models. “Nested models” means that one is a special case of the other. 
## ---- warning=FALSE, message=FALSE---------------------------------------

lrtest(model_WU) # Not significant

#' 
#' **Beta-coefficients and Pseudo R-squared.**
#' Summary on the fit of the model.
#' 
## ----warning=FALSE, message=FALSE----------------------------------------
summary(model_WU)


#' **95% Confidence Intervals of each predictor.**
#' 
## ---- warning=FALSE, message=FALSE---------------------------------------

confint(model_WU, level = 0.9)


#' 
#' **Plot of fitted values and residuals.**
#' 
## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------

plot(fitted(model_WU),
     residuals(model_WU),
     main = "Plot of model fitted values and residuals",
     xlab = "fitted values",
     ylab = "residuals")

#' 
#' Below is a figure that examines the effect of longitude on the proportion of place names that are categorized as "Western Use".
## ----echo=F, warning=F, message=F----------------------------------------

# Add predicted values to the dataset
WU$predlm = predict(model_WU, type = "response")
# apparently it's hard to do add 95% CI to Beta regression

# Plot WU
ggplot(data=WU, aes(x=X, y=percent_percent)) +
  geom_point(show.legend=T) +
  xlab("Longitude (west-east)") +
  ylab("Percent of Park Names") +
  facet_grid(rows=vars(problem), scales="free_y") +
  geom_line(aes(y=predlm), size =1) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'gray90') ,
    strip.text=element_text(size=12, face="bold"),
    strip.background = element_rect(fill=NA)
  )


#' 
#' **Conclusion on WU.**
#' Based on the log-likelihood ratio test, we rejected the null hypothesis and and conclude that the alternative hypothesis is true at the 95% confidence level. Specifically, we found that longitude is a marginally significant predictor of the proportion of place names associated with the category "Western Use" (WU) across the US national parks studies (*X^2^*(1) = 3.5981, *p* = 0.05785). Specifically, for each  1 degree increase in longitude the proportion of place names associated with western use decreases by 0.02 (95% CI = -0.03, -0.005).
#' 
## ------------------------------------------------------------------------
# use gam and mgcv functions to generate plots

mygam = gam(percent_percent ~ X, 
            family=betar(link="log"), 
            data = WU)

min <- min(WU$X)
max <- max(WU$X)
new.x <- expand.grid(X= seq(min, max, length.out = 1000))
new.y <- predict(mygam, newdata = new.x, se.fit = TRUE, type="response")
new.y <- data.frame(new.y)
addThese <- data.frame(new.x, new.y)
addThese <- rename(addThese, y = fit, SE = se.fit)
addThese <- mutate(addThese, lwr = y - 1.96 * SE, upr = y + 1.96 * SE) # calculating the 95% confidence interval

# Plot
WU.plot <- ggplot(data=WU, aes(x=X, y=percent_percent)) +
  geom_point(show.legend=T, size=0.6) +
  xlab("Longitude") +
  ylab("Proportion of Park Names") +
  # facet_grid(rows=vars(problem), scales="free_y") +
  geom_line(data = addThese, aes(x=X, y=y)) + 
  geom_line(data = addThese, aes(x=X, y=lwr), color = "#E1BE6A", linetype = "dashed") + 
  geom_line(data = addThese, aes(x=X, y=upr), color = "#E1BE6A", linetype = "dashed") + 
  xlim(min(WU$X - 0.5), max(WU$X + 0.5)) +
  ylim(0, max(WU$percent_percent+0.02)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'white', color = "gray60") ,
    strip.text=element_text(size=12, face="bold"),
    strip.background = element_rect(fill=NA)
  )
WU.plot


#' 
#' ## Effect of longitude and latitude on IPN
#' 
## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
## Step 6: Make similar dataset for plotting Indigenous place names over time and longitude

# wrangle the data needed and join coords to parks, removing redundant columns
df.map2 <- df %>% 
  filter(!is.na(indig_or_wstrn)) %>% 
  dplyr::group_by(np, indig_or_wstrn) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(percent = round(100 * n/sum(n),1)) %>%
  arrange(desc(indig_or_wstrn)) %>% #, desc(percent)
  #left_join(NParks) %>% #join the coords and data together
  pivot_wider(names_from = indig_or_wstrn, values_from = percent, #pivot to wide format
              values_fill = list(percent = 0),
              id_cols=np) # %>% 
# dplyr::select(-UNIT_TYPE, -REGION, -METADATA) 

## join to existing df.map that has all the spatial info and years
df.map <- left_join(df.map, df.map2, by = c("np" = "np"))

## change to long format
df.map.long2 <- df.map[,c(1:5, 14, 17, 18)] %>%
  gather("i_or_w", "percent", -np, -X, -Y, -UNIT_NAME, -STATE, -yr)

#' 
## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
#############################
## Step 6: Make facet scatterplots for IPN by longitude

labs <- df.map.long2 %>% 
  filter(i_or_w=="Indigenous") %>%
  filter(percent > 15 )

ipn_longitude <- df.map.long2 %>%
  filter(i_or_w=="Indigenous") %>%  # since I or W is either/or only need to plot one (other is just reverse)

ggplot(aes(x=X, y=percent)) +
  geom_point(show.legend=T) +
  geom_smooth(method=lm) + 
  coord_cartesian(ylim=c(-5,100), xlim=c(-157, -73), expand=F) +
  geom_hline(yintercept=0, color="gray50", size=0.5) +
  #geom_text(x = -140, y = 40, label = lm_eqn(df), parse = TRUE) +
  xlab("Longitude (west-east)") +
  ylab("Percent") +
  geom_label_repel(data=labs, aes(label=np),
                   segment.color="green", 
                   segment.size = 0.5,
                   box.padding=3,
                   fill=NA,
                   ylim=c(25, 100)) + 
  #facet_wrap(vars(i_or_w)) + #, scales="free_y")+
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, size=0.5)
  )	 


# quartz(width=6, height=4) # might still need to adjust window proportions before saving
# ipn_longitude

# lmlongipn <- lm(percent~X+i_or_w, data=df.map.long2)
# summary(lmlongipn)

# lmlongipn_int <- lm(percent~X*i_or_w, data=df.map.long2)
# summary(lmlongipn_int)

#' 
#' ## Beta Regression
#' 
#' **Joint test.**
#' 
## ----echo=F, message=F, warning=F----------------------------------------
# Beta distributions require non absolute 0 data.
df.map.long2$percent_percent <- (df.map.long2$percent)/100
df.map.long2$percent_percent <- df.map.long2$percent_percent + ifelse(df.map.long2$percent_percent == 0, 0.000001, 0)
df.map.long2$percent_percent <- df.map.long2$percent_percent - ifelse(df.map.long2$percent_percent == 1.00, 0.999999, 0)

#' 
## ----message=FALSE, warning=FALSE----------------------------------------

# Model for IPN  to test if sig. more W than I names
model_v2 = betareg(percent_percent ~ X+i_or_w,   
                   data = df.map.long2, link = "log")

# or  to test if X is sig. for proportion I
model_v3 = betareg(percent_percent ~ X,
                   data = df.map.long2[df.map.long2$i_or_w=="Indigenous",], link = "log")

joint_tests(model_v3)  # long. is sign. p=0.0012


#' 
#' **Likelihood Ratio Test.**
#' Evaluation with a Likelihood Ratio Test of Nested Models (LRT). The Likelihood-Ratio test (sometimes called the likelihood-ratio chi-squared test) is a hypothesis test that helps you choose the “best” model between two nested models. “Nested models” means that one is a special case of the other. 
## ---- warning=FALSE, message=FALSE---------------------------------------

lrtest(model_v3)    # significant p=0.011  

#' 
#' **Beta-coefficients and Pseudo R-squared.**
#' Summary on the fit of the model.
#' 
## ----warning=FALSE, message=FALSE----------------------------------------
summary(model_v2)   # significantly more W than I (p=0.02)


#' 
#' **Plot of fitted values and residuals.**
#' 
## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------

plot(fitted(model_v2),
     residuals(model_v2),
     main = "Plot of model fitted values and residuals",
     xlab = "fitted values",
     ylab = "residuals")

plot(fitted(model_v3),
     residuals(model_v3),
     main = "Plot of model fitted values and residuals",
     xlab = "fitted values",
     ylab = "residuals")


#' 
#' **95% Confidence Intervals of each predictor.**
#' 
## ---- warning=FALSE, message=FALSE---------------------------------------

confint(model_v3, level = 0.9)


#' 
## ----echo=F, message=F, warning=F----------------------------------------
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables

se <- function(x) { sd(x) / sqrt(length(x)) }
head(df.map.long2)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# to compare if there are significantly more or less Indigenous compared to western names
df2 <- data_summary(df.map.long2, varname="percent_percent", 
                    groupnames=c("i_or_w"))

# can't get se to work in the function above, so doing it manually
df2$se <- c(
  se(df.map.long2$percent_percent[df.map.long2$i_or_w %in% "Indigenous"]),
  se(df.map.long2$percent_percent[df.map.long2$i_or_w %in% "Western"])
)
df2  # Indigenous names average 15+/-4.5% SE of park names, Western average 79+/-6.9% of park names

#' 
#' **Conclusion on IPN**
#' Significant proportion of places names in US National Parks. There is no effect of longitude or the interaction between longitude and IPN categorization on the proportion of places names within a US National Park that are classified as either IPN or Western. There is a significantly more western place names than indigenous place names across US National Parks.
## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=df2, aes(x=i_or_w, y=percent_percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin=percent_percent-sd, ymax=percent_percent+sd), width=.2,
                position=position_dodge(.9)) +
  xlab("Place Name Category") +
  ylab("Proportion") +
  theme_minimal()

#' 
#' 
#' 
#' #### If you want a scatter plot: 
IPNdat <- df.map.long2[df.map.long2$i_or_w=="Indigenous",]

mygam = gam(percent_percent ~ X, 
      family=betar(link="log"), 
      data = IPNdat)

min <- min(IPNdat$X)
max <- max(IPNdat$X)
new.x <- expand.grid(X= seq(min, max, length.out = 1000))
new.y <- predict(mygam, newdata = new.x, se.fit = TRUE, type="response")
new.y <- data.frame(new.y)
addThese <- data.frame(new.x, new.y)
addThese <- dplyr::rename(addThese, y = fit, SE = se.fit)
addThese <- dplyr::mutate(addThese, lwr = y - 1.96 * SE, upr = y + 1.96 * SE) # calculating the 95% confidence interval

IPN.plot <- ggplot(data=IPNdat, aes(x=X, y=percent_percent)) +
  geom_point(show.legend=T, size=0.6) +
  xlab("Longitude") +
  ylab("Proportion of Park Names") +
  #facet_grid(rows=vars(problem), scales="free_y") +
  geom_line(data = addThese, aes(x=X, y=y)) +
  geom_line(data = addThese, aes(x=X, y=lwr), color = "#E1BE6A", linetype = "dashed") +
  geom_line(data = addThese, aes(x=X, y=upr), color = "#E1BE6A", linetype = "dashed") +
  xlim(min(IPNdat$X - 0.5), max(IPNdat$X + 0.5)) +
  ylim(0, max(IPNdat$percent_percent+0.02)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'white', color = "gray60") ,
    strip.text=element_text(size=12, face="bold"),
    strip.background = element_rect(fill=NA)
  )
IPN.plot
## ------------------------------------------------------------------------

library(ggpubr)


# filters for which points to label
labs_WU <- WU %>%    
  filter(X < -145 | percent > 15 | X > -80)

WU.plot_forpub <- WU.plot + 
  ylab("Proportion of park names") +
  xlab("Longitude") +
  geom_label_repel(data=labs_WU, aes(label=short),
                   segment.color="#40B0A6",   # "steelblue1"
                   #segment.size = 0.5,
                   box.padding=1,
                   #fill=alpha("gray100", 1),
                   fill="gray100",
                   color="#40B0A6",
                   size=3,
                   nudge_y = 0.001) + 
  annotate("text", x=-100, y=0.4, label="Appropriation from\nIndigenous languages", size=3.5, fontface="bold") +
  theme(plot.margin = unit(c(0.0,0.5,0.5,0.5), "cm"),
        legend.position="none",
        axis.title=element_text(size=10, color="black", face="bold"),
        axis.text.y=element_text(size=9, color="black", margin=margin(l=5, unit="pt")),
        axis.text.x=element_text(size=9, color="black"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = 'white')
  )
WU.plot_forpub

quartz(width=3, height=4) # might still need to adjust window proportions before saving

WU.plot_forpub

quartz.save("./outputs/figs/scatter-betareg_WUonly.png", type="png", device=dev.cur(), dpi=300, bg="white")
dev.off()






#' 
## ------------------------------------------------------------------------
cor.test(MC$Y, MC$yr, method = "spearman", exact = F)
cor.test(AIGP$Y, AIGP$yr, method = "spearman", exact = F)
cor.test(WU$Y, WU$yr, method = "spearman", exact = F)
cor.test(PR$Y, PR$yr, method = "spearman", exact = F)

#' 
