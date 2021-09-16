# #knitr::opts_chunk$set(echo = TRUE)
# #packages_needed <- c("ggplot2", # graphics
#                      "plyr",
#                      "arm", # display() etc.
#                      "MASS",
#                      "dplyr",
#                      "ggfortify"
# #)
# #k_to_install <- packages_needed [!( packages_needed %in% rownames(installed.packages())  )]
# #if(length(pk_to_install)>0 ){
#   install.packages(pk_to_install,repos="http://cran.r-project.org")








library(ggplot2)
library(arm)
library(ggfortify)

############# Running Bat mist net Species Richness Vs. Acoustic Species Richness with both poisson and quasipoisson 
Bat_M.A_Data<- read.csv("Data/GLM_Assignment_2.csv")

View(Bat_M.A_Data)

Bat_NetVSAcoust.glm<- glm(Net_Species_Richness~ Acoustic_Species_Richness, data=Bat_M.A_Data , family= poisson(link = "log"))

Bat_NetVSAcoust.plot<- ggplot(Bat_NetVSAcoust.glm, aes(Acoustic_Species_Richness, Net_Species_Richness)) + 
  geom_point(size=3) +stat_smooth(method = glm, method.args = list(family = poisson(link = "log"))) +
  labs(title="GLM Family= Poisson, Bat Acoustics VS. Mist-net Numbers") +
  xlab ("Acoustic # of Species") +
  ylab ("MistNet # of Species")
View(Bat_NetVSAcoust.glm)

autoplot(Bat_NetVSAcoust.glm)

anova(Bat_NetVSAcoust.glm)

summary(Bat_NetVSAcoust.glm)

with(summary(Bat_NetVSAcoust.glm),1-deviance/null.deviance)

Bat_NetVSAcoust.glm2<- glm(Net_Species_Richness~ Acoustic_Species_Richness, data=Bat_M.A_Data , family= quasipoisson)

Bat_NetVSAcoust.plot2<- ggplot(Bat_NetVSAcoust.glm, aes(Acoustic_Species_Richness, Net_Species_Richness)) + 
  geom_point(size=3) +stat_smooth(method = glm, method.args = list(family = quasipoisson(link = "log"))) +
  labs(title="GLM Family= Quasipoisson, Bat Acoustics VS. Mist-net Numbers") +
  xlab ("Acoustic # of Species") +
  ylab ("MistNet # of Species")

autoplot(Bat_NetVSAcoust.glm2)

anova(Bat_NetVSAcoust.glm2)

summary(Bat_NetVSAcoust.glm2)

###################### Running EPFU Occurance by Site Visual Scores using Binomial Family

EPFU_OccuranceVSSite_Quality.glm<- glm(EPFU_Occurance~ Visual_Score, data=Bat_M.A_Data , family= binomial(link = "logit"))

EPFU_OccuranceVSSite_Quality.plot<- ggplot(EPFU_OccuranceVSSite_Quality.glm, aes(Visual_Score, EPFU_Occurance)) + 
  geom_point(size=3) +stat_smooth(method = glm, method.args = list(family = binomial)) +
  labs(title="GLM Family= Binomial,  EPFU Occurance by Site Visual Quality") +
  xlab ("Site Quality Category") +
  ylab ("EPFU Occurance")

autoplot(EPFU_OccuranceVSSite_Quality.glm)

anova(EPFU_OccuranceVSSite_Quality.glm)

summary(EPFU_OccuranceVSSite_Quality.glm)

