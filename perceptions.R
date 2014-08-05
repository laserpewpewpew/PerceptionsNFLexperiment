################################################
# Perceptions vs. Reality - Survey Experiments #
# Kellen Gracey                                #
# Department of Political Science              #
# University of Iowa                           #
################################################
library(foreign)
mydata <- data.frame(read.spss("CCES12_IOW_OUTPUT_vv_20130930.sav"))

mydata$teammatch[mydata$randpair_set=="Includes favorite team (unless no favorite picked or team has a BYE that day"] <- 1
mydata$teammatch[mydata$randpair_set=="Random pairing "] <- 0

mydata$favewin[mydata$IOW009=="The $top_team "] <- 1
mydata$favewin[mydata$IOW009!="The $top_team "] <- 0

mydata$closewin[mydata$IOW009b=="It will be a close game"] <- 1
mydata$closewin[mydata$IOW009b!="It will be a close game"] <- 0

mydata$favewinsclosegame[mydata$favewin==1 & mydata$closewin==1] <- 1
mydata$favewinsclosegame[mydata$favewin!=1 & mydata$closewin!=1] <- 0

mydata$likelytowatch[mydata$IOW010=="Very likely"] <- 3
mydata$likelytowatch[mydata$IOW010=="Somewhat likely"] <- 2
mydata$likelytowatch[mydata$IOW010=="Not very likely"] <- 1
mydata$likelytowatch[mydata$IOW010=="Don't know"] <- 0
mydata$likelytowatch[mydata$IOW010=="Skipped"] <- NA