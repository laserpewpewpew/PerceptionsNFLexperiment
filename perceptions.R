################################################
# Perceptions vs. Reality - Survey Experiments #
# Kellen Gracey                                #
# Department of Political Science              #
# University of Iowa                           #
################################################
library(foreign)
library(stargazer)
mydata <- data.frame(read.spss("CCES12_IOW_OUTPUT_vv_20130930.sav"))


#############################
# NFL Perception Experiment #
#############################

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

mydata$NFLattn[mydata$IOW006=="Very closely"] <- 3
mydata$NFLattn[mydata$IOW006=="Somewhat closely"] <- 2
mydata$NFLattn[mydata$IOW006=="Not very closely"] <- 1
mydata$NFLattn[mydata$IOW006=="Not at all"] <- 0
mydata$NFLattn[mydata$IOW006=="Skipped"] <- NA

mydata$sportstalk[mydata$IOW007=="Very often"] <- 3
mydata$sportstalk[mydata$IOW007=="Often"] <- 2
mydata$sportstalk[mydata$IOW007=="Sometimes"] <- 1
mydata$sportstalk[mydata$IOW007=="Never"] <- 0
mydata$sportstalk[mydata$IOW007=="Skipped"] <- NA

mydata$age <- (2012 - mydata$birthyr)

mydata$educ <- as.character(mydata$educ)
mydata$edu <- NA
mydata$edu[mydata$educ=="No HS"] <- 0
mydata$edu[mydata$educ=="High school graduate"] <- 1
mydata$edu[mydata$educ=="Some college"] <- 2
mydata$edu[mydata$educ=="2-year"] <- 3
mydata$edu[mydata$educ=="4-year"] <- 4
mydata$edu[mydata$educ=="Post-grad"] <- 5

mydata$female[mydata$gender=="Female"] <- 1
mydata$female[mydata$gender=="Male"] <- 0

mydata$white[mydata$race=="White"] <- 1
mydata$white[mydata$race!="White"] <- 0

mydata$whitenonhisp[mydata$white==1 & mydata$hispanic=="No"] <- 1
mydata$whitenonhisp[mydata$white!=1 & mydata$hispanic!="No"] <- 0

glm1 <- glm(favewinsclosegame ~ likelytowatch + NFLattn + sportstalk + V102 + age + edu + female + whitenonhisp, family=binomial, data=mydata)
stargazer(glm1)


###############################################
# Presidential Election Perception Experiment #
###############################################

mydata$favorobama[mydata$IOW002=="Barack Obama, the Democrat"] <- 1
mydata$favorobama[mydata$IOW002!="Barack Obama, the Democrat"] <- 0
mydata$favorromney[mydata$IOW002=="Mitt Romney, the Republican"] <- 1
mydata$favorromney[mydata$IOW002!="Mitt Romney, the Republican"] <- 0

mydata$obamaclose[mydata$IOW003=="Will be a close election" & mydata$favorobama==1] <- 1
mydata$obamaclose[mydata$IOW003!="Will be a close election" & mydata$favorobama!=1] <- 0
mydata$romneyclose[mydata$IOW003=="Will be a close election" & mydata$favorromney==1] <- 1
mydata$romneyclose[mydata$IOW003!="Will be a close election" & mydata$favorromney!=1] <- 0



########################################
# House Election Perception Experiment #
########################################

mydata$favorone[mydata$IOW004=="$HouseCand1Name, the $HouseCand1Party candidate"] <- 1
mydata$favorone[mydata$IOW004!="$HouseCand1Name, the $HouseCand1Party candidate"] <- 0
mydata$favortwo[mydata$IOW004=="$HouseCand2Name, the $HouseCand2Party candidate"] <- 1
mydata$favortwo[mydata$IOW004!="$HouseCand2Name, the $HouseCand2Party candidate"] <- 0

mydata$oneclose[mydata$IOW005=="Will be a close election"& mydata$favorone==1] <- 1
mydata$oneclose[mydata$IOW005!="Will be a close election"& mydata$favorone!=1] <- 0
mydata$twoclose[mydata$IOW005=="Will be a close election"& mydata$favortwo==1] <- 1
mydata$twoclose[mydata$IOW005!="Will be a close election"& mydata$favortwo!=1] <- 0


##############################
# Recodes for political vars #
##############################

mydata$informed[mydata$IOW029=="Less informed"] <- 0
mydata$informed[mydata$IOW029=="More informed"] <- 2
mydata$informed[mydata$IOW029=="About the same"] <- 1
mydata$informed[mydata$IOW029=="Don't know"] <- NA
mydata$informed[mydata$IOW029=="Skipped"] <- NA

mydata$othersinformed[mydata$IOW030=="Strongly agree"] <- 4
mydata$othersinformed[mydata$IOW030=="Agree"] <- 3
mydata$othersinformed[mydata$IOW030=="Neither agree nor disagree"] <- 2
mydata$othersinformed[mydata$IOW030=="Disagree"] <- 1
mydata$othersinformed[mydata$IOW030=="Strongly disagree"] <- 0
mydata$othersinformed[mydata$IOW030=="Don't know"] <- NA
mydata$othersinformed[mydata$IOW030=="Skipped"] <- NA

mydata$libideo[mydata$ideo5=="Very liberal"] <- 4
mydata$libideo[mydata$ideo5=="Liberal"] <- 3
mydata$libideo[mydata$ideo5=="Moderate"] <- 2
mydata$libideo[mydata$ideo5=="Conservative"] <- 1
mydata$libideo[mydata$ideo5=="Very Conservative"] <- 0
mydata$libideo[mydata$ideo5=="Not sure"] <- NA

mydata$dem[mydata$pid3=="Democrat "] <- 1
mydata$dem[mydata$pid3!="Democrat "] <- 0
mydata$rep[mydata$pid3=="Republican "] <- 1
mydata$rep[mydata$pid3!="Republican "] <- 0
mydata$thirdparty[mydata$pid3=="Other "] <- 1
mydata$thirdparty[mydata$pid3!="Other "] <- 0
mydata$independent[mydata$pid3=="Independent "] <- 1
mydata$independent[mydata$pid3!="Independent "] <- 0

mydata$demobamawin[mydata$dem==1 & mydata$favorobama==1] <- 1
mydata$demobamawin[mydata$dem!=1 & mydata$favorobama!=1] <- 0

mydata$repromneywin[mydata$rep==1 & mydata$favorromney==1] <- 1
mydata$repromneywin[mydata$rep!=1 & mydata$favorromney!=1] <- 0


#########
# Model #
#########

glm2 <- glm(obamaclose ~ informed + othersinformed + libideo + dem + rep + age + edu + female + whitenonhisp + favewinsclosegame, family="binomial", data=mydata)
glm3 <- glm(romneyclose ~ informed + othersinformed + libideo + dem + rep + age + edu + female + whitenonhisp + favewinsclosegame, family="binomial", data=mydata)
glm4 <- glm(obamaclose ~ informed + othersinformed + libideo + dem + rep + age + edu + female + whitenonhisp, family="binomial", data=mydata)
glm5 <- glm(romneyclose ~ informed + othersinformed + libideo + dem + rep + age + edu + female + whitenonhisp, family="binomial", data=mydata)
stargazer(glm2, glm3, glm4, glm5)

glm6 <- glm(oneclose ~ informed + othersinformed + libideo + dem + rep + age + edu + female + whitenonhisp + favewinsclosegame,family=binomial, data=mydata)
glm7 <- glm(twoclose ~ informed + othersinformed + libideo + dem + rep + age + edu + female + whitenonhisp + favewinsclosegame,family=binomial, data=mydata)
glm8 <- glm(oneclose ~ informed + othersinformed + libideo + dem + rep + age + edu + female + whitenonhisp ,family=binomial, data=mydata)
glm9 <- glm(twoclose ~ informed + othersinformed + libideo + dem + rep + age + edu + female + whitenonhisp ,family=binomial, data=mydata)
stargazer(glm6, glm7, glm8, glm9)

