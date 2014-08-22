################################################
# Perceptions vs. Reality - Survey Experiments #
# Kellen Gracey                                #
# Department of Political Science              #
# University of Iowa                           #
################################################
library(foreign)
library(stargazer)
library(MASS)
mydata <- data.frame(read.spss("CCES12_IOW_OUTPUT_vv_20130930.sav"))


#############################
# NFL Perception Experiment #
#############################

mydata$teammatch[mydata$randpair_set=="Includes favorite team (unless no favorite picked or team has a BYE that day"] <- 1
mydata$teammatch[mydata$randpair_set=="Random pairing "] <- 0

mydata$favewin[mydata$IOW009=="The $top_team "] <- 1
mydata$favewin[mydata$IOW009!="The $top_team "] <- 0

mydata$dummyclose[mydata$IOW009=="Too close to call"] <- 1
mydata$dummyclose[mydata$IOW009!="Too close to call"] <- 0

mydata$closewin[mydata$IOW009b=="It will be a close game"] <- 1
mydata$closewin[mydata$IOW009=="Too close to call"] <- 1
mydata$closewin[mydata$IOW009b=="The $IOW009b_insert are heavily favored to win"] <- 0
mydata$closewin[mydata$IOW009b=="Don't know"] <- 0
mydata$closewin[mydata$IOW009b=="Not Asked"] <- 0

mydata$closewin[mydata$dummyclose==1] <- 1

mydata$perceiveclose[mydata$IOW009b=="It will be a close game"] <- 1

mydata$favewinsclosegame[mydata$favewin==1 & mydata$closewin==1] <- 1
mydata$favewinsclosegame[mydata$favewin!=1 & mydata$closewin!=1] <- 0

mydata$likelytowatch[mydata$IOW010=="Very likely"] <- 3
mydata$likelytowatch[mydata$IOW010=="Somewhat likely"] <- 2
mydata$likelytowatch[mydata$IOW010=="Not very likely"] <- 1
mydata$likelytowatch[mydata$IOW010=="Don't know"] <- 1
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

mydata$hispanicdummy[mydata$hispanic=="Yes"] <- 1
mydata$hispanicdummy[mydata$hispanic=="Not Asked"] <- 1
mydata$hispanicdummy[mydata$hispanic=="No"] <- 0
mydata$hispanicdummy[mydata$hispanic=="Skipped"] <- NA

######################################################
# Coding Team Match Ups to check for competitiveness #
######################################################

mydata$includesfave[mydata$randpair_set=="Random pairing "] <- 0
mydata$includesfave[mydata$randpair_set=="Includes favorite team (unless no favorite picked or team has a BYE that day"] <- 1

includesfave <- mydata[mydata$includesfave==1,]
randpair <- mydata[mydata$includesfave==0,]

mydata$fan[mydata$IOW008b=="Not Asked"] <- 0
mydata$fan[mydata$IOW008b!="Not Asked"] <- 1

mydata$likelytowatch <- as.factor(mydata$likelytowatch)

mydata$likelydich[mydata$likelytowatch==3] <- 1
mydata$likelydich[mydata$likelytowatch==2] <- 0
mydata$likelydich[mydata$likelytowatch==1] <- 0

mydata$topteamrecode <- as.character(gsub(" ", "", mydata$top_team))
mydata$oppteamrecode <- as.character(gsub(" ", "", mydata$opp_team))

mydata$matchup <- paste(mydata$topteamrecode, mydata$oppteamrecode, sep=" vs. ")

mydata$scorediff[mydata$matchup=="ArizonaCardinals vs. BuffaloBills"] <- 3
mydata$scorediff[mydata$matchup=="ArizonaCardinals vs. GreenBayPackers"] <- 14
mydata$scorediff[mydata$matchup=="ArizonaCardinals vs. MinnesotaVikings"] <- 7
mydata$scorediff[mydata$matchup=="ArizonaCardinals vs. SanFrancisco49ers"] <- 21
mydata$scorediff[mydata$matchup=="ArizonaCardinals vs. St.LouisRams"] <- 14
mydata$scorediff[mydata$matchup=="AtlantaFalcons vs. DallasCowboys"] <- 6
mydata$scorediff[mydata$matchup=="AtlantaFalcons vs. OaklandRaiders"] <- 3
mydata$scorediff[mydata$matchup=="AtlantaFalcons vs. PhiladelphiaEagles"] <- 13
mydata$scorediff[mydata$matchup=="AtlantaFalcons vs. WashingtonRedskins"] <- 7
mydata$scorediff[mydata$matchup=="BaltimoreRavens vs. ClevelandBrowns"] <- 10
mydata$scorediff[mydata$matchup=="BaltimoreRavens vs. DallasCowboys"] <- 2
mydata$scorediff[mydata$matchup=="BaltimoreRavens vs. HoustonTexans"] <- 30
mydata$scorediff[mydata$matchup=="BaltimoreRavens vs. KansasCityChiefs"] <- 3
mydata$scorediff[mydata$matchup=="BuffaloBills vs. ArizonaCardinals"] <- 3
mydata$scorediff[mydata$matchup=="BuffaloBills vs. HoustonTexans"] <- 12
mydata$scorediff[mydata$matchup=="BuffaloBills vs. SanFrancisco49ers"] <- 42
mydata$scorediff[mydata$matchup=="BuffaloBills vs. TennesseeTitans"] <- 1
mydata$scorediff[mydata$matchup=="CarolinaPanthers vs. ChicagoBears"] <- 1
mydata$scorediff[mydata$matchup=="CarolinaPanthers vs. DallasCowboys"] <- 5
mydata$scorediff[mydata$matchup=="CarolinaPanthers vs. SeattleSeahawks"] <- 4
mydata$scorediff[mydata$matchup=="ChicagoBears vs. CarolinaPanthers"] <- 1
mydata$scorediff[mydata$matchup=="ChicagoBears vs. DetroitLions"] <- 6
mydata$scorediff[mydata$matchup=="ChicagoBears vs. JacksonvilleJaguars"] <- 38
mydata$scorediff[mydata$matchup=="ChicagoBears vs. TennesseeTitans"] <- 31
mydata$scorediff[mydata$matchup=="CincinnatiBengals vs. ClevelandBrowns"] <- 10
mydata$scorediff[mydata$matchup=="CincinnatiBengals vs. MiamiDolphins"] <- 4
mydata$scorediff[mydata$matchup=="CincinnatiBengals vs. PittsburghSteelers"] <- 7
mydata$scorediff[mydata$matchup=="ClevelandBrowns vs. BaltimoreRavens"] <- 10
mydata$scorediff[mydata$matchup=="ClevelandBrowns vs. CincinnatiBengals"] <- 10
mydata$scorediff[mydata$matchup=="ClevelandBrowns vs. IndianapolisColts"] <- 4
mydata$scorediff[mydata$matchup=="ClevelandBrowns vs. NewYorkGiants"] <- 14
mydata$scorediff[mydata$matchup=="ClevelandBrowns vs. SanDiegoChargers"] <- 1
mydata$scorediff[mydata$matchup=="DallasCowboys vs. BaltimoreRavens"] <- 1
mydata$scorediff[mydata$matchup=="DallasCowboys vs. CarolinaPanthers"] <- 5
mydata$scorediff[mydata$matchup=="DallasCowboys vs. NewYorkGiants"] <- 2
mydata$scorediff[mydata$matchup=="DenverBroncos vs. CarolinaPanthers"] <- 22
mydata$scorediff[mydata$matchup=="DenverBroncos vs. CincinnatiBengals"] <- 8
mydata$scorediff[mydata$matchup=="DenverBroncos vs. NewEnglandPatriots"] <- 10
mydata$scorediff[mydata$matchup=="DenverBroncos vs. NewOrleansSaints"] <- 20
mydata$scorediff[mydata$matchup=="DenverBroncos vs. SanDiegoChargers"] <- 7
mydata$scorediff[mydata$matchup=="DetroitLions vs. ChicagoBears"] <- 6
mydata$scorediff[mydata$matchup=="DetroitLions vs. JacksonvilleJaguars"] <- 17
mydata$scorediff[mydata$matchup=="DetroitLions vs. PhiladelphiaEagles"] <- 3
mydata$scorediff[mydata$matchup=="DetroitLions vs. SeattleSeahawks"] <- 4
mydata$scorediff[mydata$matchup=="GreenBayPackers vs. ArizonaCardinals"] <- 14
mydata$scorediff[mydata$matchup=="GreenBayPackers vs. HoustonTexans"] <- 18
mydata$scorediff[mydata$matchup=="GreenBayPackers vs. IndianapolisColts"] <- 3
mydata$scorediff[mydata$matchup=="GreenBayPackers vs. JacksonvilleJaguars"] <- 9
mydata$scorediff[mydata$matchup=="GreenBayPackers vs. St.LouisRams"] <- 10
mydata$scorediff[mydata$matchup=="HoustonTexans vs. BaltimoreRavens"] <- 30
mydata$scorediff[mydata$matchup=="HoustonTexans vs. BuffaloBills"] <- 12
mydata$scorediff[mydata$matchup=="HoustonTexans vs. GreenBayPackers"] <- 18
mydata$scorediff[mydata$matchup=="HoustonTexans vs. NewYorkJets"] <- 6
mydata$scorediff[mydata$matchup=="IndianapolisColts vs. ClevelandBrowns"] <- 4
mydata$scorediff[mydata$matchup=="IndianapolisColts vs. GreenBayPackers"] <- 3
mydata$scorediff[mydata$matchup=="IndianapolisColts vs. MiamiDolphins"] <- 3
mydata$scorediff[mydata$matchup=="IndianapolisColts vs. NewYorkJets"] <- 26
mydata$scorediff[mydata$matchup=="IndianapolisColts vs. TennesseeTitans"] <- 6
mydata$scorediff[mydata$matchup=="JacksonvilleJaguars vs. ChicagoBears"] <- 38
mydata$scorediff[mydata$matchup=="JacksonvilleJaguars vs. DetroitLions"] <- 17
mydata$scorediff[mydata$matchup=="JacksonvilleJaguars vs. GreenBayPackers"] <- 9
mydata$scorediff[mydata$matchup=="JacksonvilleJaguars vs. OaklandRaiders"] <- 3
mydata$scorediff[mydata$matchup=="KansasCityChiefs vs. BaltimoreRavens"] <- 3
mydata$scorediff[mydata$matchup=="KansasCityChiefs vs. OaklandRaiders"] <- 10
mydata$scorediff[mydata$matchup=="KansasCityChiefs vs. PittsburghSteelers"] <- 3
mydata$scorediff[mydata$matchup=="KansasCityChiefs vs. SanDiegoChargers"] <- 18
mydata$scorediff[mydata$matchup=="KansasCityChiefs vs. TampaBayBuccaneers"] <- 28
mydata$scorediff[mydata$matchup=="MiamiDolphins vs. CincinnatiBengals"] <- 4
mydata$scorediff[mydata$matchup=="MiamiDolphins vs. IndianapolisColts"] <- 3
mydata$scorediff[mydata$matchup=="MiamiDolphins vs. NewYorkJets"] <- 21
mydata$scorediff[mydata$matchup=="MiamiDolphins vs. St.LouisRams"] <- 3
mydata$scorediff[mydata$matchup=="MinnesotaVikings vs. ArizonaCardinals"] <- 7
mydata$scorediff[mydata$matchup=="MinnesotaVikings vs. SeattleSeahawks"] <- 10
mydata$scorediff[mydata$matchup=="MinnesotaVikings vs. TampaBayBuccaneers"] <- 19
mydata$scorediff[mydata$matchup=="MinnesotaVikings vs. TennesseeTitans"] <- 23
mydata$scorediff[mydata$matchup=="MinnesotaVikings vs. WashingtonRedskins"] <- 12
mydata$scorediff[mydata$matchup=="NewEnglandPatriots vs. DenverBroncos"] <- 10
mydata$scorediff[mydata$matchup=="NewEnglandPatriots vs. NewYorkJets"] <- 3
mydata$scorediff[mydata$matchup=="NewEnglandPatriots vs. SeattleSeahawks"] <- 1
mydata$scorediff[mydata$matchup=="NewEnglandPatriots vs. St.LouisRams"] <- 38
mydata$scorediff[mydata$matchup=="NewOrleansSaints vs. DenverBroncos"] <- 20
mydata$scorediff[mydata$matchup=="NewOrleansSaints vs. SanDiegoChargers"] <- 7
mydata$scorediff[mydata$matchup=="NewOrleansSaints vs. TampaBayBuccaneers"] <- 7
mydata$scorediff[mydata$matchup=="NewYorkGiants vs. ClevelandBrowns"] <- 14
mydata$scorediff[mydata$matchup=="NewYorkGiants vs. DallasCowboys"] <- 5
mydata$scorediff[mydata$matchup=="NewYorkGiants vs. PittsburghSteelers"] <- 4
mydata$scorediff[mydata$matchup=="NewYorkGiants vs. SanFrancisco49ers"] <- 23
mydata$scorediff[mydata$matchup=="NewYorkGiants vs. WashingtonRedskins"] <- 4
mydata$scorediff[mydata$matchup=="NewYorkJets vs. HoustonTexans"] <- 6
mydata$scorediff[mydata$matchup=="NewYorkJets vs. IndianapolisColts"] <- 26
mydata$scorediff[mydata$matchup=="NewYorkJets vs. MiamiDolphins"] <- 21
mydata$scorediff[mydata$matchup=="NewYorkJets vs. NewEnglandPatriots"] <- 30
mydata$scorediff[mydata$matchup=="OaklandRaiders vs. AtlantaFalcons"] <- 3
mydata$scorediff[mydata$matchup=="OaklandRaiders vs. JacksonvilleJaguars"] <- 3
mydata$scorediff[mydata$matchup=="OaklandRaiders vs. KansasCityChiefs"] <- 10
mydata$scorediff[mydata$matchup=="OaklandRaiders vs. TampaBayBuccaneers"] <- 10
mydata$scorediff[mydata$matchup=="PhiladelphiaEagles vs. AtlantaFalcons"] <- 13
mydata$scorediff[mydata$matchup=="PhiladelphiaEagles vs. DetroitLions"] <- 3
mydata$scorediff[mydata$matchup=="PhiladelphiaEagles vs. NewOrleansSaints"] <- 15
mydata$scorediff[mydata$matchup=="PhiladelphiaEagles vs. PittsburghSteelers"] <- 2
mydata$scorediff[mydata$matchup=="PittsburghSteelers vs. CincinnatiBengals"] <- 7
mydata$scorediff[mydata$matchup=="PittsburghSteelers vs. NewYorkGiants"] <- 4
mydata$scorediff[mydata$matchup=="PittsburghSteelers vs. PhiladelphiaEagles"] <- 2
mydata$scorediff[mydata$matchup=="PittsburghSteelers vs. TennesseeTitans"] <- 3
mydata$scorediff[mydata$matchup=="PittsburghSteelers vs. WashingtonRedskins"] <- 15
mydata$scorediff[mydata$matchup=="SanDiegoChargers vs. ClevelandBrowns"] <- 1
mydata$scorediff[mydata$matchup=="SanDiegoChargers vs. DenverBroncos"] <- 11
mydata$scorediff[mydata$matchup=="SanDiegoChargers vs. NewOrleansSaints"] <- 7
mydata$scorediff[mydata$matchup=="SanFrancisco49ers vs. ArizonaCardinals"] <- 21
mydata$scorediff[mydata$matchup=="SanFrancisco49ers vs. BuffaloBills"] <- 42
mydata$scorediff[mydata$matchup=="SanFrancisco49ers vs. NewYorkGiants"] <- 23
mydata$scorediff[mydata$matchup=="SanFrancisco49ers vs. SeattleSeahawks"] <- 7
mydata$scorediff[mydata$matchup=="SeattleSeahawks vs. CarolinaPanthers"] <- 4
mydata$scorediff[mydata$matchup=="SeattleSeahawks vs. DetroitLions"] <- 4
mydata$scorediff[mydata$matchup=="SeattleSeahawks vs. NewEnglandPatriots"] <- 1
mydata$scorediff[mydata$matchup=="SeattleSeahawks vs. SanFrancisco49ers"] <- 7
mydata$scorediff[mydata$matchup=="St.LouisRams vs. ArizonaCardinals"] <- 14
mydata$scorediff[mydata$matchup=="St.LouisRams vs. GreenBayPackers"] <- 10
mydata$scorediff[mydata$matchup=="St.LouisRams vs. MiamiDolphins"] <- 3
mydata$scorediff[mydata$matchup=="St.LouisRams vs. NewEnglandPatriots"] <- 38
mydata$scorediff[mydata$matchup=="TampaBayBuccaneers vs. KansasCityChiefs"] <- 28
mydata$scorediff[mydata$matchup=="TampaBayBuccaneers vs. NewOrleansSaints"] <- 7
mydata$scorediff[mydata$matchup=="TennesseeTitans vs. BuffaloBills"] <- 1
mydata$scorediff[mydata$matchup=="TennesseeTitans vs. ChicagoBears"] <- 31
mydata$scorediff[mydata$matchup=="TennesseeTitans vs. IndianapolisColts"] <- 6
mydata$scorediff[mydata$matchup=="TennesseeTitans vs. MinnesotaVikings"] <- 23
mydata$scorediff[mydata$matchup=="TennesseeTitans vs. PittsburghSteelers"] <- 3
mydata$scorediff[mydata$matchup=="WashingtonRedskins vs. AtlantaFalcons"] <- 7
mydata$scorediff[mydata$matchup=="WashingtonRedskins vs. MinnesotaVikings"] <- 12
mydata$scorediff[mydata$matchup=="WashingtonRedskins vs. NewYorkGiants"] <- 4
mydata$scorediff[mydata$matchup=="WashingtonRedskins vs. PittsburghSteelers"] <- 15

mydata$newscore <- 100-mydata$scorediff

mydata$ot[mydata$matchup=="ArizonaCardinals vs. BuffaloBills"] <- 1
mydata$ot[mydata$matchup=="ArizonaCardinals vs. GreenBayPackers"] <- 0
mydata$ot[mydata$matchup=="ArizonaCardinals vs. MinnesotaVikings"] <- 0
mydata$ot[mydata$matchup=="ArizonaCardinals vs. SanFrancisco49ers"] <- 0
mydata$ot[mydata$matchup=="ArizonaCardinals vs. St.LouisRams"] <- 0
mydata$ot[mydata$matchup=="AtlantaFalcons vs. DallasCowboys"] <- 0
mydata$ot[mydata$matchup=="AtlantaFalcons vs. OaklandRaiders"] <- 0
mydata$ot[mydata$matchup=="AtlantaFalcons vs. PhiladelphiaEagles"] <- 0
mydata$ot[mydata$matchup=="AtlantaFalcons vs. WashingtonRedskins"] <- 0
mydata$ot[mydata$matchup=="BaltimoreRavens vs. ClevelandBrowns"] <- 0
mydata$ot[mydata$matchup=="BaltimoreRavens vs. DallasCowboys"] <- 0
mydata$ot[mydata$matchup=="BaltimoreRavens vs. HoustonTexans"] <- 0
mydata$ot[mydata$matchup=="BaltimoreRavens vs. KansasCityChiefs"] <- 0
mydata$ot[mydata$matchup=="BuffaloBills vs. ArizonaCardinals"] <- 1
mydata$ot[mydata$matchup=="BuffaloBills vs. HoustonTexans"] <- 0
mydata$ot[mydata$matchup=="BuffaloBills vs. SanFrancisco49ers"] <- 0
mydata$ot[mydata$matchup=="BuffaloBills vs. TennesseeTitans"] <- 0
mydata$ot[mydata$matchup=="CarolinaPanthers vs. ChicagoBears"] <- 0
mydata$ot[mydata$matchup=="CarolinaPanthers vs. DallasCowboys"] <- 0
mydata$ot[mydata$matchup=="CarolinaPanthers vs. SeattleSeahawks"] <- 0
mydata$ot[mydata$matchup=="ChicagoBears vs. CarolinaPanthers"] <- 0
mydata$ot[mydata$matchup=="ChicagoBears vs. DetroitLions"] <- 0
mydata$ot[mydata$matchup=="ChicagoBears vs. JacksonvilleJaguars"] <- 0
mydata$ot[mydata$matchup=="ChicagoBears vs. TennesseeTitans"] <- 0
mydata$ot[mydata$matchup=="CincinnatiBengals vs. ClevelandBrowns"] <- 0
mydata$ot[mydata$matchup=="CincinnatiBengals vs. MiamiDolphins"] <- 0
mydata$ot[mydata$matchup=="CincinnatiBengals vs. PittsburghSteelers"] <- 0
mydata$ot[mydata$matchup=="ClevelandBrowns vs. BaltimoreRavens"] <- 0
mydata$ot[mydata$matchup=="ClevelandBrowns vs. CincinnatiBengals"] <- 0
mydata$ot[mydata$matchup=="ClevelandBrowns vs. IndianapolisColts"] <- 0
mydata$ot[mydata$matchup=="ClevelandBrowns vs. NewYorkGiants"] <- 0
mydata$ot[mydata$matchup=="ClevelandBrowns vs. SanDiegoChargers"] <- 0
mydata$ot[mydata$matchup=="DallasCowboys vs. BaltimoreRavens"] <- 0
mydata$ot[mydata$matchup=="DallasCowboys vs. CarolinaPanthers"] <- 0
mydata$ot[mydata$matchup=="DallasCowboys vs. NewYorkGiants"] <- 0
mydata$ot[mydata$matchup=="DenverBroncos vs. CarolinaPanthers"] <- 0
mydata$ot[mydata$matchup=="DenverBroncos vs. CincinnatiBengals"] <- 0
mydata$ot[mydata$matchup=="DenverBroncos vs. NewEnglandPatriots"] <- 0
mydata$ot[mydata$matchup=="DenverBroncos vs. NewOrleansSaints"] <- 0
mydata$ot[mydata$matchup=="DenverBroncos vs. SanDiegoChargers"] <- 0
mydata$ot[mydata$matchup=="DetroitLions vs. ChicagoBears"] <- 0
mydata$ot[mydata$matchup=="DetroitLions vs. JacksonvilleJaguars"] <- 0
mydata$ot[mydata$matchup=="DetroitLions vs. PhiladelphiaEagles"] <- 1
mydata$ot[mydata$matchup=="DetroitLions vs. SeattleSeahawks"] <- 0
mydata$ot[mydata$matchup=="GreenBayPackers vs. ArizonaCardinals"] <- 0
mydata$ot[mydata$matchup=="GreenBayPackers vs. HoustonTexans"] <- 0
mydata$ot[mydata$matchup=="GreenBayPackers vs. IndianapolisColts"] <- 0
mydata$ot[mydata$matchup=="GreenBayPackers vs. JacksonvilleJaguars"] <- 0
mydata$ot[mydata$matchup=="GreenBayPackers vs. St.LouisRams"] <- 0
mydata$ot[mydata$matchup=="HoustonTexans vs. BaltimoreRavens"] <- 0
mydata$ot[mydata$matchup=="HoustonTexans vs. BuffaloBills"] <- 0
mydata$ot[mydata$matchup=="HoustonTexans vs. GreenBayPackers"] <- 0
mydata$ot[mydata$matchup=="HoustonTexans vs. NewYorkJets"] <- 0
mydata$ot[mydata$matchup=="IndianapolisColts vs. ClevelandBrowns"] <- 0
mydata$ot[mydata$matchup=="IndianapolisColts vs. GreenBayPackers"] <- 0
mydata$ot[mydata$matchup=="IndianapolisColts vs. MiamiDolphins"] <- 0
mydata$ot[mydata$matchup=="IndianapolisColts vs. NewYorkJets"] <- 0
mydata$ot[mydata$matchup=="IndianapolisColts vs. TennesseeTitans"] <- 1
mydata$ot[mydata$matchup=="JacksonvilleJaguars vs. ChicagoBears"] <- 0
mydata$ot[mydata$matchup=="JacksonvilleJaguars vs. DetroitLions"] <- 0
mydata$ot[mydata$matchup=="JacksonvilleJaguars vs. GreenBayPackers"] <- 0
mydata$ot[mydata$matchup=="JacksonvilleJaguars vs. OaklandRaiders"] <- 1
mydata$ot[mydata$matchup=="KansasCityChiefs vs. BaltimoreRavens"] <- 0
mydata$ot[mydata$matchup=="KansasCityChiefs vs. OaklandRaiders"] <- 0
mydata$ot[mydata$matchup=="KansasCityChiefs vs. PittsburghSteelers"] <- 1
mydata$ot[mydata$matchup=="KansasCityChiefs vs. SanDiegoChargers"] <- 0
mydata$ot[mydata$matchup=="KansasCityChiefs vs. TampaBayBuccaneers"] <- 0
mydata$ot[mydata$matchup=="MiamiDolphins vs. CincinnatiBengals"] <- 0
mydata$ot[mydata$matchup=="MiamiDolphins vs. IndianapolisColts"] <- 0
mydata$ot[mydata$matchup=="MiamiDolphins vs. NewYorkJets"] <- 0
mydata$ot[mydata$matchup=="MiamiDolphins vs. St.LouisRams"] <- 0
mydata$ot[mydata$matchup=="MinnesotaVikings vs. ArizonaCardinals"] <- 0
mydata$ot[mydata$matchup=="MinnesotaVikings vs. SeattleSeahawks"] <- 0
mydata$ot[mydata$matchup=="MinnesotaVikings vs. TampaBayBuccaneers"] <- 0
mydata$ot[mydata$matchup=="MinnesotaVikings vs. TennesseeTitans"] <- 0
mydata$ot[mydata$matchup=="MinnesotaVikings vs. WashingtonRedskins"] <- 0
mydata$ot[mydata$matchup=="NewEnglandPatriots vs. DenverBroncos"] <- 0
mydata$ot[mydata$matchup=="NewEnglandPatriots vs. NewYorkJets"] <- 1
mydata$ot[mydata$matchup=="NewEnglandPatriots vs. SeattleSeahawks"] <- 0
mydata$ot[mydata$matchup=="NewEnglandPatriots vs. St.LouisRams"] <- 0
mydata$ot[mydata$matchup=="NewOrleansSaints vs. DenverBroncos"] <- 0
mydata$ot[mydata$matchup=="NewOrleansSaints vs. SanDiegoChargers"] <- 0
mydata$ot[mydata$matchup=="NewOrleansSaints vs. TampaBayBuccaneers"] <- 0
mydata$ot[mydata$matchup=="NewYorkGiants vs. ClevelandBrowns"] <- 0
mydata$ot[mydata$matchup=="NewYorkGiants vs. DallasCowboys"] <- 0
mydata$ot[mydata$matchup=="NewYorkGiants vs. PittsburghSteelers"] <- 0
mydata$ot[mydata$matchup=="NewYorkGiants vs. SanFrancisco49ers"] <- 0
mydata$ot[mydata$matchup=="NewYorkGiants vs. WashingtonRedskins"] <- 0
mydata$ot[mydata$matchup=="NewYorkJets vs. HoustonTexans"] <- 0
mydata$ot[mydata$matchup=="NewYorkJets vs. IndianapolisColts"] <- 0
mydata$ot[mydata$matchup=="NewYorkJets vs. MiamiDolphins"] <- 0
mydata$ot[mydata$matchup=="NewYorkJets vs. NewEnglandPatriots"] <- 0
mydata$ot[mydata$matchup=="OaklandRaiders vs. AtlantaFalcons"] <- 0
mydata$ot[mydata$matchup=="OaklandRaiders vs. JacksonvilleJaguars"] <- 1
mydata$ot[mydata$matchup=="OaklandRaiders vs. KansasCityChiefs"] <- 0
mydata$ot[mydata$matchup=="OaklandRaiders vs. TampaBayBuccaneers"] <- 0
mydata$ot[mydata$matchup=="PhiladelphiaEagles vs. AtlantaFalcons"] <- 0
mydata$ot[mydata$matchup=="PhiladelphiaEagles vs. DetroitLions"] <- 1
mydata$ot[mydata$matchup=="PhiladelphiaEagles vs. NewOrleansSaints"] <- 0
mydata$ot[mydata$matchup=="PhiladelphiaEagles vs. PittsburghSteelers"] <- 0
mydata$ot[mydata$matchup=="PittsburghSteelers vs. CincinnatiBengals"] <- 0
mydata$ot[mydata$matchup=="PittsburghSteelers vs. NewYorkGiants"] <- 0
mydata$ot[mydata$matchup=="PittsburghSteelers vs. PhiladelphiaEagles"] <- 0
mydata$ot[mydata$matchup=="PittsburghSteelers vs. TennesseeTitans"] <- 0
mydata$ot[mydata$matchup=="PittsburghSteelers vs. WashingtonRedskins"] <- 0
mydata$ot[mydata$matchup=="SanDiegoChargers vs. ClevelandBrowns"] <- 0
mydata$ot[mydata$matchup=="SanDiegoChargers vs. DenverBroncos"] <- 0
mydata$ot[mydata$matchup=="SanDiegoChargers vs. NewOrleansSaints"] <- 0
mydata$ot[mydata$matchup=="SanFrancisco49ers vs. ArizonaCardinals"] <- 0
mydata$ot[mydata$matchup=="SanFrancisco49ers vs. BuffaloBills"] <- 0
mydata$ot[mydata$matchup=="SanFrancisco49ers vs. NewYorkGiants"] <- 0
mydata$ot[mydata$matchup=="SanFrancisco49ers vs. SeattleSeahawks"] <- 0
mydata$ot[mydata$matchup=="SeattleSeahawks vs. CarolinaPanthers"] <- 0
mydata$ot[mydata$matchup=="SeattleSeahawks vs. DetroitLions"] <- 0
mydata$ot[mydata$matchup=="SeattleSeahawks vs. NewEnglandPatriots"] <- 0
mydata$ot[mydata$matchup=="SeattleSeahawks vs. SanFrancisco49ers"] <- 0
mydata$ot[mydata$matchup=="St.LouisRams vs. ArizonaCardinals"] <- 0
mydata$ot[mydata$matchup=="St.LouisRams vs. GreenBayPackers"] <- 0
mydata$ot[mydata$matchup=="St.LouisRams vs. MiamiDolphins"] <- 0
mydata$ot[mydata$matchup=="St.LouisRams vs. NewEnglandPatriots"] <- 0
mydata$ot[mydata$matchup=="TampaBayBuccaneers vs. KansasCityChiefs"] <- 0
mydata$ot[mydata$matchup=="TampaBayBuccaneers vs. NewOrleansSaints"] <- 0
mydata$ot[mydata$matchup=="TennesseeTitans vs. BuffaloBills"] <- 0
mydata$ot[mydata$matchup=="TennesseeTitans vs. ChicagoBears"] <- 0
mydata$ot[mydata$matchup=="TennesseeTitans vs. IndianapolisColts"] <- 1
mydata$ot[mydata$matchup=="TennesseeTitans vs. MinnesotaVikings"] <- 0
mydata$ot[mydata$matchup=="TennesseeTitans vs. PittsburghSteelers"] <- 0
mydata$ot[mydata$matchup=="WashingtonRedskins vs. AtlantaFalcons"] <- 0
mydata$ot[mydata$matchup=="WashingtonRedskins vs. MinnesotaVikings"] <- 0
mydata$ot[mydata$matchup=="WashingtonRedskins vs. NewYorkGiants"] <- 0
mydata$ot[mydata$matchup=="WashingtonRedskins vs. PittsburghSteelers"] <- 0

mydata$closedich[mydata$scorediff<=3] <- 1
mydata$closedich[mydata$scorediff>3] <- 0

#####################
# Running the Model #
#####################
glm1 <- glm(closewin ~ newscore + ot + fan + favewin + NFLattn + sportstalk + age + edu + female + whitenonhisp, family=binomial, data=mydata)
glm2 <- glm(closewin ~ newscore + ot + newscore*ot + fan + favewin + NFLattn + sportstalk + age + edu + female + whitenonhisp, family=binomial, data=mydata)

polr.likely <- polr(likelytowatch ~ newscore + ot + closewin + fan + NFLattn + sportstalk + age + edu + female + white + hispanicdummy, na.action=na.omit, data=mydata, method=c("logistic"))
glm.likely <- glm(likelydich ~ newscore + ot + newscore*ot + closewin + fan + NFLattn + sportstalk + age + edu + female + white + hispanicdummy, family=binomial, data=mydata)
stargazer(polr.likely, glm.likely)

glm.close.ot <- glm(favewinsclosegame ~ closedich + ot + likelytowatch + NFLattn + sportstalk + age + edu + female + whitenonhisp, family=binomial, data=mydata, weights=V102)
glm.scorediff<- glm(favewinsclosegame ~ scorediff + likelytowatch + NFLattn + sportstalk + age + edu + female + whitenonhisp, family=binomial, data=mydata, weights=V102)

stargazer(glm1, glm2)

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

