# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#
# File: 004_prepForAnalysis.R 											#
# -----------------------  												#
#																	   	#
# This script reads in the wave 1 and wave 2 data and does some final 	#
# cleaning in order to output finalized datasets for the observational 	#
# and experimental villages to be used in the impact analysis. 			#
#																	   	#
# The major purposes of this script are: 								#
# 1. CLEANS AND CREATES A FEW ADDITIONAL VARIABLES					   	#
# 2. CREATES A MERGED DATASET OF WAVE 1 AND WAVE 2 DATA (LONG FORMAT) 	#
# 3. DESCRIBES AND TRIMS KEY INDICATORS 								#
# 4. CREATES A VILLAGE-LEVEL DATASET WITH KEY INDICATORS 				#
# 5. EXPORTS TWO CSVs CONTAINING THE FINALIZED OBSERVATIONAL AND 		#
#  EXPERIMENTAL DATASETS. 												#
#																	   	#
# NOTES: 																#
# ...																   	#
#																	   	#
#																	   	#
# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#

library(foreign)
library(reshape)
library(dplyr)
library(plyr)
library(plm)

# ---------------------------------------------------------------------	#
# Sets the working directory and reads in the wave 1 and wave 2 data:

# Path for Bobbie:
bmPath <- "/Users/Dell/Dropbox/Ghana (2)" 
bmPath2 <- "/Users/Dell/Desktop/Hiscox Research/Ghana/Bobbie's code"
globalPath <- bmPath2 # (for your own working directory, replace bmPath.)
setwd(globalPath)

# reads in the wave 1 farmer data.
setwd(paste0(bmPath,"/data/1stwave/Tousedata", space=""))
wave1Data <- read.dta("Farmer_Survey_Cleaned_with_key_indicators.dta")
dim(wave1Data); head(wave1Data[,1:10])

# reads in the wave 2 farmer data.
setwd(paste0(bmPath,"/data/2ndwave/Code/cleanedDatasets", space=""))
wave2Data <- read.dta("Farmer_Survey_withKeyIndicators_wave2.dta")
dim(wave2Data); head(wave2Data[,1:10])


# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#
#			1. CLEANS AND CREATES A FEW ADDITIONAL VARIABLES		   	#
# 																		#
# This section (a) cleans a few variables that were not cleaned in the 	#
# preceding files and (b) creates a few additional variables for the 	#
# impact analysis. 														#
# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#

# ---------------------------------------- #
# A FEW FIXES TO WAVE 1 DATA CLEANING
# This block of code makes a few fixes to the wave 1 data cleaning. These fixes 
# should later be moved to the appropriate stata files for waves 1 (and 2 where 
# appropriate). 
# ---------------------------------------- #

# --------------
# Temporary fix to ggherb and ggorg in wave 1.
#table(wave1Data$d35yesno_3, exclude=NULL)
#table(wave1Data$d35yesno_5, exclude=NULL)
# remove ggherb and ggorg:
wave1Data <- wave1Data[,!names(wave1Data) %in% c("ggherb", "ggorg")]

# rename d35yesno_3 and d35yesno_5 to ggherb and ggorg
wave1Data <- rename(wave1Data, c("d35yesno_3"="ggherb", "d35yesno_5"="ggorg"))

# --------------
# Temporary fix to n7children in wave 1.

# recode "" and "-998" to NA
wave1Data$n7children[wave1Data$n7children=="" | wave1Data$n7children=="-998"] <- NA

# recode 60 to NA, since this is an unrealistic value.
wave1Data$n7children[wave1Data$n7children=="60"] <- NA

# convert to numeric
wave1Data$n7children <- as.numeric(wave1Data$n7children)

# rename n7children to n7_children for consistency with wave 2.
wave1Data <- rename(wave1Data, c("n7children"="n7_children"))


# --------------
# Temporary fix to n8yesno in wave 1.

wave1Data$n8yesno[wave1Data$n8yesno=="" | wave1Data$n8yesno=="-888" | wave1Data$n8yesno=="-998" | wave1Data$n8yesno=="-999"] <- NA

# convert to numeric
wave1Data$n8yesno <- as.numeric(wave1Data$n8yesno)

# rename n8yesno to n8_future for consistency with wave 2.
wave1Data <- rename(wave1Data, c("n8yesno"="n8_future"))

# --------------
# Temporary fix to n9yesno in wave 1.

wave1Data$n9yesno[wave1Data$n9yesno=="" | wave1Data$n9yesno=="-998" | wave1Data$n9yesno=="-999"] <- NA

# convert to numeric
wave1Data$n9yesno <- as.numeric(wave1Data$n9yesno)

# rename n9yesno to n9_recommend for consistency with wave 2.
wave1Data <- rename(wave1Data, c("n9yesno"="n9_recommend"))


# --------------
# Temporary fix to n10 in wave 1.

# convert low income
wave1Data$n10low_income[wave1Data$n10low_income==""] <- "0"
wave1Data$n10low_income <- as.numeric(wave1Data$n10low_income)

# convert low status
wave1Data$n10low_status[wave1Data$n10low_status==""] <- "0"
wave1Data$n10low_status <- as.numeric(wave1Data$n10low_status)

# convert work too hard
wave1Data$n10work_too_hard[wave1Data$n10work_too_hard==""] <- "0"
wave1Data$n10work_too_hard <- as.numeric(wave1Data$n10work_too_hard)

# convert better opportunities
wave1Data$n10better_opportunities_in_other[wave1Data$n10better_opportunities_in_other=="" | wave1Data$n10better_opportunities_in_other=="4"] <- "0"
wave1Data$n10better_opportunities_in_other <- as.numeric(wave1Data$n10better_opportunities_in_other)

# convert not enough land
wave1Data$n10not_enough_land[wave1Data$n10not_enough_land==""] <- "0"
wave1Data$n10not_enough_land <- as.numeric(wave1Data$n10not_enough_land)

# convert another reason
wave1Data$n10another_reason[wave1Data$n10another_reason=="" | wave1Data$n10another_reason=="-888"] <- "0"
wave1Data$n10another_reason <- as.numeric(wave1Data$n10another_reason)

# rename n10 variables from wave 2 to match wave 1 names.
wave2Data <- rename(wave2Data, c("n10whynot_1"="n10low_income", "n10whynot_2"="n10low_status", "n10whynot_3"="n10work_too_hard", "n10whynot_4"="n10better_opportunities_in_other", "n10whynot_5"="n10not_enough_land", "n10whynot_6"="n10another_reason"))


# --------------
# Temporary fix to ggharvestcocoaKG in wave 1.

# recodes any units to NA if code==NA:
codeVars <- c("c26code_1", "c26code_2", "c26code_3", "c26code_4", "c26code_5")
unitVars <- c("c26unit_1", "c26unit_2", "c26unit_3", "c26unit_4", "c26unit_5")
for (i in 1:length(codeVars)){
	wave1Data[ is.na(wave1Data[,codeVars[i]]), unitVars[i]] <- NA
}
wave1Data$ggharvestcocoaKG <- rowSums(wave1Data[,unitVars], na.rm=T)


# --------------
# Temporary fix to ggfsizeacres in wave 1.

codeVars <- c("c18code_1", "c18code_2", "c18code_3", "c18code_4", "c18code_5")
unitVars <- c("c18unit_1", "c18unit_2", "c18unit_3", "c18unit_4", "c18unit_5")
for (i in 1:length(codeVars)){
	# print(table(is.na(wave1Data[,codeVars[i]]) & !is.na(wave1Data[,unitVars[i]])))
	wave1Data[ is.na(wave1Data[,codeVars[i]]), unitVars[i]] <- NA
}
wave1Data$ggfsizeacres <- rowSums(wave1Data[,unitVars], na.rm=T)

# --------------
# Temporary fix to ggfsizecocoaacres in wave 1.

fraction1 <- c("c25fraction1_1", "c25fraction1_2", "c25fraction1_3", "c25fraction1_4", "c25fraction1_5")
fraction2 <- c("c25fraction2_1", "c25fraction2_2", "c25fraction2_3", "c25fraction2_4", "c25fraction2_5")
fracNumeric <- c("c25fracnumeric_1", "c25fracnumeric_2", "c25fracnumeric_3", "c25fracnumeric_4", "c25fracnumeric_5")
cocoaArea <- c("cccocoarea_1", "cccocoarea_2", "cccocoarea_3", "cccocoarea_4", "cccocoarea_5")

# Fixes c25* variables: replaces 0 divided by 0 with 0, as opposed to NA.
# Fixes cocoaarea* variables using the revised c25* and c18* variables
for (i in 1:length(fraction1)){
	wave1Data[ wave1Data[, fraction1[i]]==0 & wave1Data[, fraction2[i]]==0 & !is.na(wave1Data[, fraction1[i]]) & !is.na(wave1Data[, fraction2[i]]), fracNumeric[i]] <- 0
	wave1Data[,cocoaArea[i]] <- wave1Data[,unitVars[i]] * wave1Data[,fracNumeric[i]]
}

# updates ggfsizecocoaacres with the revised variables:
wave1Data$ggfsizecocoaacres <- rowSums(wave1Data[,cocoaArea], na.rm=T)

# NOTE TO SELF: In wave2Data, ggfsizecocoaacres is getting assigned NA even if some 
# cocoaArea columns have 0. I recode these rows to 0. For consistency across waves, if 
# all cocoaArea rows are NA, then ggfsizecocoaacres gets NA. 
wave2Data$ggfsizecocoaacres[is.na(wave2Data$ggfsizecocoaacres)] <- 0
wave1Data$ggfsizecocoaacres[rowSums(is.na(wave1Data[,cocoaArea]))==5] <- NA
wave2Data$ggfsizecocoaacres[is.na(wave2Data$cccocoarea_1)] <- NA

# --------------
# Temporary fix to ggyieldall_cocarea and ggyieldall_allarea in wave 1.

wave1Data$ggyieldall_cocarea <- wave1Data$ggharvestcocoaKG/(wave1Data$ggfsizecocoaacres*(1/2.47105381))
wave1Data$ggyieldall_allarea <- wave1Data$ggharvestcocoaKG/(wave1Data$ggfsizeacres*(1/2.47105381))
wave2Data$ggyieldall_cocarea <- wave2Data$ggharvestcocoaKG/(wave2Data$ggfsizecocoaacres*(1/2.47105381))
wave2Data$ggyieldall_allarea <- wave2Data$ggharvestcocoaKG/(wave2Data$ggfsizeacres*(1/2.47105381))


# replace NaN and Inf with 0 (as they both result from one or both of ggharvestcocoaKG 
# and ggfsizecocoaacres being 0):
wave1Data$ggyieldall_cocarea[wave1Data$ggyieldall_cocarea==Inf & !is.na(wave1Data$ggyieldall_cocarea)] <- 0
wave1Data$ggyieldall_cocarea[is.nan(wave1Data$ggyieldall_cocarea)] <- 0

wave1Data$ggyieldall_allarea[wave1Data$ggyieldall_allarea==Inf & !is.na(wave1Data$ggyieldall_allarea)] <- 0
wave1Data$ggyieldall_allarea[is.nan(wave1Data$ggyieldall_allarea)] <- 0

wave2Data$ggyieldall_cocarea[wave2Data$ggyieldall_cocarea==Inf & !is.na(wave2Data$ggyieldall_cocarea)] <- 0
wave2Data$ggyieldall_cocarea[is.nan(wave2Data$ggyieldall_cocarea)] <- 0

wave2Data$ggyieldall_allarea[wave2Data$ggyieldall_allarea==Inf & !is.na(wave2Data$ggyieldall_allarea)] <- 0
wave2Data$ggyieldall_allarea[is.nan(wave2Data$ggyieldall_allarea)] <- 0


# NOTE TO SELF: in contrast to the stata code (where they were recoded to NA), there are 
# now observations for ggyield* and ggfsize* equal to 0. May want to revisit this later, 
# but for now it seems most appropriate to leave them as 0. 

# NOTE TO SELF: a fair bit has been changed with this farm size and yield variables. May 
# want to double-check this code again later. 

# --------------
# Temporary fix to ggfractionsold in wave 1 (because ggharvestcocoaKG was altered slightly).
wave1Data$ggfractionsold <- wave1Data$ggsoldcocoaKG / wave1Data$ggharvestcocoaKG
wave1Data$ggfractionsold[wave1Data$ggfractionsold==Inf & !is.na(wave1Data$ggfractionsold)] <- NA

# NOTE TO SELF: there are values where ggsoldcocoaKG > 0 and ggharvestcocoaKG==0 in the 
# wave 1 data. These are all treated as NA, but this just highlights the need to come back 
# to these variables and sort out some of these oddities.

# Wave 1 data has fraction sold > 1. These are recoded to NA
wave1Data$ggfractionsold[wave1Data$ggfractionsold>1 & !is.na(wave1Data$ggfractionsold)] <- NA

# --------------
# Temporary fix to ggincomeothernonagro and ggincometotnoncrop in wave 1.

incomeVars <- c("ggincomecocoa","ggincomeothcrops","ggincomecropstot","ggincomecropstotcap","ggincomeotheragro","ggincomeothernonagro","ggincomeremit","ggincometotnoncrop","ggincometotal","ggincometotalcap")

# Shows that ggincomeothernonagro and ggincometotnoncrop have negative values in wave 1. 
summary(wave1Data[,incomeVars])
summary(wave2Data[,incomeVars])

# removes negative values from e2cedis_3, which is what is causing the negative 
# values in ggincomeothernonagro.
wave1Data$e2cedis_3[wave1Data$e2cedis_3 < 0] <- NA

# re-computes ggincomeothernonagro
e2cedis <- c("e2cedis_1", "e2cedis_2", "e2cedis_3", "e2cedis_4", "e2cedis_5", "e2cedis_6")
wave1Data$ggincomeothernonagro <- rowSums(wave1Data[,e2cedis], na.rm=TRUE)

# re-computes ggincometotnoncrop, which is the sum of ggincomeothernonagro, ggincomeotheragro, and ggincomeremit
wave1Data$ggincometotnoncrop <- rowSums(wave1Data[,c("ggincomeothernonagro", "ggincomeotheragro", "ggincomeremit")], na.rm=TRUE)


# ---------------------------------------- #
# Computes the average survey length for wave 2
# ---------------------------------------- #

# NOTE TO SELF: there are four seemingly relevant variables here: l_infosheet_starttime, end_time, start, and end. The variable start is mostly NA and about half of the variable end is NA as well. Hence, I use l_infosheet_starttime and end_time to compute survey length (which appears to be correct form looking at the .xml file).

# converts start and end time from character to time variables.
wave2Data$l_infosheet_starttime <- as.POSIXct(wave2Data$l_infosheet_starttime, tz="UTC", format="%I:%M:%S %p")
wave2Data$end_time <- as.POSIXct(wave2Data$end_time, tz="UTC", format="%I:%M:%S %p")

# compues the difference between start and end times in hours.
wave2Data$surveyLength <- as.numeric(difftime(wave2Data$end_time, wave2Data$l_infosheet_starttime, tz="UTC", units="hours"))

# NOTE TO SELF: ALMOST 18% of values are below zero. In addition, 14% are below -14 and about 3% are greater than 10. Looking at the raw data below, it seems that this is because of enumerator issues. Due to the difficulty in figuring out exactly the enumerators did wrong, I simply drop all values below 0 and all values above the 95th percentile (equal to 3.595).
# cbind(wave2Data$surveyLength, wave2Data$l_infosheet_starttime, wave2Data$end_time)
# quantile(wave2Data$surveyLength, na.rm=T, seq(0,1,0.01))
wave2Data$surveyLength[wave2Data$surveyLength>quantile(wave2Data$surveyLength,.96, na.rm=TRUE)]<-NA
wave2Data$surveyLength[wave2Data$surveyLength<0]<-NA

summary(wave2Data$surveyLength)
sd(wave2Data$surveyLength, na.rm=TRUE)

# plots the distribution of survey length for wave 2.
hist(wave2Data$surveyLength, breaks=100, col="gray", main="Survey Length (hours), Wave 2", freq=TRUE, ylab="Frequency", xlab="")
abline(v=mean(wave2Data$surveyLength, na.rm=TRUE), col="red", lwd=2)
text(mean(wave2Data$surveyLength, na.rm=TRUE),70, labels="Mean", pos=4, offset=0.5, cex=1, col="red")


# ---------------------------------------- #
# Creates variables relating to farm workers
# ---------------------------------------- #

# TO BE COMPLETED... #


# ---------------------------------------- #
# Creates variables relating to organization membership
# ---------------------------------------- #

# Creates a variable for number of organized group memberships.
wave1Data$ggOrgGroupNum <- wave1Data$k2group
wave1Data$ggOrgGroupNum[is.na(wave1Data$ggOrgGroupNum)] <- 0

wave2Data$ggOrgGroupNum <- wave2Data$k2_number
wave2Data$ggOrgGroupNum[is.na(wave2Data$ggOrgGroupNum)] <- 0

# table(wave2Data$ggOrgGroupNum, wave2Data$k2_number, exclude=NULL)

# Creates a variable for whether farmer has a leadership position in the most important farmer org
# they are a part of.
wave1Data$ggOrgLeader <- wave1Data$k6yesno
wave1Data$ggOrgLeader[is.na(wave1Data$ggOrgLeader)] <- 0

wave2Data$ggOrgLeader <- wave2Data$k6_leader
wave2Data$ggOrgLeader[is.na(wave2Data$ggOrgLeader)] <- 0

# Creates a variable for whether farmer is a member of a farm group or cooperative.
wave1Data$ggFarmGroupOrCoop <- wave1Data$k3farm_group_or_coop
wave1Data$ggFarmGroupOrCoop[wave1Data$ggFarmGroupOrCoop==2] <- 1 # 10 values were coded as 2 because of how k3farm_group_or_coop was cleaned in wave 1.
 
wave2Data$ggFarmGroupOrCoop <- wave2Data$k3farm_group_or_coop


# ---------------------------------------- #
# Creates variables relating to farmer training
# ---------------------------------------- #

# Creates a variable for total number of trainings received
wave1Data$ggTrainingNum <- rowSums(wave1Data[,c("k17number_1", "k17number_2", "k17number_3", "k17number_4", "k17number_5", "k17number_6", "k17number_7")], na.rm=T)

wave2Data$ggTrainingNum <- rowSums(wave2Data[,c("k17_cocobod", "k17_mfa", "k17_lbc", "k17_coop", "k17_ngo", "k17_farmers", "k17_other")], na.rm=T)


# Creates 8 variables, one for whether each of 8 different types of training was received. 

# TO BE COMPLETED ... # 


# ---------------------------------------- #
# Creates variables relating to farmer finances
# ---------------------------------------- #

# Creates a variable for whether farmer has a susu account.
wave1Data$j2yesno[wave1Data$j2yesno=="****"] <- NA
wave1Data$j2yesno[wave1Data$j2yesno==""] <- NA
wave1Data$j2yesno <- as.numeric(wave1Data$j2yesno)

wave1Data$ggsusu <- wave1Data$j2yesno
wave1Data$ggsusu[is.na(wave1Data$ggsusu)] <- 0

wave2Data$ggsusu <- wave2Data$j2_susu
wave2Data$ggsusu[is.na(wave2Data$ggsusu)] <- 0

# Creates a variable for whether spouse has an account
wave1Data$j3yesno[wave1Data$j3yesno=="" | wave1Data$j3yesno=="-888" | wave1Data$j3yesno=="-999" | wave1Data$j3yesno=="*"] <- NA
# recodes 3 and 2 to 0. 
wave1Data$j3yesno[wave1Data$j3yesno=="3" | wave1Data$j3yesno=="2"] <- 0
wave1Data$j3yesno <- as.numeric(wave1Data$j3yesno)

wave1Data$ggSpouseAccount <- wave1Data$j3yesno
wave2Data$ggSpouseAccount <- wave2Data$j3_spouseaccount

# Creates a variable for size of largest loan received.
wave1Data$ggLargestLoan <- wave1Data$j6cedis
wave1Data$ggLargestLoan[is.na(wave1Data$ggLargestLoan)] <- 0

wave2Data$ggLargestLoan <- wave2Data$j6_largestloan
wave2Data$ggLargestLoan[is.na(wave2Data$ggLargestLoan)] <- 0



# ---------------------------------------- #
# CREATES CHILD LABOR KEY INDICATORS FOR WAVE 1 AND WAVE 2
# This block of code creates the child labor indicators to be used in the analysis. The 
# code is based largely off of Becca's wave 1 code for creating these indicators.
# 
# NOTE TO SELF: scroll down to the bottom of this section for comments on variables
# that still need to be created for child labor.
# ---------------------------------------- #

# NOTE: the minimum age for a child I use here is 5 years old. The maximum is 17 years old.

# ---------------- #
# Creates dummy variables for children of different age groups.
# --------- #

# NOTE TO SELF: In Becca's code, at one point a child is defined as someone under the age of 17, not including 17. Yet, the ILO categories for child labor are for ages 5-17 inclusive.

# age variables over which to iterate.
ageVars <- c("age_01", "age_02", "age_03", "age_04", "age_05", "age_06", "age_07", "age_08", "age_09", "age_10", "age_11", "age_12", "age_13", "age_14", "age_15")

# ----------------- #
# creates an "adult" dummy variable for HH members where age >= 18.
# ---------- #

# creates an empty matrix to store the dummy variables for adults.
adultDFWave1 <- as.data.frame(matrix(NA, nrow=dim(wave1Data)[1], ncol=length(ageVars)))
adultDFWave2 <- as.data.frame(matrix(NA, nrow=dim(wave2Data)[1], ncol=length(ageVars)))
colnames(adultDFWave1) <- c("adult_01", "adult_02", "adult_03", "adult_04", "adult_05", "adult_06", "adult_07", "adult_08", "adult_09", "adult_10", "adult_11", "adult_12", "adult_13", "adult_14", "adult_15")
colnames(adultDFWave2) <- colnames(adultDFWave1)

# for each of the 15 age vars, creates a new variable equal to 1 if age >=18. Then attaches this variable to the dataframe.
for (i in 1:length(ageVars)){
	adultDFWave1[,i] <- ifelse(wave1Data[,ageVars[i]]>=18, 1, 0)
	adultDFWave2[,i] <- ifelse(wave2Data[,ageVars[i]]>=18, 1, 0)
}

# concatenates these adult dummies to the wave 1 and wave 2 dataframes.
wave1Data <- cbind(wave1Data, adultDFWave1)
wave2Data <- cbind(wave2Data, adultDFWave2)


# ----------------- #
# creates a "childUnder18" dummy variable for HH members where age < 18. 
# ---------- #

# creates an empty matrix to store the dummy variables for children under 18.
child18DFWave1 <- as.data.frame(matrix(NA, nrow=dim(wave1Data)[1], ncol=length(ageVars)))
child18DFWave2 <- as.data.frame(matrix(NA, nrow=dim(wave2Data)[1], ncol=length(ageVars)))
colnames(child18DFWave1) <- c("childUnder18_01", "childUnder18_02", "childUnder18_03", "childUnder18_04", "childUnder18_05", "childUnder18_06", "childUnder18_07", "childUnder18_08", "childUnder18_09", "childUnder18_10", "childUnder18_11", "childUnder18_12", "childUnder18_13", "childUnder18_14", "childUnder18_15")
colnames(child18DFWave2) <- colnames(child18DFWave1)

# for each of the 15 age vars, creates a new variable equal to 1 if age is between 5 and 17 (inclusive). Then attaches this variable to the dataframe.
for (i in 1:length(ageVars)){
	child18DFWave1[,i] <- ifelse(wave1Data[,ageVars[i]]<18 & wave1Data[,ageVars[i]]>4, 1, 0)
	child18DFWave2[,i] <- ifelse(wave2Data[,ageVars[i]]<18 & wave2Data[,ageVars[i]]>4, 1, 0)
}

# concatenates these child < 18 dummies to the wave 1 and wave 2 dataframes.
wave1Data <- cbind(wave1Data, child18DFWave1)
wave2Data <- cbind(wave2Data, child18DFWave2)


# ----------------- #
# creates a "childUnder15" dummy variable for HH members where age < 15. 
# ---------- #

# creates an empty matrix to store the dummy variables for children under 15.
child15DFWave1 <- as.data.frame(matrix(NA, nrow=dim(wave1Data)[1], ncol=length(ageVars)))
child15DFWave2 <- as.data.frame(matrix(NA, nrow=dim(wave2Data)[1], ncol=length(ageVars)))
colnames(child15DFWave1) <- c("childUnder15_01", "childUnder15_02", "childUnder15_03", "childUnder15_04", "childUnder15_05", "childUnder15_06", "childUnder15_07", "childUnder15_08", "childUnder15_09", "childUnder15_10", "childUnder15_11", "childUnder15_12", "childUnder15_13", "childUnder15_14", "childUnder15_15")
colnames(child15DFWave2) <- colnames(child15DFWave1)

# for each of the 15 age vars, creates a new variable equal to 1 if age <15. Then attaches this variable to the dataframe.
for (i in 1:length(ageVars)){
	child15DFWave1[,i] <- ifelse(wave1Data[,ageVars[i]]<15 & wave1Data[,ageVars[i]]>4, 1, 0)
	child15DFWave2[,i] <- ifelse(wave2Data[,ageVars[i]]<15 & wave2Data[,ageVars[i]]>4, 1, 0)
}

# concatenates these child < 15 dummies to the wave 1 and wave 2 dataframes.
wave1Data <- cbind(wave1Data, child15DFWave1)
wave2Data <- cbind(wave2Data, child15DFWave2)


# ----------------- #
# creates a "childUnder12" dummy variable for HH members where age < 12. 
# ---------- #


# creates an empty matrix to store the dummy variables for children under 12.
child12DFWave1 <- as.data.frame(matrix(NA, nrow=dim(wave1Data)[1], ncol=length(ageVars)))
child12DFWave2 <- as.data.frame(matrix(NA, nrow=dim(wave2Data)[1], ncol=length(ageVars)))
colnames(child12DFWave1) <- c("childUnder12_01", "childUnder12_02", "childUnder12_03", "childUnder12_04", "childUnder12_05", "childUnder12_06", "childUnder12_07", "childUnder12_08", "childUnder12_09", "childUnder12_10", "childUnder12_11", "childUnder12_12", "childUnder12_13", "childUnder12_14", "childUnder12_15")
colnames(child12DFWave2) <- colnames(child12DFWave1)

# for each of the 15 age vars, creates a new variable equal to 1 if age <12. Then attaches this variable to the dataframe.
for (i in 1:length(ageVars)){
	child12DFWave1[,i] <- ifelse(wave1Data[,ageVars[i]]<12 & wave1Data[,ageVars[i]]>4, 1, 0)
	child12DFWave2[,i] <- ifelse(wave2Data[,ageVars[i]]<12 & wave2Data[,ageVars[i]]>4, 1, 0)
}

# concatenates these child < 12 dummies to the wave 1 and wave 2 dataframes.
wave1Data <- cbind(wave1Data, child12DFWave1)
wave2Data <- cbind(wave2Data, child12DFWave2)


# ----------------- #
# creates a "childUnder5" dummy variable for HH members where age < 5. 
# ---------- #

# creates an empty matrix to store the dummy variables for children under 5.
child5DFWave1 <- as.data.frame(matrix(NA, nrow=dim(wave1Data)[1], ncol=length(ageVars)))
child5DFWave2 <- as.data.frame(matrix(NA, nrow=dim(wave2Data)[1], ncol=length(ageVars)))
colnames(child5DFWave1) <- c("childUnder5_01", "childUnder5_02", "childUnder5_03", "childUnder5_04", "childUnder5_05", "childUnder5_06", "childUnder5_07", "childUnder5_08", "childUnder5_09", "childUnder5_10", "childUnder5_11", "childUnder5_12", "childUnder5_13", "childUnder5_14", "childUnder5_15")
colnames(child5DFWave2) <- colnames(child5DFWave1)

# for each of the 15 age vars, creates a new variable equal to 1 if age <5. Then attaches this variable to the dataframe.
for (i in 1:length(ageVars)){
	child5DFWave1[,i] <- ifelse(wave1Data[,ageVars[i]]<5, 1, 0)
	child5DFWave2[,i] <- ifelse(wave2Data[,ageVars[i]]<5, 1, 0)
}

# concatenates these child < 5 dummies to the wave 1 and wave 2 dataframes.
wave1Data <- cbind(wave1Data, child5DFWave1)
wave2Data <- cbind(wave2Data, child5DFWave2)


# ---------------- #
# Computes the number of children and adults in a HH
# --------- #

# creates an integer variable for the number of adults in a HH.
wave1Data$numAdults <- rowSums(adultDFWave1, na.rm=T)
wave2Data$numAdults <- rowSums(adultDFWave2, na.rm=T)

# creates an integer variable for the number of children in a HH between the ages of 5 and 17.
wave1Data$numChildren <- rowSums(child18DFWave1, na.rm=T)
wave2Data$numChildren <- rowSums(child18DFWave2, na.rm=T)


# ---------------- #
# Computes the hours worked on cocoa farms for each member of a HH
# during the busy season.
# --------- #

# ---------------- #
# The current criteria for identifying child labour used by the ILO's Statistical 
# Information and
# Monitoring Program on Child Labour (SIMPOC) for its global child labour estimates is:
# 
# A child under 12 who is economically active for 1 or more hours per week,
# A child 14 and under who is economically active for at least 14 hours per week,
# A child 17 and under who is economically active for at least 43 hours per week
# A child 17 and under who participates in activities that are "hazardous by nature or circumstance" for 1 or more hours per week
# A child 17 and under who participates in an "unconditional worst form of child labour" such as trafficked children, children in bondage or forced labour, armed conflict, prostitution, pornography, illicit activities.
# ---------------- #

# creates a dataframe containing the hours worked per week for each HH member.
hrsPerWkWave2 <- as.data.frame(cbind(wave2Data$b4_tot_hrs_01, wave2Data$b4_tot_hrs_02,wave2Data$b4_tot_hrs_03, wave2Data$b4_tot_hrs_04,wave2Data$b4_tot_hrs_05, wave2Data$b4_tot_hrs_06,wave2Data$b4_tot_hrs_07,wave2Data$b4_tot_hrs_08, wave2Data$b4_tot_hrs_09,wave2Data$b4_tot_hrs_10,wave2Data$b4_tot_hrs_11,wave2Data$b4_tot_hrs_12,wave2Data$b4_tot_hrs_13,wave2Data$b4_tot_hrs_14,wave2Data$b4_tot_hrs_15))

hrsPerWkWave1 <- as.data.frame(cbind(wave1Data$b4_tot_hrs_01, wave1Data$b4_tot_hrs_02,wave1Data$b4_tot_hrs_03, wave1Data$b4_tot_hrs_04,wave1Data$b4_tot_hrs_05, wave1Data$b4_tot_hrs_06,wave1Data$b4_tot_hrs_07,wave1Data$b4_tot_hrs_08, wave1Data$b4_tot_hrs_09,wave1Data$b4_tot_hrs_10,wave1Data$b4_tot_hrs_11,wave1Data$b4_tot_hrs_12,wave1Data$b4_tot_hrs_13,wave1Data$b4_tot_hrs_14,wave1Data$b4_tot_hrs_15))


# computes the number of children under 18 working on the cocoa farm, the total hours per week worked by these children, and the hours worked per child under 18.
wave2Data$numChildWorkUnder18 <- rowSums(wave2Data[,colnames(child18DFWave2)]*hrsPerWkWave2 > 0, na.rm=T)
wave2Data$hoursChildLaborUnder18 <- rowSums(wave2Data[,colnames(child18DFWave2)]*hrsPerWkWave2, na.rm=T)
wave2Data$hoursPerChildUnder18 <- wave2Data$hoursChildLaborUnder18 / wave2Data$numChildWorkUnder18
wave2Data$hoursPerChildUnder18[is.nan(wave2Data$hoursPerChildUnder18)] <- 0

wave1Data$numChildWorkUnder18 <- rowSums(wave1Data[,colnames(child18DFWave2)]*hrsPerWkWave1 > 0, na.rm=T)
wave1Data$hoursChildLaborUnder18 <- rowSums(wave1Data[,colnames(child18DFWave2)]*hrsPerWkWave1, na.rm=T)
wave1Data$hoursPerChildUnder18 <- wave1Data$hoursChildLaborUnder18 / wave1Data$numChildWorkUnder18
wave1Data$hoursPerChildUnder18[is.nan(wave1Data$hoursPerChildUnder18)] <- 0


# computes the number of children under 15 working on the cocoa farm, the total hours per week worked by these children, and the hours worked per child under 15.
wave2Data$numChildWorkUnder15 <- rowSums(wave2Data[,colnames(child15DFWave2)]*hrsPerWkWave2 > 0, na.rm=T)
wave2Data$hoursChildLaborUnder15 <- rowSums(wave2Data[,colnames(child15DFWave2)]*hrsPerWkWave2, na.rm=T)
wave2Data$hoursPerChildUnder15 <- wave2Data$hoursChildLaborUnder15 / wave2Data$numChildWorkUnder15
wave2Data$hoursPerChildUnder15[is.nan(wave2Data$hoursPerChildUnder15)] <- 0

wave1Data$numChildWorkUnder15 <- rowSums(wave1Data[,colnames(child15DFWave2)]*hrsPerWkWave1 > 0, na.rm=T)
wave1Data$hoursChildLaborUnder15 <- rowSums(wave1Data[,colnames(child15DFWave2)]*hrsPerWkWave1, na.rm=T)
wave1Data$hoursPerChildUnder15 <- wave1Data$hoursChildLaborUnder15 / wave1Data$numChildWorkUnder15
wave1Data$hoursPerChildUnder15[is.nan(wave1Data$hoursPerChildUnder15)] <- 0


# computes the number of children under 12 working on the cocoa farm, the total hours per week worked by these children, and the hours worked per child under 12.
wave2Data$numChildWorkUnder12 <- rowSums(wave2Data[,colnames(child12DFWave2)]*hrsPerWkWave2 > 0, na.rm=T)
wave2Data$hoursChildLaborUnder12 <- rowSums(wave2Data[,colnames(child12DFWave2)]*hrsPerWkWave2, na.rm=T)
wave2Data$hoursPerChildUnder12 <- wave2Data$hoursChildLaborUnder12 / wave2Data$numChildWorkUnder12
wave2Data$hoursPerChildUnder12[is.nan(wave2Data$hoursPerChildUnder12)] <- 0

wave1Data$numChildWorkUnder12 <- rowSums(wave1Data[,colnames(child12DFWave2)]*hrsPerWkWave1 > 0, na.rm=T)
wave1Data$hoursChildLaborUnder12 <- rowSums(wave1Data[,colnames(child12DFWave2)]*hrsPerWkWave1, na.rm=T)
wave1Data$hoursPerChildUnder12 <- wave1Data$hoursChildLaborUnder12 / wave1Data$numChildWorkUnder12
wave1Data$hoursPerChildUnder12[is.nan(wave1Data$hoursPerChildUnder12)] <- 0


# creates a dummy variable for whether there is child labor for a child under 12
wave2Data$childLaborUnder12 <- ifelse(wave2Data$hoursChildLaborUnder12 > 0, 1, 0)
wave1Data$childLaborUnder12 <- ifelse(wave1Data$hoursChildLaborUnder12 > 0, 1, 0)

# creates a dummy variable for whether there is child labor for a child 14 or under.
wave2Data$childLaborUnder15 <- ifelse(apply(wave2Data[,colnames(child15DFWave2)]*hrsPerWkWave2, 1, function(x) ifelse(all(is.na(x)), NA, max(x,na.rm=T))) > 14, 1, 0)
wave1Data$childLaborUnder15 <- ifelse(apply(wave1Data[,colnames(child15DFWave2)]*hrsPerWkWave1, 1, function(x) ifelse(all(is.na(x)), NA, max(x,na.rm=T))) > 14, 1, 0)

# creates a dummy variable for whether there is child labor for a child 17 or under
wave2Data$childLaborUnder18 <- ifelse(apply(wave2Data[,colnames(child18DFWave2)]*hrsPerWkWave2, 1, function(x) ifelse(all(is.na(x)), NA, max(x,na.rm=T))) > 43, 1, 0)
wave1Data$childLaborUnder18 <- ifelse(apply(wave1Data[,colnames(child18DFWave2)]*hrsPerWkWave1, 1, function(x) ifelse(all(is.na(x)), NA, max(x,na.rm=T))) > 43, 1, 0)

# creates a dummy variable for whether there is child labor for any child in the household.
wave2Data$childLaborAny <- ifelse(wave2Data$childLaborUnder12==1 | wave2Data$childLaborUnder15==1 | wave2Data$childLaborUnder18==1, 1, 0)
wave1Data$childLaborAny <- ifelse(wave1Data$childLaborUnder12==1 | wave1Data$childLaborUnder15==1 | wave1Data$childLaborUnder18==1, 1, 0)

# NOTE: children under the age of 5 are excluded from these child labor measures.

# ---------------- #
# Computes the hours worked outside of the cocoa farms for each member of a HH 
# during the busy season.
# --------- #

# The current criteria for identifying child labour used by the ILO's Statistical Information and
# Monitoring Program on Child Labour (SIMPOC) for its global child labour estimates is:
# A child under 12 who is economically active for 1 or more hours per week,
# A child 14 and under who is economically active for at least 14 hours per week,
# A child 17 and under who is economically active for at least 43 hours per week
# A child 17 and under who participates in activities that are "hazardous by nature or circumstance" for 1 or more hours per week
# A child 17 and under who participates in an "unconditional worst form of child labour" such as trafficked children, children in bondage or forced labour, armed conflict, prostitution, pornography, illicit activities.

# creates a dataframe containing the hours worked per week for each HH member outside
# of the cocoa farms.
outsideHrsPerWkWave2 <- as.data.frame(cbind(wave2Data$b7_tot_hrs_01, wave2Data$b7_tot_hrs_02,wave2Data$b7_tot_hrs_03, wave2Data$b7_tot_hrs_04,wave2Data$b7_tot_hrs_05, wave2Data$b7_tot_hrs_06,wave2Data$b7_tot_hrs_07,wave2Data$b7_tot_hrs_08, wave2Data$b7_tot_hrs_09,wave2Data$b7_tot_hrs_10,wave2Data$b7_tot_hrs_11,wave2Data$b7_tot_hrs_12,wave2Data$b7_tot_hrs_13,wave2Data$b7_tot_hrs_14,wave2Data$b7_tot_hrs_15))

outsideHrsPerWkWave1 <- as.data.frame(cbind(wave1Data$b7_tot_hrs_01, wave1Data$b7_tot_hrs_02,wave1Data$b7_tot_hrs_03, wave1Data$b7_tot_hrs_04,wave1Data$b7_tot_hrs_05, wave1Data$b7_tot_hrs_06,wave1Data$b7_tot_hrs_07,wave1Data$b7_tot_hrs_08, wave1Data$b7_tot_hrs_09,wave1Data$b7_tot_hrs_10,wave1Data$b7_tot_hrs_11,wave1Data$b7_tot_hrs_12,wave1Data$b7_tot_hrs_13,wave1Data$b7_tot_hrs_14,wave1Data$b7_tot_hrs_15))


# computes the number of children under 18 working on the cocoa farm, the total hours per week worked by these children, and the hours worked per child under 18.
wave2Data$numChildWorkUnder18Outside <- rowSums(wave2Data[,colnames(child18DFWave2)]*outsideHrsPerWkWave2 > 0, na.rm=T)
wave2Data$hoursChildLaborUnder18Outside <- rowSums(wave2Data[,colnames(child18DFWave2)]*outsideHrsPerWkWave2, na.rm=T)
wave2Data$hoursPerChildUnder18Outside <- wave2Data$hoursChildLaborUnder18Outside / wave2Data$numChildWorkUnder18Outside
wave2Data$hoursPerChildUnder18Outside[is.nan(wave2Data$hoursPerChildUnder18Outside)] <- 0

wave1Data$numChildWorkUnder18Outside <- rowSums(wave1Data[,colnames(child18DFWave2)]*outsideHrsPerWkWave1 > 0, na.rm=T)
wave1Data$hoursChildLaborUnder18Outside <- rowSums(wave1Data[,colnames(child18DFWave2)]*outsideHrsPerWkWave1, na.rm=T)
wave1Data$hoursPerChildUnder18Outside <- wave1Data$hoursChildLaborUnder18Outside / wave1Data$numChildWorkUnder18Outside
wave1Data$hoursPerChildUnder18Outside[is.nan(wave1Data$hoursPerChildUnder18Outside)] <- 0


# computes the number of children under 15 working on the cocoa farm, the total hours per week worked by these children, and the hours worked per child under 15.
wave2Data$numChildWorkUnder15Outside <- rowSums(wave2Data[,colnames(child15DFWave2)]*outsideHrsPerWkWave2 > 0, na.rm=T)
wave2Data$hoursChildLaborUnder15Outside <- rowSums(wave2Data[,colnames(child15DFWave2)]*outsideHrsPerWkWave2, na.rm=T)
wave2Data$hoursPerChildUnder15Outside <- wave2Data$hoursChildLaborUnder15Outside / wave2Data$numChildWorkUnder15Outside
wave2Data$hoursPerChildUnder15Outside[is.nan(wave2Data$hoursPerChildUnder15Outside)] <- 0

wave1Data$numChildWorkUnder15Outside <- rowSums(wave1Data[,colnames(child15DFWave2)]*outsideHrsPerWkWave1 > 0, na.rm=T)
wave1Data$hoursChildLaborUnder15Outside <- rowSums(wave1Data[,colnames(child15DFWave2)]*outsideHrsPerWkWave1, na.rm=T)
wave1Data$hoursPerChildUnder15Outside <- wave1Data$hoursChildLaborUnder15Outside / wave1Data$numChildWorkUnder15Outside
wave1Data$hoursPerChildUnder15Outside[is.nan(wave1Data$hoursPerChildUnder15Outside)] <- 0


# computes the number of children under 12 working on the cocoa farm, the total hours per week worked by these children, and the hours worked per child under 12.
wave2Data$numChildWorkUnder12Outside <- rowSums(wave2Data[,colnames(child12DFWave2)]*outsideHrsPerWkWave2 > 0, na.rm=T)
wave2Data$hoursChildLaborUnder12Outside <- rowSums(wave2Data[,colnames(child12DFWave2)]*outsideHrsPerWkWave2, na.rm=T)
wave2Data$hoursPerChildUnder12Outside <- wave2Data$hoursChildLaborUnder12Outside / wave2Data$numChildWorkUnder12Outside
wave2Data$hoursPerChildUnder12Outside[is.nan(wave2Data$hoursPerChildUnder12Outside)] <- 0

wave1Data$numChildWorkUnder12Outside <- rowSums(wave1Data[,colnames(child12DFWave2)]*outsideHrsPerWkWave1 > 0, na.rm=T)
wave1Data$hoursChildLaborUnder12Outside <- rowSums(wave1Data[,colnames(child12DFWave2)]*outsideHrsPerWkWave1, na.rm=T)
wave1Data$hoursPerChildUnder12Outside <- wave1Data$hoursChildLaborUnder12Outside / wave1Data$numChildWorkUnder12Outside
wave1Data$hoursPerChildUnder12Outside[is.nan(wave1Data$hoursPerChildUnder12Outside)] <- 0


# creates a dummy variable for whether there is child labor for a child under 12 outside
# of the cocoa farms.
wave2Data$childLaborUnder12Outside <- ifelse(wave2Data$hoursChildLaborUnder12Outside > 0, 1, 0)
wave1Data$childLaborUnder12Outside <- ifelse(wave1Data$hoursChildLaborUnder12Outside > 0, 1, 0)

# creates a dummy variable for whether there is child labor for a child 14 or under outside
# of the cocoa farms.
wave2Data$childLaborUnder15Outside <- ifelse(apply(wave2Data[,colnames(child15DFWave2)]*outsideHrsPerWkWave2, 1, function(x) ifelse(all(is.na(x)), NA, max(x,na.rm=T))) > 14, 1, 0)
wave1Data$childLaborUnder15Outside <- ifelse(apply(wave1Data[,colnames(child15DFWave2)]*outsideHrsPerWkWave1, 1, function(x) ifelse(all(is.na(x)), NA, max(x,na.rm=T))) > 14, 1, 0)

# creates a dummy variable for whether there is child labor for a child 17 or under outside
# of the cocoa farms.
wave2Data$childLaborUnder18Outside <- ifelse(apply(wave2Data[,colnames(child18DFWave2)]*outsideHrsPerWkWave2, 1, function(x) ifelse(all(is.na(x)), NA, max(x,na.rm=T))) > 43, 1, 0)
wave1Data$childLaborUnder18Outside <- ifelse(apply(wave1Data[,colnames(child18DFWave2)]*outsideHrsPerWkWave1, 1, function(x) ifelse(all(is.na(x)), NA, max(x,na.rm=T))) > 43, 1, 0)

# creates a dummy variable for whether there is child labor for any child in the household 
# outside of the cocoa farms.
wave2Data$childLaborAnyOutside <- ifelse(wave2Data$childLaborUnder12Outside==1 | wave2Data$childLaborUnder15Outside==1 | wave2Data$childLaborUnder18Outside==1, 1, 0)
wave1Data$childLaborAnyOutside <- ifelse(wave1Data$childLaborUnder12Outside==1 | wave1Data$childLaborUnder15Outside==1 | wave1Data$childLaborUnder18Outside==1, 1, 0)


# NOTE: children under the age of 5 are excluded from these child labor measures.

# NOTE TO SELF: should double-check that these "outside the farm" labor measures have
# been computed correctly. This code is a direct analog to the "on the farm" measures,
# though it would be good to do a few more cross-checks nevertheless.


# ---------------- #
# Computes the hours worked on cocoa farms for each member of a HH
# during the low season.
# --------- #

# to be completed...
# Just need to copy and paste the above code for hours worked during the busy season
# and adapt it to the low season variable.


# ---------------- #
# Computes the hours worked outside of cocoa farms for each member of a HH
# during the low season.
# --------- #

# to be completed...
# Just need to copy and paste the above code for hours worked during the busy season
# and adapt it to the low season variable.

# NOTE TO SELF: once all these "hours worked" variables have been created (i.e. hours
# worked on farm busy season, off farm busy season, on farm low season, off farm low
# season), then we can create an aggregate measure of child labor (i.e. add together
# on + off farm labor in a given season and count as child labor if this sum exceeds
# the threshold for child labor in either the busy or low season).


# ---------------- #
# Computes prevalence of injuries as a result of work on cocoa farms.
# --------- #

# to be completed...
# now that B10 is cleaned, we can add this measure. Can compute for each age group
# and by gender.


# ---------------- #
# Computes the number of children in a HH participating in hazardous work. 
# --------- #

# NOTE TO SELF: this cannot be computed because of the issue with b3_04 - b3_15. (see 
# email from Jitka).


# ---------------- #
# ????
# --------- #

# NOTE TO SELF: What other child labor measures do we want to add?
# e.g. of peripheral relevance, N7-N10 (which are now cleaned) provide 
# some attitudinal measures on whether the head of household wants his/her 
# children to go into cocoa farming.


# ---------------------------------------- #
# CREATES GENDER EQUITY/DISPARITY VARIABLES FOR WAVE 1 AND WAVE 2
# This block of code prepares the variables for gender equity/disparity that will
# be used in the analysis. 
# 
# NOTE TO SELF: what other gender disparity measures do we want to add? There is 
# still quite a bit of work to do here.
# ---------------------------------------- #

# ---------------- #
# Computes measures for disparity in cocoa production/income between female and
# male-headed HH.
# --------- #

# ---------------- #
# Computes the proportion of cocoa farm labor provided by females.
# --------- #

sexVars <- c("sex_01", "sex_02", "sex_03", "sex_04", "sex_05", "sex_06", "sex_07", "sex_08", "sex_09", "sex_10", "sex_11", "sex_12", "sex_13", "sex_14", "sex_15")

# computes the number of females in each HH.
wave2Data$numFemales <- rowSums(1-wave2Data[,sexVars], na.rm=T)
wave1Data$numFemales <- rowSums(1-wave1Data[,sexVars], na.rm=T)

# computes the number of females working on the cocoa farm, the total hours per week worked by these females, and the hours worked per female.
wave2Data$numWorkingFemales <- rowSums((1-wave2Data[,sexVars]) * (hrsPerWkWave2 > 0), na.rm=T)
wave2Data$hoursWorkedFemale <- rowSums((1-wave2Data[,sexVars]) * hrsPerWkWave2, na.rm=T)
wave2Data$hoursWorkedPerFemale <- wave2Data$hoursWorkedFemale / wave2Data$numWorkingFemales
wave2Data$hoursWorkedPerFemale[is.nan(wave2Data$hoursWorkedPerFemale)] <- 0

wave1Data$numWorkingFemales <- rowSums((1-wave1Data[,sexVars]) * (hrsPerWkWave1 > 0), na.rm=T)
wave1Data$hoursWorkedFemale <- rowSums((1-wave1Data[,sexVars]) * hrsPerWkWave1, na.rm=T)
wave1Data$hoursWorkedPerFemale <- wave1Data$hoursWorkedFemale / wave1Data$numWorkingFemales
wave1Data$hoursWorkedPerFemale[is.nan(wave1Data$hoursWorkedPerFemale)] <- 0


# NOTE: it is worthwhile to independently compute these measures for males, 
# rather than just subtracting off the number of Males from the total HH size, 
# since there are some rows with NA's for gender. 

# computes the number of males in each HH.
wave2Data$numMales <- rowSums(wave2Data[,sexVars], na.rm=T)
wave1Data$numMales <- rowSums(wave1Data[,sexVars], na.rm=T)

# computes the number of males working on the cocoa farm, the total hours per week worked by these males, and the hours worked per male.
wave2Data$numWorkingMales <- rowSums((wave2Data[,sexVars]) * (hrsPerWkWave2 > 0), na.rm=T)
wave2Data$hoursWorkedMale <- rowSums((wave2Data[,sexVars]) * hrsPerWkWave2, na.rm=T)
wave2Data$hoursWorkedPerMale <- wave2Data$hoursWorkedMale / wave2Data$numWorkingMales
wave2Data$hoursWorkedPerMale[is.nan(wave2Data$hoursWorkedPerMale)] <- 0

wave1Data$numWorkingMales <- rowSums((wave1Data[,sexVars]) * (hrsPerWkWave1 > 0), na.rm=T)
wave1Data$hoursWorkedMale <- rowSums((wave1Data[,sexVars]) * hrsPerWkWave1, na.rm=T)
wave1Data$hoursWorkedPerMale <- wave1Data$hoursWorkedMale / wave1Data$numWorkingMales
wave1Data$hoursWorkedPerMale[is.nan(wave1Data$hoursWorkedPerMale)] <- 0

# NOTE TO SELF: in each wave, there are a few rows that are tagged as having 0 working 
# females and 0 working males. Should look into where these are coming from. Some of 
# these are coming from the fact that some rows have a sum of 0 hours worked per week,
# though this does not appear to account for all observations.
table(wave1Data$numWorkingMales, wave1Data$numWorkingFemales, exclude=NULL)
table(wave2Data$numWorkingMales, wave2Data$numWorkingFemales, exclude=NULL)
table(rowSums(hrsPerWkWave1, na.rm=TRUE)==0)
table(rowSums(hrsPerWkWave2, na.rm=TRUE)==0)

# ---------------- #
# Computes the proportion of cocoa farm labor provided by females.
# NOTE: the denominator is female hours plus male hours, rather than using the rowSums
# function over hrsPerWkWave1, because some sex variables are missing.
# --------- #

wave2Data$propFemaleCocoaLabor <- wave2Data$hoursWorkedFemale / (wave2Data$hoursWorkedFemale + wave2Data$hoursWorkedMale)
# NOTE TO SELF: replace NaN with NA. These are the rows where both the numerator and 
# denominator are 0 (see NOTE TO SELF above for more detail.)
wave2Data$propFemaleCocoaLabor[is.nan(wave2Data$propFemaleCocoaLabor)] <- NA

wave1Data$propFemaleCocoaLabor <- wave1Data$hoursWorkedFemale / (wave1Data$hoursWorkedFemale + wave1Data$hoursWorkedMale)
# NOTE TO SELF: replace NaN with NA. These are the rows where both the numerator and 
# denominator are 0 (see NOTE TO SELF above for more detail.)
wave1Data$propFemaleCocoaLabor[is.nan(wave1Data$propFemaleCocoaLabor)] <- NA

# NOTE TO SELF: there are 5 observations in wave 1 where the number of females is supposedly
# equal to the total HH size, yet the proportion of female labor is less than 1. The problem
# appears to be with a few errors in gghhsize in wave 1, which should be looked at more
# closely at some point. Since it only affects a few observations, I've let this be for now.
# table(wave1Data$propFemaleCocoaLabor[wave1Data$numFemales==wave1Data$gghhsize], exclude=NULL)
# cbind(wave1Data$propFemaleCocoaLabor[wave1Data$numFemales==wave1Data$gghhsize], 
# 	wave1Data$numFemales[wave1Data$numFemales==wave1Data$gghhsize], 
# 	wave1Data$numMales[wave1Data$numFemales==wave1Data$gghhsize], 
# 	wave1Data$gghhsize[wave1Data$numFemales==wave1Data$gghhsize], 
# 	wave1Data$sex_01[wave1Data$numFemales==wave1Data$gghhsize], 
# 	wave1Data$sex_02[wave1Data$numFemales==wave1Data$gghhsize], 
# 	wave1Data$sex_03[wave1Data$numFemales==wave1Data$gghhsize])


# NOTE TO SELF: later, produce the same measures for adult females only.



# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#
#																	   	#
#  2. CREATES A MERGED DATASET OF WAVE 1 AND WAVE 2 DATA (LONG FORMAT) 	#
#																	   	#
# This block of code merges wave 1 and wave 2 data in long format, such #
# that a single row is a farmer-year. The dataset contains basic 		#
# information on each village, as well as data on a range of key 		#
# indicators as measured in wave 2 (average HH size, average cocoa 		#
# income, average cocoa price/kg, ...). 								#
#																	   	#
# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#

# Subsets the data by a varlist of key indicators and identifying variables.
# These datasets are very wide (over 1700 variables). To simplify things, 
# we keep just a handful of identifying variables and key indicators:

# renames column names in wave 1 and wave 2 in order to allow for proper appending.
wave1Data <- rename(wave1Data, c("farmerquestionaire_softw_id"="softwid", "infosheet_nameofvillage"="villageName", "infosheet_name_mainfarmer"="l_mainfarmername", "infosheet_nameofdistrict"="l_district", "POINT_X"="l_gpslongitude", "POINT_Y"="l_gpslatitude", "ggavgpericeKG"="ggavgpriceKG")) 
wave2Data <- rename(wave2Data, c("wave2_clean_localityname"="villageName"))
wave1Data$wave2_in1stwave <- NA

# Varlist of identifying variables:
#idVarList <- c("newloccode", "farmerquestionaire_softw_id", "infosheet_nameofvillage", "infosheet_name_mainfarmer", "infosheet_nameofdistrict", "POINT_X", "POINT_Y")
idVarList <- c("newloccode", "softwid", "l_mainfarmername", "villageName", "wave2_in1stwave", "l_gpslongitude", "l_gpslatitude", "l_district")

# Varlist of key indicators:
keyIndicatorList <- c("gghhsize","ggagehhhead","ggschoolinghhhead","ggmeanage", "ggsavings", "gggenderhhhead","ggnofarms","ggfsizeacres","ggfsizecocoaacres","ggbuyers","ggharvestcocoaKG","ggyieldall_cocarea","ggyieldall_allarea","ggfraclost","ggsoldcocoaKG","ggfractionsold","ggincomecocoa","ggincomeothcrops","ggincomecropstot","ggincomecropstotcap","ggincomeotheragro","ggincomeothernonagro","ggincomeremit","ggincometotnoncrop","ggincometotal","ggincometotalcap","ggeverschoolhhhead","gggenderhhhead","ggliterate","ggtraining","ggreceivedloan","ggbank","ggorggroup","ggcar","ggchainsaw","ggmistblower","gghhhwives","ggborninvillage","ggelectric","gghealthinsurance","ggcontactedleader","gginvestment","ggmalaria","ggtertiary","ggcertifiedfarmerorg","ggpolitics","ggfinancialassistance","gginfo","gglegaltitle","ggsharecrop","ggwouldlosefallow", "ggashanti", "ggbrong", "ggcentral", "ggeastern", "ggwestern", "ggavgpriceKG", "ggknowsft", "ggorganic", "ggdumfert", "gginsect", "ggherb", "ggfungi", "ggorg", "ggseedpods", "ggseedling", "ggknap", "ggmist", "ggtraining", "ggorggroup", "ggcertifiedfarmerorg", "gginvestment", "childLaborAny","childLaborUnder18", "childLaborUnder15", "childLaborUnder12", "hoursPerChildUnder18", "hoursPerChildUnder15", "hoursPerChildUnder12", "childLaborAnyOutside","childLaborUnder18Outside", "childLaborUnder15Outside", "childLaborUnder12Outside", "hoursPerChildUnder18Outside", "hoursPerChildUnder15Outside", "hoursPerChildUnder12Outside", "hoursWorkedPerFemale", "propFemaleCocoaLabor")

# subsets wave 1 and wave 2 data
wave1Data <- wave1Data[,c(idVarList, keyIndicatorList)]
wave2Data <- wave2Data[,c(idVarList, keyIndicatorList)]

# add binary variable to identify wave 1 and wave 2 observations. Intuitively, "2" refers to wave 2 observations. 
wave1Data$surveyWave <- 1
wave2Data$surveyWave <- 2

# --------------
# Appends wave 2 data to wave 1 data (long format).
farmerData <- rbind(wave1Data, wave2Data)



# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#
#																	   	#
# 				3. DESCRIBES AND TRIMS KEY INDICATORS 					#
#																	   	#
# This section of code plots the histograms of all key variables, trims #
# (most of) them at the .99 percentile, and takes the log of many 		#
# continuous variables for usage later in the impact analysis. 			#
#																	   	#
# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#



# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#
# Checks the distribution of continuous variables: trims at the 0.99 	#
# percentile and takes the natural log if necessary. 					# 
#																	   	#
# NOTE TO SELF: I have commented out variables that have not been 		#
# cleaned yet.															# 
# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#

##Number of farms
hist(farmerData$ggnofarms)
qqnorm(farmerData$ggnofarms)
qqline(farmerData$ggnofarms) 
farmerData$ggnofarms[farmerData$ggnofarms>quantile(farmerData$ggnofarms,.99, na.rm=TRUE)]<-NA
#farmerData$ggnofarmslog<-log(farmerData$ggnofarms)
#qqnorm(farmerData$ggnofarmslog)
#abline(0,1) 

##Household Size
hist(farmerData$gghhsize)
qqnorm(farmerData$gghhsize)
qqline(farmerData$gghhsize) 
farmerData$gghhsize[farmerData$gghhsize>quantile(farmerData$gghhsize,.99, na.rm=TRUE)]<-NA

##Age of Household Head
hist(farmerData$ggagehhhead)
qqnorm(farmerData$ggagehhhead)
qqline(farmerData$ggagehhhead) 
farmerData$ggagehhhead[farmerData$ggagehhhead>quantile(farmerData$ggagehhhead,.99, na.rm=TRUE)]<-NA

##Years of schooling of Household Head
hist(farmerData$ggschoolinghhhead)
qqnorm(farmerData$ggschoolinghhhead)
qqline(farmerData$ggschoolinghhhead) 
farmerData$ggschoolinghhhead[farmerData$ggschoolinghhhead>=quantile(farmerData$ggschoolinghhhead,.99, na.rm=TRUE)]<-NA
farmerData$ggschoolinghhheadlog <-log(farmerData$ggschoolinghhhead+1)
hist(farmerData$ggschoolinghhheadlog)

##Mean Age in Household
hist(farmerData$ggmeanage)
qqnorm(farmerData$ggmeanage)
qqline(farmerData$ggmeanage)
farmerData$ggmeanage[farmerData$ggmeanage>quantile(farmerData$ggmeanage,.99, na.rm=TRUE)]<-NA
farmerData$ggmeanage[farmerData$ggmeanage==0]<-NA
farmerData$ggmeanagelog<-log(farmerData$ggmeanage)
hist(farmerData$ggmeanagelog)
qqnorm(farmerData$ggmeanagelog)
qqline(farmerData$ggmeanagelog) 

##Savings
hist(farmerData$ggsavings)
qqnorm(farmerData$ggsavings)
qqline(farmerData$ggsavings) 
farmerData$ggsavings[farmerData$ggsavings>quantile(farmerData$ggsavings,.99, na.rm=TRUE)]<-NA
farmerData$ggsavingslog<-log(farmerData$ggsavings+1)
hist(farmerData$ggsavingslog)
qqnorm(farmerData$ggsavingslog)
qqline(farmerData$ggsavingslog) 

##Farm Size (Acres)
hist(farmerData$ggfsizeacres)
qqnorm(farmerData$ggfsizeacres)
qqline(farmerData$ggfsizeacres) 
farmerData$ggfsizeacres[farmerData$ggfsizeacres>quantile(farmerData$ggfsizeacres,.99, na.rm=TRUE)]<-NA
farmerData$ggfsizeacreslog<-log(farmerData$ggfsizeacres+1)
hist(farmerData$ggfsizeacreslog)
qqnorm(farmerData$ggfsizeacreslog)
qqline(farmerData$ggfsizeacreslog) 

##Cocoa area size (Acres)
hist(farmerData$ggfsizecocoaacres)
qqnorm(farmerData$ggfsizecocoaacres)
qqline(farmerData$ggfsizecocoaacres) 
farmerData$ggfsizecocoaacres[farmerData$ggfsizecocoaacres>quantile(farmerData$ggfsizecocoaacres,.99, na.rm=TRUE)]<-NA
farmerData$ggfsizecocoaacreslog<-log(farmerData$ggfsizecocoaacres+1)
hist(farmerData$ggfsizecocoaacreslog)
qqnorm(farmerData$ggfsizecocoaacreslog)
qqline(farmerData$ggfsizecocoaacreslog) 

##Number of buyers
hist(farmerData$ggbuyers)
qqnorm(farmerData$ggbuyers)
qqline(farmerData$ggbuyers) 
farmerData$ggbuyers[farmerData$ggbuyers>quantile(farmerData$ggbuyers,.99, na.rm=TRUE)]<-NA

##Household Hours worked
# hist(farmerData$gghhhoursworked)
# qqnorm(farmerData$gghhhoursworked)
# abline(0,1) ##Not normal
# farmerData$gghhhoursworked[farmerData$gghhhoursworked>quantile(farmerData$gghhhoursworked,.99, na.rm=TRUE)]<-NA
# farmerData$gghhhoursworkedlog <-log(farmerData$gghhhoursworked+1) ##Use +1 to 
# ## avoid -Inf values
# hist(farmerData$gghhhoursworkedlog)
# qqnorm(farmerData$gghhhoursworkedlog)
# abline(0,1) ## somewhat better

# ##Hired Days worked
# hist(farmerData$gghireddayswork)
# qqnorm(farmerData$gghireddayswork)
# abline(0,1) ##Not normal
# farmerData$gghireddayswork[farmerData$gghireddayswork>quantile(farmerData$gghireddayswork,.99, na.rm=TRUE)]<-NA
# farmerData$gghireddaysworklog <-log(farmerData$gghireddayswork+1)
# hist(farmerData$gghireddaysworklog)
# qqnorm(farmerData$gghireddaysworklog)
# abline(0,1) ## better, although a lot of people are not hiring

# ##Average age of tree
# hist(farmerData$ggageoftree)
# qqnorm(farmerData$ggageoftree)
# abline(0,1) ##Not normal
# farmerData$ggageoftree[farmerData$ggageoftree>quantile(farmerData$ggageoftree,.99, na.rm=TRUE)]<-NA
# farmerData$ggageoftreelog <-log(farmerData$ggageoftree)
# hist(farmerData$ggageoftreelog)
# qqnorm(farmerData$ggageoftreelog)
# abline(0,1) ## better

# ##Proportion of trees too young
# hist(farmerData$ggyoungtrees)
# qqnorm(farmerData$ggyoungtrees)
# abline(0,1) ##Not normal, but outliers not a problem, and logging
# ## will not improve

# ##Number of months with some weeding
# hist(farmerData$ggweed)
# qqnorm(farmerData$ggweed)
# abline(0,1) ##Not normal, though distribution is reasonable
# ggweedlog <-log(farmerData$ggweed+1)
# hist(ggweedlog)
# qqnorm(ggweedlog)
# abline(0,1) ## maybe slightly better, though coefficients will
# ## be less interpretable (percentages of months...)

# ##Number of months with some fertilizer
# hist(farmerData$ggfert)
# qqnorm(farmerData$ggfert)
# abline(0,1) ##Not normal, though distribution is reasonable
# ggfertlog <-log(farmerData$ggfert+1)
# hist(ggfertlog)
# qqnorm(ggfertlog)
# abline(0,1) ## Logging does not significantly improve

# ##Cost of labor inputs
# hist(farmerData$ggcostlabor)
# qqnorm(farmerData$ggcostlabor)
# abline(0,1) ##Few outliers
# farmerData$ggcostlabor[farmerData$ggcostlabor>quantile(farmerData$ggcostlabor,.99, na.rm=TRUE)]<-NA
# ggcostlaborlog <-log(farmerData$ggcostlabor+1)
# hist(ggcostlaborlog)
# qqnorm(ggcostlaborlog)
# abline(0,1) ## marginally better

# ##Cost of non-labor inputs
# hist(farmerData$ggcostotherinput)
# qqnorm(farmerData$ggcostotherinput)
# abline(0,1) ##Few outliers
# farmerData$ggcostotherinput[farmerData$ggcostotherinput>quantile(farmerData$ggcostotherinput,.99, na.rm=TRUE)]<-NA
# ggcostotherinputlog <-log(farmerData$ggcostotherinput +1)
# hist(ggcostotherinputlog) ## looks good apart from 0s
# qqnorm(ggcostotherinputlog)
# abline(0,1) ## marginally better

# ##Cost of total inputs
# hist(farmerData$ggcosttotinputs)
# qqnorm(farmerData$ggcosttotinputs)
# abline(0,1) ##Few outliers
# farmerData$ggcosttotinputs[farmerData$ggcosttotinputs>quantile(farmerData$ggcosttotinputs,.99, na.rm=TRUE)]<-NA
# ggcosttotinputslog <-log(farmerData$ggcosttotinputs +1)
# hist(ggcosttotinputslog) ## looks good apart from 0s
# qqnorm(ggcosttotinputslog)
# abline(0,1) ## marginally better

##Distance to nearest village
# hist(farmerData$ggdistance)
# qqnorm(farmerData$ggdistance)
# abline(0,1) ##not normal
# farmerData$ggdistance[farmerData$ggdistance>quantile(farmerData$ggdistance,.99, na.rm=TRUE)]<-NA
# ggdistancelog <-log(farmerData$ggdistance +1)
# hist(ggcosttotinputslog) ## looks good apart from 0s
# qqnorm(ggcosttotinputslog)
# abline(0,1) ## better

# ##Months road unusable
# hist(farmerData$ggroadmonths)
# qqnorm(farmerData$ggroadmonths)
# abline(0,1) ##Distribution perfectly reasonable
# ## and logging will make worse

# ##Distance to nearest market
# hist(farmerData$ggdistancemarket)
# qqnorm(farmerData$ggdistancemarket)
# abline(0,1) ##not normal
# farmerData$ggdistancemarket[farmerData$ggdistancemarket>quantile(farmerData$ggdistancemarket,.99, na.rm=TRUE)]<-NA
# ggdistancemarketlog <-log(farmerData$ggdistancemarket +1)
# hist(ggdistancemarketlog) ## looks decent
# qqnorm(ggdistancemarketlog)
# abline(0,1) ## better

# ##Distance to nearest primary
# hist(farmerData$ggdistanceprimary)
# qqnorm(farmerData$ggdistanceprimary)
# abline(0,1) ##not normal
# farmerData$ggdistanceprimary[farmerData$ggdistanceprimary>quantile(farmerData$ggdistanceprimary,.99, na.rm=TRUE)]<-NA
# ggdistanceprimarylog <-log(farmerData$ggdistanceprimary +1)
# hist(ggdistanceprimarylog) ## looks good apart from 0s
# qqnorm(ggdistanceprimarylog)
# abline(0,1) ## better

# ##Distance to nearest health facility
# hist(farmerData$ggdistancehealth)
# qqnorm(farmerData$ggdistancehealth)
# abline(0,1) ##not normal
# farmerData$ggdistancehealth[farmerData$ggdistancehealth>quantile(farmerData$ggdistancehealth,.99, na.rm=TRUE)]<-NA
# ggdistancehealthlog <-log(farmerData$ggdistancehealth +1)
# hist(ggdistancehealthlog) ## looks ok
# qqnorm(ggdistancehealthlog)
# abline(0,1) ## better

##Proportion of households with electricity
hist(farmerData$ggelectric)
qqnorm(farmerData$ggelectric)
qqline(farmerData$ggelectric) 

##Cocoa harvest (kg)
hist(farmerData$ggharvestcocoaKG)
qqnorm(farmerData$ggharvestcocoaKG)
qqline(farmerData$ggharvestcocoaKG)
farmerData$ggharvestcocoaKG[farmerData$ggharvestcocoaKG>quantile(farmerData$ggharvestcocoaKG,.99, na.rm=TRUE)]<-NA
farmerData$ggharvestcocoaKGlog <-log(farmerData$ggharvestcocoaKG +1)
hist(farmerData$ggharvestcocoaKGlog) 
qqnorm(farmerData$ggharvestcocoaKGlog)
qqline(farmerData$ggharvestcocoaKGlog)

##Cocoa yield per ha (all area)
hist(farmerData$ggyieldall_allarea)
qqnorm(farmerData$ggyieldall_allarea)
qqline(farmerData$ggyieldall_allarea) 
farmerData$ggyieldall_allarea[farmerData$ggyieldall_allarea>quantile(farmerData$ggyieldall_allarea,.99, na.rm=TRUE)]<-NA
farmerData$ggyieldall_allarealog <-log(farmerData$ggyieldall_allarea +1)
hist(farmerData$ggyieldall_allarealog) 
qqnorm(farmerData$ggyieldall_allarealog)
qqline(farmerData$ggyieldall_allarealog)

##Cocoa yield per ha (cocoa area)
hist(farmerData$ggyieldall_cocarea)
qqnorm(farmerData$ggyieldall_cocarea)
qqline(farmerData$ggyieldall_cocarea) 
farmerData$ggyieldall_cocarea[farmerData$ggyieldall_cocarea>quantile(farmerData$ggyieldall_cocarea,.99, na.rm=TRUE)]<-NA
farmerData$ggyieldall_cocarealog <-log(farmerData$ggyieldall_cocarea+1)
hist(farmerData$ggyieldall_cocarealog) 
qqnorm(farmerData$ggyieldall_cocarealog)
qqline(farmerData$ggyieldall_cocarealog)

##Fraction of harvest lost to problems
hist(farmerData$ggfraclost) 
qqnorm(farmerData$ggfraclost)
qqline(farmerData$ggfraclost) 

##Total cocoa sold (kg)
hist(farmerData$ggsoldcocoaKG)
qqnorm(farmerData$ggsoldcocoaKG)
qqline(farmerData$ggsoldcocoaKG)
farmerData$ggsoldcocoaKG[farmerData$ggsoldcocoaKG>quantile(farmerData$ggsoldcocoaKG,.99, na.rm=TRUE)]<-NA
farmerData$ggsoldcocoaKGlog <-log(farmerData$ggsoldcocoaKG+1)
hist(farmerData$ggsoldcocoaKGlog) 
qqnorm(farmerData$ggsoldcocoaKGlog)
qqline(farmerData$ggsoldcocoaKGlog)

##Fraction of harvest sold
hist(farmerData$ggfractionsold) 
farmerData$ggfractionsold[farmerData$ggfractionsold>1]<-NA

##Average price received
hist(farmerData$ggavgpriceKG)
qqnorm(farmerData$ggavgpriceKG)
qqline(farmerData$ggavgpriceKG) 

##Total income from cocoa
hist(farmerData$ggincomecocoa)
qqnorm(farmerData$ggincomecocoa)
qqline(farmerData$ggincomecocoa) 
farmerData$ggincomecocoa[farmerData$ggincomecocoa>quantile(farmerData$ggincomecocoa,.99, na.rm=TRUE)]<-NA
farmerData$ggincomecocoalog <-log(farmerData$ggincomecocoa+1)
hist(farmerData$ggincomecocoalog) 
qqnorm(farmerData$ggincomecocoalog)
qqline(farmerData$ggincomecocoalog)

##Total income from other crops
hist(farmerData$ggincomeothcrops)
qqnorm(farmerData$ggincomeothcrops)
qqline(farmerData$ggincomeothcrops) 
farmerData$ggincomeothcrops[farmerData$ggincomeothcrops>quantile(farmerData$ggincomeothcrops,.99, na.rm=TRUE)]<-NA
farmerData$ggincomeothcropslog <-log(farmerData$ggincomeothcrops+1)
hist(farmerData$ggincomeothcropslog) 
qqnorm(farmerData$ggincomeothcropslog)
qqline(farmerData$ggincomeothcropslog)

##Total crop income
hist(farmerData$ggincomecropstot)
qqnorm(farmerData$ggincomecropstot)
qqline(farmerData$ggincomecropstot) 
farmerData$ggincomecropstot[farmerData$ggincomecropstot>quantile(farmerData$ggincomecropstot,.99, na.rm=TRUE)]<-NA
farmerData$ggincomecropstotlog <-log(farmerData$ggincomecropstot)
hist(farmerData$ggincomecropstotlog) 
qqnorm(farmerData$ggincomecropstotlog)
qqline(farmerData$ggincomecropstotlog)

##Total crop income per capita
hist(farmerData$ggincomecropstotcap)
qqnorm(farmerData$ggincomecropstotcap)
qqline(farmerData$ggincomecropstotcap) 
farmerData$ggincomecropstotcap[farmerData$ggincomecropstotcap>quantile(farmerData$ggincomecropstotcap,.99, na.rm=TRUE)]<-NA
farmerData$ggincomecropstotcaplog <-log(farmerData$ggincomecropstotcap+1)
hist(farmerData$ggincomecropstotcaplog) 
qqnorm(farmerData$ggincomecropstotcaplog)
qqline(farmerData$ggincomecropstotcaplog)

##Income from other agro businesses
hist(farmerData$ggincomeotheragro)
qqnorm(farmerData$ggincomeotheragro)
qqline(farmerData$ggincomeotheragro) 
farmerData$ggincomeotheragro[farmerData$ggincomeotheragro>quantile(farmerData$ggincomeotheragro,.99, na.rm=TRUE)]<-NA
farmerData$ggincomeotheragrolog <-log(farmerData$ggincomeotheragro+1)
hist(farmerData$ggincomeotheragrolog) 
qqnorm(farmerData$ggincomeotheragrolog)
qqline(farmerData$ggincomeotheragrolog)

##Income from non-agro businesses
hist(farmerData$ggincomeothernonagro)
qqnorm(farmerData$ggincomeothernonagro)
qqline(farmerData$ggincomeothernonagro) 
farmerData$ggincomeothernonagro[farmerData$ggincomeothernonagro>quantile(farmerData$ggincomeothernonagro,.99, na.rm=TRUE)]<-NA
farmerData$ggincomeothernonagrolog <-log(farmerData$ggincomeothernonagro+1)
hist(farmerData$ggincomeothernonagrolog) 
qqnorm(farmerData$ggincomeothernonagrolog)
qqline(farmerData$ggincomeothernonagrolog)

##Income from remittances
hist(farmerData$ggincomeremit)
qqnorm(farmerData$ggincomeremit)
qqline(farmerData$ggincomeremit) 
farmerData$ggincomeremit[farmerData$ggincomeremit>quantile(farmerData$ggincomeremit,.99, na.rm=TRUE)]<-NA
farmerData$ggincomeremitlog <-log(farmerData$ggincomeremit+1)
hist(farmerData$ggincomeremitlog) 
qqnorm(farmerData$ggincomeremitlog)
qqline(farmerData$ggincomeremitlog)

##Total non-crop income
hist(farmerData$ggincometotnoncrop)
qqnorm(farmerData$ggincometotnoncrop)
qqline(farmerData$ggincometotnoncrop) 
farmerData$ggincometotnoncrop[farmerData$ggincometotnoncrop>quantile(farmerData$ggincometotnoncrop,.99, na.rm=TRUE)]<-NA
farmerData$ggincometotnoncroplog <-log(farmerData$ggincometotnoncrop+1)
hist(farmerData$ggincometotnoncroplog) 
qqnorm(farmerData$ggincometotnoncroplog)
qqline(farmerData$ggincometotnoncroplog)

##Total Income
hist(farmerData$ggincometotal)
qqnorm(farmerData$ggincometotal)
qqline(farmerData$ggincometotal) 
farmerData$ggincometotal[farmerData$ggincometotal>quantile(farmerData$ggincometotal,.99, na.rm=TRUE)]<-NA
farmerData$ggincometotallog <-log(farmerData$ggincometotal+1)
hist(farmerData$ggincometotallog) 
qqnorm(farmerData$ggincometotal)
qqline(farmerData$ggincometotal)

##Total Income per cap
hist(farmerData$ggincometotalcap)
qqnorm(farmerData$ggincometotalcap)
qqline(farmerData$ggincometotalcap) 
farmerData$ggincometotalcap[farmerData$ggincometotalcap>quantile(farmerData$ggincometotalcap,.99, na.rm=TRUE)]<-NA
farmerData$ggincometotalcaplog <-log(farmerData$ggincometotalcap+1)
hist(farmerData$ggincometotalcaplog) 
qqnorm(farmerData$ggincometotalcaplog)
qqline(farmerData$ggincometotalcaplog)

##Hours of labor per child under 18
hist(farmerData$hoursPerChildUnder18)
qqnorm(farmerData$hoursPerChildUnder18)
qqline(farmerData$hoursPerChildUnder18) 
farmerData$hoursPerChildUnder18[farmerData$hoursPerChildUnder18>quantile(farmerData$hoursPerChildUnder18,.99, na.rm=TRUE)]<-NA

##Hours of labor per child under 15
hist(farmerData$hoursPerChildUnder15)
qqnorm(farmerData$hoursPerChildUnder15)
qqline(farmerData$hoursPerChildUnder15) 
farmerData$hoursPerChildUnder15[farmerData$hoursPerChildUnder15>quantile(farmerData$hoursPerChildUnder15,.99, na.rm=TRUE)]<-NA

##Hours of labor per child under 12
hist(farmerData$hoursPerChildUnder12)
qqnorm(farmerData$hoursPerChildUnder12)
qqline(farmerData$hoursPerChildUnder12) 
farmerData$hoursPerChildUnder12[farmerData$hoursPerChildUnder12>quantile(farmerData$hoursPerChildUnder12,.99, na.rm=TRUE)]<-NA



# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #
#																		#
# 		4. CREATES A VILLAGE-LEVEL DATASET WITH KEY INDICATORS 		   	#
#																	   	#
# This block of code aggregates the farmer data to the village level. 	#
# The dataset contains basic information on each village, as well as 	#
# data on a range of key indicators as measured in wave 2 (average HH 	#
# size, average cocoa income, average cocoa price/kg, ...). 			#
# 																		#
# NOTE TO SELF: see notes from meeting with Jens for a few non-trivial 	#
# changes I need to make to this section. 								#
#																	   	#
# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#

# NOTE TO SELF: there are 20 interviews in wave 1 without a newloccode. This needs to be looked into.
table(farmerData$newloccode=="")

# aggregates the number of interviews per village (as well as which region each village is in).
result1 <- ddply(farmerData, .(newloccode, surveyWave), summarize, ggnumberofinterviews = length(na.omit(newloccode)), ggashanti = ggashanti[1], ggbrong = ggbrong[1], ggcentral = ggcentral[1], ggeastern = ggeastern[1], ggwestern = ggwestern[1])
# result1 <- result1[order(result1$newloccode, result1$surveyWave),]

# prints a table of the number of villages with a given number of interviews, divided by survey wave.
table(result1$ggnumberofinterviews, result1$surveyWave)

# NOTE TO SELF: there are a number of villages with only 1 or 2 interviews in each survey wave, as well as a few villages with 17 or more interviews. Looking at the raw wave 2 data, some of the villages with 1 or 2 interviews only had 1 or 2 interviews going in. Others may have had 5 or 6 interviews, but a few of these interviews were incomplete, duplicates, or dropped for some other reason. 
# later in the code, I drop any remaining villages with 2 or fewer interviews.

# aggregates the key indicators to the village level by taking the mean across farmer observations in each village.
keyIndicatorList <- c("surveyWave","newloccode", "gghhsize","ggagehhhead","ggschoolinghhhead","ggmeanage", "gggenderhhhead","ggsavings", "ggnofarms","ggfsizeacres","ggfsizecocoaacres","ggbuyers","ggharvestcocoaKG", "ggyieldall_cocarea","ggyieldall_allarea","ggfraclost","ggsoldcocoaKG","ggfractionsold","ggincomecocoa","ggincomeothcrops","ggincomecropstot","ggincomecropstotcap","ggincomeotheragro","ggincomeothernonagro","ggincomeremit","ggincometotnoncrop","ggincometotal","ggincometotalcap","ggeverschoolhhhead","gggenderhhhead","ggliterate","ggtraining","ggreceivedloan","ggbank","ggorggroup","ggcar","ggchainsaw","ggmistblower","gghhhwives","ggborninvillage","ggelectric","gghealthinsurance","ggcontactedleader","gginvestment","ggmalaria","ggtertiary","ggcertifiedfarmerorg","ggpolitics","ggfinancialassistance","gginfo","gglegaltitle","ggsharecrop","ggwouldlosefallow", "ggschoolinghhheadlog", "ggmeanagelog", "ggsavingslog", "ggfsizeacreslog", "ggfsizecocoaacreslog","ggharvestcocoaKGlog", "ggyieldall_allarealog", "ggyieldall_cocarealog", "ggsoldcocoaKGlog", "ggincomecocoalog", "ggincomeothcropslog", "ggincomecropstotlog", "ggincomecropstotcaplog", "ggincomeotheragrolog", "ggincomeothernonagrolog","ggincomeremitlog", "ggincometotnoncroplog", "ggincometotallog", "ggincometotalcaplog", "ggavgpriceKG","ggknowsft", "ggorganic", "ggdumfert", "gginsect", "ggherb", "ggfungi", "ggorg", "ggseedpods", "ggseedling", "ggknap", "ggmist", "ggtraining", "ggorggroup", "ggcertifiedfarmerorg", "gginvestment", "childLaborAny","childLaborUnder18", "childLaborUnder15", "childLaborUnder12", "hoursPerChildUnder18", "hoursPerChildUnder15", "hoursPerChildUnder12", "childLaborAnyOutside","childLaborUnder18Outside", "childLaborUnder15Outside", "childLaborUnder12Outside", "hoursPerChildUnder18Outside", "hoursPerChildUnder15Outside", "hoursPerChildUnder12Outside", "hoursWorkedPerFemale", "propFemaleCocoaLabor")

result2 <- ddply(farmerData[,keyIndicatorList], .(newloccode,surveyWave), colwise(mean, na.rm=T))

# merges result1 and result2 to create a combined village-level dataset for wave 2.
villageData <- merge(result1, result2, by=c("newloccode", "surveyWave"))
dim(villageData)
rm(result1, result2)

# Note to self: need to check where NAs are coming from here and whether they reflect some kind of error. Also need to randomly check a few means to double-check that things have worked properly.


# ------------ # ------------
# Merges wave 2 data with sampling frame information and wave 1 key indicators
# ------------

# reads in sampling frame and wave 1 key indicator data.
# NOTE: in order to read the file into R, I created a copy of the sampling frame and saved 
# it as a stata 12 version. The copy is "updated_sampling_frame_data_v7_copySTATA12.dta" 
# and the original is "updated_sampling_frame_data_v7.dta".
sampFrame <- read.dta(paste0(bmPath,"/data/samplingframe/updated_sampling_frame_data_v7_copySTATA12.dta", space=""))
dim(sampFrame); summary(sampFrame)
names(sampFrame)

# drops all wave 1 key indicators from sampling frame:
sampFrame <- sampFrame[,1:which(colnames(sampFrame)=="wave1_replaced")]

# merges wave2Data with sampFrame dataframe.
data <- merge(villageData, sampFrame, by = "newloccode")
dim(data)

# stores the data in panel format
data <- plm.data(data, indexes=c("newloccode", "surveyWave"))

# separates the data into the wave 2 CCP cohort (experimental study) and the wave 1 CCP cohort (observational study)
expData <- data[!is.na(data$wave2_experiment_pair) & data$surveyWave==2,]
obsData <- data[is.na(data$wave2_experiment_pair),]
dim(expData)
dim(obsData)

# informally checks that the two waves are properly separated.
table(expData$wave1_treated_ccp==1) # should all be false.
table(obsData$wave1_surveyed==1) # should all be true... but there are 35 FALSE. What is going on here? Need to check this.

# checks how many treated observations in each study. 
table(expData$wave2_experiment_assigned) 
table(obsData$wave1_treated_ccp[obsData$surveyWave==1]) # wave 1: 94 treated, 225 control.
table(obsData$wave1_treated_ccp[obsData$surveyWave==2]) # wave 2: 90 treated, 218 control.


# ------------ # ------------
# Manually checks that the merge worked correctly for the experimental villages
# ------------ 

# Creates a dataset of just experimental villages from the sampling frame itself (d1) and a dataset of just experimental villages from the merged data (d2) in order to manually check the merge.
d1 <- sampFrame[!is.na(sampFrame$wave2_experiment_assigned),c("newloccode", "wave2_clean_localityname", "wave2_experiment_pair", "wave2_experiment_assigned")]
d1 <- d1[order(d1$wave2_experiment_pair, d1$newloccode),]

d2 <- data[!is.na(data$wave2_experiment_assigned),c("newloccode", "wave2_clean_localityname", "wave2_experiment_pair", "wave2_experiment_assigned", "surveyWave"),]
# NOTE TO SELF: about half of the experimental villages were surveyed in wave 1, so I could also use these in balance checks. 
table(d2$surveyWave, d2$wave2_experiment_assigned)
d2 <- d2[d2$surveyWave==2,]
d2 <- d2[order(d2$wave2_experiment_pair, d2$newloccode),]

d1
d2
rm(d1,d2)
# NOTE: I've manually checked that each of the 36 experimental villages that we have in the final merged data to be sure that they have the correct pair number, treatment assignment, and name as in the sampling frame. 


# ------------ # ------------
# Drops 4 blocks in the experimental data because only one of two villages was re-interviewed.
# ------------ 

# Of 48 villages paired into 24 blocks, 12 villages were not surveyed in wave 2. This leaves 16 blocks (32 villages) where both villages were surveyed in wave 2 and 4 blocks (4 villages) in which only one village in the block was surveyed in wave 2. For the analysis of the randomized experiment, we only examine those blocks where both villages in the block were surveyed (for a total of 32 villages).

# this table sshow which blocks are missing a village in wave 2. 
table(expData$wave2_experiment_pair, expData$wave2_experiment_assigned)

# Drops the villages in the 4 blocks where only one of two villages was surveyed in wave 2 (blocks 19, 53, 59, and 62).
blocksToDrop <- c(19,53,59,62)
expData <- expData[!expData$wave2_experiment_pair %in% blocksToDrop,]
nVillages <- length(levels(as.factor(expData$newloccode)))


# ------------ # ------------
# Manually checks that the merge worked correctly for the observational villages 
# (i.e. checks that the treated villages are also tagged as treated villages in the sampling frame and that the control villages are also tagged as control villages in the sampling frame.)
# ------------ 

# Checks that all newloccodes in the treatment group are also in the treatment group in the sampling frame.
setdiff(unique(obsData$newloccode[obsData$wave1_treated_ccp==1]), unique(sampFrame$newloccode[sampFrame$wave1_treated_ccp==1])) # (should return empty vector)

# checks that all newloccodes in the control group are also in the control group in the sampling frame.
setdiff(unique(obsData$newloccode[obsData$wave1_treated_ccp==0]), unique(sampFrame$newloccode[sampFrame$wave1_treated_ccp==0])) # (should return empty vector)


# ----------- # -----------
# Creates the dataset of the 90 treated villages and their corresponding matched villages within the geographical caliper:
# -----------

# Of the 308 villages covered in waves 1 and 2, we first want to compare the 90 treated villages to each of their corresponding matched villages within the geographical caliper. Hence, here I restrict the observational data to contain only the treated villages and their matched villages within the geographical caliper.
	
# checks that every observation in obsData is either tagged as treated or control.
length(obsData$wave1_treated_ccp[obsData$wave1_treated_ccp==1]) + length(obsData$wave1_treated_ccp[obsData$wave1_treated_ccp==0]) == dim(obsData)[1] # (should return TRUE)

# replaces wave1_newloccode_co=="" with NA (note: wave1_newloccode_co contains the newloccode of the control match for a treated village)
obsData$wave1_newloccode_co[obsData$wave1_newloccode_co==""] <- NA

# divides the data into treated villages and control villages
obsDataTreatVill <- obsData[obsData$wave1_treated_ccp==1,] 		# n=184
obsDataControlVill <- obsData[obsData$wave1_treated_ccp==0,]	# n=443

# -----------
# a. removes four treated villages that do NOT have a control match.
hasNoControlMatch <- obsDataTreatVill$newloccode[is.na(obsDataTreatVill$wave1_newloccode_co)]
hasNoControlMatch <- unique(hasNoControlMatch) 
hasNoControlMatch # these 4 treated villages do not have a control match. 

# drops the 4 treated villages that do not have a control match, leaving us with 86 treated villages.
obsDataTreatVill <- obsDataTreatVill[!(obsDataTreatVill$newloccode %in% hasNoControlMatch),]

# Note to self: Why don't these treated villages have a control match? Even just looking at the original sampling frame, these treated villages do not have a matched control village. Ask Jens what is going on here.

# Note: at this point in the code, we have 86 treatment villages in wave 2, each of which has a control village match in the sampling frame. 


# -----------
# b. extracts the vector of newloccodes for the control villages (within the geographical caliper) that are matched to each of the 86 treated village. 

# for each control village, prints the number of treated villages that are matched to it.
numControlVillToTreated <- aggregate(obsDataTreatVill$newloccode[obsDataTreatVill$surveyWave==2], by=list(obsDataTreatVill$wave1_newloccode_co[obsDataTreatVill$surveyWave==2]), FUN=function(x) length(unique(x))) # n=84 villages. This makes sense, since there are 86 treatment villages and 2 of them share a control village that has already been matched to a treatment village.
# Note: the above command is run only on wave 2 observations since we will only be using villages that have wave 1 and wave 2 data.

# lists the newloccode for any treated villages that share the same control match.
numControlVillToTreated[numControlVillToTreated[,2]>1,]
# two control villages are each matched to two treated villages. Newloccodes for these two control villages: 105001948010, 504013400013.

# finds the newloccodes of the pairs of treated villages that share a control village.
obsDataTreatVill[obsDataTreatVill$wave1_newloccode_co=="105001948010" | obsDataTreatVill$wave1_newloccode_co=="504013400013", c("newloccode", "wave1_treated_ccp", "surveyWave")]
# the newloccodes for the four treated villages sharing a pair of control villages are: 105001744001, 105001948007, 503007400007, 507004400004.

# NOTE TO SELF: Is there supposed to be matching with replacement, or does this reflect an error somewhere? Ask Jens. For now, I still include these villages in the analysis. Note that this issue with multiple treatment villages sharing a control village appears in the sampling frame, so it should not be an issue with this R code. 
	
# -----------
# c. keeps only those 84 control villages that are matched to a treatment village.

# retrieves the list of 84 control villages that are matched to treatment villages surveyed in wave 2.
matchedControlVillages <- unique(obsDataTreatVill$wave1_newloccode_co[obsDataTreatVill$surveyWave==2]) 

# Of these 84 control villages, lists the newloccodes that do not appear in the data. (also double-checks that these do at least appear in the sampling frame). 
setdiff(matchedControlVillages, unique(data$newloccode)) # 22 missing control villages in the full data
setdiff(matchedControlVillages, unique(obsDataControlVill$newloccode)) # 33 missing in the control data
setdiff(matchedControlVillages, unique(sampFrame$newloccode)) # none missing in the sampling frame

# lists all 62 of the 84 control villages that appear in the full dataset, along with wave2 experiment assignment. This shows us that there are a handful of "control" villages that are in the experiment and get assigned to treatment in the experiment. Note that these oddities also appear in the sampling frame. 
data[data$newloccode %in% matchedControlVillages ,c("newloccode", "wave2_experiment_assigned","wave1_treated_ccp", "wave2_experiment_pair")]
# Note to self: there are a handful of control villages that are in one of the experimental pairs. In addition, about half of these are villages that were assigned to treatment in the experiment. For now, these villages are simply left out of the observational data analysis. 

# drops all control villages except for the 84 that are matched to a treatment village, leaving 51 control villages.
obsDataControlVill2 <- obsDataControlVill[obsDataControlVill$newloccode %in% matchedControlVillages,]
dim(obsDataControlVill2)
unique(obsDataControlVill2$newloccode[obsDataControlVill2$surveyWave==2])
# NOTE TO SELF: there are a large number of control villages that were not surveyed in the follow-up. We are only left with 51 of 84 matched control villages. All 51 of these villages were surveyed in wave 1 and wave 2. Of these 33 villages, 10 are in the experiment. Another 1 of the villages is actually marked as a treated village in the obs data. The other 22 missing villages do not appear in the raw wave 2 data either, so it is not a problem with the code. 

# NOTE TO SELF: There are 35 villages in the sampling frame that were not assigned to CCP treatment but did receive treatment from some other program. None of these 35 villages are in this list of 84 control villages. 
table(obsDataControlVill2$wave1_treated_ccp, obsDataControlVill2$wave1_treated_agroeco_cargill) 
setdiff(unique(sampFrame$newloccode[sampFrame$wave1_treated_agroeco_cargill==1]), matchedControlVillages) # returns 35 newloccodes, showing no overlap between the sets.

# similarly, none of these 35 villages are in the experimental dataset.
table(expData$wave1_treated_agroeco_cargill)

# -----------
# d. keeps only those treatment villages that have a matched control village in wave 2.
obsDataTreatVill2 <- obsDataTreatVill[obsDataTreatVill$wave1_newloccode_co %in% obsDataControlVill2$newloccode,] 
dim(obsDataTreatVill2)

# Note to self: this cuts down the number of treated villages from 86 to 54.
unique(obsDataTreatVill2$newloccode)


# -----------
# e. keeps only those villages that were surveyed in both waves 1 and 2.

table(obsDataControlVill2$newloccode, obsDataControlVill2$surveyWave) # all 51 control villages appear in waves 1 and 2.
table(obsDataTreatVill2$newloccode, obsDataTreatVill2$surveyWave) # 51 of 54 treatment villages appear in waves 1 and 2.

# drops the 3 treatment villages that only appear in waves 1 OR 2.
villsToDrop <- obsDataTreatVill2$newloccode[!obsDataTreatVill2$newloccode %in% obsDataTreatVill2$newloccode[obsDataTreatVill2$surveyWave==2] | !obsDataTreatVill2$newloccode %in% obsDataTreatVill2$newloccode[obsDataTreatVill2$surveyWave==1]] # newloccodes of villages to drop: 508704400704, 508001400001, 503001756001.
obsDataTreatVill2 <- obsDataTreatVill2[!obsDataTreatVill2$newloccode %in% villsToDrop,]

# NOTE TO SELF: 2 of these dropped villages only appear in wave 2. This is odd, since both have wave2_in1stwave==1 and there should not be any non-experimental villages in wave 2 that were not surveyed in wave 1. Need to look into what is going on here. Were these 2 villages dropped in wave 1 for some reason? 

# -----------
# f. appends the remaining treated villages with the remaining control villages and stores them in a dataframe for analysis.
obsDataAnalysis <- rbind(obsDataTreatVill2, obsDataControlVill2)
dim(obsDataAnalysis)
# Note: this leaves us with a total of 102 villages. 	

# stores the number of treated and control villages. 
nTreatedVillages <- length(obsDataAnalysis$newloccode[obsDataAnalysis$surveyWave==2 & obsDataAnalysis$wave1_treated_ccp==1])
 # n=51
nControlVillages <- length(obsDataAnalysis$newloccode[obsDataAnalysis$surveyWave==2 & obsDataAnalysis$wave1_treated_ccp==0]) # n=51

# stores the number of villages
nVillages <- nTreatedVillages + nControlVillages

# --------------------------------------------------------------------- #
# drops any villages with less than 2 interviews.

# prints a table of the number of villages with a given number of interviews, divided by survey wave.
addmargins(table(as.character(expData$newloccode), expData$ggnumberofinterviews)) # expData
addmargins(table(as.character(obsDataAnalysis$newloccode), obsDataAnalysis$ggnumberofinterviews)) # obsData
addmargins(table(as.character(obsDataAnalysis$newloccode[obsDataAnalysis$ggnumberofinterviews < 3]), obsDataAnalysis$ggnumberofinterviews[obsDataAnalysis$ggnumberofinterviews < 3])) # obsData

# drops any villages with less than 2 interviews.
# expData <- expData[expData$ggnumberofinterviews < 3]
# obsDataAnalysis <- obsDataAnalysis[obsDataAnalysis$ggnumberofinterviews < 3]

# NOTE TO SELF: come back to obsDataAnalysis later. It's not as simple as just dropping 
# villages with less than 3 observations. I need to make sure to drop the village in both 
# waves and to drop its match. Maybe I should just run this earlier in the code. 

# --------------------------------------------------------------------- #
# checks the difference in the number of interviews in a village between wave 1 and wave 2 
# for the observational villages (not relevant for experimental villages since they were 
# only surveyed in wave 2).

# transforms obsData to wide format.
obsDataAnalysisWide <- reshape(data=obsDataAnalysis, timevar="surveyWave", idvar=c("newloccode"), direction="wide")

# creates a matrix showing the number of interviews in each wave for observational villages
intDiffObsData <- cbind(as.numeric(as.character(obsDataAnalysisWide$newloccode)), obsDataAnalysisWide$ggnumberofinterviews.1, obsDataAnalysisWide$ggnumberofinterviews.2, obsDataAnalysisWide$ggnumberofinterviews.2 - obsDataAnalysisWide$ggnumberofinterviews.1)
intDiffObsData[order(intDiffObsData[,4]),]

# NOTE TO SELF: the largest difference is 10 interviews and there are a few additional 
# villages with a difference of 7, 8, or 9 interviews between waves. Is there a cut-off
# we want to use to drop any of these villages (e.g. drop villages with difference larger 
# than 5)? If we decide to drop any of these villages, we will want to move this code block
# earlier, since dropping any village means we also need to drop its treatment/control 
# pairing. The above code will take care of that so that we don't have to write duplicate 
# code.


# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #
# 		CREATES HH-LEVEL DATASETS TO BE USED IN THE ANALYSIS 			#
# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

# stores the newloccodes for the experimental villages and non-randomized villages
# to be used in the analysis.
expNewloccodes <- expData$newloccode
obsNewloccodes <- unique(obsDataAnalysis$newloccode) # used unique because there are two obs for each village

# Subsets the HH data to the 32 experimental villages to be included in the analysis.
expDataHH <- farmerData[farmerData$newloccode %in% expNewloccodes & farmerData$surveyWave==2,]

# merges treatment assignment and treatment block to the HH data.
expDataHH <- join(expDataHH, expData[,c("newloccode", "wave2_experiment_assigned", "wave2_experiment_pair")], by="newloccode", type="left", match="all")

# Subsets the HH data to the 102 non-randomized villages to be included in the analysis.
obsDataHH <- farmerData[farmerData$newloccode %in% obsNewloccodes,]

# merges treatment assignment to the HH data.
obsDataHH <- join(obsDataHH, obsDataAnalysis[,c("newloccode", "wave1_treated_ccp", "surveyWave")], by=c("newloccode", "surveyWave"), type="left", match="all")


# --------------------------------------------------------------------- #
# checks that the subset and merge worked as expected.

# sum of number of interviews in expData should equal number of rows in expDataHH
sum(expData$ggnumberofinterviews) == dim(expDataHH)[1] # should return true
table(expDataHH$newloccode, expDataHH$wave2_experiment_assigned)

# sum of number of interviews in obsDataHH should equal number of rows in obsDataAnalysis
sum(obsDataAnalysis$ggnumberofinterviews) == dim(obsDataHH)[1] # should return true
table(obsDataHH$newloccode, obsDataHH$surveyWave)

# Number of villages in expDataHH should be 32 and newloccodes should exactly match expData
length(unique(expDataHH$newloccode)) # should return 32
setequal(expDataHH$newloccode,expData$newloccode) # should be true.

# checks that newloccodes of treated villages match exactly in each dataset.
length(unique(expDataHH$newloccode[expDataHH$wave2_experiment_assigned==1])) # should return 16
setequal(expDataHH$newloccode[expDataHH$wave2_experiment_assigned==1], expData$newloccode[expData$wave2_experiment_assigned==1]) # should be true.

# checks that newloccodes of control villages match exactly in each dataset.
length(unique(expDataHH$newloccode[expDataHH$wave2_experiment_assigned==0])) # should return 16
setequal(expDataHH$newloccode[expDataHH$wave2_experiment_assigned==0], expData$newloccode[expData$wave2_experiment_assigned==0]) # should be true.

# checks that for each experiment pair number the newloccodes match exactly
for (i in 1:length(unique(expDataHH$wave2_experiment_pair))){
	pairNum <- unique(expDataHH$wave2_experiment_pair)[i]
	print(setequal(expDataHH$newloccode[expDataHH$wave2_experiment_pair==pairNum], expData$newloccode[expData$wave2_experiment_pair==pairNum]))
} # should return TRUE 16 times


# Number of villages in obsDataHH should be 102 and newloccodes should exactly match 
# obsDataAnalysis
length(unique(obsDataHH$newloccode[obsDataHH$surveyWave==1])) # should return 102
length(unique(obsDataHH$newloccode[obsDataHH$surveyWave==2])) # should return 102
setequal(obsDataHH$newloccode,obsDataAnalysis$newloccode) # should be true.
setequal(unique(obsDataHH$newloccode[obsDataHH$surveyWave==1]),unique(obsDataHH$newloccode[obsDataHH$surveyWave==2])) # should be true.

# checks that newloccodes of treated villages match exactly in each dataset.
length(unique(obsDataHH$newloccode[obsDataHH$wave1_treated_ccp==1])) # should return 51
setequal(obsDataHH$newloccode[obsDataHH$wave1_treated_ccp==1], obsDataAnalysis$newloccode[obsDataAnalysis$wave1_treated_ccp==1]) # should be true.

# checks that newloccodes of control villages match exactly in each dataset.
length(unique(obsDataHH$newloccode[obsDataHH$wave1_treated_ccp==0])) # should return 51
setequal(obsDataHH$newloccode[obsDataHH$wave1_treated_ccp==0], obsDataAnalysis$newloccode[obsDataAnalysis$wave1_treated_ccp==0]) # should be true.

# --------------------------------------------------------------------- #


# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #


# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #
# 	5. EXPORTS TWO CSVs CONTAINING THE FINALIZED OBSERVATIONAL AND 		#
#  	   EXPERIMENTAL DATASETS. 											#
# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

# Saves the analysis dataframes to csv.
setwd(paste0(globalPath,"/data/2ndwave/Code/cleanedDatasets", space=""))
write.csv(expData, file="expDataForanalysis_village.csv")
write.csv(obsDataAnalysis, file="obsDataForAnalysis_village.csv")
write.csv(expDataHH, file="expDataForanalysis_HH.csv")
write.csv(obsDataHH, file="obsDataForAnalysis_HH.csv")


# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#
# 				END OF WAVE 2 DATA ANALYSIS FILE					   	#
# ---------------------------------------------------------------------	#
# ---------------------------------------------------------------------	#
