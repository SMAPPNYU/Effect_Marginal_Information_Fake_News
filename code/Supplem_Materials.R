
#Analysis - Testing hypotheses from second pre-registration

#Load in libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)
library(Rmisc)
library(plm)
library(lmtest)
library(sandwich)
library(rsq)
library(stargazer)
library(xtable)
library(irr)

############################# Load Data ###################################################


#Pull in this data: Search Experiment 1: Study 1:
Misl_False_Search_MF <- read_csv('.//data//MF_Search_Exp_Misl_False.csv',
                                 col_types = cols(
                                   .default = col_character()
                                 ))



Misl_False_Search_MF$Match_FC <- ifelse(Misl_False_Search_MF$Evaluation == 'FM',1,0)
Misl_False_Search_MF$Match_FC <- ifelse(Misl_False_Search_MF$Evaluation == 'Misl/False',1,Misl_False_Search_MF$Match_FC)

Misl_False_Search_MF$Susc_FN <- ifelse(Misl_False_Search_MF$Evaluation == 'T',1,0)
Misl_False_Search_MF$Susc_FN <- ifelse(Misl_False_Search_MF$Evaluation == 'True',1,Misl_False_Search_MF$Susc_FN)

Misl_False_Search_T <- read_csv('.//data//T_Search_Exp_Misl_False.csv',
                                col_types = cols(
                                  .default = col_character()
                                ))



Misl_False_Search_T$Match_FC <- ifelse(Misl_False_Search_T$Evaluation == 'T',1,0)
Misl_False_Search_T$Match_FC <- ifelse(Misl_False_Search_T$Evaluation == 'True',1,Misl_False_Search_T$Match_FC)

Misl_False_Search_T$Susc_FN <- ifelse(Misl_False_Search_T$Evaluation == 'FM',1,0)
Misl_False_Search_T$Susc_FN <- ifelse(Misl_False_Search_T$Evaluation == 'Misl/False',1,Misl_False_Search_T$Susc_FN)

#Pull in this data: Search Experiment 2: Study 1:
Data_Bef_Aft_MF <- read_csv('.//data//Data_Bef_Aft_Misl_False.csv',
                            col_types = cols(
                              .default = col_character()
                            ))



Data_Bef_Aft_MF$Match_FC <- ifelse(Data_Bef_Aft_MF$Evaluation == 'FM',1,0)
Data_Bef_Aft_MF$Match_FC <- ifelse(Data_Bef_Aft_MF$Evaluation == 'Misl/False',1,Data_Bef_Aft_MF$Match_FC)

Data_Bef_Aft_MF$Susc_FN <- ifelse(Data_Bef_Aft_MF$Evaluation == 'T',1,0)
Data_Bef_Aft_MF$Susc_FN <- ifelse(Data_Bef_Aft_MF$Evaluation == 'True',1,Data_Bef_Aft_MF$Susc_FN)

#Pull in this data: Search Experiment 2: Study 1:
Data_Bef_Aft_T <- read_csv('.//data//Data_Bef_Aft_True.csv',
                           col_types = cols(
                             .default = col_character()
                           ))

Data_Bef_Aft_T$Match_FC <- ifelse(Data_Bef_Aft_T$Evaluation == 'T',1,0)
Data_Bef_Aft_T$Match_FC <- ifelse(Data_Bef_Aft_T$Evaluation == 'True',1,Data_Bef_Aft_T$Match_FC)

Data_Bef_Aft_T$Susc_FN <- ifelse(Data_Bef_Aft_T$Evaluation == 'FM',1,0)
Data_Bef_Aft_T$Susc_FN <- ifelse(Data_Bef_Aft_T$Evaluation == 'Misl/False',1,Data_Bef_Aft_T$Susc_FN)



#Data from False/Misleading Articles from Study 2 and Experiment 2:
Data_Bef_Aft_Covid <- read_csv('.//data//Experiment_2_Study_2_Misl_False.csv',
                               col_types = cols(
                                 .default = col_character()
                               ))


Data_Bef_Aft_Covid$Match_FC <- ifelse(Data_Bef_Aft_Covid$Evaluation == 'FM',1,0)
Data_Bef_Aft_Covid$Match_FC <- ifelse(Data_Bef_Aft_Covid$Evaluation == 'Misl/False',1,Data_Bef_Aft_Covid$Match_FC)

Data_Bef_Aft_Covid$Susc_FN <- ifelse(Data_Bef_Aft_Covid$Evaluation == 'T',1,0)
Data_Bef_Aft_Covid$Susc_FN <- ifelse(Data_Bef_Aft_Covid$Evaluation == 'True',1,Data_Bef_Aft_Covid$Susc_FN)

#Data from False/Misleading Articles from Study 2 and Experiment 2:
Data_Bef_Aft_Covid_T <- read_csv('.//data//Experiment_2_Study_2_T.csv',
                                 col_types = cols(
                                   .default = col_character()
                                 ))


Data_Bef_Aft_Covid_T$Match_FC <- ifelse(Data_Bef_Aft_Covid_T$Evaluation == 'T',1,0)
Data_Bef_Aft_Covid_T$Match_FC <- ifelse(Data_Bef_Aft_Covid_T$Evaluation == 'True',1,Data_Bef_Aft_Covid_T$Match_FC)

Data_Bef_Aft_Covid_T$Susc_FN <- ifelse(Data_Bef_Aft_Covid_T$Evaluation == 'FM',1,0)
Data_Bef_Aft_Covid_T$Susc_FN <- ifelse(Data_Bef_Aft_Covid_T$Evaluation == 'Misl/False',1,Data_Bef_Aft_Covid_T$Susc_FN)




Misl_False_Pay <- read_csv('.//data//Paid_Survey_Data_FM.csv',
                           col_types = cols(
                             .default = col_character()
                           ))



Misl_False_Pay$Match_FC <- ifelse(Misl_False_Pay$Evaluation == 'FM',1,0)
Misl_False_Pay$Susc_FN <- ifelse(Misl_False_Pay$Evaluation == 'T',1,0)

True_Pay <- read_csv('.//data//Paid_Survey_Data_T.csv',
                     col_types = cols(
                       .default = col_character()
                     ))


True_Pay$Match_FC <- ifelse(True_Pay$Evaluation == 'T',1,0)
True_Pay$Susc_FN <- ifelse(True_Pay$Evaluation == 'FM',1,0)




Misl_False_Pay$Dummy_Incongruence <- ifelse(Misl_False_Pay$lean == 'L' & Misl_False_Pay$Ideology_Score > 0,1,0)
Misl_False_Pay$Dummy_Incongruence <- ifelse(Misl_False_Pay$lean == 'C' & Misl_False_Pay$Ideology_Score < 0,1,Misl_False_Pay$Dummy_Incongruence)
Misl_False_Pay$Dummy_Incongruence <- ifelse(is.na(Misl_False_Pay$Ideology_Score),NA,Misl_False_Pay$Dummy_Incongruence)


Misl_False_Pay$Congruent_Word <- ifelse(Misl_False_Pay$Dummy_Incongruence == 1,'Incongruent','Moderate')
Misl_False_Pay$Congruent_Word <- ifelse(Misl_False_Pay$Dummy_Congruence == 1,'Congruent',Misl_False_Pay$Congruent_Word)
Misl_False_Pay$Congruent_Word <- ifelse(is.na(Misl_False_Pay$Ideology_Score),NA,Misl_False_Pay$Congruent_Word)



Experiment_3_Data <- read_csv('.//data//Exp_3_Data.csv',
                              col_types = cols(
                                .default = col_character()
                              ))


Experiment_3_Data$Match_FC <- ifelse(Experiment_3_Data$Evaluation == 'FM',1,0)
Experiment_3_Data$Susc_FN <- ifelse(Experiment_3_Data$Evaluation == 'T',1,0)

Experiment_3_Data_T <- read_csv('.//data//Exp_3_Data_True.csv',
                                col_types = cols(
                                  .default = col_character()
                                ))

Experiment_3_Data_T$Match_FC <- ifelse(Experiment_3_Data_T$Evaluation == 'T',1,0)
Experiment_3_Data_T$Susc_FN <- ifelse(Experiment_3_Data_T$Evaluation == 'FM',1,0)

#Congruence Data:

Experiment_3_Data$Dummy_Incongruence <- ifelse(Experiment_3_Data$lean == 'L' & Experiment_3_Data$Ideology_Score > 0,1,0)
Experiment_3_Data$Dummy_Incongruence <- ifelse(Experiment_3_Data$lean == 'C' & Experiment_3_Data$Ideology_Score < 0,1,Experiment_3_Data$Dummy_Incongruence)
Experiment_3_Data$Dummy_Incongruence <- ifelse(is.na(Experiment_3_Data$Ideology_Score),NA,Experiment_3_Data$Dummy_Incongruence)

Experiment_3_Data$Congruent_Word <- ifelse(Experiment_3_Data$Dummy_Incongruence == 1,'Incongruent','Moderate')
Experiment_3_Data$Congruent_Word <- ifelse(Experiment_3_Data$Dummy_Congruence == 1,'Congruent',Experiment_3_Data$Congruent_Word)
Experiment_3_Data$Congruent_Word <- ifelse(is.na(Experiment_3_Data$Ideology_Score),NA,Experiment_3_Data$Congruent_Word)


#Congruence Data:
Experiment_3_Data_T$Dummy_Incongruence <- ifelse(Experiment_3_Data_T$lean == 'L' & Experiment_3_Data_T$Ideology_Score > 0,1,0)
Experiment_3_Data_T$Dummy_Incongruence <- ifelse(Experiment_3_Data_T$lean == 'C' & Experiment_3_Data_T$Ideology_Score < 0,1,Experiment_3_Data_T$Dummy_Incongruence)
Experiment_3_Data_T$Dummy_Incongruence <- ifelse(is.na(Experiment_3_Data_T$Ideology_Score),NA,Experiment_3_Data_T$Dummy_Incongruence)

Experiment_3_Data_T$Congruent_Word <- ifelse(Experiment_3_Data_T$Dummy_Incongruence == 1,'Incongruent','Moderate')
Experiment_3_Data_T$Congruent_Word <- ifelse(Experiment_3_Data_T$Dummy_Congruence == 1,'Congruent',Experiment_3_Data_T$Congruent_Word)
Experiment_3_Data_T$Congruent_Word <- ifelse(is.na(Experiment_3_Data_T$Ideology_Score),NA,Experiment_3_Data_T$Congruent_Word)



#Summary Statistics:

#Control (External):
Search_Control <- rbind(Misl_False_Search_MF,Misl_False_Search_T)
Search_Control <- Search_Control %>% filter(Treat_Search == 0)
N_Obs_1 <- length(unique(Search_Control$ResponseId))
Age_1 <- round(mean(as.numeric(Search_Control$Age),na.rm=T),2)
Search_Control$Education_Score <- as.numeric(Search_Control$Education_Score)
Search_Control$Education_Score <- ifelse(Search_Control$Education_Score > 2,1,0)
Educ_1 <- round(mean(as.numeric(Search_Control$Education_Score),na.rm=T),2)
Search_Control$Gender_Score <- ifelse(Search_Control$Gender == 'Female',1,0)
Gend_1 <- round(mean(as.numeric(Search_Control$Gender_Score),na.rm=T),2)

#Experiment (External):
Search_Exp <- rbind(Misl_False_Search_MF,Misl_False_Search_T)
Search_Exp <- Search_Exp %>% filter(Treat_Search == 1)
N_Obs_2 <- length(unique(Search_Exp$ResponseId))
Age_2 <- round(mean(as.numeric(Search_Exp$Age),na.rm=T),2)
Search_Exp$Education_Score <- as.numeric(Search_Exp$Education_Score)
Search_Exp$Education_Score <- ifelse(Search_Exp$Education_Score > 2,1,0)
Educ_2 <- round(mean(as.numeric(Search_Exp$Education_Score),na.rm=T),2)
Search_Exp$Gender_Score <- ifelse(Search_Exp$Gender == 'Female',1,0)
Gend_2 <- round(mean(as.numeric(Search_Exp$Gender_Score),na.rm=T),2)

#Control (Head/Source):
Head_Control <- rbind(Experiment_3_Data,Experiment_3_Data_T)
Head_Control <- Head_Control %>% filter(Source == 1)
Head_Control <- Head_Control %>% filter(Headline == 0)
N_Obs_3 <- length(unique(Head_Control$ResponseId))
Age_3 <- round(mean(as.numeric(Head_Control$Age),na.rm=T),2)
Head_Control$Education_Score <- as.numeric(Head_Control$Education_Score)
Head_Control$Education_Score <- ifelse(Head_Control$Education_Score > 2,1,0)
Educ_3 <- round(mean(as.numeric(Head_Control$Education_Score),na.rm=T),2)
Head_Control$Gender_Score <- ifelse(Head_Control$Gender == 'Female',1,0)
Gend_3 <- round(mean(as.numeric(Head_Control$Gender_Score),na.rm=T),2)

unique(Head_Control$Article_day)

#Exp (Head/Source):
Head_Control <- rbind(Experiment_3_Data,Experiment_3_Data_T)
Head_Control <- Head_Control %>% filter(Source == 1)
Head_Control <- Head_Control %>% filter(Headline == 1)
N_Obs_4 <- length(unique(Head_Control$ResponseId))
Age_4 <- round(mean(as.numeric(Head_Control$Age),na.rm=T),2)
Head_Control$Education_Score <- as.numeric(Head_Control$Education_Score)
Head_Control$Education_Score <- ifelse(Head_Control$Education_Score > 2,1,0)
Educ_4 <- round(mean(as.numeric(Head_Control$Education_Score),na.rm=T),2)
Head_Control$Gender_Score <- ifelse(Head_Control$Gender == 'Female',1,0)
Gend_4 <- round(mean(as.numeric(Head_Control$Gender_Score),na.rm=T),2)

#Control (Head/Source):
Head_Control <- rbind(Experiment_3_Data,Experiment_3_Data_T)
Head_Control <- Head_Control %>% filter(Source == 0)
Head_Control <- Head_Control %>% filter(Headline == 1)
N_Obs_5 <- length(unique(Head_Control$ResponseId))
Age_5 <- round(mean(as.numeric(Head_Control$Age),na.rm=T),2)
Head_Control$Education_Score <- as.numeric(Head_Control$Education_Score)
Head_Control$Education_Score <- ifelse(Head_Control$Education_Score > 2,1,0)
Educ_5 <- round(mean(as.numeric(Head_Control$Education_Score),na.rm=T),2)
Head_Control$Gender_Score <- ifelse(Head_Control$Gender == 'Female',1,0)
Gend_5 <- round(mean(as.numeric(Head_Control$Gender_Score),na.rm=T),2)


#Control (Head/Source):
Head_Control <- rbind(Experiment_3_Data,Experiment_3_Data_T)
Head_Control <- Head_Control %>% filter(Source == 0)
Head_Control <- Head_Control %>% filter(Headline == 0)
N_Obs_6 <- length(unique(Head_Control$ResponseId))
Age_6 <- round(mean(as.numeric(Head_Control$Age),na.rm=T),2)
Head_Control$Education_Score <- as.numeric(Head_Control$Education_Score)
Head_Control$Education_Score <- ifelse(Head_Control$Education_Score > 2,1,0)
Educ_6 <- round(mean(as.numeric(Head_Control$Education_Score),na.rm=T),2)
Head_Control$Gender_Score <- ifelse(Head_Control$Gender == 'Female',1,0)
Gend_6 <- round(mean(as.numeric(Head_Control$Gender_Score),na.rm=T),2)


#Create table of demographics:

Obs_c <- c(N_Obs_1,N_Obs_2,N_Obs_3,N_Obs_4,N_Obs_5,N_Obs_6)
Age_c <- c(Age_1,Age_2,Age_3,Age_4,Age_5,Age_6)
Educ_c <- c(Educ_1,Educ_2,Educ_3,Educ_4,Educ_5,Educ_6)
Gend_c <- c(Gend_1,Gend_2,Gend_3,Gend_4,Gend_5,Gend_6)
Article_T <- c('Full Article - Source - No Exernal Information',
               'Full Article - Source - Exernal Information',
               'Full Article - Source - No Exernal Information',
               'Headline - Source - No Exernal Information',
               'Headline - No Source - No Exernal Information',
               'Full Article - No Source - No Exernal Information')

Matrix_D <- matrix(c(Article_T,Obs_c,Age_c,Educ_c,Gend_c),ncol = 5)

colnames(Matrix_D) <- c('Article_Type','Num. Obs','Age','Education','Gender')


#(Hypothesis 3.1)

Search_Separate <- Misl_False_Search_MF

#### (1.2) Create Figure - Search For Information:
Misl_False_Search_1 <- Search_Separate %>% select(Match_FC,Treat_Search,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
Misl_False_Search_1 = na.omit(Misl_False_Search_1)

Misl_False_Search_1$Match_FC <- as.numeric(Misl_False_Search_1$Match_FC)
Misl_False_Search_1$Age <- as.numeric(Misl_False_Search_1$Age)
Misl_False_Search_1$Treat_Search <- as.numeric(Misl_False_Search_1$Treat_Search)
Misl_False_Search_1$Education_Score <- as.numeric(Misl_False_Search_1$Education_Score)
Misl_False_Search_1$Income_Score <- as.numeric(Misl_False_Search_1$Income_Score)
Misl_False_Search_1$Ideology_Score <- as.numeric(Misl_False_Search_1$Ideology_Score)

#Run linear regression and produce coefficient values:
fit_Search_MF_T_1 = glm(Match_FC ~ Treat_Search + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = Misl_False_Search_1)
#Produce clustere standard errors:
CI_Search_MF_T_1 = coefci(fit_Search_MF_T_1, vcov. = vcovCL(fit_Search_MF_T_1, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))

lin_Search_Separate_MF_T_1 = coeftest(fit_Search_MF_T_1, vcov. = vcovCL(fit_Search_MF_T_1, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_1_1 <- nrow(Misl_False_Search_1)
#R-squared
Rsq_Tab_1_1 <- rsq(fit_Search_MF_T_1)
A_Rsq_Tab_1_1 <- rsq(fit_Search_MF_T_1,adj=TRUE)
A_Rsq_Tab_1_1 <- round(A_Rsq_Tab_1_1,3)
Rsq_Tab_1_1 <- round(Rsq_Tab_1_1,3)


#F-statistic
glm.0 <- glm(Match_FC ~ 1,data=Misl_False_Search_1)
Results <- anova(fit_Search_MF_T_1, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]
F_Tab_1_1 <- round(F_Tab_1_1,3)
F_Tab_1_1 <- paste0(F_Tab_1_1,'^{***}')

Search_Articles_1 <- unique(Misl_False_Search_1$Article_day)




#(Hypothesis 3.2)

Search_Separate <- Misl_False_Search_MF


Misl_False_Search_1 <- Search_Separate %>% select(Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
Misl_False_Search_1 = na.omit(Misl_False_Search_1)


Misl_False_Search_1$Susc_FN <- as.numeric(Misl_False_Search_1$Susc_FN)
Misl_False_Search_1$Age <- as.numeric(Misl_False_Search_1$Age)
Misl_False_Search_1$Treat_Search <- as.numeric(Misl_False_Search_1$Treat_Search)
Misl_False_Search_1$Education_Score <- as.numeric(Misl_False_Search_1$Education_Score)
Misl_False_Search_1$Income_Score <- as.numeric(Misl_False_Search_1$Income_Score)
Misl_False_Search_1$Ideology_Score <- as.numeric(Misl_False_Search_1$Ideology_Score)


Misl_False_Search_Fig_1 <- Misl_False_Search_1 %>% mutate(Treat_Search_Cat = ifelse(Treat_Search == 0,'Control', 'Treatment'))


#Run linear regression and produce coefficient values:
fit_Search_MF_T_2 = glm(Susc_FN ~ Treat_Search + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = Misl_False_Search_1)
#Produce clustere standard errors:
CI_Search_MF_T_2 = coefci(fit_Search_MF_T_2, vcov. = vcovCL(fit_Search_MF_T_2, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))

lin_Search_Separate_MF_T_2 = coeftest(fit_Search_MF_T_2, vcov. = vcovCL(fit_Search_MF_T_2, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_1_2 <- nrow(Misl_False_Search_1)
#R-squared
Rsq_Tab_1_2 <- rsq(fit_Search_MF_T_2)
A_Rsq_Tab_1_2 <- rsq(fit_Search_MF_T_2,adj=TRUE)
A_Rsq_Tab_1_2 <- round(A_Rsq_Tab_1_2,3)
Rsq_Tab_1_2 <- round(Rsq_Tab_1_2,3)


#F-statistic
glm.0 <- glm(Susc_FN ~ 1,data=Misl_False_Search_1)
Results <- anova(fit_Search_MF_T_2, glm.0, test="F")
F_Tab_1_2 <- Results$F[2]
F_Tab_1_2 <- round(F_Tab_1_2,3)
F_Tab_1_2 <- paste0(F_Tab_1_2,'^{***}')

Search_Articles_2 <- unique(Misl_False_Search_1$Article_day)



#(Hypothesis 3.3)

Search_Separate <- Misl_False_Search_T


Misl_False_Search_1 <- Search_Separate %>% select(Match_FC,Treat_Search,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
Misl_False_Search_1 = na.omit(Misl_False_Search_1)


Misl_False_Search_1$Match_FC <- as.numeric(Misl_False_Search_1$Match_FC)
Misl_False_Search_1$Age <- as.numeric(Misl_False_Search_1$Age)
Misl_False_Search_1$Treat_Search <- as.numeric(Misl_False_Search_1$Treat_Search)
Misl_False_Search_1$Education_Score <- as.numeric(Misl_False_Search_1$Education_Score)
Misl_False_Search_1$Income_Score <- as.numeric(Misl_False_Search_1$Income_Score)
Misl_False_Search_1$Ideology_Score <- as.numeric(Misl_False_Search_1$Ideology_Score)

#Run linear regression and produce coefficient values:
fit_Search_MF_T_3 = glm(Match_FC ~ Treat_Search + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = Misl_False_Search_1)
#Produce clustere standard errors:
CI_Search_MF_T_3 = coefci(fit_Search_MF_T_3, vcov. = vcovCL(fit_Search_MF_T_3, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))

lin_Search_Separate_MF_T_3 = coeftest(fit_Search_MF_T_3, vcov. = vcovCL(fit_Search_MF_T_3, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_1_3 <- nrow(Misl_False_Search_1)
#R-squared
Rsq_Tab_1_3 <- rsq(fit_Search_MF_T_3)
A_Rsq_Tab_1_3 <- rsq(fit_Search_MF_T_3,adj=TRUE)
A_Rsq_Tab_1_3 <- round(A_Rsq_Tab_1_3,3)
Rsq_Tab_1_3 <- round(Rsq_Tab_1_3,3)


#F-statistic
glm.0 <- glm(Match_FC ~ 1,data=Misl_False_Search_1)
Results <- anova(fit_Search_MF_T_3, glm.0, test="F")
F_Tab_1_3 <- Results$F[2]
F_Tab_1_3 <- round(F_Tab_1_3,3)
F_Tab_1_3 <- paste0(F_Tab_1_3,'^{***}')

Search_Articles_3 <- unique(Misl_False_Search_1$Article_day)




#(Hypothesis 2.1)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)
Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Full_Data <- Exp_3_Data %>% filter(Headline == 0)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 1 | Article == 2 | Article == 3)
Full_Data$T_Dummy <- ifelse(Full_Data$Evaluation == 'T',1,0)

unique(Full_Data$Evaluation)


MF_Exp_3 <- Full_Data %>% select(T_Dummy,Standardize,Source,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$T_Dummy <- as.numeric(MF_Exp_3$T_Dummy)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)

#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_1 = glm(T_Dummy ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_1 <- coefci(fit_Standardize_MF_T_1, vcov. = vcovCL(fit_Standardize_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_1 = coeftest(fit_Standardize_MF_T_1, vcov. = vcovCL(fit_Standardize_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_1 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_1 <- rsq(fit_Standardize_MF_T_1)
A_Rsq_Tab_3_1 <- rsq(fit_Standardize_MF_T_1,adj=TRUE)
A_Rsq_Tab_3_1 <- round(A_Rsq_Tab_3_1,3)
Rsq_Tab_3_1 <- round(Rsq_Tab_3_1,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_1, glm.0, test="F")
F_Tab_3_1 <- Results$F[2]
F_Tab_3_1 <- round(F_Tab_3_1,3)
F_Tab_3_1 <- paste0(F_Tab_3_1,'^{***}')

Search_Articles_7 <- unique(MF_Exp_3$Article_day)




#(Hypothesis 2.2)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)

Full_Data <- Exp_3_Data %>% filter(Headline == 0)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 4 | Article == 5)
Full_Data$T_Dummy <- ifelse(Full_Data$Evaluation == 'T',1,0)

unique(Full_Data$Evaluation)


MF_Exp_3 <- Full_Data %>% select(T_Dummy,Standardize,Source,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$T_Dummy <- as.numeric(MF_Exp_3$T_Dummy)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)


#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_2 = glm(T_Dummy ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_2 <- coefci(fit_Standardize_MF_T_2, vcov. = vcovCL(fit_Standardize_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_2 = coeftest(fit_Standardize_MF_T_2, vcov. = vcovCL(fit_Standardize_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_3_2 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_2 <- rsq(fit_Standardize_MF_T_2)
A_Rsq_Tab_3_2 <- rsq(fit_Standardize_MF_T_2,adj=TRUE)
A_Rsq_Tab_3_2 <- round(A_Rsq_Tab_3_2,3)
Rsq_Tab_3_2 <- round(Rsq_Tab_3_2,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_2, glm.0, test="F")
F_Tab_3_2 <- Results$F[2]
F_Tab_3_2 <- round(F_Tab_3_2,3)
F_Tab_3_2 <- paste0(F_Tab_3_2,'^{***}')

Search_Articles_8 <- unique(MF_Exp_3$Article_day)


#(Hypothesis 2.3)


# (2.E)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Headline == 1)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 1 | Article == 2 | Article == 3)
Full_Data$T_Dummy <- ifelse(Full_Data$Evaluation == 'T',1,0)



MF_Exp_3 <- Full_Data %>% select(T_Dummy,Standardize,Source,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$T_Dummy <- as.numeric(MF_Exp_3$T_Dummy)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)



#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_3 = glm(T_Dummy ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_3 <- coefci(fit_Standardize_MF_T_3, vcov. = vcovCL(fit_Standardize_MF_T_3, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_3 = coeftest(fit_Standardize_MF_T_3, vcov. = vcovCL(fit_Standardize_MF_T_3, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_5 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_5 <- rsq(fit_Standardize_MF_T_3)
A_Rsq_Tab_3_5 <- rsq(fit_Standardize_MF_T_3,adj=TRUE)
A_Rsq_Tab_3_5 <- round(A_Rsq_Tab_3_5,3)
Rsq_Tab_3_5 <- round(Rsq_Tab_3_5,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_3, glm.0, test="F")
F_Tab_3_5 <- Results$F[2]
F_Tab_3_5 <- round(F_Tab_3_5,3)
F_Tab_3_5 <- paste0(F_Tab_3_5,'^{***}')

Search_Articles_11 <- unique(MF_Exp_3$Article_day)




# (Hypothesis 2.4)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)


Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Dummy_Congruence <- as.numeric(Exp_3_Data$Dummy_Congruence)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Headline == 1)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 4 | Article == 5)
Full_Data$T_Dummy <- ifelse(Full_Data$Evaluation == 'T',1,0)

unique(Full_Data$Evaluation)


MF_Exp_3 <- Full_Data %>% select(T_Dummy,Standardize,Source,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$T_Dummy <- as.numeric(MF_Exp_3$T_Dummy)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)


#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_4 = glm(T_Dummy ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_4 <- coefci(fit_Standardize_MF_T_4, vcov. = vcovCL(fit_Standardize_MF_T_4, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_4 = coeftest(fit_Standardize_MF_T_4, vcov. = vcovCL(fit_Standardize_MF_T_4, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_6 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_6 <- rsq(fit_Standardize_MF_T_4)
A_Rsq_Tab_3_6 <- rsq(fit_Standardize_MF_T_4,adj=TRUE)
A_Rsq_Tab_3_6 <- round(A_Rsq_Tab_3_6,3)
Rsq_Tab_3_6 <- round(Rsq_Tab_3_6,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_4, glm.0, test="F")
F_Tab_3_6 <- Results$F[2]
F_Tab_3_6 <- round(F_Tab_3_6,3)
F_Tab_3_6 <- paste0(F_Tab_3_6,'^{***}')

Search_Articles_12 <- unique(MF_Exp_3$Article_day)




#(Hypothesis 1.1) Standardized: Effect of Headline
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Dummy_Congruence <- as.numeric(Exp_3_Data$Dummy_Congruence)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Source == 0)

MF_Exp_3 <- Full_Data %>% select(Match_FC,Headline,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)


MF_Exp_3$Match_FC <- as.numeric(MF_Exp_3$Match_FC)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)


MF_Exp_3$Headline = ifelse(MF_Exp_3$Headline == 1,0,1)

#Run linear regression and produce coefficient values:
fit_Headline_MF_T_1 = glm(Match_FC ~ Headline + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Headline_MF_T_1 <- coefci(fit_Headline_MF_T_1, vcov. = vcovCL(fit_Headline_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Headline_MF_T_1 = coeftest(fit_Headline_MF_T_1, vcov. = vcovCL(fit_Headline_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_4_1 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_4_1 <- rsq(fit_Headline_MF_T_1)
A_Rsq_Tab_4_1 <- rsq(fit_Headline_MF_T_1,adj=TRUE)
A_Rsq_Tab_4_1 <- round(A_Rsq_Tab_4_1,3)
Rsq_Tab_4_1 <- round(Rsq_Tab_4_1,3)


#F-statistic
glm.0 <- glm(Match_FC ~ 1,data=MF_Exp_3)
Results <- anova(fit_Headline_MF_T_1, glm.0, test="F")
F_Tab_4_1 <- Results$F[2]
F_Tab_4_1 <- round(F_Tab_4_1,3)
F_Tab_4_1 <- paste0(F_Tab_4_1,'^{***}')

Search_Articles_15 <- unique(MF_Exp_3$Article_day)





#(Hypothesis 1.2) With Source: Effect of Headline
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Dummy_Congruence <- as.numeric(Exp_3_Data$Dummy_Congruence)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Source == 1)

MF_Exp_3 <- Full_Data %>% select(Match_FC,Headline,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)


MF_Exp_3$Match_FC <- as.numeric(MF_Exp_3$Match_FC)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)

MF_Exp_3$Headline = ifelse(MF_Exp_3$Headline == 1,0,1)


#Run linear regression and produce coefficient values:
fit_Headline_MF_T_2 = glm(Match_FC ~ Headline + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Headline_MF_T_2 <- coefci(fit_Headline_MF_T_2, vcov. = vcovCL(fit_Headline_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Headline_MF_T_2 = coeftest(fit_Headline_MF_T_2, vcov. = vcovCL(fit_Headline_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_4_2 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_4_2 <- rsq(fit_Headline_MF_T_2)
A_Rsq_Tab_4_2 <- rsq(fit_Headline_MF_T_2,adj=TRUE)
A_Rsq_Tab_4_2 <- round(A_Rsq_Tab_4_2,3)
Rsq_Tab_4_2 <- round(Rsq_Tab_4_2,3)


#F-statistic
glm.0 <- glm(Match_FC ~ 1,data=MF_Exp_3)
Results <- anova(fit_Headline_MF_T_2, glm.0, test="F")
F_Tab_4_2 <- Results$F[2]
F_Tab_4_2 <- round(F_Tab_4_2,3)
F_Tab_4_2 <- paste0(F_Tab_4_2,'^{***}')

Search_Articles_16 <- unique(MF_Exp_3$Article_day)









##  TABLES  ---  Categorical Score:


write(stargazer(lin_results_Headline_MF_T_1,
                lin_results_Headline_MF_T_2,
                lin_Search_Separate_MF_T_2,
                lin_Search_Separate_MF_T_1,
                model.numbers=FALSE,
                column.labels=c("(H1.1)","(H1.2)","(H3.1)","(H3.2)"),
                title="Measuring Effect of Additional Information on Matching the Fact-Checker's Evaluation",
                align=TRUE,
                omit = c(Search_Articles_1,Search_Articles_2,Search_Articles_3,
                         Search_Articles_7,Search_Articles_8,Search_Articles_11,Search_Articles_12,
                         Search_Articles_15,Search_Articles_16,
                         'Constant','GenderOther'),
                covariate.labels = c('Treatment (Full Text)','Treatment (Search)','Education','Age','Gender (Male)','Income','Ideology'),
                star.cutoffs = c(0.05, 0.01, 0.001),
                add.lines = list(c("Observations",Obs_Tab_1_1,Obs_Tab_4_1,Obs_Tab_4_2),
                                 c("R-squared",Rsq_Tab_1_1,Rsq_Tab_4_1,Rsq_Tab_4_2),
                                 c("Adj. R-squared",A_Rsq_Tab_1_1,A_Rsq_Tab_4_1,A_Rsq_Tab_4_2),
                                 c("F-Statistic",F_Tab_1_1,F_Tab_4_1,F_Tab_4_2))),file='.//tables//Table_1_JEPS.txt')




write(stargazer(lin_results_Standardize_MF_T_1,
                lin_results_Standardize_MF_T_2,
                lin_results_Standardize_MF_T_3,
                lin_results_Standardize_MF_T_4,
                lin_Search_Separate_MF_T_3,
                model.numbers=FALSE,
                column.labels=c("(H2.1)","(H2.2)","(H2.3)","(H2.4)","(H3.3)"),
                title="Measuring Effect of Additional Information on Rating an Article as True",
                align=TRUE,
                omit = c(Search_Articles_1,Search_Articles_2,Search_Articles_3,
                         Search_Articles_7,Search_Articles_8,Search_Articles_11,Search_Articles_12,
                         Search_Articles_15,Search_Articles_16,
                         'Constant','GenderOther'),
                covariate.labels = c('Treatment (Source)','Treatment (Search)','Education','Age','Gender (Male)','Income','Ideology'),
                star.cutoffs = c(0.05, 0.01, 0.001),
                add.lines = list(c("Observations",Obs_Tab_1_2,Obs_Tab_1_3,Obs_Tab_3_1,Obs_Tab_3_2,Obs_Tab_3_5,Obs_Tab_3_6),
                                 c("R-squared",Rsq_Tab_1_2,Rsq_Tab_1_3,Rsq_Tab_3_1,Rsq_Tab_3_2,Rsq_Tab_3_5,Rsq_Tab_3_6),
                                 c("Adj. R-squared",A_Rsq_Tab_1_2,A_Rsq_Tab_1_3,A_Rsq_Tab_3_1,A_Rsq_Tab_3_2,A_Rsq_Tab_3_5,A_Rsq_Tab_3_6),
                                 c("F-Statistic",F_Tab_1_2,F_Tab_1_3,F_Tab_3_1,F_Tab_3_2,F_Tab_3_5,F_Tab_3_6))),file='.//tables//Table_2_JEPS.txt')



#Multiple Hypothesis Testing:

#Placing all fo the p-values into a vector:

P_Values_Hyp <-  c(lin_results_Headline_MF_T_1[2,4],
                   lin_results_Headline_MF_T_2[2,4],
                   lin_results_Standardize_MF_T_1[2,4],
                   lin_results_Standardize_MF_T_2[2,4],
                   lin_results_Standardize_MF_T_3[2,4],
                   lin_results_Standardize_MF_T_4[2,4],
                   lin_Search_Separate_MF_T_3[2,4],
                   lin_Search_Separate_MF_T_1[2,4],
                   lin_Search_Separate_MF_T_2[2,4])

P_Values_Hyp <- round(P_Values_Hyp,4)

#Using the Banjamini and Hochberg (1995) method report new p-values.
FDR_Adj_P <- p.adjust(P_Values_Hyp,method='fdr',n=length(P_Values_Hyp))

FDR_Adj_P <- round(FDR_Adj_P,4)


#Using the Bonferroni method report new p-values.
Bonf_Adj_P <- p.adjust(P_Values_Hyp,method='bonferroni',n=length(P_Values_Hyp))

Bonf_Adj_P <- round(Bonf_Adj_P,4)

Hyp_list <- c('(H1.1)',
              '(H1.2)',
              '(H2.1)',
              '(H2.2)',
              '(H2.3)',
              '(H2.4)',
              '(H3.1)',
              '(H3.2)',
              '(H3.3)')

#Convert from numeric and character:

P_Values_Hyp <- as.character(P_Values_Hyp)
FDR_Adj_P <- as.character(FDR_Adj_P)
Bonf_Adj_P <- as.character(Bonf_Adj_P)


mat_P <- matrix(c(P_Values_Hyp,
  FDR_Adj_P,
  Bonf_Adj_P),ncol=9,byrow=T)


rownames(mat_P) <- c('Unadjusted P-Value',
                     'P-Value (FDR Adjusted)',
                     'P-Value (Bonferroni Adjusted)')

colnames(mat_P) <- Hyp_list

print(xtable(mat_P,
             caption='Unadjusted and Adjusted P-Values Testing Each Hypothesis'),
      caption.placement = 'top',
      file='.//tables//Multiple_Hyp.txt')



## Likert Scores(H2.1,H2.2,H2.3,H2.4,H3.3)  :

#(Hypothesis 3.3)

Search_Separate <- Misl_False_Search_MF


Misl_False_Search_1 <- Search_Separate %>% select(Likert_Evaluation,Treat_Search,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
Misl_False_Search_1 = na.omit(Misl_False_Search_1)


Misl_False_Search_1$Likert_Evaluation <- as.numeric(Misl_False_Search_1$Likert_Evaluation)
Misl_False_Search_1$Age <- as.numeric(Misl_False_Search_1$Age)
Misl_False_Search_1$Treat_Search <- as.numeric(Misl_False_Search_1$Treat_Search)
Misl_False_Search_1$Education_Score <- as.numeric(Misl_False_Search_1$Education_Score)
Misl_False_Search_1$Income_Score <- as.numeric(Misl_False_Search_1$Income_Score)
Misl_False_Search_1$Ideology_Score <- as.numeric(Misl_False_Search_1$Ideology_Score)


Misl_False_Search_Fig_1 <- Misl_False_Search_1 %>% mutate(Treat_Search_Cat = ifelse(Treat_Search == 0,'Control', 'Treatment'))


#Run linear regression and produce coefficient values:
fit_Search_MF_T_2 = glm(Likert_Evaluation ~ Treat_Search + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = Misl_False_Search_1)
#Produce clustere standard errors:
CI_Search_MF_T_2 = coefci(fit_Search_MF_T_2, vcov. = vcovCL(fit_Search_MF_T_2, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))

lin_Search_Separate_MF_T_2 = coeftest(fit_Search_MF_T_2, vcov. = vcovCL(fit_Search_MF_T_2, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_1_2 <- nrow(Misl_False_Search_1)
#R-squared
Rsq_Tab_1_2 <- rsq(fit_Search_MF_T_2)
A_Rsq_Tab_1_2 <- rsq(fit_Search_MF_T_2,adj=TRUE)
A_Rsq_Tab_1_2 <- round(A_Rsq_Tab_1_2,3)
Rsq_Tab_1_2 <- round(Rsq_Tab_1_2,3)


#F-statistic
glm.0 <- glm(Likert_Evaluation ~ 1,data=Misl_False_Search_1)
Results <- anova(fit_Search_MF_T_2, glm.0, test="F")
F_Tab_1_2 <- Results$F[2]
F_Tab_1_2 <- round(F_Tab_1_2,3)
F_Tab_1_2 <- paste0(F_Tab_1_2,'^{***}')

Search_Articles_2 <- unique(Misl_False_Search_1$Article_day)




#(Hypothesis 2.1)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)
Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Full_Data <- Exp_3_Data %>% filter(Headline == 0)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 1 | Article == 2 | Article == 3)
Full_Data$T_Dummy <- ifelse(Full_Data$Evaluation == 'T',1,0)

unique(Full_Data$Evaluation)


MF_Exp_3 <- Full_Data %>% select(Likert_Evaluation,Standardize,Source,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$Likert_Evaluation <- as.numeric(MF_Exp_3$Likert_Evaluation)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)

#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_1 = glm(Likert_Evaluation ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_1 <- coefci(fit_Standardize_MF_T_1, vcov. = vcovCL(fit_Standardize_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_1 = coeftest(fit_Standardize_MF_T_1, vcov. = vcovCL(fit_Standardize_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_1 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_1 <- rsq(fit_Standardize_MF_T_1)
A_Rsq_Tab_3_1 <- rsq(fit_Standardize_MF_T_1,adj=TRUE)
A_Rsq_Tab_3_1 <- round(A_Rsq_Tab_3_1,3)
Rsq_Tab_3_1 <- round(Rsq_Tab_3_1,3)


#F-statistic
glm.0 <- glm(Likert_Evaluation ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_1, glm.0, test="F")
F_Tab_3_1 <- Results$F[2]
F_Tab_3_1 <- round(F_Tab_3_1,3)
F_Tab_3_1 <- paste0(F_Tab_3_1,'^{***}')

Search_Articles_7 <- unique(MF_Exp_3$Article_day)




#(Hypothesis 2.2)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)

Full_Data <- Exp_3_Data %>% filter(Headline == 0)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 4 | Article == 5)
Full_Data$Likert_Evaluation <- ifelse(Full_Data$Evaluation == 'T',1,0)

unique(Full_Data$Evaluation)


MF_Exp_3 <- Full_Data %>% select(Likert_Evaluation,Standardize,Source,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$Likert_Evaluation <- as.numeric(MF_Exp_3$Likert_Evaluation)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)


#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_2 = glm(Likert_Evaluation ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_2 <- coefci(fit_Standardize_MF_T_2, vcov. = vcovCL(fit_Standardize_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_2 = coeftest(fit_Standardize_MF_T_2, vcov. = vcovCL(fit_Standardize_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_3_2 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_2 <- rsq(fit_Standardize_MF_T_2)
A_Rsq_Tab_3_2 <- rsq(fit_Standardize_MF_T_2,adj=TRUE)
A_Rsq_Tab_3_2 <- round(A_Rsq_Tab_3_2,3)
Rsq_Tab_3_2 <- round(Rsq_Tab_3_2,3)


#F-statistic
glm.0 <- glm(Likert_Evaluation ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_2, glm.0, test="F")
F_Tab_3_2 <- Results$F[2]
F_Tab_3_2 <- round(F_Tab_3_2,3)
F_Tab_3_2 <- paste0(F_Tab_3_2,'^{***}')

Search_Articles_8 <- unique(MF_Exp_3$Article_day)


#(Hypothesis 2.3)


# (2.E)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Headline == 1)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 1 | Article == 2 | Article == 3)







MF_Exp_3 <- Full_Data %>% select(Likert_Evaluation,Standardize,Source,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$Likert_Evaluation <- as.numeric(MF_Exp_3$Likert_Evaluation)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)



#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_3 = glm(Likert_Evaluation ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_3 <- coefci(fit_Standardize_MF_T_3, vcov. = vcovCL(fit_Standardize_MF_T_3, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_3 = coeftest(fit_Standardize_MF_T_3, vcov. = vcovCL(fit_Standardize_MF_T_3, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_5 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_5 <- rsq(fit_Standardize_MF_T_3)
A_Rsq_Tab_3_5 <- rsq(fit_Standardize_MF_T_3,adj=TRUE)
A_Rsq_Tab_3_5 <- round(A_Rsq_Tab_3_5,3)
Rsq_Tab_3_5 <- round(Rsq_Tab_3_5,3)


#F-statistic
glm.0 <- glm(Likert_Evaluation ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_3, glm.0, test="F")
F_Tab_3_5 <- Results$F[2]
F_Tab_3_5 <- round(F_Tab_3_5,3)
F_Tab_3_5 <- paste0(F_Tab_3_5,'^{***}')

Search_Articles_11 <- unique(MF_Exp_3$Article_day)




# (Hypothesis 2.4)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)


Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Dummy_Congruence <- as.numeric(Exp_3_Data$Dummy_Congruence)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Headline == 1)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 4 | Article == 5)


MF_Exp_3 <- Full_Data %>% select(Likert_Evaluation,Standardize,Source,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$Likert_Evaluation <- as.numeric(MF_Exp_3$Likert_Evaluation)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)


#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_4 = glm(Likert_Evaluation ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_4 <- coefci(fit_Standardize_MF_T_4, vcov. = vcovCL(fit_Standardize_MF_T_4, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_4 = coeftest(fit_Standardize_MF_T_4, vcov. = vcovCL(fit_Standardize_MF_T_4, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_6 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_6 <- rsq(fit_Standardize_MF_T_4)
A_Rsq_Tab_3_6 <- rsq(fit_Standardize_MF_T_4,adj=TRUE)
A_Rsq_Tab_3_6 <- round(A_Rsq_Tab_3_6,3)
Rsq_Tab_3_6 <- round(Rsq_Tab_3_6,3)


#F-statistic
glm.0 <- glm(Likert_Evaluation ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_4, glm.0, test="F")
F_Tab_3_6 <- Results$F[2]
F_Tab_3_6 <- round(F_Tab_3_6,3)
F_Tab_3_6 <- paste0(F_Tab_3_6,'^{***}')

Search_Articles_12 <- unique(MF_Exp_3$Article_day)


##  TABLES  ---  Categorical Score:

write(stargazer(lin_results_Standardize_MF_T_1,
                lin_results_Standardize_MF_T_2,
                lin_results_Standardize_MF_T_3,
                lin_results_Standardize_MF_T_4,
                lin_Search_Separate_MF_T_2,
                model.numbers=FALSE,
                column.labels=c("(H2.1)","(H2.2)","(H2.3)","(H2.4)","(H3.3)"),
                title="Measuring Effect of Additional Information on Rating an Article as True (Likert Score)",
                align=TRUE,
                omit = c(Search_Articles_1,Search_Articles_2,Search_Articles_3,
                         Search_Articles_7,Search_Articles_8,Search_Articles_11,Search_Articles_12,
                         Search_Articles_15,Search_Articles_16,
                         'Constant','GenderOther'),
                covariate.labels = c('Treatment (Source)','Treatment (Search)','Education','Age','Gender (Male)','Income','Ideology'),
                star.cutoffs = c(0.05, 0.01, 0.001),
                add.lines = list(c("Observations",Obs_Tab_1_2,Obs_Tab_1_3,Obs_Tab_3_1,Obs_Tab_3_2,Obs_Tab_3_5,Obs_Tab_3_6),
                                 c("R-squared",Rsq_Tab_1_2,Rsq_Tab_1_3,Rsq_Tab_3_1,Rsq_Tab_3_2,Rsq_Tab_3_5,Rsq_Tab_3_6),
                                 c("Adj. R-squared",A_Rsq_Tab_1_2,A_Rsq_Tab_1_3,A_Rsq_Tab_3_1,A_Rsq_Tab_3_2,A_Rsq_Tab_3_5,A_Rsq_Tab_3_6),
                                 c("F-Statistic",F_Tab_1_2,F_Tab_1_3,F_Tab_3_1,F_Tab_3_2,F_Tab_3_5,F_Tab_3_6))),file='.//tables//Table_3_JEPS.txt')








#Logistic regression:

#(Hypothesis 3.1)

Search_Separate <- rbind(Misl_False_Search_MF)

#### (1.2) Create Figure - Search For Information:
Misl_False_Search_1 <- Search_Separate %>% select(Match_FC,Treat_Search,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
Misl_False_Search_1 = na.omit(Misl_False_Search_1)

Misl_False_Search_1$Match_FC <- as.numeric(Misl_False_Search_1$Match_FC)
Misl_False_Search_1$Age <- as.numeric(Misl_False_Search_1$Age)
Misl_False_Search_1$Treat_Search <- as.numeric(Misl_False_Search_1$Treat_Search)
Misl_False_Search_1$Education_Score <- as.numeric(Misl_False_Search_1$Education_Score)
Misl_False_Search_1$Income_Score <- as.numeric(Misl_False_Search_1$Income_Score)
Misl_False_Search_1$Ideology_Score <- as.numeric(Misl_False_Search_1$Ideology_Score)

#Run linear regression and produce coefficient values:
fit_Search_MF_T_1 = glm(Match_FC ~ Treat_Search + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = Misl_False_Search_1,family=binomial)
#Produce clustere standard errors:
CI_Search_MF_T_1 = coefci(fit_Search_MF_T_1, vcov. = vcovCL(fit_Search_MF_T_1, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))

lin_Search_Separate_MF_T_1 = coeftest(fit_Search_MF_T_1, vcov. = vcovCL(fit_Search_MF_T_1, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_1_1 <- nrow(Misl_False_Search_1)
#R-squared
Rsq_Tab_1_1 <- rsq(fit_Search_MF_T_1)
A_Rsq_Tab_1_1 <- rsq(fit_Search_MF_T_1,adj=TRUE)
A_Rsq_Tab_1_1 <- round(A_Rsq_Tab_1_1,3)
Rsq_Tab_1_1 <- round(Rsq_Tab_1_1,3)

Search_Articles_1 <- unique(Misl_False_Search_1$Article_day)



#(Hypothesis 3.2)

Search_Separate <- Misl_False_Search_T


Misl_False_Search_1 <- Search_Separate %>% select(Match_FC,Treat_Search,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
Misl_False_Search_1 = na.omit(Misl_False_Search_1)


Misl_False_Search_1$Match_FC <- as.numeric(Misl_False_Search_1$Match_FC)
Misl_False_Search_1$Age <- as.numeric(Misl_False_Search_1$Age)
Misl_False_Search_1$Treat_Search <- as.numeric(Misl_False_Search_1$Treat_Search)
Misl_False_Search_1$Education_Score <- as.numeric(Misl_False_Search_1$Education_Score)
Misl_False_Search_1$Income_Score <- as.numeric(Misl_False_Search_1$Income_Score)
Misl_False_Search_1$Ideology_Score <- as.numeric(Misl_False_Search_1$Ideology_Score)

#Run linear regression and produce coefficient values:
fit_Search_MF_T_3 = glm(Match_FC ~ Treat_Search + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = Misl_False_Search_1,family=binomial)
#Produce clustere standard errors:
CI_Search_MF_T_3 = coefci(fit_Search_MF_T_3, vcov. = vcovCL(fit_Search_MF_T_3, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))

lin_Search_Separate_MF_T_3 = coeftest(fit_Search_MF_T_3, vcov. = vcovCL(fit_Search_MF_T_3, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_1_3 <- nrow(Misl_False_Search_1)
#R-squared
Rsq_Tab_1_3 <- rsq(fit_Search_MF_T_3)
A_Rsq_Tab_1_3 <- rsq(fit_Search_MF_T_3,adj=TRUE)
A_Rsq_Tab_1_3 <- round(A_Rsq_Tab_1_3,3)
Rsq_Tab_1_3 <- round(Rsq_Tab_1_3,3)

Search_Articles_3 <- unique(Misl_False_Search_1$Article_day)








#(Hypothesis 1.1) Standardized: Effect of Headline
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)


Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Dummy_Congruence <- as.numeric(Exp_3_Data$Dummy_Congruence)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Source == 0)

MF_Exp_3 <- Full_Data %>% select(Match_FC,Headline,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$Match_FC <- as.numeric(MF_Exp_3$Match_FC)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)


MF_Exp_3$Headline = ifelse(MF_Exp_3$Headline == 1,0,1)

#Run linear regression and produce coefficient values:
fit_Headline_MF_T_1 = glm(Match_FC ~ Headline + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3,family=binomial)
#Produce clustere standard errors:
CI_Headline_MF_T_1 <- coefci(fit_Headline_MF_T_1, vcov. = vcovCL(fit_Headline_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Headline_MF_T_1 = coeftest(fit_Headline_MF_T_1, vcov. = vcovCL(fit_Headline_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_4_1 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_4_1 <- rsq(fit_Headline_MF_T_1)
A_Rsq_Tab_4_1 <- rsq(fit_Headline_MF_T_1,adj=TRUE)
A_Rsq_Tab_4_1 <- round(A_Rsq_Tab_4_1,3)
Rsq_Tab_4_1 <- round(Rsq_Tab_4_1,3)

Search_Articles_15 <- unique(MF_Exp_3$Article_day)





#(Hypothesis 1.2) With Source: Effect of Headline
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Dummy_Congruence <- as.numeric(Exp_3_Data$Dummy_Congruence)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Source == 1)

MF_Exp_3 <- Full_Data %>% select(Match_FC,Headline,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$Match_FC <- as.numeric(MF_Exp_3$Match_FC)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)

MF_Exp_3$Headline = ifelse(MF_Exp_3$Headline == 1,0,1)


#Run linear regression and produce coefficient values:
fit_Headline_MF_T_2 = glm(Match_FC ~ Headline + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3,family=binomial)
#Produce clustere standard errors:
CI_Headline_MF_T_2 <- coefci(fit_Headline_MF_T_2, vcov. = vcovCL(fit_Headline_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Headline_MF_T_2 = coeftest(fit_Headline_MF_T_2, vcov. = vcovCL(fit_Headline_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_4_2 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_4_2 <- rsq(fit_Headline_MF_T_2)
A_Rsq_Tab_4_2 <- rsq(fit_Headline_MF_T_2,adj=TRUE)
A_Rsq_Tab_4_2 <- round(A_Rsq_Tab_4_2,3)
Rsq_Tab_4_2 <- round(Rsq_Tab_4_2,3)


Search_Articles_16 <- unique(MF_Exp_3$Article_day)

write(stargazer(lin_results_Headline_MF_T_1,
                lin_results_Headline_MF_T_2,
                lin_Search_Separate_MF_T_3,
                lin_Search_Separate_MF_T_1,
                model.numbers=FALSE,
                column.labels=c("(H1.1)","(H1.2)","(H3.1)","(H3.2)"),
                title="Measuring Effect of Additional Information on Matching the Fact-Checker's Evaluation - Categorical (Logistic Regression)",
                align=TRUE,
                omit = c(Search_Articles_1,Search_Articles_2,Search_Articles_3,
                         Search_Articles_7,Search_Articles_8,Search_Articles_11,Search_Articles_12,
                         Search_Articles_15,Search_Articles_16,
                         'Constant','GenderOther'),
                covariate.labels = c('Treatment (Full Text)','Treatment (Search)','Education','Age','Gender (Male)','Income','Ideology'),
                star.cutoffs = c(0.05, 0.01, 0.001),
                add.lines = list(c("Observations",Obs_Tab_1_1,Obs_Tab_4_1,Obs_Tab_4_2),
                                 c("R-squared",Rsq_Tab_1_1,Rsq_Tab_4_1,Rsq_Tab_4_2),
                                 c("Adj. R-squared",A_Rsq_Tab_1_1,A_Rsq_Tab_4_1,A_Rsq_Tab_4_2))),
                file='.//tables//Table_4_JEPS.txt')








######################################################



########################################################### Load in Control Data
#Specify the name of the csv file of data that you are uploading.
data_file = ".//data//Control_Survey.csv"
#Read in the csv file
Control_Survey <- read_csv(data_file,
                           col_types = cols(
                             Likert_Evaluation = col_double(),
                             Age = col_double(),
                             CRT_Score = col_double(),
                             PK_Score = col_double(),
                             Ideology_Score = col_double(),
                             Education_Score = col_double(),
                             Male_Gender_Dummy = col_double(),
                             Income_Score = col_double(),
                             Trust_Score = col_double(),
                             Dig_Lit_Avg = col_double(),
                             .default = col_character()
                           ))

Control_Survey <- Control_Survey %>%
  mutate(Paid = 0)

Control_Survey <- Control_Survey %>%
  mutate(Paid_Word = 'No')

unique(Control_Survey$day)


########################################################### Load in FAct Checkers Data

#Specify the name of the csv file of data that you are uploading.
data_file = ".//data//fact_checkers_byarticle_all_sets.csv"
#Read in the csv file
FC_Final_Data <- read_csv(data_file,
                          col_types = cols(
                            .default = col_character()
                          ))

colnames(FC_Final_Data)[2] <- 'day'

colnames(FC_Final_Data)[3] <- 'Article'





########################################################### Load in Experiment Data

#Specify the name of the csv file of data that you are uploading.
data_file = ".//data//Paid_Survey.csv"

#Read in the csv file
Experiment_Survey <- read_csv(data_file,
                              col_types = cols(
                                Likert_Evaluation = col_double(),
                                Age = col_double(),
                                CRT_Score = col_double(),
                                PK_Score = col_double(),
                                Ideology_Score = col_double(),
                                Education_Score = col_double(),
                                Male_Gender_Dummy = col_double(),
                                Income_Score = col_double(),
                                Trust_Score = col_double(),
                                Dig_Lit_Avg = col_double(),
                                .default = col_character()
                              ))

Experiment_Survey <- Experiment_Survey %>%
  mutate(Paid = 1)

Experiment_Survey <- Experiment_Survey %>%
  mutate(Paid_Word = 'Yes')

unique(Experiment_Survey$day)

####################### Merge Individual Responses

Final_Data <- bind_rows(Experiment_Survey,Control_Survey)







########################################################### Merge Fact-Checking Data and individual responses.
Final_Data <- merge(Final_Data,FC_Final_Data,by=c('day','Article'))

Final_Data$Evaluation <- ifelse(Final_Data$Evaluation == "True",'T',Final_Data$Evaluation)
Final_Data$Evaluation <- ifelse(Final_Data$Evaluation == "Misl/False",'FM',Final_Data$Evaluation)

#Create a dummy variable whether response matched the fact-checkers 
Final_Data <- Final_Data %>%
  mutate(Match_FC = ifelse(Evaluation == mode,1,0))

#Create a dummy variable whether responses stated that an article was true, when fact checkers stated it was misleading/false
Final_Data <- Final_Data %>%
  mutate(Susc_FN = ifelse(Evaluation == 'T' & mode == 'FM',1,0))



#Crete a variable for each unique article
Final_Data <- Final_Data %>% mutate(Article_day = paste0(day,sep='_',Article))

#Create familiar story variable
Final_Data <- Final_Data %>%
  mutate(Familiar_Dummy = ifelse(Familiar_Story == 'Yes',1,0))


#Create sharing variable
Final_Data <- Final_Data %>%
  mutate(Share_Dummy = ifelse(Share_Social_Media == 'Yes' | Share_Social_Media == 'Maybe',1,0))

Final_Data <- Final_Data %>%
  mutate(Share_Dummy = ifelse(Share_Social_Media == 'I would never share something on social media',NA,Share_Dummy))

#Create Digital Literacy Groups:
Final_Data <- Final_Data %>% mutate(DigLit_Group = ifelse(Dig_Lit_Avg  < 3.3,'1-3.3', as.character(Dig_Lit_Avg)))
Final_Data <- Final_Data %>% mutate(DigLit_Group = ifelse(Dig_Lit_Avg >= 3.3 & Dig_Lit_Avg < 3.9,'3.3-3.9', DigLit_Group))
Final_Data <- Final_Data %>% mutate(DigLit_Group = ifelse(Dig_Lit_Avg >= 3.9 & Dig_Lit_Avg <= 4.5 ,'3.9-4.5', DigLit_Group))
Final_Data <- Final_Data %>% mutate(DigLit_Group = ifelse(Dig_Lit_Avg > 4.5,'> 4.5', DigLit_Group))

#News Consumption Groups
Final_Data <- Final_Data %>% mutate(News_Score = 0)
Final_Data <- Final_Data %>% mutate(News_Score = ifelse(Q_News_Coverage == 'Daily',3, News_Score))
Final_Data <- Final_Data %>% mutate(News_Score = ifelse(Q_News_Coverage == 'Often',2, News_Score))
Final_Data <- Final_Data %>% mutate(News_Score = ifelse(Q_News_Coverage == 'A little bit',1, News_Score))

#Only days when both experiments were in action:
Final_Data <- Final_Data %>%
  filter(day == 'dec 16' | 
           day == 'dec 17' | 
           day == 'dec 18' | 
           day == 'dec 19' | 
           day == 'jan 6' | 
           day == 'jan 7' | 
           day == 'jan 8' | 
           day == 'jan 9' | 
           day == 'jan 13' | 
           day == 'jan 14' |
           day == 'feb 4' |
           day == 'feb 5' |
           day == 'feb 6')

article_topics_clean <- read.csv('.//data//article_topics_clean.csv')

Final_Data <- merge(Final_Data,article_topics_clean,by.x='article_number',by.y='article_num')



Final_Data$Dummy_Congruence <- ifelse(Final_Data$lean == 'L' & Final_Data$Ideology_Score < 0, 1,0)
Final_Data$Dummy_Congruence <- ifelse(Final_Data$lean == 'C' & Final_Data$Ideology_Score > 0, 1,Final_Data$Dummy_Congruence)
Final_Data$Dummy_Congruence <- ifelse(is.na(Final_Data$Ideology_Score),NA,Final_Data$Dummy_Congruence)
Final_Data$Dummy_Congruence_Con <- Final_Data$Dummy_Congruence
Final_Data$Dummy_Congruence_Lib <- Final_Data$Dummy_Congruence
Final_Data$Dummy_Congruence_Con <- ifelse(Final_Data$Ideology_Score < 0,0,Final_Data$Dummy_Congruence_Con)
Final_Data$Dummy_Congruence_Lib <- ifelse(Final_Data$Ideology_Score > 0,0,Final_Data$Dummy_Congruence_Lib)

Final_Data$Gender_Dummy <- ifelse(Final_Data$Gender == 'Female',1,0)

Misl_False_D <- Final_Data %>%
  filter(mode == 'FM')

True_D <- Final_Data %>%
  filter(mode == 'T')

Misl_False_Data <- Misl_False_D %>% select(Paid_Word,Evaluation)

Misl_False_Data <- na.omit(Misl_False_Data)

Misl_False_Data <- Misl_False_Data %>% dplyr::group_by(Paid_Word) %>% dplyr::count(Evaluation)

Misl_False_Data <- Misl_False_Data %>% dplyr::group_by(Paid_Word) %>% mutate(Total = sum(n))

Misl_False_Data <- Misl_False_Data %>% mutate(Prop = n/Total)

Misl_False_Data$Paid_Word <- as.character(Misl_False_Data$Paid_Word)
Misl_False_Data$Evaluation <- as.character(Misl_False_Data$Evaluation)


Misl_False_Data$Evaluation <- ifelse(Misl_False_Data$Evaluation == 'T','True',Misl_False_Data$Evaluation)
Misl_False_Data$Evaluation <- ifelse(Misl_False_Data$Evaluation == 'FM','False/\nMisleading',Misl_False_Data$Evaluation)
Misl_False_Data$Evaluation <- ifelse(Misl_False_Data$Evaluation == 'CND','Could Not\nDetermine',Misl_False_Data$Evaluation)

ggplot(data = Misl_False_Data, aes(x=Evaluation, y=Prop,fill=factor(Paid_Word))) + 
  geom_bar(position=position_dodge(),
           stat="identity",
           width=0.5) +
  scale_fill_manual(values=c('black','red'), name = "Extra Financial\nIncentive?") +
  scale_y_continuous(name="Proportion of Evaluations\n") +
  scale_x_discrete(name='Categorical Evaluations',limits=c('True','False/\nMisleading','Could Not\nDetermine')) +
  coord_cartesian(ylim = c(0,1.0)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        title =element_text(size=20, face='bold'),
        legend.text = element_text(size=16))


ggsave('.//figures//Paid_Prop_FM.png',width = 9)





True_Data <- True_D %>% select(Paid_Word,Evaluation)

True_Data <- na.omit(True_Data)

True_Data <- True_Data %>% dplyr::group_by(Paid_Word) %>% dplyr::count(Evaluation)

True_Data <- True_Data %>% dplyr::group_by(Paid_Word) %>% mutate(Total = sum(n))

True_Data <- True_Data %>% mutate(Prop = n/Total)

True_Data$Paid_Word <- as.character(True_Data$Paid_Word)
True_Data$Evaluation <- as.character(True_Data$Evaluation)


True_Data$Evaluation <- ifelse(True_Data$Evaluation == 'T','True',True_Data$Evaluation)
True_Data$Evaluation <- ifelse(True_Data$Evaluation == 'FM','False/\nMisleading',True_Data$Evaluation)
True_Data$Evaluation <- ifelse(True_Data$Evaluation == 'CND','Could Not\nDetermine',True_Data$Evaluation)

ggplot(data = True_Data, aes(x=Evaluation, y=Prop,fill=factor(Paid_Word))) + 
  geom_bar(position=position_dodge(),
           stat="identity",
           width=0.5) +
  scale_fill_manual(values=c('black','red'), name = "Extra Financial\nIncentive?") +
  scale_y_continuous(name="Proportion of Evaluations\n") +
  scale_x_discrete(name='Categorical Evaluations',limits=c('True','False/\nMisleading','Could Not\nDetermine')) +
  coord_cartesian(ylim = c(0,1.0)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        title =element_text(size=20, face='bold'),
        legend.text = element_text(size=16))


ggsave('.//figures//Paid_Prop_T.png',width = 9)









#Inter-Rater Reliability Scores:
Article_Ratings <- read.csv('.//data//Ratings_Ideology_Topic_1.csv')
colnames(Article_Ratings)[1] <- 'Article_num'

# Articles in Experiments:
Exp_art_num <- c(unique(Misl_False_Search_T$article_number),unique(Misl_False_Search_MF$article_number),unique(Experiment_3_Data$article_number),unique(Experiment_3_Data_T$article_number))


Article_Ratings_Exp <- Article_Ratings %>% filter(Article_num %in% Exp_art_num)




library(irr)

#Table Search:

#Main Topic IRR statistics:
#Transform from factor into string:
Article_Ratings_Exp$Rating <- as.character(Article_Ratings_Exp$Rating)

#Produce dataset with only  ideological perspective ratings:
Ideo_Rating_1 <- Article_Ratings_Exp %>% filter(Rating == 'Ideology')

#Only select data needed:
Ideo_Rating_1 <- Ideo_Rating_1 %>% select(Rater_1,Rater_2,Rater_3,Rater_4)

#standardize ratings so they are all upper case:
Ideo_Rating_1$Rater_1 <- toupper(Ideo_Rating_1$Rater_1)
Ideo_Rating_1$Rater_2 <- toupper(Ideo_Rating_1$Rater_2)
Ideo_Rating_1$Rater_3 <- toupper(Ideo_Rating_1$Rater_3)
Ideo_Rating_1$Rater_4 <- toupper(Ideo_Rating_1$Rater_4)


#Produce agreement:
Id_Agree_1 <- round(agree(Ideo_Rating_1)$value,2)

#Produce Fleis Kappa:
Id_FK_1 <- round(kappam.fleiss(Ideo_Rating_1)$value,2)




#Create matrix with Data:

Table_6_matrix <- matrix(c('Partisan lean of of articles (4 categories)','All Articles',Id_Agree_1,Id_FK_1),
                         nrow=1,
                         byrow=T)

#Rename columns:
colnames(Table_6_matrix) <- c('Coding Task','Group of Articles','Agreement','Fleiss Kappa')


#Write table to txt file:
write(print(xtable(Table_6_matrix,caption='Inter-Rater Reliability Statistics for Ideological Perspective of Articles'),
            include.rownames=FALSE,
            caption.placement='top'),file='.//tables//Table_Agreement.txt')







#Fleiss Kappa:




unique(Misl_False_Search_T$day)

unique(Experiment_3_Data_T$day)


FCer_Data <- read.csv('.//data//Fact_Checker_Data_Master.csv')

FCer_Data <- FCer_Data %>% select(Date,Pariticipant_1_Eval,Pariticipant_2_Eval,Pariticipant_3_Eval,Pariticipant_4_Eval,Pariticipant_5_Eval,Pariticipant_6_Eval)

Days_of_study <- c('7-Jan','20-Nov','21-Nov','3-Dec','4-Dec','5-Dec','9-Dec','10-Dec','11-Dec','8-Jan','9-Jan','13-Jan','14-Jan','15-Jan','21-Jan','23-Jan','27-Jan','28-Jan')

FCer_Data <- FCer_Data %>% filter(Date %in% Days_of_study)

FCer_Data$Pariticipant_1_Eval <- ifelse(is.na(FCer_Data$Pariticipant_1_Eval),FCer_Data$Pariticipant_6_Eval,FCer_Data$Pariticipant_1_Eval)
FCer_Data$Pariticipant_2_Eval <- ifelse(is.na(FCer_Data$Pariticipant_2_Eval),FCer_Data$Pariticipant_6_Eval,FCer_Data$Pariticipant_2_Eval)
FCer_Data$Pariticipant_3_Eval <- ifelse(is.na(FCer_Data$Pariticipant_3_Eval),FCer_Data$Pariticipant_6_Eval,FCer_Data$Pariticipant_3_Eval)
FCer_Data$Pariticipant_4_Eval <- ifelse(is.na(FCer_Data$Pariticipant_4_Eval),FCer_Data$Pariticipant_6_Eval,FCer_Data$Pariticipant_4_Eval)
FCer_Data$Pariticipant_5_Eval <- ifelse(is.na(FCer_Data$Pariticipant_5_Eval),FCer_Data$Pariticipant_6_Eval,FCer_Data$Pariticipant_5_Eval)
FCer_Data$Pariticipant_6_Eval <- ifelse(is.na(FCer_Data$Pariticipant_6_Eval),FCer_Data$Pariticipant_6_Eval,FCer_Data$Pariticipant_6_Eval)



FCer_Data <- FCer_Data %>% select(Pariticipant_1_Eval,Pariticipant_2_Eval,Pariticipant_3_Eval,Pariticipant_4_Eval,Pariticipant_5_Eval)



#Produce agreement:
Id_Agree_1 <- round(agree(FCer_Data)$value,2)

#Produce Fleis Kappa:
Id_FK_1 <- round(kappam.fleiss(FCer_Data)$value,2)







Table_6_matrix <- matrix(c('Veracity of Article (3 categories)','All Articles in Both Experiments',Id_Agree_1,Id_FK_1),
                         nrow=1,
                         byrow=T)

#Rename columns:
colnames(Table_6_matrix) <- c('Coding Task','Group of Articles','Agreement','Fleiss Kappa')


#Write table to txt file:
write(print(xtable(Table_6_matrix,caption='Inter-Rater Reliability Statistics for Veracity Evaluations of Articles'),
            include.rownames=FALSE,
            caption.placement='top'),file='.//tables//Table_FC_Agreement.txt')




############################## All Analyses - Likert - Categorical ##################################


Search_1 <- Misl_False_Search_T %>% select(Likert_Evaluation,Match_FC)
colnames(Search_1) <- c('Likert','Categorical')

Search_2 <- Misl_False_Search_MF %>% select(Likert_Evaluation,Susc_FN)
colnames(Search_2) <- c('Likert','Categorical')

Exp_3_1 <- Experiment_3_Data_T %>% select(Likert_Evaluation,Match_FC)
colnames(Exp_3_1) <- c('Likert','Categorical')

Exp_3_2 <- Experiment_3_Data %>% select(Likert_Evaluation,Susc_FN)
colnames(Exp_3_2) <- c('Likert','Categorical')

Compare_Lik_Cat <- rbind(Search_1,Search_2,Exp_3_1,Exp_3_2)

Compare_Lik_Cat$Likert <- as.numeric(Compare_Lik_Cat$Likert)

Compare_Lik_Cat <- na.omit(Compare_Lik_Cat)

Model_Compare_Measures <- lm(Likert ~ Categorical,data=Compare_Lik_Cat)


lin_results_Model_Compare_Measures = coeftest(Model_Compare_Measures)

#Number of observations:
Obs_Tab_1_1 <- nrow(Compare_Lik_Cat)
#R-squared
Rsq_Tab_1_1 <- rsq(Model_Compare_Measures)
A_Rsq_Tab_1_1 <- rsq(Model_Compare_Measures,adj=TRUE)
A_Rsq_Tab_1_1 <- round(A_Rsq_Tab_1_1,3)
Rsq_Tab_1_1 <- round(Rsq_Tab_1_1,3)

#F-statistic
glm.0 <- glm(Likert ~ 1,Compare_Lik_Cat)
Mod_summary <- summary(Model_Compare_Measures)
F_Tab_1_1 <-round(Mod_summary$fstatistic,3)
F_Tab_1_1 <- paste0(F_Tab_1_1,'^{***}')



Model_Compare_Measures <- glm(Likert ~ Categorical,data=Compare_Lik_Cat)


lin_results_Model_Compare_Measures = coeftest(Model_Compare_Measures)




write(stargazer(lin_results_Model_Compare_Measures,
                column.labels=c("(1)"),
                title="Measuring Correlation Between Evaluating an Article as True on the Ordinal Measure of Perceived Veracity",
                covariate.labels = c('Categorical Measure (True)'),
                align=TRUE,
                star.cutoffs = c(0.05, 0.01, 0.001),
                add.lines = list(c("Observations",Obs_Tab_1_1),
                                 c("R-squared",Rsq_Tab_1_1),
                                 c("Adj. R-squared",A_Rsq_Tab_1_1),
                                 c("F-Statistic",F_Tab_1_1))),file='.//tables//Table_Compare_Measures.txt')






######################## RUN IT WITHOUT CONDITIONING VARIABLES #######################################


#(Hypothesis 3.1)

Search_Separate <- Misl_False_Search_MF

#### (1.2) Create Figure - Search For Information:
Misl_False_Search_1 <- Search_Separate %>% select(Match_FC,Treat_Search,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
Misl_False_Search_1 = na.omit(Misl_False_Search_1)

Misl_False_Search_1$Match_FC <- as.numeric(Misl_False_Search_1$Match_FC)
Misl_False_Search_1$Age <- as.numeric(Misl_False_Search_1$Age)
Misl_False_Search_1$Treat_Search <- as.numeric(Misl_False_Search_1$Treat_Search)
Misl_False_Search_1$Education_Score <- as.numeric(Misl_False_Search_1$Education_Score)
Misl_False_Search_1$Income_Score <- as.numeric(Misl_False_Search_1$Income_Score)
Misl_False_Search_1$Ideology_Score <- as.numeric(Misl_False_Search_1$Ideology_Score)

#Run linear regression and produce coefficient values:
fit_Search_MF_T_1 = glm(Match_FC ~ Treat_Search, data = Misl_False_Search_1)
#Produce clustere standard errors:
CI_Search_MF_T_1 = coefci(fit_Search_MF_T_1, vcov. = vcovCL(fit_Search_MF_T_1, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))

lin_Search_Separate_MF_T_1 = coeftest(fit_Search_MF_T_1, vcov. = vcovCL(fit_Search_MF_T_1, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_1_1 <- nrow(Misl_False_Search_1)
#R-squared
Rsq_Tab_1_1 <- rsq(fit_Search_MF_T_1)
A_Rsq_Tab_1_1 <- rsq(fit_Search_MF_T_1,adj=TRUE)
A_Rsq_Tab_1_1 <- round(A_Rsq_Tab_1_1,3)
Rsq_Tab_1_1 <- round(Rsq_Tab_1_1,3)


#F-statistic
glm.0 <- glm(Match_FC ~ 1,data=Misl_False_Search_1)
Results <- anova(fit_Search_MF_T_1, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]
F_Tab_1_1 <- round(F_Tab_1_1,3)
F_Tab_1_1 <- paste0(F_Tab_1_1,'^{***}')

Search_Articles_1 <- unique(Misl_False_Search_1$Article_day)




#(Hypothesis 3.2)

Search_Separate <- Misl_False_Search_MF


Misl_False_Search_1 <- Search_Separate %>% select(Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
Misl_False_Search_1 = na.omit(Misl_False_Search_1)


Misl_False_Search_1$Susc_FN <- as.numeric(Misl_False_Search_1$Susc_FN)
Misl_False_Search_1$Age <- as.numeric(Misl_False_Search_1$Age)
Misl_False_Search_1$Treat_Search <- as.numeric(Misl_False_Search_1$Treat_Search)
Misl_False_Search_1$Education_Score <- as.numeric(Misl_False_Search_1$Education_Score)
Misl_False_Search_1$Income_Score <- as.numeric(Misl_False_Search_1$Income_Score)
Misl_False_Search_1$Ideology_Score <- as.numeric(Misl_False_Search_1$Ideology_Score)


Misl_False_Search_Fig_1 <- Misl_False_Search_1 %>% mutate(Treat_Search_Cat = ifelse(Treat_Search == 0,'Control', 'Treatment'))


#Run linear regression and produce coefficient values:
fit_Search_MF_T_2 = glm(Susc_FN ~ Treat_Search, data = Misl_False_Search_1)
#Produce clustere standard errors:
CI_Search_MF_T_2 = coefci(fit_Search_MF_T_2, vcov. = vcovCL(fit_Search_MF_T_2, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))

lin_Search_Separate_MF_T_2 = coeftest(fit_Search_MF_T_2, vcov. = vcovCL(fit_Search_MF_T_2, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_1_2 <- nrow(Misl_False_Search_1)
#R-squared
Rsq_Tab_1_2 <- rsq(fit_Search_MF_T_2)
A_Rsq_Tab_1_2 <- rsq(fit_Search_MF_T_2,adj=TRUE)
A_Rsq_Tab_1_2 <- round(A_Rsq_Tab_1_2,3)
Rsq_Tab_1_2 <- round(Rsq_Tab_1_2,3)


#F-statistic
glm.0 <- glm(Susc_FN ~ 1,data=Misl_False_Search_1)
Results <- anova(fit_Search_MF_T_2, glm.0, test="F")
F_Tab_1_2 <- Results$F[2]
F_Tab_1_2 <- round(F_Tab_1_2,3)
F_Tab_1_2 <- paste0(F_Tab_1_2,'^{***}')

Search_Articles_2 <- unique(Misl_False_Search_1$Article_day)



#(Hypothesis 3.3)

Search_Separate <- Misl_False_Search_T


Misl_False_Search_1 <- Search_Separate %>% select(Match_FC,Treat_Search,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
Misl_False_Search_1 = na.omit(Misl_False_Search_1)


Misl_False_Search_1$Match_FC <- as.numeric(Misl_False_Search_1$Match_FC)
Misl_False_Search_1$Age <- as.numeric(Misl_False_Search_1$Age)
Misl_False_Search_1$Treat_Search <- as.numeric(Misl_False_Search_1$Treat_Search)
Misl_False_Search_1$Education_Score <- as.numeric(Misl_False_Search_1$Education_Score)
Misl_False_Search_1$Income_Score <- as.numeric(Misl_False_Search_1$Income_Score)
Misl_False_Search_1$Ideology_Score <- as.numeric(Misl_False_Search_1$Ideology_Score)

#Run linear regression and produce coefficient values:
fit_Search_MF_T_3 = glm(Match_FC ~ Treat_Search, data = Misl_False_Search_1)
#Produce clustere standard errors:
CI_Search_MF_T_3 = coefci(fit_Search_MF_T_3, vcov. = vcovCL(fit_Search_MF_T_3, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))

lin_Search_Separate_MF_T_3 = coeftest(fit_Search_MF_T_3, vcov. = vcovCL(fit_Search_MF_T_3, cluster = list(Misl_False_Search_1$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_1_3 <- nrow(Misl_False_Search_1)
#R-squared
Rsq_Tab_1_3 <- rsq(fit_Search_MF_T_3)
A_Rsq_Tab_1_3 <- rsq(fit_Search_MF_T_3,adj=TRUE)
A_Rsq_Tab_1_3 <- round(A_Rsq_Tab_1_3,3)
Rsq_Tab_1_3 <- round(Rsq_Tab_1_3,3)


#F-statistic
glm.0 <- glm(Match_FC ~ 1,data=Misl_False_Search_1)
Results <- anova(fit_Search_MF_T_3, glm.0, test="F")
F_Tab_1_3 <- Results$F[2]
F_Tab_1_3 <- round(F_Tab_1_3,3)
F_Tab_1_3 <- paste0(F_Tab_1_3,'^{***}')

Search_Articles_3 <- unique(Misl_False_Search_1$Article_day)




#(Hypothesis 2.1)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)
Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Full_Data <- Exp_3_Data %>% filter(Headline == 0)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 1 | Article == 2 | Article == 3)
Full_Data$T_Dummy <- ifelse(Full_Data$Evaluation == 'T',1,0)

unique(Full_Data$Evaluation)


MF_Exp_3 <- Full_Data %>% select(T_Dummy,Standardize,Source,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$T_Dummy <- as.numeric(MF_Exp_3$T_Dummy)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)

#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_1 = glm(T_Dummy ~ Source, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_1 <- coefci(fit_Standardize_MF_T_1, vcov. = vcovCL(fit_Standardize_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_1 = coeftest(fit_Standardize_MF_T_1, vcov. = vcovCL(fit_Standardize_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_1 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_1 <- rsq(fit_Standardize_MF_T_1)
A_Rsq_Tab_3_1 <- rsq(fit_Standardize_MF_T_1,adj=TRUE)
A_Rsq_Tab_3_1 <- round(A_Rsq_Tab_3_1,3)
Rsq_Tab_3_1 <- round(Rsq_Tab_3_1,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_1, glm.0, test="F")
F_Tab_3_1 <- Results$F[2]
F_Tab_3_1 <- round(F_Tab_3_1,3)
F_Tab_3_1 <- paste0(F_Tab_3_1,'^{***}')

Search_Articles_7 <- unique(MF_Exp_3$Article_day)




#(Hypothesis 2.2)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)

Full_Data <- Exp_3_Data %>% filter(Headline == 0)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 4 | Article == 5)
Full_Data$T_Dummy <- ifelse(Full_Data$Evaluation == 'T',1,0)

unique(Full_Data$Evaluation)


MF_Exp_3 <- Full_Data %>% select(T_Dummy,Standardize,Source,Education_Score,Age,Gender,Income_Score,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$T_Dummy <- as.numeric(MF_Exp_3$T_Dummy)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)


#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_2 = glm(T_Dummy ~ Source, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_2 <- coefci(fit_Standardize_MF_T_2, vcov. = vcovCL(fit_Standardize_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_2 = coeftest(fit_Standardize_MF_T_2, vcov. = vcovCL(fit_Standardize_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))


#Number of observations:
Obs_Tab_3_2 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_2 <- rsq(fit_Standardize_MF_T_2)
A_Rsq_Tab_3_2 <- rsq(fit_Standardize_MF_T_2,adj=TRUE)
A_Rsq_Tab_3_2 <- round(A_Rsq_Tab_3_2,3)
Rsq_Tab_3_2 <- round(Rsq_Tab_3_2,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_2, glm.0, test="F")
F_Tab_3_2 <- Results$F[2]
F_Tab_3_2 <- round(F_Tab_3_2,3)
F_Tab_3_2 <- paste0(F_Tab_3_2,'^{***}')

Search_Articles_8 <- unique(MF_Exp_3$Article_day)


#(Hypothesis 2.3)


# (2.E)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Headline == 1)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 1 | Article == 2 | Article == 3)
Full_Data$T_Dummy <- ifelse(Full_Data$Evaluation == 'T',1,0)



MF_Exp_3 <- Full_Data %>% select(T_Dummy,Standardize,Source,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$T_Dummy <- as.numeric(MF_Exp_3$T_Dummy)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)



#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_3 = glm(T_Dummy ~ Source, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_3 <- coefci(fit_Standardize_MF_T_3, vcov. = vcovCL(fit_Standardize_MF_T_3, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_3 = coeftest(fit_Standardize_MF_T_3, vcov. = vcovCL(fit_Standardize_MF_T_3, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_5 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_5 <- rsq(fit_Standardize_MF_T_3)
A_Rsq_Tab_3_5 <- rsq(fit_Standardize_MF_T_3,adj=TRUE)
A_Rsq_Tab_3_5 <- round(A_Rsq_Tab_3_5,3)
Rsq_Tab_3_5 <- round(Rsq_Tab_3_5,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_3, glm.0, test="F")
F_Tab_3_5 <- Results$F[2]
F_Tab_3_5 <- round(F_Tab_3_5,3)
F_Tab_3_5 <- paste0(F_Tab_3_5,'^{***}')

Search_Articles_11 <- unique(MF_Exp_3$Article_day)




# (Hypothesis 2.4)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)


Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Dummy_Congruence <- as.numeric(Exp_3_Data$Dummy_Congruence)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Headline == 1)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 4 | Article == 5)
Full_Data$T_Dummy <- ifelse(Full_Data$Evaluation == 'T',1,0)

unique(Full_Data$Evaluation)


MF_Exp_3 <- Full_Data %>% select(T_Dummy,Standardize,Source,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)

MF_Exp_3$T_Dummy <- as.numeric(MF_Exp_3$T_Dummy)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Standardize <- as.numeric(MF_Exp_3$Standardize)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)


#Run linear regression and produce coefficient values:
fit_Standardize_MF_T_4 = glm(T_Dummy ~ Source, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_4 <- coefci(fit_Standardize_MF_T_4, vcov. = vcovCL(fit_Standardize_MF_T_4, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_4 = coeftest(fit_Standardize_MF_T_4, vcov. = vcovCL(fit_Standardize_MF_T_4, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_6 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_6 <- rsq(fit_Standardize_MF_T_4)
A_Rsq_Tab_3_6 <- rsq(fit_Standardize_MF_T_4,adj=TRUE)
A_Rsq_Tab_3_6 <- round(A_Rsq_Tab_3_6,3)
Rsq_Tab_3_6 <- round(Rsq_Tab_3_6,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_4, glm.0, test="F")
F_Tab_3_6 <- Results$F[2]
F_Tab_3_6 <- round(F_Tab_3_6,3)
F_Tab_3_6 <- paste0(F_Tab_3_6,'^{***}')

Search_Articles_12 <- unique(MF_Exp_3$Article_day)




#(Hypothesis 1.1) Standardized: Effect of Headline
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Dummy_Congruence <- as.numeric(Exp_3_Data$Dummy_Congruence)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Source == 0)

MF_Exp_3 <- Full_Data %>% select(Match_FC,Headline,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)


MF_Exp_3$Match_FC <- as.numeric(MF_Exp_3$Match_FC)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)


MF_Exp_3$Headline = ifelse(MF_Exp_3$Headline == 1,0,1)

#Run linear regression and produce coefficient values:
fit_Headline_MF_T_1 = glm(Match_FC ~ Headline, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Headline_MF_T_1 <- coefci(fit_Headline_MF_T_1, vcov. = vcovCL(fit_Headline_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Headline_MF_T_1 = coeftest(fit_Headline_MF_T_1, vcov. = vcovCL(fit_Headline_MF_T_1, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_4_1 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_4_1 <- rsq(fit_Headline_MF_T_1)
A_Rsq_Tab_4_1 <- rsq(fit_Headline_MF_T_1,adj=TRUE)
A_Rsq_Tab_4_1 <- round(A_Rsq_Tab_4_1,3)
Rsq_Tab_4_1 <- round(Rsq_Tab_4_1,3)


#F-statistic
glm.0 <- glm(Match_FC ~ 1,data=MF_Exp_3)
Results <- anova(fit_Headline_MF_T_1, glm.0, test="F")
F_Tab_4_1 <- Results$F[2]
F_Tab_4_1 <- round(F_Tab_4_1,3)
F_Tab_4_1 <- paste0(F_Tab_4_1,'^{***}')

Search_Articles_15 <- unique(MF_Exp_3$Article_day)





#(Hypothesis 1.2) With Source: Effect of Headline
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)

Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Dummy_Congruence <- as.numeric(Exp_3_Data$Dummy_Congruence)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Source == 1)

MF_Exp_3 <- Full_Data %>% select(Match_FC,Headline,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId,Ideology_Score)
#Remove NA values:
MF_Exp_3 <- na.omit(MF_Exp_3)


MF_Exp_3$Match_FC <- as.numeric(MF_Exp_3$Match_FC)
MF_Exp_3$Age <- as.numeric(MF_Exp_3$Age)
MF_Exp_3$Education_Score <- as.numeric(MF_Exp_3$Education_Score)
MF_Exp_3$Income_Score <- as.numeric(MF_Exp_3$Income_Score)
MF_Exp_3$Ideology_Score <- as.numeric(MF_Exp_3$Ideology_Score)

MF_Exp_3$Headline = ifelse(MF_Exp_3$Headline == 1,0,1)


#Run linear regression and produce coefficient values:
fit_Headline_MF_T_2 = glm(Match_FC ~ Headline, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Headline_MF_T_2 <- coefci(fit_Headline_MF_T_2, vcov. = vcovCL(fit_Headline_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Headline_MF_T_2 = coeftest(fit_Headline_MF_T_2, vcov. = vcovCL(fit_Headline_MF_T_2, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_4_2 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_4_2 <- rsq(fit_Headline_MF_T_2)
A_Rsq_Tab_4_2 <- rsq(fit_Headline_MF_T_2,adj=TRUE)
A_Rsq_Tab_4_2 <- round(A_Rsq_Tab_4_2,3)
Rsq_Tab_4_2 <- round(Rsq_Tab_4_2,3)


#F-statistic
glm.0 <- glm(Match_FC ~ 1,data=MF_Exp_3)
Results <- anova(fit_Headline_MF_T_2, glm.0, test="F")
F_Tab_4_2 <- Results$F[2]
F_Tab_4_2 <- round(F_Tab_4_2,3)
F_Tab_4_2 <- paste0(F_Tab_4_2,'^{***}')

Search_Articles_16 <- unique(MF_Exp_3$Article_day)









##  TABLES  ---  Categorical Score:


write(stargazer(lin_results_Headline_MF_T_1,
                lin_results_Headline_MF_T_2,
                lin_Search_Separate_MF_T_2,
                lin_Search_Separate_MF_T_1,
                model.numbers=FALSE,
                column.labels=c("(H1.1)","(H1.2)","(H3.1)","(H3.2)"),
                title="Measuring Effect of Additional Information on Matching the Fact-Checker's Evaluation Without Conditioning on Control Variables",
                align=TRUE,
                omit = c(Search_Articles_1,Search_Articles_2,Search_Articles_3,
                         Search_Articles_7,Search_Articles_8,Search_Articles_11,Search_Articles_12,
                         Search_Articles_15,Search_Articles_16,
                         'Constant','GenderOther'),
                covariate.labels = c('Treatment (Full Text)','Treatment (Search)'),
                star.cutoffs = c(0.05, 0.01, 0.001),
                add.lines = list(c("Observations",Obs_Tab_1_1,Obs_Tab_4_1,Obs_Tab_4_2),
                                 c("R-squared",Rsq_Tab_1_1,Rsq_Tab_4_1,Rsq_Tab_4_2),
                                 c("Adj. R-squared",A_Rsq_Tab_1_1,A_Rsq_Tab_4_1,A_Rsq_Tab_4_2),
                                 c("F-Statistic",F_Tab_1_1,F_Tab_4_1,F_Tab_4_2))),file='.//tables//Table_1_JEPS_No_Controls.txt')




write(stargazer(lin_results_Standardize_MF_T_1,
                lin_results_Standardize_MF_T_2,
                lin_results_Standardize_MF_T_3,
                lin_results_Standardize_MF_T_4,
                lin_Search_Separate_MF_T_3,
                model.numbers=FALSE,
                column.labels=c("(H2.1)","(H2.2)","(H2.3)","(H2.4)","(H3.3)"),
                title="Measuring Effect of Additional Information on Rating an Article as True Without Conditioning on Control Variables",
                align=TRUE,
                omit = c(Search_Articles_1,Search_Articles_2,Search_Articles_3,
                         Search_Articles_7,Search_Articles_8,Search_Articles_11,Search_Articles_12,
                         Search_Articles_15,Search_Articles_16,
                         'Constant','GenderOther'),
                covariate.labels = c('Treatment (Source)','Treatment (Search)','Education','Age','Gender (Male)','Income','Ideology'),
                star.cutoffs = c(0.05, 0.01, 0.001),
                add.lines = list(c("Observations",Obs_Tab_1_2,Obs_Tab_1_3,Obs_Tab_3_1,Obs_Tab_3_2,Obs_Tab_3_5,Obs_Tab_3_6),
                                 c("R-squared",Rsq_Tab_1_2,Rsq_Tab_1_3,Rsq_Tab_3_1,Rsq_Tab_3_2,Rsq_Tab_3_5,Rsq_Tab_3_6),
                                 c("Adj. R-squared",A_Rsq_Tab_1_2,A_Rsq_Tab_1_3,A_Rsq_Tab_3_1,A_Rsq_Tab_3_2,A_Rsq_Tab_3_5,A_Rsq_Tab_3_6),
                                 c("F-Statistic",F_Tab_1_2,F_Tab_1_3,F_Tab_3_1,F_Tab_3_2,F_Tab_3_5,F_Tab_3_6))),file='.//tables//Table_2_JEPS_No_Controls.txt')





















