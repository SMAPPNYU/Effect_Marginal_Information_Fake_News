
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

############################# Load Data ###################################################

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

colnames(Matrix_D) <- c('Article Type','Num. Obs','Age','Education','Gender')





library(xtable)
xtable(Matrix_D)


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
fit_Standardize_MF_T_5 = glm(T_Dummy ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_5 <- coefci(fit_Standardize_MF_T_5, vcov. = vcovCL(fit_Standardize_MF_T_5, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_5 = coeftest(fit_Standardize_MF_T_5, vcov. = vcovCL(fit_Standardize_MF_T_5, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_5 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_5 <- rsq(fit_Standardize_MF_T_5)
A_Rsq_Tab_3_5 <- rsq(fit_Standardize_MF_T_5,adj=TRUE)
A_Rsq_Tab_3_5 <- round(A_Rsq_Tab_3_5,3)
Rsq_Tab_3_5 <- round(Rsq_Tab_3_5,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_5, glm.0, test="F")
F_Tab_3_5 <- Results$F[2]
F_Tab_3_5 <- round(F_Tab_3_5,3)
F_Tab_3_5 <- paste0(F_Tab_3_5,'^{***}')

Search_Articles_11 <- unique(MF_Exp_3$Article_day)




# (Hypothesis 2.4)
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)


Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Ideology_Score <- as.numeric(Exp_3_Data$Ideology_Score)

Full_Data <- Exp_3_Data %>% filter(Headline == 1)
Full_Data$Standardize <- ifelse(Full_Data$Source == 0,1,0)
Full_Data <- Full_Data %>% filter(Article == 4 | Article == 5)
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
fit_Standardize_MF_T_6 = glm(T_Dummy ~ Source + Education_Score + Age + Gender + Income_Score + Ideology_Score, data = MF_Exp_3)
#Produce clustere standard errors:
CI_Standardize_MF_T_6 <- coefci(fit_Standardize_MF_T_6, vcov. = vcovCL(fit_Standardize_MF_T_6, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

lin_results_Standardize_MF_T_6 = coeftest(fit_Standardize_MF_T_6, vcov. = vcovCL(fit_Standardize_MF_T_6, cluster = list(MF_Exp_3$ResponseId), type = "HC0"))

#Number of observations:
Obs_Tab_3_6 <- nrow(MF_Exp_3)
#R-squared
Rsq_Tab_3_6 <- rsq(fit_Standardize_MF_T_6)
A_Rsq_Tab_3_6 <- rsq(fit_Standardize_MF_T_6,adj=TRUE)
A_Rsq_Tab_3_6 <- round(A_Rsq_Tab_3_6,3)
Rsq_Tab_3_6 <- round(Rsq_Tab_3_6,3)


#F-statistic
glm.0 <- glm(T_Dummy ~ 1,data=MF_Exp_3)
Results <- anova(fit_Standardize_MF_T_6, glm.0, test="F")
F_Tab_3_6 <- Results$F[2]
F_Tab_3_6 <- round(F_Tab_3_6,3)
F_Tab_3_6 <- paste0(F_Tab_3_6,'^{***}')

Search_Articles_12 <- unique(MF_Exp_3$Article_day)




#(Hypothesis 1.1) Standardized: Effect of Headline
Exp_3_Data <- rbind(Experiment_3_Data,Experiment_3_Data_T)


Exp_3_Data$Headline <- as.numeric(Exp_3_Data$Headline)
Exp_3_Data$Source <- as.numeric(Exp_3_Data$Source)
Exp_3_Data$Article <- as.numeric(Exp_3_Data$Article)
Exp_3_Data$Congruent_Source <- as.numeric(Exp_3_Data$Congruent_Source)
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







####################################################################################################################################################################


Coef_names <- rev(c('[H1.1 Full Text]\n{All News}\n(No Source)',
                    '[H1.2 Full Text]\n{All News}\n(Source)'))


Coefficients <- c(fit_Headline_MF_T_1$coefficients[2],
                  fit_Headline_MF_T_2$coefficients[2])

CI_Upper <- c(CI_Headline_MF_T_1[2,2],
              CI_Headline_MF_T_2[2,2])            

CI_Lower <- c(CI_Headline_MF_T_1[2,1],
              CI_Headline_MF_T_2[2,1])           


d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()

d_matrix <- data.frame(d_matrix)


d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)

d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)


d_matrix <- d_matrix %>% arrange(desc(row_number()))




d_matrix$x<-c(0.1,0.2)



ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Period") +
  ylab("\nEffect of piece of information [in brackets] on matching                                        \nfact-checker's evaluation of a type of article {in braces}                                      \n when other pieces of information are provided (in parentheses)                                   ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.15,0.15) +
  scale_x_continuous(" \n",breaks=c(0.1,0.2),labels=Coef_names,limits=c(0.0,0.3)) +
  coord_flip()

ggsave('.//figures//Coefficients_1.png',height=8,width=8)



Coef_names <- c('[H2.4 Source]\n{Mainstream News}\n(Headline/Lede)',
                '[H2.3 Source]\n{Low-Quality News}\n(Headline/Lede)',
                '[H2.2 Source]\n{Mainstream News}\n(Full Text)',
                '[H2.1 Source]\n{Low-Quality News}\n(Full Text)')

Coefficients <- c(fit_Standardize_MF_T_6$coefficients[2],
                  fit_Standardize_MF_T_5$coefficients[2],
                  fit_Standardize_MF_T_2$coefficients[2],
                  fit_Standardize_MF_T_1$coefficients[2])


CI_Upper <- c(CI_Standardize_MF_T_6[2,2],
              CI_Standardize_MF_T_5[2,2],
              CI_Standardize_MF_T_2[2,2],
              CI_Standardize_MF_T_1[2,2])            



CI_Lower <- c(CI_Standardize_MF_T_6[2,1],
              CI_Standardize_MF_T_5[2,1],
              CI_Standardize_MF_T_2[2,1],
              CI_Standardize_MF_T_1[2,1])



d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()

d_matrix <- data.frame(d_matrix)


d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)

d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)


d_matrix <- d_matrix %>% arrange(desc(row_number()))




d_matrix$x<-c(0.1,0.2,0.3,0.4)



ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=3) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Period") +
  ylab("\nEffect of providing piece of information [in brackets]                                  \n on rating a type of news article {in braces} as true                                     \n when other pieces of information are provided (in parentheses)                                        ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.15,0.15) +
  scale_x_continuous(" \n",breaks=c(0.1,0.2,0.3,0.4),labels=Coef_names,limits=c(0.0,0.5)) +
  coord_flip()


ggsave('.//figures//Coefficients_2.png',height=8,width=8)




Coef_names <- rev(c('[H3.1 External Information]\n{True News}\n(Source and Full Text)',
                    '[H3.2 External Information]\n{False/Misleading News}\n(Source and Full Text)'))


Coefficients <- c(fit_Search_MF_T_1$coefficients[2],
                  fit_Search_MF_T_3$coefficients[2])

CI_Upper <- c(CI_Search_MF_T_1[2,2],
              CI_Search_MF_T_3[2,2])            

CI_Lower <- c(CI_Search_MF_T_1[2,1],
              CI_Search_MF_T_3[2,1])           


d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()

d_matrix <- data.frame(d_matrix)


d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)

d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)


d_matrix <- d_matrix %>% arrange(desc(row_number()))




d_matrix$x<-c(0.1,0.2)



ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Period") +
  ylab("\nEffect of piece of information [in brackets] on matching                                                  \nfact-checker's evaluation of a type of article {in braces}                                                 \n when other pieces of information are provided (in parentheses)                                                ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.15,0.15) +
  scale_x_continuous(" \n",breaks=c(0.1,0.2),labels=Coef_names,limits=c(0.0,0.3)) +
  coord_flip()

ggsave('.//figures//Coefficients_3.png',height=8,width=8)



Coef_names <- c('[H3.3 External Information]\n{False/Misleading News}\n(Source and Full Text)')

Coefficients <- c(fit_Search_MF_T_2$coefficients[2])

CI_Upper <- c(CI_Search_MF_T_2[2,2])            

CI_Lower <- c(CI_Search_MF_T_2[2,1])



d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()

d_matrix <- data.frame(d_matrix)


d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)

d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)


d_matrix <- d_matrix %>% arrange(desc(row_number()))




d_matrix$x<-c(0.1)



ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=3) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Period") +
  ylab("\nEffect of providing piece of information [in brackets]                                             \n on rating a type of news article {in braces} as true                                                   \n when other pieces of information are provided (in parentheses)                                                     ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.15,0.15) +
  scale_x_continuous(" \n",breaks=c(0.1),labels=Coef_names,limits=c(0.0,0.2)) +
  coord_flip()


ggsave('.//figures//Coefficients_4.png',height=8,width=8)







