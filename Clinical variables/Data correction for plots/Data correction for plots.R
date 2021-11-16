library(dplyr)
library(readxl)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(car)
library(nlme)
library(stargazer)
library(dotwhisker)
library(sjPlot)
require(xtable)
library(TSA)
library(jtools)
library(emmeans)
library(graphics)
library(tidyr)
library(ordinal)
library(viridis)
library(lmerTest)
library(xlsx)


## ---- echo=FALSE-----------------------------------------------------------------------------------
data_Female<-read_excel("data/210330_Gubert_EE-EX_Data_All.xlsx","Female_EEEX_Data_Final")
#converting water content into the same format as Males
data_Female$FH20_Wk7<-data_Female$FH20_Wk7*100
data_Female$FH20_Wk8<-data_Female$FH20_Wk8*100
data_Female$FH20_Wk9<-data_Female$FH20_Wk9*100
data_Female$FH20_Wk10<-data_Female$FH20_Wk10*100
data_Female$FH20_Wk11<-data_Female$FH20_Wk11*100
data_Female$FH20_Wk12<-data_Female$FH20_Wk12*100
#Counting the number of columns with missing values
countNA <- function(df) apply(df, MARGIN = 1, FUN = function(x) length(x[is.na(x)]))
#Filter out sample with more than 80% missing values
new_f_data<-data_Female %>% filter(countNA(data_Female)<60)
# Extracting Colon length columns for the analysis
data_female_clean<-new_f_data[,2:4] %>%mutate(Gender=rep("Female",69)) %>%
  mutate(new_f_data[,6],new_f_data[,c(34,35,42:45,48,49,54,55,58:65,68:75)])

data_Male<-read_excel("data/210330_Gubert_EE-EX_Data_All.xlsx","Male_EEEX_Data_Final")
#converting water content into nearest integer
data_Male$FH20_Wk12<-round(data_Male$FH20_Wk12)
#Counting the number of columns with missing values
countNA <- function(df) apply(df, MARGIN = 1, FUN = function(x) length(x[is.na(x)]))
#Filter out sample with more than 80% missing values
new_m_data<-data_Male %>% filter(countNA(data_Male)<60)
# Extracting interested columns for the analysis
data_male_clean<-new_m_data[,c(2,4:5)] %>%mutate(Gender=rep("Male",70)) %>%
  mutate(new_m_data[,1],new_m_data[,c(65, 59,64,60,61,63,39,40,45,46,49:56,66:73)])
#colnames(data_female_clean);colnames(data_male_clean)
colnames(data_female_clean)<-colnames(data_male_clean)

# Combine both Males and Females
data_clean<-rbind(data_female_clean, data_male_clean)

#Converting Genotype, Housing, Gender and Box to be factor variables
# When creating the factors put the reference category first in the levels argument 
data_clean$Genotype<-factor(data_clean$Genotype,levels = c("WT", "HD"))
data_clean$Housing <-factor(data_clean$Housing , levels = c("SH","EE","EX"))
data_clean$Gender<-factor(data_clean$Gender, levels = c("Female","Male"))
data_clean$Box <-factor(data_clean$Box )
colnames(data_clean)[which(names(data_clean) == "Gender")] <- "Sex"


## ---- echo=FALSE-----------------------------------------------------------------------------------
data_female_clean_time<-new_f_data[,2:4] %>%mutate(Gender=rep("Female",69)) %>%
  mutate(new_f_data[,c(6:20,28:33,36:41)])
data_male_clean_time<-new_m_data[,c(2,4:5)] %>%mutate(Gender=rep("Male",70)) %>%
  mutate(new_m_data[,1],new_m_data[,c(13:38)])
#colnames(data_female_clean);colnames(data_male_clean)
colnames(data_male_clean_time)<-colnames(data_female_clean_time)

# Combine both Males and Females
data_clean_time<-rbind(data_female_clean_time, data_male_clean_time)

#Converting Genotype, Housing, Gender and Box to be factor variables
data_clean_time$Genotype<-factor(data_clean_time$Genotype, levels=c("WT","HD"))
data_clean_time$Housing <-factor(data_clean_time$Housing ,  levels = c("SH","EE","EX"))
data_clean_time$Gender<-factor(data_clean_time$Gender, levels=c("Female", "Male"))
data_clean_time$Box <-factor(data_clean_time$Box )

colnames(data_clean_time)[which(names(data_clean_time) == "Gender")] <- "Sex"

long_data<-data_clean_time %>% 
  gather(v, value, Weight_Wk6:FH20_Wk12) %>% 
  separate(v, c("col", "Week"),sep="_Wk") %>% 
  arrange(MOUSE_ID) %>% 
  spread(col, value)


long_data$Week <-as.numeric(long_data$Week)
long_data_sorted<-long_data[order(long_data$MOUSE_ID,long_data$Box,long_data$Week),]
rotarod_data<-long_data_sorted[,c(1:6,9)]%>%filter(Week!=6)

######################## Food and Water intake per box by Gender#######

data_intake_Male<-read_excel("data/210330_Gubert_EE-EX_Data_All.xlsx","Male_EEEX_Data_Final_Food_H20")
#Sorting the data by Box number and inclusding a Sex column
data_intake_Male_sorted<-data_intake_Male[order(data_intake_Male$Box),-4]
data_intake_Male_sorted<-data_intake_Male_sorted%>%mutate(Sex=rep("Male", length(data_intake_Male_sorted$Box)), .after=Housing)

data_intake_Female<-read_excel("data/210330_Gubert_EE-EX_Data_All.xlsx","Female_EEEX_Data_Final_Food_H20",skip=1)


# Matching the Column order to Males and inclusding a Sex column
data_intake_Female_sorted<-data_intake_Female[,c(3,1,2,4:15)]
data_intake_Female_sorted<-data_intake_Female_sorted%>%mutate(Sex=rep("Female", length(data_intake_Female_sorted$...3)), .after=...2)

#colnames(data_intake_Female_sorted);colnames(data_intake_Male_sorted)
colnames(data_intake_Female_sorted)<-colnames(data_intake_Male_sorted)

food_intake<-rbind(data_intake_Female_sorted,data_intake_Male_sorted)

long_data_food<-food_intake %>% 
  gather(v, value, Food_Intake_Wk6:Water_Intake_Wk11) %>% 
  separate(v, c("col", "Week"), sep="_Wk") %>% 
  arrange(Box) %>% 
  spread(col, value)

long_data_food$Week <-as.numeric(long_data_food$Week)
food_sorted<-long_data_food[order(long_data_food$Box,long_data_food$Week),]

food_sorted$Genotype<-factor(food_sorted$Genotype, levels=c("WT","HD"))
food_sorted$Housing <-factor(food_sorted$Housing ,  levels = c("SH","EE","EX"))
food_sorted$Sex<-factor(food_sorted$Sex, levels=c("Female", "Male"))
food_sorted$Box <-factor(food_sorted$Box )

########Function to correct for Interactions######
corrData<-function( yVariable, dataSet, formula,indexMain){
  f <- formula(paste(yVariable, formula, sep = "~"))
  lmeModel <- lme(fixed = f, random = ~ 1 |Box, data = subset(dataSet, !is.na(eval(parse(text =yVariable )))))
  fixedCoef<-t(as.matrix( lmeModel$coefficients$fixed))
  fixedCoef_Interaction<-t(as.matrix(fixedCoef[,-indexMain]))
  modMatrix<-t(model.matrix(f, 
                            data = subset(dataSet, !is.na(dataSet[,yVariable]))))
  
  
  modMatrix_Interaction<-modMatrix[-indexMain,]
  newVarName<-paste0("New",yVariable)
  Cordata = subset(dataSet, !is.na(dataSet[,yVariable]))
  assign(newVarName,Cordata[,yVariable]-fixedCoef_Interaction%*%modMatrix_Interaction)
  CordataNew=cbind(Cordata[,indexMain], get(newVarName))
  return(CordataNew)
}

d1<-corrData( "Rotarod", rotarod_data, "Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype", indexMain=1:6)

write.xlsx(d1, file="CorrectedData_v2.xlsx", sheetName="Rotarod", append=TRUE)

vNames=c("Weight","FH20", "FOutput")
for(i in 1:length(vNames)){
     d1<-corrData( vNames[i], long_data_sorted, "Genotype + Housing+ Sex+Week+
             Genotype:Sex+Sex:Housing+
             Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype", indexMain=1:6)
    write.xlsx(d1, file="CorrectedData_v2.xlsx", sheetName=vNames[i], append=TRUE)
}


vNames=c("Food_Intake", "Water_Intake")
for(i in 1:length(vNames)){
  d1<-corrData( vNames[i], food_sorted, "Genotype + Housing+ Sex+Week+
             Genotype:Sex+Sex:Housing+
             Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype", indexMain=1:6)
  write.xlsx(d1, file="CorrectedData_v2.xlsx", sheetName=vNames[i], append=TRUE)
}        


vNames=c("Colon_Length","Gut_Permeability_FITC","Gut_Transit_Time",
         "Brain_Weight", "Caecum_Weight", "Caecum_Length",
         "DG_Swing_Fore","DG_Swing_Hind","DG_Stride_Fore",
         "DG_Stride_Hind","DG_Stride_Length_Fore","DG_Stride_Length_Hind",
         "DG_Absolute_Paw_Angle_Fore","DG_Absolute_Paw_Angle_Hind",
         "DG_Stance_Width_Fore","DG_Stance_Width_Hind",
         "DG_Propel_Brake_Ratio_FORE","DG_Propel_Brake_Ratio_HIND",
         "Acetate","Proprionate","Isobutyrate","Butyrate",
         "Methylbutyrate2","Isovalerate", "Valerate","Caproate")
for(i in 1:length(vNames)){
  d1<-corrData( vNames[i], data_clean, "Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype", indexMain=1:5)

  write.xlsx(d1, file="CorrectedData_v2.xlsx", sheetName=vNames[i], append=TRUE)
}
