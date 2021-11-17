## ----global_options, include=FALSE----------------------------------------------
library(knitr)
# global options to show by default the code, dump the figures into /Figures etc
knitr::opts_chunk$set(dpi = 100, 
                      echo=TRUE, 
                      warning=FALSE, message=FALSE, eval = TRUE,
                      fig.show=TRUE, fig.width= 5,fig.height= 5,fig.align='center', out.width = '50%', fig.path= 'Figures/')
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
library(ggpubr)
library(rstatix)
library(patchwork)
library(kableExtra)


## ---- echo=FALSE----------------------------------------------------------------
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


## ----echo=FALSE-----------------------------------------------------------------
count_table <- xtabs(~Genotype+Housing+Sex, data=data_clean)
ftable(count_table) # print table


## ---- echo=FALSE----------------------------------------------------------------
m1 <- lme(Brain_Weight ~ Genotype + Housing+ 
                    Sex+Genotype:Sex+Housing:Sex+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Brain_Weight)))

#summary(mixed.lmer)

#tab_model(mixed.lmer)

stargazer(m1, type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE,dep.var.labels="Brain weight")


## ---- echo=FALSE----------------------------------------------------------------
M1 <- lme(Gut_Permeability_FITC ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Gut_Permeability_FITC)))
M2 <- lme(Gut_Transit_Time ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Gut_Transit_Time)))
M3 <- lme(Colon_Length~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Colon_Length)))
M4 <- lme(Caecum_Weight ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Caecum_Weight)))

stargazer(M1,M2,M3,M4, type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE,
         dep.var.labels=c("Gut permeability", "Gut transit time","Colon length","Caecum weight"))


## ---- echo=FALSE----------------------------------------------------------------

M1 <- lme(Caecum_Length ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Caecum_Length)))
M2 <- lme(DG_Swing_Fore ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Swing_Fore)))
M3 <- lme(DG_Swing_Hind ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Swing_Hind)))
M4 <- lme(DG_Stride_Fore ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Stride_Fore)))

stargazer(M1,M2,M3,M4, type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE,
         dep.var.labels=c("Caecum length", "Swing time forepaw","Swing time hind paw","Stride time forepaw"))



## ---- echo=FALSE----------------------------------------------------------------

M1 <- lme(DG_Stride_Hind ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Stride_Hind)))
M2 <- lme(DG_Stride_Length_Fore ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Stride_Length_Fore)))
M3 <- lme(DG_Stride_Length_Hind ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Stride_Length_Hind)))

stargazer(M1,M2,M3, type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE,
         dep.var.labels=c("Stride time hind paw", "Stride length forepaw","Stride length hindpaw"))



## ---- echo=FALSE----------------------------------------------------------------

M1 <- lme(DG_Absolute_Paw_Angle_Fore ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Absolute_Paw_Angle_Fore)))
M2 <- lme(DG_Absolute_Paw_Angle_Hind ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Absolute_Paw_Angle_Hind)))
stargazer(M1,M2, type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE,
         dep.var.labels=c("Absolute paw angle forepaw", "Absolute paw angle hindpaw"))


## ---- echo=FALSE----------------------------------------------------------------
M1 <- lme(DG_Stance_Width_Fore ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Stance_Width_Fore)))
M2 <- lme(DG_Stance_Width_Hind ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Stance_Width_Hind)))


stargazer(M1,M2,  type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE, 
         dep.var.labels=c("Stance width forepaw", "Stance width hindpaw"))



## ---- echo=FALSE----------------------------------------------------------------

M1 <- lme(DG_Propel_Brake_Ratio_FORE ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Propel_Brake_Ratio_FORE)))
M2 <- lme(DG_Propel_Brake_Ratio_HIND ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Propel_Brake_Ratio_HIND)))

stargazer(M1,M2,  type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE, 
         dep.var.labels=c("propel:brake ratio forepaw","propel:brake ratio hind paw"))



## ---- echo=FALSE----------------------------------------------------------------

M1 <- lme(Acetate ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Acetate)))

M2 <- lme(Proprionate ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Proprionate)))

M3 <- lme(Isobutyrate ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Isobutyrate)))
M4 <- lme(Butyrate ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Butyrate)))

stargazer(M1,M2,M3,M4, type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE)



## ---- echo=FALSE----------------------------------------------------------------

M1<- lme(Methylbutyrate2 ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Methylbutyrate2)))


M2 <- lme(Isovalerate ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Isovalerate)))
M3 <- lme(Valerate ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Valerate)))
M4 <- lme(Caproate ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Caproate)))

stargazer(M1,M2,M3,M4, type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE)



## ---- echo=FALSE----------------------------------------------------------------
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
#Filtering out Week 6 from Rotarod data
rotarod_data<-long_data_sorted[,c(1:6,9)]%>%filter(Week!=6)

m1 <- lme(Weight ~ Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype,
          random =(~1|Box), data = subset(long_data_sorted, !is.na(Weight)))
m2 <- lme(Rotarod ~ Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype,
          random =(~1|Box), data = subset(rotarod_data, !is.na(Rotarod)))
m3<- lme(FH20 ~ Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype,
          random =(~1|Box), data = subset(long_data_sorted, !is.na(FH20)))
m4 <- lme(FOutput ~ Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype,
          random =(~1|Box), data = subset(long_data_sorted, !is.na(FOutput)))

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

m5<- lme(Food_Intake ~ Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype,
          random =(~1|Box), data = subset(food_sorted, !is.na(Food_Intake)))
m6 <- lme(Water_Intake ~ Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype,
          random =(~1|Box), data = subset(food_sorted, !is.na(Water_Intake)))



stargazer(m5,m6,m1,m2,m3,m4, type = "text",
          report = ('vc*p'),
          digits = 3,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          digit.separator = "", notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001", "Constants are significant (p<0.001)"), notes.append = FALSE)




## ---- echo=FALSE----------------------------------------------------------------
data_female_clasping<-new_f_data[,2:4] %>%mutate(Gender=rep("Female",69)) %>%
  mutate(new_f_data[,6],new_f_data[,c(21:27)])
data_male_clasping<-new_m_data[,c(2,4:5)] %>%mutate(Gender=rep("Male",70)) %>%
  mutate(new_m_data[,1],new_m_data[,c(6:12)])
#colnames(data_female_clean);colnames(data_male_clean)
colnames(data_male_clasping)<-colnames(data_female_clasping)

# Combine both Males and Females
data_clasping<-rbind(data_female_clasping, data_male_clasping)

#Converting Genotype, Housing, Gender and Box to be factor variables
data_clasping$Genotype<-factor(data_clasping$Genotype, levels=c("WT","HD"))
data_clasping$Housing <-factor(data_clasping$Housing ,  levels = c("SH","EE","EX"))
data_clasping$Gender<-factor(data_clasping$Gender, levels=c("Female", "Male"))
data_clasping$Box <-factor(data_clasping$Box )
data_clasping$Clasping_Wk6<-factor(data_clasping$Clasping_Wk6, ordered = TRUE , levels = c(0:4))
data_clasping$Clasping_Wk7<-factor(data_clasping$Clasping_Wk7, ordered = TRUE , levels = c(0:4))
data_clasping$Clasping_Wk8<-factor(data_clasping$Clasping_Wk8, ordered = TRUE , levels = c(0:4))
data_clasping$Clasping_Wk9<-factor(data_clasping$Clasping_Wk9, ordered = TRUE , levels = c(0:4))
data_clasping$Clasping_Wk10<-factor(data_clasping$Clasping_Wk10, ordered = TRUE , levels = c(0:4))
data_clasping$Clasping_Wk11<-factor(data_clasping$Clasping_Wk11, ordered = TRUE , levels = c(0:4))
data_clasping$Clasping_Wk12<-factor(data_clasping$Clasping_Wk12, ordered = TRUE , levels = c(0:4))

colnames(data_clasping)[which(names(data_clasping) == "Gender")] <- "Sex"
Clasping_Week6<-summary(data_clasping$Clasping_Wk6)
Clasping_Week7<-summary(data_clasping$Clasping_Wk7)
Clasping_Week8<-summary(data_clasping$Clasping_Wk8)
Clasping_Week9<-summary(data_clasping$Clasping_Wk9)
Clasping_Week10<-summary(data_clasping$Clasping_Wk10)[-6]
Clasping_Week11<-summary(data_clasping$Clasping_Wk11)
Clasping_Week12<-summary(data_clasping$Clasping_Wk12)
count<-rbind(Clasping_Week6,Clasping_Week7,Clasping_Week8,Clasping_Week9,Clasping_Week10,Clasping_Week11,Clasping_Week12)
count
data_c<-data.frame(Week=c(6:12),count) %>% gather(Clasping, value, X0:X4)
data_c$Clasping<-factor(data_c$Clasping, levels = c("X0","X1","X2","X3","X4"), labels = c(0:4), ordered = TRUE)



## ----barplot_clas, echo = FALSE, fig.cap ='Bar plot for clasping over the 5 weeks', fig.pos="H",  fig.width = 10, fig.height = 9,  out.width = "1\\textwidth"----
ggplot(data_c, aes(fill=Clasping, y=value, x=Week)) + 
    geom_bar( stat="identity", position = position_stack(reverse = TRUE)) +
    scale_fill_viridis(discrete = T)+ylab("Count")


## ----echo=FALSE-----------------------------------------------------------------
long_data_clasping<-data_clasping %>% 
  gather(v, value, Clasping_Wk6:Clasping_Wk12) %>% 
  separate(v, c("col", "Week"),sep="_Wk") %>% 
  arrange(MOUSE_ID) %>% 
  spread(col, value)


long_data_clasping$Week <-as.numeric(long_data_clasping$Week)
long_data_clasping_sorted<-long_data_clasping[order(long_data_clasping$MOUSE_ID,
                                                    long_data_clasping$Box,long_data_clasping$Week),]
long_data_clasping_sorted$Clasping<-factor(long_data_clasping_sorted$Clasping,ordered = TRUE , levels = c(0:4))
#library(ordinal)
mod = clmm(Clasping~Genotype + Housing+ Sex+Week+(1|Box)
           , data=long_data_clasping_sorted, Hess=TRUE)
summary(mod)



## ----barplot_clas_sex, echo = FALSE, fig.cap ='Bar plot for clasping over the 5 weeks between sex', fig.pos="H",  out.width = "0.6\\textwidth"----
data_c_group<-data.frame(long_data_clasping_sorted %>% filter(complete.cases(.)) %>%
   group_by(Week,Clasping, Sex, Genotype,Housing) %>% 
     tally())
ggplot(data_c_group, aes(fill=Clasping, y=n, x=Week)) + 
    geom_bar( stat="identity", position = position_stack(reverse = TRUE)) +
    scale_fill_viridis(discrete = T)+facet_wrap(~Sex)+ylab("Count")


## ----barplot_clas_geno, echo = FALSE, fig.cap ='Bar plot for clasping over the 5 weeks between genotype', fig.pos="H",  out.width = "0.6\\textwidth"----
ggplot(data_c_group, aes(fill=Clasping, y=n, x=Week)) + 
    geom_bar( stat="identity", position = position_stack(reverse = TRUE)) +
    scale_fill_viridis(discrete = T)+facet_wrap(~Genotype)+ylab("Count")


## ---- echo=FALSE----------------------------------------------------------------
X1 <- lme(Brain_Weight ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Brain_Weight)))
X2 <- lme(DG_Swing_Fore ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Swing_Fore)))
X3 <- lme(DG_Swing_Hind ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Swing_Hind)))
X4 <- lme(DG_Stride_Length_Fore ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Stride_Length_Fore)))
X5 <- lme(DG_Stride_Length_Hind ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Stride_Length_Hind)))
X6 <- lme(DG_Stance_Width_Hind ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Stance_Width_Hind)))
X7 <- lme(DG_Propel_Brake_Ratio_FORE ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(DG_Propel_Brake_Ratio_FORE)))
X8 <- lme(Butyrate ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Butyrate)))
X9 <- lme(Valerate ~ Genotype + Housing+ Sex+
            Genotype:Sex+Sex:Housing+Housing:Genotype,
          random =(~1|Box), data = subset(data_clean, !is.na(Valerate)))
X10 <- lme(Water_Intake ~ Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype,
          random =(~1|Box), data = subset(food_sorted, !is.na(Water_Intake)))

X11 <- lme(Weight ~ Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype,
          random =(~1|Box), data = subset(long_data_sorted, !is.na(Weight)))
X12 <- lme(Rotarod ~ Genotype + Housing+ Sex+Week+
            Genotype:Sex+Sex:Housing+
            Genotype:Week+ Sex:Week + Housing:Week+Housing:Genotype,
          random =(~1|Box), data = subset(rotarod_data, !is.na(Rotarod)))


#stance widtg forepaw, propel:brake ratio forepaw, buterate, valerate


## ----brain_weight, echo=FALSE---------------------------------------------------
x<-emmeans(X1, pairwise ~ Genotype*Sex|Housing)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex",caption = "Post-hoc results for genotype and sex given housing type in variable brain weight") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(7,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ----emmeans, echo = FALSE, fig.cap ='Graphical comparisons from emmeans method', fig.pos="H",  out.width = "0.9\\textwidth"----
plot(x, comparisons = TRUE)


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X2, pairwise ~ Genotype|Housing*Sex)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(8,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X3, pairwise ~ Genotype|Housing*Sex)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(8,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X4, pairwise ~ Genotype|Housing*Sex)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(8,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X5, pairwise ~ Housing|Genotype*Sex)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(8,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X5, pairwise ~ Housing|Genotype)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(7,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X6, pairwise ~ Genotype|Housing*Sex)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(8,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X7, pairwise ~ Genotype|Housing*Sex)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(8,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X8, pairwise ~ Sex|Housing)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
  column_spec(7,bold = that_cell,
              color =  ifelse(that_cell,"red","black"))%>%
  column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X9, pairwise ~ Sex|Housing)
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
  column_spec(7,bold = that_cell,
              color =  ifelse(that_cell,"red","black"))%>%
  column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X10, pairwise ~ Genotype*Housing*Sex| Week, 
        at = list(Week = c(6:11)))
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex", caption="Post-hoc results for sex, genotype and housing in water intake variable") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
  column_spec(7,bold = that_cell,
              color =  ifelse(that_cell,"red","black"))%>%
  column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X11, pairwise ~ Genotype*Housing*Sex|Week,
           at = list(Week = c(6:12)))
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex", caption="Post-hoc results for sex, genotype and housing in weight variable") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(7,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X12, pairwise ~ Genotype*Housing|Sex*Week,
           at = list(Week = c(6:12)))
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex", caption="Post-hoc results for genotype and housing given sex type in rotarod variable") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(8,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))


## ---- echo=FALSE----------------------------------------------------------------
x<-emmeans(X13, pairwise ~ Genotype*Sex|Housing*Week,
           at = list(Week = c(6:12)))
df<-data.frame(x$contrasts)
that_cell<-df$p.value<0.05
kbl(x$contrasts, longtable=T, linesep = "", booktabs = T, digits = c(4,4,4,4,4,4),format = "latex", caption="Post-hoc results for sex and genotype given housing type in variable clasping score") %>%
  kable_styling(latex_options = c("repeat_header"))%>%
column_spec(8,bold = that_cell,
            color =  ifelse(that_cell,"red","black"))%>%
column_spec(1,color =  ifelse(that_cell,"red","black"))

## ----refmgr references, results="asis", echo=FALSE------------------------------
# Print


## -------------------------------------------------------------------------------
sessionInfo()

