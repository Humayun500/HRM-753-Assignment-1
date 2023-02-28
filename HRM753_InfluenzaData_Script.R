save.image ("C:/Users/humay/Dropbox/HRM/HRM 753/Lec 2/Assignment/HRM753_InfluenzaData.RData")
HRM753_InfluenzaData <- read.csv("C:/Users/humay/Dropbox/HRM/HRM 753/Lec 2/Assignment/HRM753_InfluenzaData.csv")

library (tidyverse)
library(freqtables)
options (scipen=999)

count (HRM753_InfluenzaData)

typeof (HRM753_InfluenzaData$age)

HRM753_InfluenzaData %>% 
  freq_table(male)

HRM753_InfluenzaData %>%
count (male)

HRM753_InfluenzaData %>% 
  freq_table(age)
mean (HRM753_InfluenzaData$age)
sd (HRM753_InfluenzaData$age)

summary(HRM753_InfluenzaData$age)
is.na (HRM753_InfluenzaData$age)
is.na (HRM753_InfluenzaData$age)

HRM753_InfluenzaData %>% 
  freq_table(member.x)

mean (HRM753_InfluenzaData$age)
sd (HRM753_InfluenzaData$age)

HRM753_InfluenzaData %>% 
  freq_table()

typeof (HRM753_InfluenzaData$house_size)

class (HRM753_InfluenzaData$house_size)

mean (HRM753_InfluenzaData$house_size, na.rm=TRUE)
sd (HRM753_InfluenzaData$house_size, na.rm=TRUE)
sum(is.na (HRM753_InfluenzaData$house_size))

HRM753_InfluenzaData$member.x
HRM753_InfluenzaData$member.y
HRM753_InfluenzaData$member.x= recode (HRM753_InfluenzaData$member.x,
                              "0"="1"
                              )
HRM753_InfluenzaData$member.x=as.integer(HRM753_InfluenzaData$member.x)

view (HRM753_InfluenzaData$member.x)

head (HRM753_InfluenzaData$member.x)
HRM753_InfluenzaData$member.x
HRM753_InfluenzaData$member.y
HRM753_InfluenzaData$familysize

HRM753_InfluenzaData$Space_factor =
  as.integer(HRM753_InfluenzaData$house_size/HRM753_InfluenzaData$familysize) 

class (HRM753_InfluenzaData$Space_factor)
mean (HRM753_InfluenzaData$Space_factor, na.rm=T)
sd (HRM753_InfluenzaData$Space_factor, na.rm=T)


typeof (HRM753_InfluenzaData$vac0809)
table (HRM753_InfluenzaData$vac0809)
view (HRM753_InfluenzaData$vac0809)
HRM753_InfluenzaData$vac0809= as.factor(HRM753_InfluenzaData$vac0809)
class (HRM753_InfluenzaData$vac0809)
sum(is.na (HRM753_InfluenzaData$vac0809))
HRM753_InfluenzaData %>% 
  freq_table(vac0809)

#vacsh0910.mid is not found in the data set. 
HRM753_InfluenzaData$vacsh0910.mid
view (HRM753_InfluenzaData$vacsh0910.mid)
typeof (HRM753_InfluenzaData$vacsh0910.mid)
HRM753_InfluenzaData$vacsh0910.mid= as.factor(HRM753_InfluenzaData$vacsh0910.mid)

HRM753_InfluenzaData$vacph10910.mid

view (HRM753_InfluenzaData$vacph10910.mid)
typeof (HRM753_InfluenzaData$vacph10910.mid)
HRM753_InfluenzaData$vacph10910.mid= as.factor(HRM753_InfluenzaData$vacph10910.mid)
sum(is.na (HRM753_InfluenzaData$vacph10910.mid))
table (HRM753_InfluenzaData$vacph10910.mid)
HRM753_InfluenzaData %>% 
  freq_table(vacph10910.mid)

#Seasonal influenza A/H1N1 antibody titres
view (HRM753_InfluenzaData$prevax.sH1)
typeof (HRM753_InfluenzaData$prevax.sH1)
table (HRM753_InfluenzaData$prevax.sH1)

HRM753_InfluenzaData$prevax.sH1= as.factor(HRM753_InfluenzaData$prevax.sH1)

sum(is.na (HRM753_InfluenzaData$prevax.sH1))

mean (HRM753_InfluenzaData$prevax.sH1, na.rm=TRUE)
sd (HRM753_InfluenzaData$prevax.sH1, na.rm=TRUE)

median (HRM753_InfluenzaData$prevax.sH1, na.rm=TRUE)
range (HRM753_InfluenzaData$prevax.sH1, na.rm=TRUE)

sum(is.na (HRM753_InfluenzaData$prevax.sH1))

#Post vaccine
view (HRM753_InfluenzaData$postvax.sH1)
typeof (HRM753_InfluenzaData$postvax.sH1)
HRM753_InfluenzaData$postvax.sH1.int= as.integer(HRM753_InfluenzaData$postvax.sH1)

sum(is.na (HRM753_InfluenzaData$postvax.sH1.int))

typeof (HRM753_InfluenzaData$postvax.sH1)

median (HRM753_InfluenzaData$postvax.sH1.int, na.rm=TRUE)

range (HRM753_InfluenzaData$postvax.sH1.int, na.rm=TRUE)

sum(is.na (HRM753_InfluenzaData$prevax.sH1))

#ans: 2

shapiro.test (HRM753_InfluenzaData$age)
shapiro.test (HRM753_InfluenzaData$Space_factor)
shapiro.test (HRM753_InfluenzaData$prevax.sH1)

#Ans: 3
cor.test(HRM753_InfluenzaData$prevax.sH1,HRM753_InfluenzaData$Space_factor, method="kendall", na.rm=T)
cor.test(HRM753_InfluenzaData$prevax.sH1,HRM753_InfluenzaData$Space_factor, method="pearson", na.rm=T)
cor.test(HRM753_InfluenzaData$prevax.sH1,HRM753_InfluenzaData$Space_factor, method="spearman", na.rm=T)

#Ans:4
#2 consecutive seasonal vaccinations (2008/09 and 2009/10)
#trivalent inactivated vaccine (TIV) as intervention means that vaccinated in 2009/10 season
#post-vaccination seasonal influenza A/H1N1 antibody titers into those that meet the ‘seroprotection’ criterion (>40) 
# intervention, vac0809, vacph10910.mid

HRM753_InfluenzaData$Space_factor =as.integer(HRM753_InfluenzaData$house_size/HRM753_InfluenzaData$familysize) 
HRM753_InfluenzaData$seroprotection= cut (HRM753_InfluenzaData$post.season.sH1, breaks = c(0,34, Inf),
                     labels = c("No", "Yes")) 
table (HRM753_InfluenzaData$seroprotection)


HRM753_InfluenzaData$consecutive.seasonal.vaccinations=  
  if (((HRM753_InfluenzaData$vac0809=="1") 
      & ((HRM753_InfluenzaData$vacph10910.mid=="1") 
         | (HRM753_InfluenzaData$intervention=="TIV")), "1")) 
    else (vac0809 == "0" | intervention == "placebo" ~ "0")

HRM753_InfluenzaData$consecutive.seasonal.vaccinations = 
           case_when(
             HRM753_InfluenzaData$vac0809 == "1" & HRM753_InfluenzaData$intervention == "TIV" ~ "1",
             HRM753_InfluenzaData$vac0809 == "0" | HRM753_InfluenzaData$intervention == "placebo" ~ "0"
           )
  )

table (HRM753_InfluenzaData$consecutive.seasonal.vaccinations)

HRM753_InfluenzaData$intervention
HRM753_InfluenzaData$vac0809
HRM753_InfluenzaData$vacph10910.mid

sum(is.na (HRM753_InfluenzaData$intervention))
sum(is.na (HRM753_InfluenzaData$vac0809))
sum(is.na (HRM753_InfluenzaData$vacph10910.mid))

table (HRM753_InfluenzaData$consecutive.seasonal.vaccinations)
sum(is.na (HRM753_InfluenzaData$consecutive.seasonal.vaccinations))

head (HRM753_InfluenzaData$consecutive.seasonal.vaccinations)
HRM753_InfluenzaData$consecutive.seasonal.vaccinations

chisq.test( HRM753_InfluenzaData$seroprotection,
            HRM753_InfluenzaData$consecutive.seasonal.vaccinations, correct=T)
class (HRM753_InfluenzaData$seroprotection)

table (HRM753_InfluenzaData$seroprotection,
       HRM753_InfluenzaData$consecutive.seasonal.vaccinations)

seroprotection= HRM753_InfluenzaData$seroprotection
cons.vac= HRM753_InfluenzaData$consecutive.seasonal.vaccinations

glm.model= glm (HRM753_InfluenzaData$seroprotection~ 
                HRM753_InfluenzaData$consecutive.seasonal.vaccinations,
                ,family= binomial)
glm.model

glm.model.1= glm(seroprotection~ 
                cons.vac,
              ,family= binomial)
glm.model.1
anova(glm.model.1, test="Chi")

or_model.1=exp(cbind(coef(glm.model.1), confint(glm.model.1, level=0.95)))
or_model.1
