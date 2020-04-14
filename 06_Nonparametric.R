school <- read.csv("/Users/srmor/OneDrive/Loyola_University_Chicago/Spring_2020/Nonparametric_Statistical_Methods/Lecture_Notes/ill_school_data.csv")

levels(ventus$Handedness)[levels(ventus$Handedness) == ""] <- NA
levels(ventus$Season)[levels(ventus$Season) == ""] <- NA

ventus <- data.frame(school$Handed, school$Favorite_Season)
colnames(ventus) <- c("Handedness", "Season")

levels(ventus$Handedness)[levels(ventus$Handedness) == ""] <- NA
levels(ventus$Season)[levels(ventus$Season) == ""] <- NA

###############
#missingness plots
#plots not showing up on github, how to fix? 
#library(VIM)
#matrixplot(ventus, sortby = 'Handedness')
#ggr(ventus, combined = TRUE, numbers = TRUE)

#library(tidyverse)

ventus <- ventus %>%
  filter(Handedness %in% c("Right-Handed", "Left-Handed", "Ambidextrous"), 
         Season %in% c("Fall", "Spring", "Summer", "Winter"))# %>%
  #droplevels()

table(ventus)
#dim(ventus)

(XsqObs <- chisq.test(table(ventus))$statistic)

set.seed(42)
n <- dim(ventus)[1]
nsim <- 1000
XsqPerms <- rep(NA, 1000)
for (i in 1:nsim){
  ventusTemp <- ventus 
  ventusTemp$Handedness <- ventus$Handedness[sample(1:n, n)]
  XsqPerms[i] <- chisq.test(table(ventusTemp))$statistic
}

sum(XsqPerms >= XsqObs) / 1000


#####################################################
measurements <- school %>%
  mutate(Height = as.numeric(levels(school$Height_cm))[school$Height_cm], 
         Right_Foot = as.numeric(levels(school$Footlength_cm))[school$Footlength_cm],
         Left_Foot = as.numeric(levels(school$Left_Footlength_cm))[school$Left_Footlength_cm], 
         Armspan = as.numeric(levels(school$Armspan_cm))[school$Armspan_cm],
         Index_Finger = as.numeric(levels(school$Index_Fingerlength_mm))[school$Index_Fingerlength_mm],
         Ring_Finger = as.numeric(levels(school$Ring_Fingerlength_mm))[school$Ring_Fingerlength_mm]) %>%
  select(Height,
         Right_Foot,
         Left_Foot,
         Armspan,
         Index_Finger,
         Ring_Finger)

#The following code finds and removes some of the most obvious data entry errors 

which((measurements$Height >= 900))
measurements$Height[188] <- NA
measurements$Height[259] <- NA

which((measurements$Right_Foot >= 900))
measurements$Right_Foot[164] <- NA

which((measurements$Index_Finger >= 1000))
measurements$Index_Finger[259] <- NA

which((measurements$Ring_Finger >= 1000))
measurements$Ring_Finger[259] <- NA


names(measurements)
library(mice)
set.seed(42)
measurements_imp <- mice(measurements, method = c("cart", "cart", "cart", "cart", "cart", "cart"))

#need to check for high leverage/influce and outliers? 
#need to remove errors on dataset?  
#remove errors before or after imputation?
#remove single error or remove the whole row? 
#okay to impute error that was removed? 
#when to scale?
#difference between scale and normalize? 
#methods on when and how to remove outliers/errors? discuss? 

measurements_rbn <- with(measurements_imp, exp = lm(Height ~ Right_Foot + Left_Foot + Armspan + Index_Finger + Ring_Finger))
round(pool(measurements_rbn)$pooled$estimate, 8)
round(pool(measurements_rbn)$pooled$t, 8)

?round
############################

measurements_imp <- mice(measurements, method = c("rf", "rf", "rf", "rf", "rf", "rf"))
measurements_rbn <- with(measurements_imp, exp = lm(Height ~ Right_Foot + Left_Foot + Armspan + Index_Finger + Ring_Finger))
pool(measurements_rbn)

###################################
