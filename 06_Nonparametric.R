school <- read.csv("/Users/srmor/OneDrive/Loyola_University_Chicago/Spring_2020/Nonparametric_Statistical_Methods/Lecture_Notes/ill_school_data.csv")

levels(ventus$Handedness)[levels(ventus$Handedness) == ""] <- NA
levels(ventus$Season)[levels(ventus$Season) == ""] <- NA


ventus <- data.frame(school$Handed, school$Favorite_Season)
colnames(ventus) <- c("Handedness", "Season")

levels(ventus$Handedness)[levels(ventus$Handedness) == ""] <- NA
levels(ventus$Season)[levels(ventus$Season) == ""] <- NA

################missingness
library(VIM)
matrixplot(ventus, sortby = 'Handedness')
aggr(ventus, combined = TRUE, numbers = TRUE)
#table(ventus)
######
#library(tidyverse)

ventus <- ventus %>%
  filter(Handedness %in% c("Right-Handed", "Left-Handed", "Ambidextrous"), 
         Season %in% c("Fall", "Spring", "Summer", "Winter"))# %>%
  #droplevels()

table(ventus)
dim(ventus)

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
names(school)
str(school)
#left footlength, index finger, ring finger
school <- read.csv("/Users/srmor/OneDrive/Loyola_University_Chicago/Spring_2020/Nonparametric_Statistical_Methods/Lecture_Notes/ill_school_data.csv")

#levels(school$Height_cm)[levels(school$Height_cm) == ""] <- NA
#levels(school$Footlength_cm)[levels(school$Footlength_cm) == ""] <- NA
#levels(school$Armspan_cm)[levels(school$Armspan_cm) == ""] <- NA
#levels(school$Left_Footlength_cm)[levels(school$Left_Footlength_cm) == ""] <- NA
#levels(school$Index_Fingerlength_mm)[levels(school$Index_Fingerlength_mm) == ""] <- NA
#levels(school$Ring_Fingerlength_mm)[levels(school$Ring_Fingerlength_mm) == ""] <- NA

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
summary(measurements$Height)
outliers <- sort(boxplot(measurements$Height, plot = FALSE)$out)
which((measurements$Height < 150) | (measurements$Height >= 200))
measurements$Height[188] <- NA
measurements$Height[218] <- NA
measurements$Height[259] <- NA

which((measurements$Right_Foot >= 1000))
measurements$Right_Foot[164] <- NA

which((measurements$Armspan >= 300))
measurements$Armspan[354] <- NA

which((measurements$Index_Finger >= 1000))
measurements$Index_Finger[259] <- NA

which((measurements$Ring_Finger >= 1000))
measurements$Ring_Finger[259] <- NA


names(measurements)
library(mice)
measurements_imp <- mice(measurements, method = c("cart", "cart", "cart", "cart", "cart", "cart"))
?mice
#need to remove outliers 
#remove outliers before or after imputation?
#when to scale?
#difference between scale and normalize
#when and how to usually remove outliers? 

measurements_rbn <- with(measurements_imp, exp = lm(Height ~ Right_Foot + Left_Foot + Armspan + Index_Finger + Ring_Finger))
pool(measurements_rbn)

############################

measurements_imp <- mice(measurements, method = c("rf", "rf", "rf", "rf", "rf", "rf"))
measurements_rbn <- with(measurements_imp, exp = lm(Height ~ Right_Foot + Left_Foot + Armspan + Index_Finger + Ring_Finger))
pool(measurements_rbn)

?lm

###################################
