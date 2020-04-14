Homework 6
================
Sandy Morales
4/11/2020

# Question 1

*Download this data: bit.ly/illschooldata.csv (Data set is from here:
<https://ww2.amstat.org/censusatschool/RandomSampleExport.cfm>).*
<br/><br/> *• Describe the data. Who is in this data set? What are some
of the intersting characteristics of this data set?* <br/><br/> The
dataset contains the survey results of 500 high school students in
Illinois between the years 2012 – 2019. The dataset contains 60 features
such as gender, age, travel time to school, number of languages spoken
as well as measurements of different body extremities. It is interesting
to note that most of the variables are coded as categorical except for
data year, class year, age, number of languages spoken, importance of
recycling rubbish, and importance of conserving water. We can also
observe missingness among the variables in the dataset. For example,
there are 50 missing observations for the importance of owning a
computer attribute and 51 missing observations for the importance of
having internet access attribute.  
<br/><br/> *• Perform the appropriate test to test the null hypothesis
that handedness (i.e. the variable named Handed) is independent of
favorite season vs the alternative hypothesis that there is some
dependence. Perform this test after removing responses that are blank.
Do you think it is ok here to remove the blanks? Explain why or why not.
Explain your reasoning for the test you chose and state your
conclusions.* <br/><br/> The following is a non-parametric chi-square
test of independence: <br/><br/> H<sub>0</sub>: There is no relationship
between handedness and favorite season. <br/><br/> H<sub>1</sub>: There
is a relationship between handedness and favorite season <br/><br/> The
p-value is 0.721. Since the p-value is greater than α = 0.05 we fail to
reject the null hypothesis and state that there is no relationship
between handedness and favorite season. <br/><br/> The data appears to
be missing completely at random and would generally be okay to perform
complete case analysis. We are left about 90% of the data when we remove
missing values. We are testing for independence between two categorical
variables, so a chi-square test for independence would be best. Since
the number of permutations would be large, we performed a simulation.

``` r
school <- read.csv("/Users/srmor/OneDrive/Loyola_University_Chicago/Spring_2020/Nonparametric_Statistical_Methods/Lecture_Notes/ill_school_data.csv")

library(tidyverse)

levels(school$Handed)[levels(school$Handed) == ""] <- NA
levels(school$Favorite_Season)[levels(school$Favorite_Season) == ""] <- NA

ventus <- school %>%
  select(Handed, Favorite_Season) %>%
  rename("Handedness" = "Handed", "Season" = "Favorite_Season")

ventus <- ventus %>%
  filter(Handedness %in% c("Right-Handed", "Left-Handed", "Ambidextrous"), 
         Season %in% c("Fall", "Spring", "Summer", "Winter"))

table(ventus)
```

    ##               Season
    ## Handedness     Fall Spring Summer Winter
    ##   Ambidextrous    7      2      4      3
    ##   Left-Handed    14      8     17      5
    ##   Right-Handed  157     47    146     39

``` r
(XsqObs <- chisq.test(table(ventus))$statistic)
```

    ## X-squared 
    ##  3.788089

``` r
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
```

    ## [1] 0.721

<br/><br/> *• Build a simple linear regression model with height as your
response and arm span as your predictor. First, you need to clean the
data, then use MICE to impute missing values using a CART model.
Estimate the simple linear regression model on each of the compeleted
data sets and use Rubin’s combining rules to combined estiamtes across
imputations. State your ﬁnal estimates for each of the slope and
intercept parameters as well as standard errors for each of these
combined estimates.* <br/><br/> The finale estimates for the intercept
and each slope are: Intercept = 93.272, Right\_Foot = -.110, Left\_Foot
= .0665, Armspan = .422, Index\_Finger = -.0000967, Ring\_Finger =
.0185. The standard errors are 125.989, .0372, .0295, .004, and .000917,
.00112 respectively.

``` r
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
```

    ## [1] 188 259

``` r
measurements$Height[188] <- NA
measurements$Height[259] <- NA

which((measurements$Right_Foot >= 900))
```

    ## [1] 164

``` r
measurements$Right_Foot[164] <- NA

which((measurements$Index_Finger >= 1000))
```

    ## [1] 259

``` r
measurements$Index_Finger[259] <- NA

which((measurements$Ring_Finger >= 1000))
```

    ## [1] 259

``` r
measurements$Ring_Finger[259] <- NA

library(mice)
measurements_imp <- mice(measurements, method = c("cart", "cart", "cart", "cart", "cart", "cart"))
```

    ## 
    ##  iter imp variable
    ##   1   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   1   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   1   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   1   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   1   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger

``` r
measurements_rbn <- with(measurements_imp, exp = lm(Height ~ Right_Foot + Left_Foot + Armspan + Index_Finger + Ring_Finger))
round(pool(measurements_rbn)$pooled$estimate, 8)
```

    ## [1] 93.27185686 -0.11036681  0.06654414  0.42160646 -0.00009665  0.01845606

``` r
round(pool(measurements_rbn)$pooled$t, 8)
```

    ## [1] 125.98932256   0.03715912   0.02953136   0.00400424   0.00091676
    ## [6]   0.00112045

<br/><br/> *• Repeat the previous problem, but use a random forest for
imputation in MICE instead of a cart model.* <br/><br/> The finale
estimates for the intercept and each slope are: Intercept = 95.863,
Right\_Foot = -.169, Left\_Foot = .117, Armspan = .402, Index\_Finger =
.00663, Ring\_Finger = .0241 The standard errors are 54.729, .0133,
.0189, .00164, .00085, and .0009 respectively.

``` r
measurements_imp <- mice(measurements, method = c("rf", "rf", "rf", "rf", "rf", "rf"))
```

    ## 
    ##  iter imp variable
    ##   1   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   1   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   1   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   1   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   1   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   2   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   3   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   4   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   1  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   2  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   3  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   4  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger
    ##   5   5  Height  Right_Foot  Left_Foot  Armspan  Index_Finger  Ring_Finger

``` r
measurements_rbn <- with(measurements_imp, exp = lm(Height ~ Right_Foot + Left_Foot + Armspan + Index_Finger + Ring_Finger))
round(pool(measurements_rbn)$pooled$estimate, 8)
```

    ## [1] 95.86344901 -0.16854900  0.11703477  0.40198961  0.00663360  0.02414886

``` r
round(pool(measurements_rbn)$pooled$t, 8)
```

    ## [1] 54.72901515  0.01336066  0.01795127  0.00164279  0.00085077  0.00090086

<br/><br/> *• Finally, put your code and results in a github repository.
In the ﬁnal version of your homework that you submit to Sakai, the
answer to this part will simply be a link to that github repository.*
