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
variables, so a chi-square test for independence would be best.

``` r
school <- read.csv("/Users/srmor/OneDrive/Loyola_University_Chicago/Spring_2020/Nonparametric_Statistical_Methods/Lecture_Notes/ill_school_data.csv")

library(tidyverse)

levels(school$Handed)[levels(school$Handed) == ""] <- NA
levels(school$Favorite_Season)[levels(school$Favorite_Season) == ""] <- NA

ventus <- school %>%
  select(Handed, Favorite_Season) %>%
  rename("Handedness" = "Handed", "Season" = "Favorite_Season")

library(VIM)
matrixplot(ventus)
```

![](06_Nonparametric-Homework_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
aggr(ventus, combined = TRUE, numbers = TRUE)
```

![](06_Nonparametric-Homework_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
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
combined estimates.*

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
summary(measurements)
```

    ##      Height          Right_Foot        Left_Foot         Armspan      
    ##  Min.   :    1.8   Min.   :   2.00   Min.   :  2.00   Min.   :  1.88  
    ##  1st Qu.:  160.0   1st Qu.:  23.00   1st Qu.: 23.00   1st Qu.:154.00  
    ##  Median :  166.6   Median :  24.00   Median : 24.00   Median :164.00  
    ##  Mean   :  377.0   Mean   :  31.43   Mean   : 25.18   Mean   :153.39  
    ##  3rd Qu.:  175.2   3rd Qu.:  26.00   3rd Qu.: 26.00   3rd Qu.:175.00  
    ##  Max.   :99999.0   Max.   :2426.00   Max.   :160.00   Max.   :431.80  
    ##  NA's   :38        NA's   :40        NA's   :66       NA's   :55      
    ##   Index_Finger      Ring_Finger     
    ##  Min.   :   0.60   Min.   :   0.70  
    ##  1st Qu.:  28.12   1st Qu.:  38.25  
    ##  Median :  72.00   Median :  72.00  
    ##  Mean   :  80.60   Mean   :  77.85  
    ##  3rd Qu.:  80.00   3rd Qu.:  82.00  
    ##  Max.   :1000.00   Max.   :1000.00  
    ##  NA's   :78        NA's   :78

``` r
which((measurements$Height < 5) | (measurements$Height >= 900))
```

    ## [1] 188 218 259

``` r
measurements$Height[188] <- NA
measurements$Height[218] <- NA
measurements$Height[259] <- NA

which((measurements$Right_Foot >= 1000))
```

    ## [1] 164

``` r
measurements$Right_Foot[164] <- NA

which((measurements$Armspan >= 300))
```

    ## [1] 354

``` r
measurements$Armspan[354] <- NA

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
measurements_rub <- with(measurements_imp, exp = lm(Height ~ Right_Foot + Left_Foot + Armspan + Index_Finger + Ring_Finger))
pool(measurements_rub)
```

    ## Class: mipo    m = 5 
    ##           term m     estimate         ubar            b            t dfcom
    ## 1  (Intercept) 5 91.094005704 3.594905e+01 2.601519e+01 6.716728e+01   494
    ## 2   Right_Foot 5 -0.081506359 9.185427e-03 1.447147e-02 2.655118e-02   494
    ## 3    Left_Foot 5  0.062913383 1.433809e-02 6.399165e-03 2.201709e-02   494
    ## 4      Armspan 5  0.433792106 1.198406e-03 5.838500e-04 1.899026e-03   494
    ## 5 Index_Finger 5  0.008389064 7.624741e-04 1.555757e-04 9.491649e-04   494
    ## 6  Ring_Finger 5  0.017894248 8.675037e-04 1.309619e-04 1.024658e-03   494
    ##           df       riv    lambda       fmi
    ## 1  17.300033 0.8684022 0.4647833 0.5175140
    ## 2   8.863691 1.8905772 0.6540483 0.7123694
    ## 3  29.822350 0.5355663 0.3487745 0.3884563
    ## 4  26.846180 0.5846267 0.3689365 0.4112243
    ## 5  81.955053 0.2448487 0.1966895 0.2156009
    ## 6 120.751964 0.1811568 0.1533724 0.1670550

<br/><br/> *• Repeat the previous problem, but use a random forest for
imputation in MICE instead of a cart model.* <br/><br/> *• Finally, put
your code and results in a github repository. In the ﬁnal version of
your homework that you submit to Sakai, the answer to this part will
simply be a link to that github repository.*
