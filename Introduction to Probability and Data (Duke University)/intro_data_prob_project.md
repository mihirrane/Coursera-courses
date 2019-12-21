---
title: "Exploring the BRFSS data"
output: 
  html_document: 
    keep_md: true
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

## Setup

### Load packages


```r
library(ggplot2)
library(dplyr)
library(grid)
library(gridBase)
library(purrr)
```

### Load data

Make sure your data and R Markdown files are in the same directory. When loaded
your data file will be called `brfss2013`. Delete this note when before you submit 
your work. 


```r
load("brfss2013.RData")
```

* * *

## Part 1: Data

In the brfss, stratified sampling was used, random sampling was done. But there is no random assignment.Participants were not randomly assigned to the tasks such as sleeping, exercising etc. So this is a observational study and so is generalizable but causality cannot be inferred.

* * *

## Part 2: Research questions

**Research quesion 1:**
For the general population in the US, is there relationship between exercising in last 30 days and income. Is there a difference based on employment status?


**Research quesion 2:**
For the general population in the US, is there a relation between ethnicity and life satisfaction. There has been lot of news correlating ethnic groups with life satisfaction, I want to inspect whether there is a relation between a particular ethnicity group and their life satisfaction. Is there a difference in results between genders?


**Research quesion 3:**
For the general population in the US, is there a correlation between the hours of sleep an individual gets night before and their energy levels next morning. Is there a difference in energy levels between genders?


* * *

## Part 3: Exploratory data analysis

**Research quesion 1:**
For the general population in the US, is there relationship between exercising in last 30 days and income. Is there a difference based on employment status?


```r
# Remove all data with NA's.
exercise.income.na <- filter(brfss2013, !is.na(exerany2), !is.na(income2), !is.na(employ1))

# Find porportion that does any exercise grouped by income.
exercise.income <- exercise.income.na %>% group_by(income2) %>% summarise(prop_exer = sum(exerany2 == "Yes") / n())

# Print
exercise.income
```

```
## # A tibble: 8 x 2
##   income2           prop_exer
##   <fct>                 <dbl>
## 1 Less than $10,000     0.607
## 2 Less than $15,000     0.600
## 3 Less than $20,000     0.626
## 4 Less than $25,000     0.650
## 5 Less than $35,000     0.689
## 6 Less than $50,000     0.734
## 7 Less than $75,000     0.780
## 8 $75,000 or more       0.842
```



```r
levels(exercise.income$income2) <- gsub(" ", "\n", levels(exercise.income$income2))
ggplot(exercise.income, aes(income2, prop_exer)) +
    geom_point(aes(income2, prop_exer)) +
    labs(title="Amount of people who exercised in last 30 days vs. income", x="Income", y="Amount of people who exercised in last 30 days")
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

There appears to be a very strong relationship between exercising and income. Again there this observational study will not lead to any causality,we can see that higher income groups do more exercise.



```r
# Find porportion that does any exercise grouped by employment status.
exercise.income.employ <- exercise.income.na %>% group_by(employ1) %>% summarise(prop_exer = sum(exerany2 == "Yes") / n())

# Print
exercise.income.employ
```

```
## # A tibble: 8 x 2
##   employ1                          prop_exer
##   <fct>                                <dbl>
## 1 Employed for wages                   0.772
## 2 Self-employed                        0.762
## 3 Out of work for 1 year or more       0.694
## 4 Out of work for less than 1 year     0.759
## 5 A homemaker                          0.734
## 6 A student                            0.851
## 7 Retired                              0.713
## 8 Unable to work                       0.513
```


```r
levels(exercise.income.employ$employ1) <- gsub(" ", "\n", levels(exercise.income.employ$employ1))
ggplot(exercise.income.employ, aes(employ1, prop_exer)) +
    geom_point(aes(employ1, prop_exer)) +
    labs(title="Amount of people who exercised in last 30 days vs employment status",
         x="employment status", y="Amount of people who exercised in last 30 days")
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The employment status with clearly the largest proportion reporting having exercised in last 30 days is “A student”. And the one with the lowest is “Out of work for 1 year or more”. Does lack of work lead to decreased motivation and thus less amount of exercise?


```r
chisq.test(exercise.income$income2, exercise.income$prop_exer, correct = FALSE)
```

```
## Warning in chisq.test(exercise.income$income2, exercise.income$prop_exer, : Chi-
## squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  exercise.income$income2 and exercise.income$prop_exer
## X-squared = 56, df = 49, p-value = 0.2289
```

The p-value is greater than 0.05 although there is a clear indication from exploratory analysis that there is dependence between income groups and amount of exercise. The chi-square test suggests otherwise.


```r
chisq.test(exercise.income.employ$employ1, exercise.income.employ$prop_exer, correct = TRUE)
```

```
## Warning in chisq.test(exercise.income.employ$employ1,
## exercise.income.employ$prop_exer, : Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  exercise.income.employ$employ1 and exercise.income.employ$prop_exer
## X-squared = 56, df = 49, p-value = 0.2289
```

The p-value is greater than 0.05 although there is a clear indication from exploratory analysis that there is dependence between employment status and amount of exercise. The chi-square test suggests otherwise.

**Research quesion 2:**
For the general population in the US, is there a relation between ethnicity and life satisfaction. There has been lot of news correlating ethnic groups with life satisfaction, I want to inspect whether there is a relation between a particular ethnicity group and their life satisfaction. Is there a difference in results between genders?


```r
ggplot(data=brfss2013, aes(x=X_race)) +geom_bar(stat='count') +theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
From the above plot, we can see that the majority population is "white only" followed by "black only" and other ethnicities respectively.


```r
ggplot(data=brfss2013, aes(x=lsatisfy)) +geom_bar(stat='count') +theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
From the above plot, we can see that most data on life satisfaction is missing. Accurate results are not guaranteed because of lack of data on life satisfaction.


```r
race.satisfaction <- select(brfss2013, X_race , sex , lsatisfy) %>% 
  filter(!is.na(X_race), !is.na(sex), !is.na(lsatisfy) )

race.satisfaction %>% group_by(X_race) %>%  summary(count=n())
```

```
##                                                   X_race         sex      
##  White only, non-Hispanic                            :8300   Male  :4054  
##  Black only, non-Hispanic                            :2947   Female:7510  
##  Hispanic                                            :  90                
##  Multiracial, non-Hispanic                           :  65                
##  American Indian or Alaskan Native only, Non-Hispanic:  64                
##  Other race only, non-Hispanic                       :  44                
##  (Other)                                             :  54                
##               lsatisfy   
##  Very satisfied   :5339  
##  Satisfied        :5472  
##  Dissatisfied     : 595  
##  Very dissatisfied: 158  
##                          
##                          
## 
```



```r
ggplot(data = race.satisfaction, aes(x = X_race, y = lsatisfy)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)   +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_grid(. ~  sex) +
    xlab("Ethnicity") +
    ylab ("Life satisfaction")
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



```r
ggplot(race.satisfaction %>% count(X_race, sex) %>%
                  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),
       aes(X_race,n, fill = sex)) +
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

We can see that the dataset contains higher females per category.


```r
ggplot(race.satisfaction %>% count(lsatisfy, sex) %>%
                  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),
       aes(lsatisfy,n, fill = sex)) +
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

We can observe that there is higher life satisfation among females over males.


```r
chisq.test(race.satisfaction$X_race, race.satisfaction$lsatisfy, correct = FALSE)
```

```
## Warning in chisq.test(race.satisfaction$X_race, race.satisfaction$lsatisfy, :
## Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  race.satisfaction$X_race and race.satisfaction$lsatisfy
## X-squared = 74.769, df = 21, p-value = 5.928e-08
```

The p-value is less than 0.05 thus we can conclude that there is dependence between life satisfaction and ethnicity.


```r
chisq.test(race.satisfaction$lsatisfy, race.satisfaction$sex, correct = FALSE)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  race.satisfaction$lsatisfy and race.satisfaction$sex
## X-squared = 9.7123, df = 3, p-value = 0.02118
```

The p-value is less than 0.05 thus we can conclude that there is dependence between life satisfaction and sex



```r
males <- race.satisfaction[which(race.satisfaction$sex == "Male"),]

ggplot(males %>% count(X_race, lsatisfy) %>%
                  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),
       aes(X_race,n, fill = lsatisfy)) +
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


```r
females <- race.satisfaction[which(race.satisfaction$sex == "Female"),]

ggplot(females %>% count(X_race, lsatisfy) %>%
                  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),
       aes(X_race,n, fill = lsatisfy)) +
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

We can observe that there is a correlation between ethnic groups and life satisfaction. The pattern does not vary based on gender. Although the count of males and females vary as the dataset contains more females as compared to males. 

**Research quesion 3:**
For the general population in the US, is there a correlation between the hours of sleep an individual gets night before and their energy levels next morning. Is there a difference in energy levels between genders?


```r
## Sleep time less than 12 hours
health.sleep <- select(brfss2013, qlhlth2 , sex , sleptim1) %>% 
  filter(!is.na(qlhlth2), !is.na(sex), sleptim1 <= 12 )

health.sleep %>% group_by(qlhlth2) %>%  summary(count=n())
```

```
##     qlhlth2          sex         sleptim1     
##  Min.   : 0.00   Male  :162   Min.   : 2.000  
##  1st Qu.: 2.00   Female:287   1st Qu.: 6.000  
##  Median :15.00                Median : 7.000  
##  Mean   :15.56                Mean   : 7.013  
##  3rd Qu.:28.00                3rd Qu.: 8.000  
##  Max.   :30.00                Max.   :12.000
```


```r
ggplot(data = health.sleep, aes(x = sleptim1, y = qlhlth2 ))+
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)   +
    scale_x_continuous(limits = c(4,10), breaks = 4:10) +
    facet_grid(. ~  sex) +
    xlab("sleptim1 = Hours of Sleep") +
    ylab ("qlhlth2: Days of full energy in the last 30 days")
```

```
## Warning: Removed 12 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 12 rows containing missing values (geom_point).
```

![](_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

There seems to be a generally positive correlation between the hours of sleep and days of full energy. The correlation seems to be slightly stronger for females than males as the males data is more widely dispersed.
