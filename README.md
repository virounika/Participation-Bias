Final project
Virounika Mina
Load the packages you will need
library(tidyverse)
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
## ✔ tibble  3.1.6     ✔ dplyr   1.0.7
## ✔ tidyr   1.1.4     ✔ stringr 1.4.0
## ✔ readr   2.1.1     ✔ forcats 0.5.1
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
library(estimatr)
Load the data
Here is the link: https://www.dropbox.com/s/8x06524ndznuhfb/final_data.csv?dl=1

dat <- read.csv("https://www.dropbox.com/s/8x06524ndznuhfb/final_data.csv?dl=1")
Introduction
Based on data from previous instructor evaluations, there appears to be a bias against female instructors and instructors of color, regardless of the objective quality of their performance in comparison to white, male instructors. Therefore, we are looking to reduce bias as much as possible by providing a descriptive video that highlights these biases and raises awareness about the problem, thus hopefully making students’ evaluations more fair.

In terms of evaluation participation, or filledeval, we found a p-value below 0.05: 9.586671e-05. This suggests the treatment had an effect on increasing overall participation. Based on the p-value, the chance of this effect having occurred by chance is low, meaning there is sufficient evidence to conclude the participation treatment is effective. We did not find sufficient evidence to conclude that the treatment increased positive sentiment for female TAs, based on the high p-value of 0.348979. However, evidence did suggest that the treatment decreased positive sentiment for male TAs with a p-value of 0.0002842746. Due to a negative estimate, the treatment did not increase positive sentiment for male TAs. The average treatment effect estimate was -0.145, indicating the treatment decreased positive sentiment towards male TAs. Additionally, we found that the treatment had a positive effect in increasing negative sentiment for male TAs and female TAs, meaning that males received more negative comments after treatment than before. However, only the results for male TAs were significant, with an estimate of 0.0879014, and p-value of 0.01280014 compared to female TAs’ 0.2124829 p-value. When it comes to the difference in means for positive sentiments for nonwhite TAs, we found a difference of -0.1428678, with a p-value below 0.05: 6.564469e-07 . Meanwhile, the difference in means for positive sentiments for white TAs is only -0.01634666 with a p-value of 0.8002743, suggesting insignificant results for that group. The high p-value for white TAs indicates the estimate is not statistically significant. The low p-value for nonwhite TAs suggests a statistically significant decrease in positive sentiment at higher levels than white TAs. Finally, we found no significant results for negative sentiment for white TAs, but found significant results for nonwhite negative sentiment. Since the estimate, 0.1079855, obtained from this was positive, it means the treatment did not reduce, rather it increased, the proportion of negative sentiment toward non-white instructors. Such findings reveal that the treatment incresed negative feedback toward non-white TAs.

Treatment
The mode of treatment will be watching a video on gender and racial biases in teaching evaluations. This video will be created by a PowerPoint slideshow with voice overs, alternating male and female TA’s voices per slide that will read off the information. The treatment video will contain information and statistics regarding gender bias in teaching evaluations, which will be read by the female TA, and racial bias in teaching evaluations, which will be read by the male TA. Following each portion, there will be a brief summary of the implications of the data. After both portions are presented, the female TA will read the concluding slide that will prompt students to fill out the course evaluations, with another comment on the anonymity of evaluations. At the beginning of section, the TA will inform students that they will have time in class to fill out evaluations. Then, the TA will proceed to show students the video. (It will be done at the beginning of section to ensure that students are engaged and present).

Research design
The sections assigned to the treatment group will watch a video on gender and racial biases in teaching evaluations and the TA will provide students with time to fill out evaluations. A control group will receive a placebo. The design will be similar to the treatment, except the video shown will not discuss the biases that can be found in evaluations. Instead, the video will cover student evaluations and their importance, read by both a male and female TA like the treatment. The design will randomly assign 50% of sections to the treatment. In this case, we had 56 treated sections and 55 control sections. The dependent variable studied will be sentiments toward the instructor, either positive or negative, as well as whether evaluations were filled out. In the dataset, this will be called sentiment_positive, sentiment_negative, and filled_eval.

Check for problems
The data is missing some values for sentiment_negative, which I did not expect. I plan to handle this by using the “na.rm = TRUE” argument whenever doing calculations that require it. Additionally, all of the sentiment_positive values are 0, which means I will not be able to find a difference of means for positive sentiment as planned later on. As a result, I will only be able to interpret the treatment’s impact on differences in negative sentiments by race and gender.

# check if any data is NA
sum(is.na(dat))
## [1] 141
# counting how many sections there are, how many responses per section
dat %>%
  group_by(section, Z) %>%
  count() %>% 
  group_by(Z) %>% 
  count()
## # A tibble: 2 × 2
## # Groups:   Z [2]
##       Z     n
##   <int> <int>
## 1     0    49
## 2     1    50
# cleaning code here as needed
head(dat)
##   class TA section ta_female        ta_race student Z filled_eval
## 1     1  1       1         1 asian american       1 0           0
## 2     1  1       1         1 asian american       2 0           1
## 3     1  1       1         1 asian american       3 0           0
## 4     1  1       1         1 asian american       4 0           1
## 5     1  1       1         1 asian american       5 0           1
## 6     1  1       1         1 asian american       6 0           1
##   sentiment_positive sentiment_negative
## 1                  0          0.2857143
## 2                  0          0.2857143
## 3                  0          0.2857143
## 4                  0          0.2857143
## 5                  0          0.2857143
## 6                  0          0.2857143
dat %>% count(filled_eval)
##   filled_eval   n
## 1           0 612
## 2           1 800
# new data created to include only section, Z, ta_race, ta_female, positive, and negative
dat_1 <- dat %>%
  group_by(section, Z, ta_race, ta_female)%>%
  summarize(positive = mean(sentiment_positive), negative = mean(sentiment_negative, na.rm = TRUE))
head(dat_1)
## # A tibble: 6 × 6
## # Groups:   section, Z, ta_race [6]
##   section     Z ta_race        ta_female positive negative
##     <int> <int> <chr>              <int>    <dbl>    <dbl>
## 1       1     0 asian american         1        0    0.286
## 2       2     1 international          1        0    0.5  
## 3       3     1 international          1        0    0.471
## 4       4     1 white                  1        0    0.4  
## 5       5     0 international          0        0    0.462
## 6       6     1 white                  0        0    0.6
# verify that new data has correct values
dat_1$negative
##  [1] 0.28571429 0.50000000 0.47058824 0.40000000 0.46153846 0.60000000
##  [7] 0.84615385 0.64285714 0.08333333 0.42105263 0.50000000 0.11111111
## [13] 0.55555556 0.72727273 0.25000000 0.66666667 0.41666667 0.61538462
## [19] 0.56250000 0.37500000 0.45454545 0.60000000 0.52941176 0.63636364
## [25] 0.50000000 0.75000000 0.50000000 0.50000000 0.25000000 0.47368421
## [31] 0.50000000 0.47058824 0.66666667 0.60000000 0.52631579 0.30769231
## [37] 0.75000000 0.41666667 0.40000000 0.42105263 0.54545455 0.58333333
## [43] 0.40000000 0.58333333 0.54545455 0.58823529 0.77777778 0.45000000
## [49] 0.45000000 0.52941176 0.63636364 0.66666667 0.53333333 0.30769231
## [55] 0.43750000 0.60000000 0.75000000 0.77777778 0.62500000 0.38461538
## [61] 0.40000000 0.25000000 0.69230769 0.45454545 0.50000000 0.36363636
## [67] 0.55000000 0.33333333 0.16666667 0.60000000 0.35714286 0.63636364
## [73] 0.40000000 0.53333333 0.42857143 0.66666667 0.40000000 0.36842105
## [79] 0.38461538 0.60000000 0.33333333 0.50000000 0.65000000 0.43750000
## [85] 0.38461538 0.64705882 0.46666667 0.83333333 0.60000000 0.30769231
## [91] 0.60000000 0.50000000 0.58333333 0.58823529 0.44444444 0.65000000
## [97] 0.78571429 0.66666667 0.33333333
dat_1$positive
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [39] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [77] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
Analysis
# analyzing overall evaluation participation
dim_dat_positive_filledeval <- difference_in_means(filled_eval ~ Z, clusters = section, data = dat)
tidy(dim_dat_positive_filledeval)
##   term   estimate  std.error statistic      p.value   conf.low conf.high
## 1    Z 0.09321869 0.02508506  3.716105 0.0003479685 0.04339426 0.1430431
##         df     outcome
## 1 91.53852 filled_eval
# did the treatment increase negative sentiment for female TAs?
dim_dat_negative_female <- difference_in_means(negative ~ Z, clusters = section, subset = ta_female == "1", data = dat_1)
tidy(dim_dat_negative_female)
##   term    estimate  std.error  statistic   p.value    conf.low  conf.high
## 1    Z -0.01330866 0.04245788 -0.3134557 0.7553186 -0.09871836 0.07210103
##         df  outcome
## 1 47.09486 negative
#did the treatment increase negative sentiment for male TAs?
## higher estimate = less negative sentiments
dim_dat_negative_notfemale <- difference_in_means(negative ~ Z, clusters = section, subset = ta_female == "0", data = dat_1)
tidy(dim_dat_negative_notfemale)
##   term   estimate  std.error statistic    p.value    conf.low conf.high
## 1    Z 0.09791732 0.04362827  2.244355 0.02999283 0.009940585 0.1858941
##         df  outcome
## 1 43.13675 negative
# creating New Variable for white (TRUE) or nonwhite (FALSE)
section_data <- dat_1 %>%
  mutate(white_tf = if_else(ta_race == "white", TRUE, FALSE))

# difference in means calculations for negative sentiment when white
dim_dat_negative_white <- difference_in_means(negative ~ Z, clusters = section, subset = white_tf == TRUE, data = section_data)
tidy(dim_dat_negative_white)
##   term    estimate  std.error statistic   p.value  conf.low conf.high       df
## 1    Z -0.06872992 0.04604813 -1.492567 0.1440526 -0.162041 0.0245812 36.89757
##    outcome
## 1 negative
# difference in means calculations for negative sentiment when nonwhite
dim_dat_negative_nonwhite <- difference_in_means(negative ~ Z, clusters = section, subset = white_tf == FALSE, data = section_data)
tidy(dim_dat_negative_nonwhite)
##   term  estimate  std.error statistic     p.value   conf.low conf.high df
## 1    Z 0.1063267 0.03371717  3.153489 0.002555183 0.03883448  0.173819 58
##    outcome
## 1 negative
## does not work as intended
dim_dat_positive_female <- difference_in_means(positive ~ Z, clusters = section, subset = ta_female == "1", data = dat_1)
tidy(dim_dat_positive_female)
##   term estimate std.error statistic p.value conf.low conf.high       df
## 1    Z        0         0       NaN     NaN        0         0 47.09486
##    outcome
## 1 positive
dim_dat_positive_notfemale <- difference_in_means(positive ~ Z, clusters = section, subset = ta_female == "0", data = dat_1)
tidy(dim_dat_positive_notfemale)
##   term estimate std.error statistic p.value conf.low conf.high       df
## 1    Z        0         0       NaN     NaN        0         0 43.13675
##    outcome
## 1 positive
## does not work as intended
dim_dat_positive_nonwhite <- difference_in_means(positive ~ Z, clusters = section, subset = ta_race != "white", data = dat_1)
tidy(dim_dat_positive_nonwhite)
##   term estimate std.error statistic p.value conf.low conf.high df  outcome
## 1    Z        0         0       NaN     NaN        0         0 58 positive
dim_dat_positive_white <- difference_in_means(positive ~ Z, clusters = section, subset = ta_race == "white", data = dat_1)
tidy(dim_dat_positive_white)
##   term estimate std.error statistic p.value conf.low conf.high       df
## 1    Z        0         0       NaN     NaN        0         0 36.89757
##    outcome
## 1 positive
# filled eval
ggplot(data=dat, mapping = aes(x = Z, y = filled_eval)) + 
  geom_jitter(width = 0.2) +
  theme_bw()


# sentiment positive by race
dat_1 %>% 
  ggplot(mapping = aes(x = Z, y = positive)) + 
  geom_jitter(width = 0.2) + 
  facet_wrap(~ta_race) +
  theme_bw()


# positive sentiment by gender
dat_1 %>% 
  ggplot(aes(Z, positive)) +
  geom_jitter(width = 0.2) +
  facet_wrap(~ta_female) +
  theme_bw()


# negative sentiment by gender 
dat_1 %>% 
  ggplot(dat_1, mapping = aes(x = Z, y = negative)) + 
  facet_wrap(~ta_female) +       
  geom_jitter(width = 0.2) +
  theme_bw()


# plotting negative sentiment by race (white and non-white)
ggplot(data = section_data)+
  geom_jitter(aes(x = Z, y = negative), width = 0.3, height = 0, alpha = 0.7)+
  facet_wrap(~white_tf) + 
  theme_bw()


Interpretation
The overall results suggest that treatments that provide information highlighting the importance of course evaluations may be effective. I found that the treatment increased overall evaluation participation as expected from the PAP. Similar to the PAP, the evidence was not strong enough to indicate whether the treatment increased negative sentiment for female TAs due to a high p-value. Meanwhile, for men, the results were statistically significant, with a positive estimate, meaning the treatment increased negative sentiment toward men. In the PAP, we found no significant results for negative sentiment for white TAs, but found significant results for nonwhite negative sentiment. The estimate, 0.1079855, obtained from this was positive. It means the treatment did not reduce, rather it increased, the proportion of negative sentiment toward non-white instructors. The estimate in this data was very close, 0.1063267, and produced similar p-values. These significant results further establish that the treatment increased negative feedback toward nonwhite TAs. Altogether, I would not spend money on implementing this intervention because various results and estimates obtained tend to generate negative results for female and nonwhite TAs, indicating a sort of backfiring effect that occurs from the treatment. Seemingly, informing students about implicit biases tends to have limited success in reducing such biases in the end. If more positive and effective data becomes available, particularly about the missing data of positive sentiment toward TAs, I would reconsider implementing this intervention.

What went wrong?
One unexpected thing that happened was the lack of data available for positive sentiment. Without this, I was unable to compare it with the PAP to see if they were similar. Instead, I relied on the negative sentiment results to draw conclusions about the effectiveness of the intervention. Everything else went as expected.
