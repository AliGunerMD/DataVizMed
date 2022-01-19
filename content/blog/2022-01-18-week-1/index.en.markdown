---
title: Week-1
author: Ali Guner
image: w1_replica.jpg
date: 2022-01-18T19:37:14+0300
slug: []
categories:
  - figure
tags:
  - mean
  - errorbar
  - JAMA
---
<!-- this is for the link button to GitHub-->
<h1 style="font-family: Arial; font-size: 30px; text-decoration: underline">
<a href="https://github.com/AliGunerMD/DataVizMed/blob/main/content/blog/2022-01-18-week-1/index.en.Rmarkdown/">GitHub</a>
</h1>
<br><br>





First week's figure is from an RCT published in JAMA Surgery. Codes for the replica of Figure-1B will be here.  
<br>
### Selected article:
**Title:** [Impact of Portable Normothermic Blood-Based Machine Perfusion on Outcomes of Liver Transplant  
The OCS Liver PROTECT Randomized Clinical Trial](https://jamanetwork.com/journals/jamasurgery/fullarticle/2787486)  
**Journal:** JAMA Surgery  
**Authors:** Markmann JF, Abouljoud MS, Ghobrial RM, et al.  
**Year:** 2022  
**PMID:** 34985503  
**DOI:** 10.1001/jamasurg.2021.6781  


<br><br>

### the original figure:
![Figure-1B](w1_org.jpg)



### Import libraries

```r
library(tidyverse)
library(scales)
library(fabricatr)      # to fabricate fake data
library(ggsci)          # for using JAMA color pallette (not needed here)

theme_set(theme_light(base_family = "Helvetica Neue")) 
```
<br>

### Prepare fabricated data

```r
# prepare a dataset for group 1:
set.seed(2022)
OCS_liver <- fabricate(
  N = 152,
  group = "OCS_liver",
  time_0 = round(rnorm(N, mean = 7.4, sd = 2.9),2),
  time_0.5 = round(rnorm(N, mean = 3.8, sd = 2.2),2),
  time_1.0 = round(rnorm(N, mean = 2.6, sd = 2.8),2),
  time_1.5 = round(rnorm(N, mean = 1.4, sd = 1.45),2),
  time_2.0 = round(rnorm(N, mean = 1.5, sd = 1.2),2),
  time_2.5 = round(rnorm(N, mean = 1.3, sd = 1.3),2),
  time_3.0 = round(rnorm(N, mean = 1.2, sd = 1.0),2),
  time_3.5 = round(rnorm(N, mean = 1.3, sd = 0.9),2),
  time_4.0 = round(rnorm(N, mean = 1.5, sd = 0.7),2),
  time_4.5 = round(rnorm(N, mean = 1.4, sd = 1.0),2),
  time_5.0 = round(rnorm(N, mean = 1.5, sd = 0.8),2),
  time_5.5 = round(rnorm(N, mean = 1.4, sd = 1.0),2)) %>% 
  as_tibble() 


# prepare a dataset for group 2:
set.seed(2022) 
ICS <- fabricate( # because the n is too small, I preferred manual values for some.
  N = 3,
  group = "ICS",
  time_0 = c(9.2, 9.8, 10.4),
  time_0.5 = c(8.8, 9.4, 10.4),
  time_1.0 = round(rnorm(N, mean = 10.5, sd = 0),2),
  time_1.5 = c(10, 11.1, 12.2),
  time_2.0 = round(rnorm(N, mean = 11, sd = 0),2)) %>% 
  as_tibble()



# Combine two dataset
combined_dataset <-  bind_rows(OCS_liver, ICS) %>% 
  mutate (patient_id = paste0("P_", row_number())) %>% 
  select(patient_id, everything(), -ID)
```
<br>

### the view of a sample from the dataset

```r
combined_dataset %>% 
        sample_n(10)
```

```
## # A tibble: 10 × 14
##    patient_id group time_0 time_0.5 time_1.0 time_1.5 time_2.0 time_2.5 time_3.0
##    <chr>      <chr>  <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 P_55       OCS_…   8.3      4.91     4.17    -0.11     0.96     3.25    -0.36
##  2 P_75       OCS_…  11.2      5.9      2.33     3.7      0.62     1.85     2.55
##  3 P_6        OCS_…  -1.01     1.65    -0.32     0.96     0.28     3.37    -0.78
##  4 P_123      OCS_…   3.73     5        3.03    -0.46    -0.88     0.89     1.6 
##  5 P_14       OCS_…   7.67     5.96    -0.99     2.17     1.21     0.83     3.97
##  6 P_7        OCS_…   4.33     1.32     2.07     2.22     2.3      2.66     3.36
##  7 P_93       OCS_…   8.64     7.12     2.12     3.11     0.54     0.36     1.72
##  8 P_112      OCS_…   4.51     3.28     0.03     0.76     2.3      0.9      1.47
##  9 P_1        OCS_…  10.0      1.02     2.84     0.67     1.47     2.13     2.01
## 10 P_51       OCS_…  12.3     -0.85    -0.23     0.95     1.58     1.44     2.47
## # … with 5 more variables: time_3.5 <dbl>, time_4.0 <dbl>, time_4.5 <dbl>,
## #   time_5.0 <dbl>, time_5.5 <dbl>
```
<br>



### Convert possible original dataset to TIDY data

```r
tidy_data <- combined_dataset %>% 
  pivot_longer(starts_with("time"),
               names_to = "time",
               values_to = "values") %>%
  filter(!is.na(values)) %>% 
  # mutate(values = if_else(values<=0, 0, values)) %>% # this is a possible mistake in the article figure. SD should not go below 0.
  group_by(group, time) %>% 
  summarise(mean= mean(values, na.rm = TRUE),
            sd= sd(values, na.rm = TRUE))  %>% 
  ungroup() %>% 
  separate(time, into = c("blank", "time"), sep = "_") %>% 
  mutate(time = factor(time)) 
```
<br>


### R codes for figure

```r
w1_replica <- tidy_data %>% 
  ggplot(aes(time, mean, color = group)) +
  geom_errorbar(data = . %>% filter(sd != 0),
                aes(ymin = mean - sd, ymax = mean + sd), width = .3, size = .3, show.legend = F) + # single errorbar was ok, but colors of edges and lines are different. Therefore, I used an additional geom_linerange
  geom_linerange(aes(ymin = mean - sd, ymax = mean + sd), color = "black", size = .3) +
  geom_point(size = 3) +
  geom_line(aes(group = group), size = 1.2, show.legend = F) +
  # scale_color_jama(labels =c("ICS" = "Turned down","OCS_liver" = "Transplanted")) + # JAMA has its own color palette, but I prefered using manual values.
  scale_color_manual(values = c( "ICS" = "#244551","OCS_liver" = "#F28118"), labels =c("ICS" = "Turned down","OCS_liver" = "Transplanted")) +
  scale_y_continuous(breaks = seq(0,14,2), labels = number_format(accuracy = 1)) +
  labs(x = "Time on OCS Liver, h",
       y = "Mean arterial lactate, mmol/L",
       title = "Lactate levels during OCS Liver perfusion\n") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "lightgray", size = .6),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(.20, "cm"),
        axis.ticks = element_line(color = "black", size = .5),
        axis.text = element_text(color = "black", size = 10),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black", size = .4),
        legend.position = c(.75, .75),
        legend.text = element_text(size = 15),
        legend.text.align = .5,
        legend.spacing.y = unit(0, "cm"),
        legend.key.height = unit(.8, "cm"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = -0.1, vjust = 2),
        axis.title.x = element_text(size = 10, vjust = -1),
        axis.title.y = element_text(size = 10, vjust = 1)) +
  guides(colour = guide_legend(override.aes = list(shape = 16, size = 5))) +
  coord_cartesian(xlim = c(0.5, n_distinct(tidy_data$time)), ylim = c(-0.5,14), expand=0, clip = "off")  # using n_distinct is better than 12 for reproducibility.

ggsave(w1_replica,
       filename = "w1_replica.jpg",
       dpi = 300,
       width = 6,
       height = 4)
```
<br>


### Final replica
![replica Figure-1B](w1_replica.jpg)
<br>

### Some personal recommendations:
**Major:**  
1. I would not prefer using negative SD (lower threshold of 1sd of mean) values.   
1. There is an overlap in the errorbars on time-0. I would prefer using position_dodge.  
1. visualizing a distribution for a small-sized group (n = 3) may not be a good idea.   

**Minor:**  
1. I would not prefer using 1.0, 2.0, 3.0, etc. 1, 2, 3, is ok.  
1. The management of tags is ok with [patchwork](https://patchwork.data-imaginist.com/articles/guides/annotation.html) package, and should be done at the end.  
1. I m not sure about the font. an update may be required.  
<br>
