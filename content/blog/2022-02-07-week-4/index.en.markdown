---
title: Week-4
author: Ali Guner
image: w4_replica.jpg
date: 2022-02-07T02:01:33+0300
slug: []
categories: 
  - table
tags:
  - table1
  - finalfit
  - flextable
  - ftExtra
editor_options: 
  chunk_output_type: console
---
<script src="{{< blogdown/postref >}}index.en_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index.en_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index.en_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index.en_files/lightable/lightable.css" rel="stylesheet" />

<!-- this is for the link button to GitHub-->
<button class="button">
    <a href="https://github.com/AliGunerMD/DataVizMed/blob/main/content/blog/2022-02-07-week-4/index.en.Rmarkdown/"> <i class="fab fa-github"></i>GitHub</a>
</button>

<br><br>


Tables are also a part of data visualization. Elegant tables may make your article better for the editors/reviewers.  


<br>

### Selected article:
**Title:** [Early Remdesivir to Prevent Progression to Severe Covid-19 in Outpatients](https://www.nejm.org/doi/full/10.1056/NEJMoa2116846)   
**Journal:** The New England Journal of Medicine    
**Authors:** Gottlieb RL, Vaca CE, Paredes R,  et al.  
**Year:** 2022    
**PMID:** [34937145](https://pubmed.ncbi.nlm.nih.gov/34937145/)  
**DOI:** 10.1056/NEJMoa2116846
     


<br><br>

### The original figure
![Table 1](w4_org.jpg)



<br>

### Import libraries

```r
library(tidyverse)
library(fabricatr)      # to fabricate fake data

library(finalfit)       # to create final output tables
library(flextable)      # to create/modify/format tables for reporting and publications
library(ftExtra)        # extensions for flextable package
library(officer)        # to manipulate Word document        
```

<br>

### Prepare fabricated data


```r
set.seed(2022)
group_remdecivir <- fabricate(
  N = 279,
  age = round(rnorm(N, mean = 50, sd = 15)),
  bmi = round(rnorm(N, mean = 31.2, sd = 6.7), 1),
  dur_symptoms = round(runif(N, min = 1.5, max = 8)),
  dur_since_sirs = round(runif(N, min = 0, max = 4)),
  viral_load = round(rnorm(N, mean = 6.31, sd = 1.75), 3),
  sex = draw_binary(N = N, prob = 0.53),
  residence_usa = draw_binary(N = N, prob = .946),
  white = draw_binary(N = N, prob = 0.817),
  black = draw_binary(N = N, prob = 0.072),
  american_indian_native = draw_binary(N = N, prob = 0.054),
  asian_native = draw_binary(N = N, prob = 0.025),
  hispanic = draw_binary(N = N, prob = 0.441),
  other = draw_binary(N = N, prob = 0.011),
  diabetes = draw_binary(N = N, prob = .62),
  obesity = draw_binary(N = N, prob = .552),
  ht = draw_binary(N = N, prob = .495),
  lung = draw_binary(N = N, prob = .24),
  cancer = draw_binary(N = N, prob = .043),
  cardiac = draw_binary(N = N, prob = .072),
  immune = draw_binary(N = N, prob = .05),
  kidney = draw_binary(N = N, prob = .025),
  liver = draw_binary(N = N, prob = .004),
  residence_nursing = draw_binary(N = N, prob = 0.029),
  group = "Remdesivir") %>%
  as_tibble()


set.seed(2022)
group_placebo <- fabricate(
  N = 283,
  age = round(rnorm(N, mean = 51, sd = 15)),
  bmi = round(rnorm(N, mean = 30.8, sd = 5.8), 1),
  dur_symptoms = round(runif(N, min = 2, max = 8)),
  dur_since_sirs = round(runif(N, min = 0, max = 5)),
  viral_load = round(rnorm(N, mean = 6.28, sd = 1.79), 3),
  sex = draw_binary(N = N, prob = 0.512),
  residence_usa = draw_binary(N = N, prob = .943),
  white = draw_binary(N = N, prob = 0.792),
  black = draw_binary(N = N, prob = 0.078),
  american_indian_native = draw_binary(N = N, prob = 0.074),
  asian_native = draw_binary(N = N, prob = 0.025),
  hispanic = draw_binary(N = N, prob = 0.396),
  other = draw_binary(N = N, prob = 0.007),
  diabetes = draw_binary(N = N, prob = .611),
  obesity = draw_binary(N = N, prob = .551),
  ht = draw_binary(N = N, prob = .459),
  lung = draw_binary(N = N, prob = .24),
  cancer = draw_binary(N = N, prob = .064),
  cardiac = draw_binary(N = N, prob = .085),
  immune = draw_binary(N = N, prob = .032),
  kidney = draw_binary(N = N, prob = .039),
  liver = draw_binary(N = N, prob = .004),
  residence_nursing = draw_binary(N = N, prob = 0.025),
  group = "Placebo"
  ) %>%
  as_tibble() 





combined_dataset <- bind_rows(group_remdecivir, group_placebo)  %>% 
  mutate (patient_id = paste0("P_", row_number())) %>% 
  select(patient_id, group, everything(), -ID) %>% 
  mutate(group = fct_relevel(group, "Remdesivir", "Placebo"),
         age_category_over60 = factor(if_else(age >= 60, "yes", "no")),
         age_category_under18 = factor(if_else(age < 18, "yes", "no"))) %>% 
  mutate_if(is.integer, as.factor) %>% 
  mutate(sex = fct_recode(sex, "female" = "0", "male" = "1")) %>% 
  mutate(across((residence_usa:residence_nursing), ~ fct_recode(., "no" = "0", "yes" = "1"))) %>% 
  add_column(age_category = "yes",      # I added three columns. This is my trick to add empty rows into the table. We can merge them in flextable step. 
             race_ethnic = "yes",       
             comorbidities = "yes")     
```


<br>

**Possible strategy:** 
Tables are good to present the exact data. but not that good as an informative figure, however, they are needed to present scientific data.
I always use {[finalfit](https://finalfit.org/)} package to prepare my tables. It is life-saver and saved my hours-days in my projects. Not only table-1, also great for logistic regression, Cox regression, multilevel/multivariable analysis.  

After I realized that finalfit object is a data.frame, I added all tidyverse tricks to improve my tables with R codes. 
Because Microsoft Word file is used to submit our papers, I added {[flextable](https://davidgohel.github.io/flextable/)},  {[officer](https://davidgohel.github.io/officer/)}, {[ftExtra](https://ftextra.atusy.net/)} packages into my workflow. Although there are other tools to present tables in R, {[flextable](https://davidgohel.github.io/flextable/)} seems the best for a word output.  


<br>

### R codes for the table

```r
explanatory <-  combined_dataset %>% 
  select(age, age_category, age_category_over60, age_category_under18, sex, residence_usa, race_ethnic, white:other, bmi,comorbidities, diabetes:residence_nursing, dur_symptoms:viral_load) %>% 
  names()

dependent <- "group"

finalfit_table <- combined_dataset %>% 
  mutate(across(c(age_category_over60, age_category_under18, residence_usa:residence_nursing), ~fct_relevel(., "yes", "no"))) %>%       # This is my trick to remove "no" rows in the flextable step.
  summary_factorlist(dependent = dependent, 
                     explanatory = explanatory,
                     p = FALSE,
                     cont_nonpara = c(26, 27),
                     total_col = TRUE,
                     add_col_totals = TRUE,
                     include_col_totals_percent = FALSE,
                     col_totals_prefix = "N=")
```

### output of finalfit package

```r
finalfit_table %>% 
        knitr::kable() %>% kableExtra::kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> label </th>
   <th style="text-align:left;"> levels </th>
   <th style="text-align:left;"> Remdesivir </th>
   <th style="text-align:left;"> Placebo </th>
   <th style="text-align:left;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Total N </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> N=279 </td>
   <td style="text-align:left;"> N=283 </td>
   <td style="text-align:left;"> N=562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:left;"> Mean (SD) </td>
   <td style="text-align:left;"> 49.7 (15.0) </td>
   <td style="text-align:left;"> 50.6 (15.0) </td>
   <td style="text-align:left;"> 50.2 (15.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age_category </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 279 (100.0) </td>
   <td style="text-align:left;"> 283 (100.0) </td>
   <td style="text-align:left;"> 562 (100.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age_category_over60 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 78 (28.0) </td>
   <td style="text-align:left;"> 80 (28.3) </td>
   <td style="text-align:left;"> 158 (28.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 201 (72.0) </td>
   <td style="text-align:left;"> 203 (71.7) </td>
   <td style="text-align:left;"> 404 (71.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age_category_under18 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 2 (0.7) </td>
   <td style="text-align:left;"> 1 (0.4) </td>
   <td style="text-align:left;"> 3 (0.5) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 277 (99.3) </td>
   <td style="text-align:left;"> 282 (99.6) </td>
   <td style="text-align:left;"> 559 (99.5) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sex </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 128 (45.9) </td>
   <td style="text-align:left;"> 143 (50.5) </td>
   <td style="text-align:left;"> 271 (48.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> 151 (54.1) </td>
   <td style="text-align:left;"> 140 (49.5) </td>
   <td style="text-align:left;"> 291 (51.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> residence_usa </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 263 (94.3) </td>
   <td style="text-align:left;"> 267 (94.3) </td>
   <td style="text-align:left;"> 530 (94.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 16 (5.7) </td>
   <td style="text-align:left;"> 16 (5.7) </td>
   <td style="text-align:left;"> 32 (5.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> race_ethnic </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 279 (100.0) </td>
   <td style="text-align:left;"> 283 (100.0) </td>
   <td style="text-align:left;"> 562 (100.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> white </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 216 (77.4) </td>
   <td style="text-align:left;"> 218 (77.0) </td>
   <td style="text-align:left;"> 434 (77.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 63 (22.6) </td>
   <td style="text-align:left;"> 65 (23.0) </td>
   <td style="text-align:left;"> 128 (22.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> black </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 21 (7.5) </td>
   <td style="text-align:left;"> 26 (9.2) </td>
   <td style="text-align:left;"> 47 (8.4) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 258 (92.5) </td>
   <td style="text-align:left;"> 257 (90.8) </td>
   <td style="text-align:left;"> 515 (91.6) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> american_indian_native </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 19 (6.8) </td>
   <td style="text-align:left;"> 19 (6.7) </td>
   <td style="text-align:left;"> 38 (6.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 260 (93.2) </td>
   <td style="text-align:left;"> 264 (93.3) </td>
   <td style="text-align:left;"> 524 (93.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> asian_native </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 7 (2.5) </td>
   <td style="text-align:left;"> 5 (1.8) </td>
   <td style="text-align:left;"> 12 (2.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 272 (97.5) </td>
   <td style="text-align:left;"> 278 (98.2) </td>
   <td style="text-align:left;"> 550 (97.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hispanic </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 117 (41.9) </td>
   <td style="text-align:left;"> 110 (38.9) </td>
   <td style="text-align:left;"> 227 (40.4) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 162 (58.1) </td>
   <td style="text-align:left;"> 173 (61.1) </td>
   <td style="text-align:left;"> 335 (59.6) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> other </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 5 (1.8) </td>
   <td style="text-align:left;"> 1 (0.4) </td>
   <td style="text-align:left;"> 6 (1.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 274 (98.2) </td>
   <td style="text-align:left;"> 282 (99.6) </td>
   <td style="text-align:left;"> 556 (98.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bmi </td>
   <td style="text-align:left;"> Mean (SD) </td>
   <td style="text-align:left;"> 31.0 (6.8) </td>
   <td style="text-align:left;"> 30.6 (5.9) </td>
   <td style="text-align:left;"> 30.8 (6.4) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> comorbidities </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 279 (100.0) </td>
   <td style="text-align:left;"> 283 (100.0) </td>
   <td style="text-align:left;"> 562 (100.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> diabetes </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 171 (61.3) </td>
   <td style="text-align:left;"> 173 (61.1) </td>
   <td style="text-align:left;"> 344 (61.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 108 (38.7) </td>
   <td style="text-align:left;"> 110 (38.9) </td>
   <td style="text-align:left;"> 218 (38.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> obesity </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 144 (51.6) </td>
   <td style="text-align:left;"> 143 (50.5) </td>
   <td style="text-align:left;"> 287 (51.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 135 (48.4) </td>
   <td style="text-align:left;"> 140 (49.5) </td>
   <td style="text-align:left;"> 275 (48.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ht </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 133 (47.7) </td>
   <td style="text-align:left;"> 112 (39.6) </td>
   <td style="text-align:left;"> 245 (43.6) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 146 (52.3) </td>
   <td style="text-align:left;"> 171 (60.4) </td>
   <td style="text-align:left;"> 317 (56.4) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lung </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 62 (22.2) </td>
   <td style="text-align:left;"> 61 (21.6) </td>
   <td style="text-align:left;"> 123 (21.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 217 (77.8) </td>
   <td style="text-align:left;"> 222 (78.4) </td>
   <td style="text-align:left;"> 439 (78.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cancer </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 9 (3.2) </td>
   <td style="text-align:left;"> 13 (4.6) </td>
   <td style="text-align:left;"> 22 (3.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 270 (96.8) </td>
   <td style="text-align:left;"> 270 (95.4) </td>
   <td style="text-align:left;"> 540 (96.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cardiac </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 13 (4.7) </td>
   <td style="text-align:left;"> 25 (8.8) </td>
   <td style="text-align:left;"> 38 (6.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 266 (95.3) </td>
   <td style="text-align:left;"> 258 (91.2) </td>
   <td style="text-align:left;"> 524 (93.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> immune </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 14 (5.0) </td>
   <td style="text-align:left;"> 9 (3.2) </td>
   <td style="text-align:left;"> 23 (4.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 265 (95.0) </td>
   <td style="text-align:left;"> 274 (96.8) </td>
   <td style="text-align:left;"> 539 (95.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kidney </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 7 (2.5) </td>
   <td style="text-align:left;"> 10 (3.5) </td>
   <td style="text-align:left;"> 17 (3.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 272 (97.5) </td>
   <td style="text-align:left;"> 273 (96.5) </td>
   <td style="text-align:left;"> 545 (97.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> liver </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 2 (0.7) </td>
   <td style="text-align:left;"> 2 (0.7) </td>
   <td style="text-align:left;"> 4 (0.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 277 (99.3) </td>
   <td style="text-align:left;"> 281 (99.3) </td>
   <td style="text-align:left;"> 558 (99.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> residence_nursing </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> 8 (2.9) </td>
   <td style="text-align:left;"> 8 (2.8) </td>
   <td style="text-align:left;"> 16 (2.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> 271 (97.1) </td>
   <td style="text-align:left;"> 275 (97.2) </td>
   <td style="text-align:left;"> 546 (97.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dur_symptoms </td>
   <td style="text-align:left;"> Median (IQR) </td>
   <td style="text-align:left;"> 5.0 (3.0 to 6.0) </td>
   <td style="text-align:left;"> 5.0 (4.0 to 6.0) </td>
   <td style="text-align:left;"> 5.0 (3.0 to 6.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dur_since_sirs </td>
   <td style="text-align:left;"> Median (IQR) </td>
   <td style="text-align:left;"> 2.0 (1.0 to 3.0) </td>
   <td style="text-align:left;"> 3.0 (1.0 to 4.0) </td>
   <td style="text-align:left;"> 2.0 (1.0 to 3.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> viral_load </td>
   <td style="text-align:left;"> Mean (SD) </td>
   <td style="text-align:left;"> 6.4 (1.9) </td>
   <td style="text-align:left;"> 6.4 (1.9) </td>
   <td style="text-align:left;"> 6.4 (1.9) </td>
  </tr>
</tbody>
</table>

<br>

### modifying finalfit output using tidyverse approach

```r
modified_finalfit_table <- finalfit_table %>% 
  dplyr::mutate(across((Remdesivir:Total), ~ if_else(label == "Total N", paste0("(", . , ")"), .))) %>%   
  dplyr::mutate(across((Remdesivir:Total), ~ if_else(label %in% c("age", "bmi", "viral_load"), str_replace_all(., " \\(", "±"), . ))) %>% 
  dplyr::mutate(across((Remdesivir:Total), ~ if_else(label %in% c("age", "bmi", "viral_load"), str_replace_all(., "\\)", ""), . ))) %>% 
  filter(levels != "no") %>% 
  filter(levels != "male") %>% 
  mutate(levels = if_else(levels %in% c("Median (IQR)", "Mean (SD)"), " ", levels),
         across(Remdesivir:Total, ~str_replace_all(., " to ", "-")), # , "Total" were removed.
         across(Remdesivir:Total, ~str_replace_all(., "\\.0", "")), # , "Total" were removed.
         levels = str_remove_all(levels, "yes"),
         label = fct_recode(label,
                             "Characteristic" = "Total N",
                             "Age—yr" = "age",
                             "≥60 yr" = "age_category_over60",
                             "<18 yr" = "age_category_under18",
                             "Female sex — no. (%)" = "sex",
                             "Residence in the United States — no. (%)" = "residence_usa",
                             "White" = "white",
                             "Black" = "black",
                             "American Indian or Alaska Native" = "american_indian_native",
                             "Asian, Native Hawaiian, or Pacific Islander" = "asian_native",
                             "Hispanic or Latinx" = "hispanic",
                             "Other" = "other",
                             "Body-mass index" = "bmi",
                             "Diabetes mellitus" = "diabetes",
                             "Obesity" = "obesity",
                             "Hypertension" = "ht",
                             "Chronic lung disease" = "lung",
                             "Current cancer" = "cancer",
                             "Cardiovascular or cerebrovascular disease" = "cardiac",
                             "Immune compromise" = "immune",
                             "Chronic kidney disease, mild or moderate" = "kidney",
                             "Chronic liver disease" = "liver",
                             "Residence in skilled nursing facility — no. (%)" = "residence_nursing",
                             "Median duration of symptoms before first infusion (IQR) — days" = "dur_symptoms",
                             "Median time since RT-PCR confirmation of SARS-CoV-2 (IQR) — days" = "dur_since_sirs",
                             "Mean SARS-CoV-2 RNA nasopharyngeal viral load — log~10~ copies/ml" = "viral_load")) %>% 
  select(-levels)
  
modified_finalfit_table %>% 
        knitr::kable() %>% kableExtra::kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> label </th>
   <th style="text-align:left;"> Remdesivir </th>
   <th style="text-align:left;"> Placebo </th>
   <th style="text-align:left;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Characteristic </td>
   <td style="text-align:left;"> (N=279) </td>
   <td style="text-align:left;"> (N=283) </td>
   <td style="text-align:left;"> (N=562) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Age—yr </td>
   <td style="text-align:left;"> 49.7±15 </td>
   <td style="text-align:left;"> 50.6±15 </td>
   <td style="text-align:left;"> 50.2±15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age_category </td>
   <td style="text-align:left;"> 279 (100) </td>
   <td style="text-align:left;"> 283 (100) </td>
   <td style="text-align:left;"> 562 (100) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ≥60 yr </td>
   <td style="text-align:left;"> 78 (28) </td>
   <td style="text-align:left;"> 80 (28.3) </td>
   <td style="text-align:left;"> 158 (28.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> &lt;18 yr </td>
   <td style="text-align:left;"> 2 (0.7) </td>
   <td style="text-align:left;"> 1 (0.4) </td>
   <td style="text-align:left;"> 3 (0.5) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Female sex — no. (%) </td>
   <td style="text-align:left;"> 128 (45.9) </td>
   <td style="text-align:left;"> 143 (50.5) </td>
   <td style="text-align:left;"> 271 (48.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Residence in the United States — no. (%) </td>
   <td style="text-align:left;"> 263 (94.3) </td>
   <td style="text-align:left;"> 267 (94.3) </td>
   <td style="text-align:left;"> 530 (94.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> race_ethnic </td>
   <td style="text-align:left;"> 279 (100) </td>
   <td style="text-align:left;"> 283 (100) </td>
   <td style="text-align:left;"> 562 (100) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> 216 (77.4) </td>
   <td style="text-align:left;"> 218 (77) </td>
   <td style="text-align:left;"> 434 (77.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black </td>
   <td style="text-align:left;"> 21 (7.5) </td>
   <td style="text-align:left;"> 26 (9.2) </td>
   <td style="text-align:left;"> 47 (8.4) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> American Indian or Alaska Native </td>
   <td style="text-align:left;"> 19 (6.8) </td>
   <td style="text-align:left;"> 19 (6.7) </td>
   <td style="text-align:left;"> 38 (6.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Asian, Native Hawaiian, or Pacific Islander </td>
   <td style="text-align:left;"> 7 (2.5) </td>
   <td style="text-align:left;"> 5 (1.8) </td>
   <td style="text-align:left;"> 12 (2.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hispanic or Latinx </td>
   <td style="text-align:left;"> 117 (41.9) </td>
   <td style="text-align:left;"> 110 (38.9) </td>
   <td style="text-align:left;"> 227 (40.4) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:left;"> 5 (1.8) </td>
   <td style="text-align:left;"> 1 (0.4) </td>
   <td style="text-align:left;"> 6 (1.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Body-mass index </td>
   <td style="text-align:left;"> 31±6.8 </td>
   <td style="text-align:left;"> 30.6±5.9 </td>
   <td style="text-align:left;"> 30.8±6.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> comorbidities </td>
   <td style="text-align:left;"> 279 (100) </td>
   <td style="text-align:left;"> 283 (100) </td>
   <td style="text-align:left;"> 562 (100) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Diabetes mellitus </td>
   <td style="text-align:left;"> 171 (61.3) </td>
   <td style="text-align:left;"> 173 (61.1) </td>
   <td style="text-align:left;"> 344 (61.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Obesity </td>
   <td style="text-align:left;"> 144 (51.6) </td>
   <td style="text-align:left;"> 143 (50.5) </td>
   <td style="text-align:left;"> 287 (51.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hypertension </td>
   <td style="text-align:left;"> 133 (47.7) </td>
   <td style="text-align:left;"> 112 (39.6) </td>
   <td style="text-align:left;"> 245 (43.6) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chronic lung disease </td>
   <td style="text-align:left;"> 62 (22.2) </td>
   <td style="text-align:left;"> 61 (21.6) </td>
   <td style="text-align:left;"> 123 (21.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Current cancer </td>
   <td style="text-align:left;"> 9 (3.2) </td>
   <td style="text-align:left;"> 13 (4.6) </td>
   <td style="text-align:left;"> 22 (3.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cardiovascular or cerebrovascular disease </td>
   <td style="text-align:left;"> 13 (4.7) </td>
   <td style="text-align:left;"> 25 (8.8) </td>
   <td style="text-align:left;"> 38 (6.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Immune compromise </td>
   <td style="text-align:left;"> 14 (5) </td>
   <td style="text-align:left;"> 9 (3.2) </td>
   <td style="text-align:left;"> 23 (4.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chronic kidney disease, mild or moderate </td>
   <td style="text-align:left;"> 7 (2.5) </td>
   <td style="text-align:left;"> 10 (3.5) </td>
   <td style="text-align:left;"> 17 (3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chronic liver disease </td>
   <td style="text-align:left;"> 2 (0.7) </td>
   <td style="text-align:left;"> 2 (0.7) </td>
   <td style="text-align:left;"> 4 (0.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Residence in skilled nursing facility — no. (%) </td>
   <td style="text-align:left;"> 8 (2.9) </td>
   <td style="text-align:left;"> 8 (2.8) </td>
   <td style="text-align:left;"> 16 (2.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Median duration of symptoms before first infusion (IQR) — days </td>
   <td style="text-align:left;"> 5 (3-6) </td>
   <td style="text-align:left;"> 5 (4-6) </td>
   <td style="text-align:left;"> 5 (3-6) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Median time since RT-PCR confirmation of SARS-CoV-2 (IQR) — days </td>
   <td style="text-align:left;"> 2 (1-3) </td>
   <td style="text-align:left;"> 3 (1-4) </td>
   <td style="text-align:left;"> 2 (1-3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean SARS-CoV-2 RNA nasopharyngeal viral load — log~10~ copies/ml </td>
   <td style="text-align:left;"> 6.4±1.9 </td>
   <td style="text-align:left;"> 6.4±1.9 </td>
   <td style="text-align:left;"> 6.4±1.9 </td>
  </tr>
</tbody>
</table>


<br>

### final touch with flextable

```r
set_flextable_defaults(
        font.family = "Open Sans",
        font.size = 10,
        padding.bottom = 3, 
        padding.top = 3,
        padding.left = 3,
        padding.right = 3)

table_1 <- modified_finalfit_table %>% 
  flextable() %>% 
  add_header_lines("[Table 1.]{color=#BD272E} Demographic and Clinical Characteristics of the Patients at Baseline.") %>% 
  align(part = "all", align = "center", j=2:ncol(modified_finalfit_table)) %>% 
  border_remove(.) %>% 
  hline(part="header", i = 1) %>% 
  hline_bottom(part="body", border = fp_border(color="black")) %>%
  hline_top(part="header", border = fp_border(color="black")) %>% 
  vline_left(border = fp_border(color="black")) %>%
  vline_right(border = fp_border(color="black")) %>% 
  bold(part="header", i = 1:2) %>% 
  bold(part="body", i = 1) %>% 
  padding( part = "body", j = 1, i = c(4,5,9:14,17:25), padding.left = 30) %>% 
  bg(bg = "#FFF8E7", part = "body", i=seq(2,nrow(modified_finalfit_table), 2)) %>% 
  bg(bg = "#F5EFE7", part = "header", i=1) %>% 
  colformat_md(j = 1,  part = "body") %>% 
  colformat_md(j = 1, part = "header") %>% 
  set_header_labels(i = 1, "label"=" ") %>% 
  compose(i = 3, j=1:ncol(modified_finalfit_table),  value = as_paragraph("Age category — no. (%)"), part = "body") %>% 
  merge_h (i = 3) %>% 
  compose(i = 8, j=1:ncol(modified_finalfit_table),  value = as_paragraph("Race or ethnic group — no. (%)"), part = "body") %>% 
  merge_h (i = 8) %>% 
  compose(i = 16, j=1:ncol(modified_finalfit_table),  value = as_paragraph("Coexisting conditions — no. (%)"), part = "body") %>% 
  merge_h (i = 16) %>% 
  footnote(part = "header", i = 1, value = as_paragraph("Plus–minus values are means ±SD. IQR denotes interquartile range, RT-PCR reverse transcriptase–polymerase chain reaction, and SARS-CoV-2 severe acute respiratory syndrome coronavirus 2."), ref_symbols = "\U2606") %>% 
  footnote(part = "body", i = 7, j = 1, value = as_paragraph("Race and ethnic group were reported by the patients. Patients could have had more than one race or ethnic group."), ref_symbols = "\U2020") %>% 
  footnote(part = "body", i = 29, j = 1, value = as_paragraph("Data are shown for the virologic analysis set, which is defined in the statistical analysis plan (available with the protocol at NEJM.org): 215 of 279 patients (77.1%) in the remdesivir group and 213 of 283 patients (75.3%) in the placebo group."), ref_symbols = "\U2021") %>% 
  hline_bottom(part="footer", border = fp_border(color="black")) %>% 
  width(j = 1, 8.6, "cm") %>% 
  width(j = 2:4, 3.6, "cm") %>% 
  fix_border_issues() 

# table_1
```


### save flextable object as a word file (portrait or landscape)

```r
sect_properties_portrait <- prop_section(
  page_size = page_size(orient = "portrait",
    width = 11.7, height = 8.3),
  type = "nextPage",
  page_margins = page_mar()
)

save_as_docx(table_1,
             path = here::here("content", "blog", "2022-02-07-week-4",paste0("table_1", ".docx")),
              pr_section = sect_properties_portrait)
```


<br>

### Final replica
![replica Table 1](w4_replica.jpg)

<br>

### Some personal comments:   

1. finalfit output is good enough. However, because we prefer .docx output for the submission process, It may be a good idea to modify it with code. It gives you reproducibility. 
flextable and ftExtra is great complements to finalfit output.
1. If a word file has more than two pages, It gives repeated header to each pages in word file. [No solution via R](https://github.com/davidgohel/flextable/issues/160), however, can be changed in word's table properties-> Row-> Uncheck "Repeat as header at the top of each page".


<br><br>
