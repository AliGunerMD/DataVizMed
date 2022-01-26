---
title: "{{ replace .TranslationBaseName "-" " " | title }}"
date: {{ .Date }}
draft: true
---

<!-- this is for the link button to GitHub-->
<h1 style="font-family: Arial; font-size: 30px; text-decoration: underline">
<a href="https://github.com/AliGunerMD/DataVizMed/blob/main/content/blog/2022-MM-DD-week-XXX/index.en.Rmarkdown/">GitHub</a>
</h1>
<br><br>


```{r setup, include = FALSE}
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
```
<SHORT NOTE>
<br>

### Selected article:
**Title:** [<article title>](<article link>))   
**Journal:** <Journal Name>    
**Authors:** <Authors>   et al.  
**Year:** <Year>    
**PMID:** [<PMID>](<PMID link>)  
**DOI:** <DOI number>     


<br><br>
### The original figure
![Figure-XXX](wXXX_org.jpg)



<br>
### Import libraries
```{r, echo = TRUE}
library(tidyverse)
library(scales)
library(fabricatr)      # to fabricate fake data
library(patchwork)      # to combine plots
library(cowplot)        # to combine plots


theme_set(theme_light(base_family = "Open Sans"))
```

<br>
### Prepare fabricated data



<br>
**Possible strategy:** 


<br>
### R codes for the figure


<br>
### Final replica
![replica Figure-XXX](wXXX_replica.jpg)

<br>
### Some personal recommendations:   

<br><br>