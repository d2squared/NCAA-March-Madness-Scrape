# NCAA-March-Madness-Scrape
My friend and I set out to build a machine learning model for the March Madness tournament in January 2020 and planned to enter it in the Google Cloud & NCAA ML Competition. This week, it was announced that March Madness was canceled due to COVID-19, also known as coronavirus. However, in the weeks leading up to the tournament, we scrapped Ken Pomeroy’s website, kenpom.com, for data since 2002. Below, I outline how we scrapped the gathered data from the website. In addition, the data files that I scrapped for the years 2002-2019 are on my Github page.

```{r}
library(rvest)
library(stringr)
library(tidyr)
library(dplyr)
library(parallel)
```
