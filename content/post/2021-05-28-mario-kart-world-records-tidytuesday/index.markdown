---
title: Mario Kart World Records - TidyTuesday
author: Yuri Lucatelli Taba
date: '2021-05-28'
slug: mario-kart-world-records-tidytuesday
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2021-05-28T17:48:21-03:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

### TidyTuesday - Week 22

*"The data this week comes from Mario Kart World Records and contains world records for the classic (if you’re a 90’s kid) racing game on the Nintendo 64."*

See more at TidyTuesday's [GitHub](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-25/readme.md)


```r
library(tidyverse)
library(tidytuesdayR)
library(ggthemes)
library(broom)
library(knitr)

theme_set(theme_tufte())
```

#### Load the weekly Data



```r
tt <- tidytuesdayR::tt_load("2021-05-25")
```

```
## 
## 	Downloading file 1 of 2: `drivers.csv`
## 	Downloading file 2 of 2: `records.csv`
```

```r
tt

drivers <- tt$drivers
records <- tt$records
```


```r
head(drivers) %>% 
  kable()
```



| position|player | total| year| records|nation    |
|--------:|:------|-----:|----:|-------:|:---------|
|        1|Penev  |   344| 1997|      NA|Australia |
|        1|Penev  |   344| 1998|     181|Australia |
|        1|Penev  |   344| 1999|     126|Australia |
|        1|Penev  |   344| 2000|      14|Australia |
|        1|Penev  |   344| 2001|       5|Australia |
|        1|Penev  |   344| 2002|      11|Australia |


```r
head(records) %>% 
  kable()
```



|track         |type      |shortcut |player  |system_played |date       |time_period |   time| record_duration|
|:-------------|:---------|:--------|:-------|:-------------|:----------|:-----------|------:|---------------:|
|Luigi Raceway |Three Lap |No       |Salam   |NTSC          |1997-02-15 |2M 12.99S   | 132.99|               1|
|Luigi Raceway |Three Lap |No       |Booth   |NTSC          |1997-02-16 |2M 9.99S    | 129.99|               0|
|Luigi Raceway |Three Lap |No       |Salam   |NTSC          |1997-02-16 |2M 8.99S    | 128.99|              12|
|Luigi Raceway |Three Lap |No       |Salam   |NTSC          |1997-02-28 |2M 6.99S    | 126.99|               7|
|Luigi Raceway |Three Lap |No       |Gregg G |NTSC          |1997-03-07 |2M 4.51S    | 124.51|              54|
|Luigi Raceway |Three Lap |No       |Rocky G |NTSC          |1997-04-30 |2M 2.89S    | 122.89|               0|

### Brazilian players performance




```r
drivers %>% 
  filter(nation == "Brazil",
         !is.na(records)) %>% 
  group_by(player) %>% 
  summarize(total_records = sum(records), .groups = "drop") %>% 
  arrange(desc(total_records)) %>% 
  kable()
```



|player  | total_records|
|:-------|-------------:|
|Karlo   |            53|
|Marcos  |             5|
|Marcelo |             4|
|Moritz  |             2|

Only 4 Brazilian players. The best one is Karlo, who has 53 world records.

Let's dig into these Karlo's 53 records.


```r
records |>
  filter(player == "Karlo") %>%
  head() %>% 
  kable()
```



|track           |type       |shortcut |player |system_played |date       |time_period |   time| record_duration|
|:---------------|:----------|:--------|:------|:-------------|:----------|:-----------|------:|---------------:|
|Luigi Raceway   |Three Lap  |Yes      |Karlo  |NTSC          |2005-09-19 |1M 33.22S   |  93.22|            2537|
|Moo Moo Farm    |Three Lap  |No       |Karlo  |NTSC          |2006-07-12 |1M 27.2S    |  87.20|             238|
|Moo Moo Farm    |Single Lap |No       |Karlo  |NTSC          |2006-01-20 |28.14S      |  28.14|             407|
|Moo Moo Farm    |Single Lap |No       |Karlo  |NTSC          |2007-03-03 |27.99S      |  27.99|            1226|
|Kalimari Desert |Three Lap  |Yes      |Karlo  |NTSC          |2005-05-16 |2M 5.42S    | 125.42|             182|
|Kalimari Desert |Three Lap  |Yes      |Karlo  |NTSC          |2005-11-14 |2M 5.16S    | 125.16|            1559|


```r
records %>% 
  filter(player == "Karlo") %>% 
  group_by(track, type, shortcut) %>% 
  summarize(total_record_duration = sum(record_duration), .groups = "drop") %>% 
  arrange(desc(total_record_duration)) %>% 
  head(10) %>% 
  kable()
```



|track           |type       |shortcut | total_record_duration|
|:---------------|:----------|:--------|---------------------:|
|Luigi Raceway   |Three Lap  |Yes      |                  2537|
|Rainbow Road    |Three Lap  |Yes      |                  2179|
|Kalimari Desert |Three Lap  |Yes      |                  2039|
|Moo Moo Farm    |Single Lap |No       |                  1633|
|Frappe Snowland |Three Lap  |Yes      |                  1526|
|Toad's Turnpike |Three Lap  |Yes      |                  1314|
|Yoshi Valley    |Three Lap  |No       |                   910|
|Royal Raceway   |Three Lap  |Yes      |                   825|
|Choco Mountain  |Three Lap  |Yes      |                   491|
|Mario Raceway   |Three Lap  |Yes      |                   377|

He held the record in Luigi Raceway for 2537 days. That is almost 7 years.


### Effectiveness of shortcuts

How much time per lap could I save using shortcuts? Where does shortcut is more effective?

- Single Lap type:


```r
records %>% 
  group_by(type, shortcut) %>% 
  summarize(avg_time = mean(time), .groups = "drop") %>% 
  ggplot(aes(shortcut, avg_time)) +
  geom_col() +
  facet_wrap(~ type) +
  labs(y = "Average Time (seconds)",
       x = "Using a shortcut?",
       title = "Average time for single and three laps races using a shortcut or not") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

It looks like getting a shortcut is not that effective when pursuing a record in Single Lap races. Actually, on average, it takes more time using shortcuts than not.

Let's test this finding with a t-student stat.


```r
single <- records %>% 
  filter(type == "Single Lap") %>% 
  t.test(time ~ shortcut, data = .) %>% 
  tidy() %>% 
  mutate(type = "Single Lap")


three <- records %>% 
  filter(type == "Three Lap") %>% 
  t.test(time ~ shortcut, data = .) %>% 
  tidy() %>% 
  mutate(type = "Three Lap") 

ttest <- bind_rows(single, three)

ggplot(ttest) +
  geom_point(aes(x = type, y = estimate)) +
  geom_errorbar(aes(type, ymin = conf.low, ymax = conf.high), width = 0.05) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = "Difference in means (95% confidence interval)",
       x = "Race type",
       title = "Difference in means test (t.test) with a 95% confidence interval",
       subtitle = "Difference between average race time using a shortcut or not by both race types") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Yeah, while taking a shortcut in a Three Lap race is really effective, on average reducing 57.81 seconds by race, the same is not true for Single Lap races, where taking a shortcut actually increases the lap time by 3.59 seconds, on average. 
