---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Using R to look at trends in climate change.
draft: false
#image: pic09.jpg
keywords: ""
slug: climateanalysis
title: Climate Change Analysis in R
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
---

Over the last five weeks, Lindsey has been in a "Data Analytics for Finance" course. This course focused on statistical analysis and visual output utilizing the R programming language. In this specific assignment, Lindsey looked at historical weather data to uncover trends and anomalies. This included making graphs that looked at changes in temperature by month and creating a density plot to understand how the average temperature has changed over the past few decades.



```{r, setup, include=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```

```{r load-libraries, include=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
library(infer)
windows(width = 12, height = 10)
```
# Climate change and temperature anomalies 


If we wanted to study climate change, we can find data on the *Combined Land-Surface Air and Sea-Surface Water Temperature Anomalies* in the Northern Hemisphere at [NASA's Goddard Institute for Space Studies](https://data.giss.nasa.gov/gistemp). The [tabular data of temperature anomalies can be found here](https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt)

To define temperature anomalies you need to have a reference, or base, period which NASA clearly states that it is the period between 1951-1980.

To load the file:

```{r weather_data, cache=TRUE}

weather <- 
  read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv", 
           skip = 1, 
           na = "***")

```


```{r tidyweather}

#select the year and the twelve month variables from dataset
tidyweather <- select(weather, "Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") %>% 
  pivot_longer(cols = 2:13, #select the monthly columns and convert the data frame from 'wide' to 'long' format
               names_to = "month", #name the new column "month"
               values_to = "delta") #name the new values under "delta"

glimpse(tidyweather)

```

## Plotting Information


```{r scatter_plot}

#create new variable
tidyweather <- tidyweather %>%
  mutate(date = ymd(paste(as.character(Year), month, "1")),
         month = month(date, label=TRUE),
         year = year(date))
#create time-series scatterplot
ggplot(tidyweather, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") + #add trendline
  theme_bw() +
  labs (
    title = "Weather Anomalies"
  )

```

Is the effect of increasing temperature more pronounced in some months? 

In December-March, the data sets are a bit more spread out and in the more recent years there appears to be a more pronounced and steep upward trend in the temperature. However, this upward trend is seen in all the months to some extent. More critical to observe, is the increase over the years where it can be observed that the increase in temperature is beginning to take on an exponential upward curve - scary! In the the prior graph, with all the data points together, there is a clear upward trend in increasing temperature, becoming especially prominent around 1975.

```{r facet_wrap, echo=FALSE}

#create scatterplots per month
ggplot(tidyweather, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") + #add trendline
  theme_bw() +
  labs (
    title = "Weather Anomalies"
  ) +
  facet_wrap(~month) #group by month

```


It is sometimes useful to group data into different time periods to study historical data. For example, we often refer to decades such as 1970s, 1980s, 1990s etc. to refer to a period of time. NASA calcuialtes a temperature anomaly, as difference form the base period of 1951-1980. The code below creates a new data frame called `comparison` that groups data in five time periods: 1881-1920, 1921-1950, 1951-1980, 1981-2010 and 2011-present.

```{r intervals}

comparison <- tidyweather %>% 
  filter(Year>= 1881) %>%     #remove years prior to 1881
  #create new variable 'interval', and assign values based on criteria below:
  mutate(interval = case_when(
    Year %in% c(1881:1920) ~ "1881-1920",
    Year %in% c(1921:1950) ~ "1921-1950",
    Year %in% c(1951:1980) ~ "1951-1980",
    Year %in% c(1981:2010) ~ "1981-2010",
    TRUE ~ "2011-present"
  ))

```
Now that we have the `interval` variable, we can create a density plot to study the distribution of monthly deviations (`delta`), grouped by the different time periods we are interested in. 

```{r density_plot}

ggplot(comparison, aes(x=delta, fill=interval))+
  geom_density(alpha=0.2) + #density plot with transparency set to 20%
  theme_bw() + #theme
  labs (
    title = "Density Plot for Monthly Temperature Anomalies",
    y     = "Density"         #changing y-axis label to sentence case
  )

```
This graph is a clear indicator that the change in temperature has been steadily increasing from the 1880s. This is seen through the movement of the curves to the right.


So far, we have been working with monthly anomalies. However, we might be interested in average annual anomalies. 

```{r averaging}

#creating yearly averages
average_annual_anomaly <- tidyweather %>% 
  group_by(Year) %>%   #grouping data by Year
  
  # creating summaries for mean delta 
  # use `na.rm=TRUE` to eliminate NA (not available) values 
  summarise(na.rm = TRUE, 
            annual_average_delta = mean(delta)) 

#plotting the data:
ggplot(average_annual_anomaly, aes(x=Year, y= annual_average_delta))+
  geom_point()+
  
  #Fit the best fit line, using LOESS method
  geom_smooth() +
  
  #change to theme_bw() to have white background + black frame around plot
  theme_bw() +
  labs (
    title = "Average Yearly Anomaly",
    y     = "Average Annual Delta"
  )                         

```
The yearly averages above also provide convincing evidence to show the rise in temperatures across time.