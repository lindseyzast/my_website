---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: Using R to look for statistical significance among movie ratings by director.
draft: false
image: pic10.jpg
keywords: ""
slug: aliquam
title: Statistical Analysis in R
---

Over the last five weeks, Lindsey has been in a "Data Analytics for Finance" course. This course focused on statistical analysis and visual output utilizing the R programming language. In this specific assignment, Lindsey recreated a graph showing the distribution of ratings for two directors, Tim Burton and Steven Spielberg, and then conducted a hypothesis test to see if the mean ratings of the directors were significantly different. 


# IMDB ratings: Differences between directors

Task: Explore whether the mean IMDB rating for Steven Spielberg and Tim Burton are the same or not. Run a hypothesis test using the `t.test` command and the `infer` package to simulate from a null distribution, where the assumed difference between the two is zero. 

> Before anything, write down the null and alternative hypotheses, as well as the resulting test statistic and the associated t-stat or p-value. At the end of the day, what do you conclude?

Null: Burton and Spielberg have the same mean rating

Alt: Burton and Spielberg have statistically different mean ratings

The resulting t-stat was equal to 3, larger than the critical value of 1.96. And the p-value was equal to 0.01, less than 0.05. Additionally, the resulting 95% confidence interval was 0.16, 1.13, which does not include zero. In conclusion, all of these statistics tell us to *reject* the null hypothesis, and that Burton and Spielberg have statistically different mean ratings.

```{r load-movies-data}
movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)
```

```{r}
#Create necessary data
burton_spielberg <- movies %>% 
  group_by(director) %>% 
  filter(director %in%  c("Tim Burton", "Steven Spielberg")) %>%  #filter out for only Burton and Spielberg
  summarise(mean_rating = mean(rating),
            sd_rating = sd(rating),
            count=n(),
            t_critical = qt(0.975, count-1),
            se_rating = sd_rating/sqrt(count),
            margin_of_error = t_critical*se_rating,
            lower = mean_rating-margin_of_error,
            higher = mean_rating+margin_of_error)

burton_spielberg


burton_spielberg %>% 
  mutate(
    director=fct_reorder(director, mean_rating)) %>% 

#Recreate graph
ggplot(burton_spielberg, aes(x=mean_rating, color=director)) +
  #add shaded region for where intervals overlap
  geom_rect(xmin=7.27, xmax=7.33, ymin=-Inf, ymax=Inf, color="grey", alpha=0.2) +
  #add confidence intervals onto the graph based on lower and upper bounds
  geom_errorbar(aes(xmin=lower, xmax=higher, y=director), size=2, width=0.2) + 
  #add points on errorbar to show means
  geom_point(aes(mean_rating, director), size = 4) + 
  #add labels for mean, lower, and upper points on errorbar
  geom_text(aes(x=mean_rating, y=director, label=round(mean_rating, digits=2), size=75), color="black", 
            position=position_dodge(width=0.9), vjust=-3) +
  geom_text(aes(x=lower, y=director, label=round(lower, digits=2)), color="black", 
            position=position_dodge(width=0.9), vjust=-3)+
  geom_text(aes(x=higher, y=director, label=round(higher, digits=2)), color="black",
            position=position_dodge(width=0.9), vjust=-3)+
  #add titles to graph
  labs(title="Do Spielberg and Burton have the same mean IMDB ratings?",
       subtitle="95% confidence intervals overlap",
       x="Mean IMDB Rating",
       y="") +
  #specify colors for each director
  scale_color_manual(values = c("#00AFBB", "#FFC0CB"))+
  #remove legend
  guides(color=FALSE, size=FALSE) +
  theme_minimal()


```
```{r, rating_hypothesistest}
#Create smaller data set 
small_data <- movies %>% 
  group_by(director) %>% 
  filter(director %in%  c("Tim Burton", "Steven Spielberg"))

set.seed(1234)
#conduct hypothesis test (infer method)
ratings_in_null_world <- small_data %>% 
  #specify variable of interest
  specify(rating ~ director) %>% 
  
  #hypothesize a null of zero difference
  hypothesize(null = "independence") %>% 
  
  #generate a bunch of simulated samples
  generate(reps=1000, type="permute") %>% 
  
  #find the mean difference of each sample
  calculate(stat = "diff in means", 
            order = c("Steven Spielberg", "Tim Burton"))

ratings_in_null_world %>% visualize()
ratings_in_null_world %>% 
  get_pvalue(obs_stat = 0.64, direction = "both")


#conduct hypothesis test (t.test method)
t.test(rating ~ director, data=small_data)


```