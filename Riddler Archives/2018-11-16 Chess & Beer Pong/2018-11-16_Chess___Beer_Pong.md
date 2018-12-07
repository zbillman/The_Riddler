2018-11-16 Chess & Beer Pong
================
zpb
December 7, 2018

The Chess riddle
================

The World Chess Championship is underway. It is a 12-game match between the world’s top two grandmasters. Many chess fans feel that 12 games is far too short for a biennial world championship match, allowing too much variance.

Say one of the players is better than his opponent to the degree that he wins 20 percent of all games, loses 15 percent of games and that 65 percent of games are drawn. Wins at this match are worth 1 point, draws a half-point for each player, and losses 0 points. In a 12-game match, the first player to 6.5 points wins.

What are the chances the better player wins a 12-game match? How many games would a match have to be in order to give the better player a 75 chance of winning the match outright? A 90 percent chance? A 99 percent chance?

Chess Monte Carlo
=================

My strategy
===========

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## √ ggplot2 3.1.0     √ purrr   0.2.5
    ## √ tibble  1.4.2     √ dplyr   0.7.8
    ## √ tidyr   0.8.2     √ stringr 1.3.1
    ## √ readr   1.2.1     √ forcats 0.3.0

    ## -- Conflicts --------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
#This gives me matches with each game spelled out
#Adding a distinct() for match gives final score of each match
results <- crossing(match = 1:1e6,
         game = 1:12) %>%
  group_by(match) %>%
  mutate(game_result = sample(c(1, 0, .5),
                              size = n(),
                              prob = c(.2, .15, .65),
                              replace = TRUE),
         score = sum(game_result)) %>%
  select(match, score) %>%
  distinct(match, .keep_all = TRUE)

#graph using the results df
results %>%
  ggplot(aes(score)) +
  geom_histogram(binwidth = 0.25) +
  geom_vline(color = "red", xintercept = 6.25) +
  scale_x_continuous(breaks = seq(0, 12, 0.5))
```

![](2018-11-16_Chess___Beer_Pong_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#This gives me a weird output without the ungroup!
#It was making the average of each trial without it
results %>%
  ungroup() %>%
  summarize(mean(score >= 6.5))
```

    ## # A tibble: 1 x 1
    ##   `mean(score >= 6.5)`
    ##                  <dbl>
    ## 1                0.521

Chess monte carlo
=================

TT strategy
===========

``` r
library(tidyverse)

#Using summarize() to get the scores of each match
scores <- crossing(match = 1:1e6,
         game = 1:12) %>%
  group_by(match) %>%
  mutate(game_result = sample(c(1, 0, .5),
                              size = n(),
                              prob = c(.2, .15, .65),
                              replace = TRUE)) %>%
  summarize(score = sum(game_result))

#graph using the scores df
scores %>%
  ggplot(aes(score)) +
  geom_histogram(binwidth = 0.25) +
  geom_vline(color = "red", xintercept = 6.25) +
  scale_x_continuous(breaks = seq(0, 12, 0.5))
```

![](2018-11-16_Chess___Beer_Pong_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
#This gives me the expected output
scores %>%
  summarize(mean(score >= 6.5))
```

    ## # A tibble: 1 x 1
    ##   `mean(score >= 6.5)`
    ##                  <dbl>
    ## 1                0.519
