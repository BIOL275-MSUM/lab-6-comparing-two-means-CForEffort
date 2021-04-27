Lab 6 Comparing two means
================
Faith Rude

Researchers studying the number of electric fish species living in
various parts of the Amazon basin were interested in whether the
presence of tributaries affected the local number of electric fish
species in the main rivers (Fernandes et al. 2004).

They counted the number of electric fish species above and below the
entrance point of a major tributary at 12 different river locations.

The data is provided in your GitHub repository.

For each question below, write a sentence answering the question and
show the code you used to come up with the answer, if applicable.

## Read in the data and load packages

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
fish <- read_csv("chap12q19ElectricFish.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   tributary = col_character(),
    ##   speciesUpstream = col_double(),
    ##   speciesDownstream = col_double()
    ## )

## Question A

> Test the hypothesis that the tributaries have no effect on the number
> of species of electric fish.

``` r
fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()
```

    ## # A tibble: 24 x 3
    ##    tributary location   species
    ##    <chr>     <chr>        <dbl>
    ##  1 Içá       Upstream        14
    ##  2 Içá       Downstream      19
    ##  3 Jutaí     Upstream        11
    ##  4 Jutaí     Downstream      18
    ##  5 Japurá    Upstream         8
    ##  6 Japurá    Downstream       8
    ##  7 Coari     Upstream         5
    ##  8 Coari     Downstream       7
    ##  9 Purus     Upstream        10
    ## 10 Purus     Downstream      16
    ## # ... with 14 more rows

``` r
fish_ttest <- t.test(species ~ location, data = fish_long)

fish_ttest
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  species by location
    ## t = 0.59249, df = 21.81, p-value = 0.5596
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.587031  8.253697
    ## sample estimates:
    ## mean in group Downstream   mean in group Upstream 
    ##                 16.41667                 14.58333

To do this, the t-test data can be analyzed, looking specifically at the
p-value. The p-value was 0.5596. This is larger than the alpha value of
0.05 meaning you can not reject the null hypothesis. Thus, the
tributaries have no significant effect on the number of species of
electric fish.

## Question B

> What is the difference in the mean numbers of species between areas
> upstream and downstream of a tributary? What is the 95% confidence
> interval of this difference in means?

To find the upper and lower mean values, the previously run t-test data
was used. Then the upper and lower mean values could be subtracted to
find the mean difference. The resulting difference is 1.83334 species.

The 95% confidence interval of the mean difference is also listed in the
t-test write up and is -4.587031 to 8.253697.

## Question C

> State the assumptions that you had to make to complete parts (A) and
> (B). Create a graph to assess whether one of those assumptions was
> met.

The assumptions for both part A and B require the two sample populations
(up and downstream) to have a normal distribution and the individuals
being selected are done so randomly.

To test this, a histogram was created to display the distributions of
the species and determine if the distributions were normal.
Unfortunately in the case of this study, neither population appears to
have a normal distribution. However, this could be attributed to a low
sample size. Given this consideration, the data is likely to have a
normal distribution if a larger sample was taken.

``` r
fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 8, 
    alpha = 0.5, 
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Question D

> Graph the distribution of body temperatures for each crab type.

First read in the data for the crabs.

``` r
crab <- read_csv("chap15q27FiddlerCrabFans.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   crabType = col_character(),
    ##   bodyTemperature = col_double()
    ## )

``` r
crab
```

    ## # A tibble: 85 x 2
    ##    crabType bodyTemperature
    ##    <chr>              <dbl>
    ##  1 female               1.9
    ##  2 female               1.6
    ##  3 female               1.4
    ##  4 female               1.1
    ##  5 female               1.6
    ##  6 female               1.8
    ##  7 female               1.9
    ##  8 female               1.7
    ##  9 female               1.5
    ## 10 female               1.8
    ## # ... with 75 more rows

Then the graph can be created for the different types of crabs.

``` r
crab %>% 
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType), 
    bins = 8, 
    alpha = 0.5, 
    position = "identity"
  ) +
 labs(x = "Body Temperature", y = "Species Count") +
  scale_fill_manual(values = c("darkorange","cyan4", "red", "blue")) +
  theme_minimal()
```

    ## Warning: Removed 1 rows containing non-finite values (stat_bin).

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Question E

> Does body temperature varies among crab types? State the null and
> alternative hypothesis, conduct and ANOVA, and interpret the results.

In the case of this study, the null hypothesis is that there is no
difference in body temperature based on type of crab. The alternate
hypothesis is that the body temperatures would vary based on the type of
crab being observed. To test this, an anova can be run.

``` r
ANOVA <- aov(bodyTemperature ~ crabType, data = crab)
ANOVA
```

    ## Call:
    ##    aov(formula = bodyTemperature ~ crabType, data = crab)
    ## 
    ## Terms:
    ##                 crabType Residuals
    ## Sum of Squares  2.641310  3.467619
    ## Deg. of Freedom        3        80
    ## 
    ## Residual standard error: 0.2081952
    ## Estimated effects may be unbalanced
    ## 1 observation deleted due to missingness

``` r
sumAN <- summary(ANOVA)
sumAN
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)    
    ## crabType     3  2.641  0.8804   20.31  7e-10 ***
    ## Residuals   80  3.468  0.0433                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 1 observation deleted due to missingness

From the Anova, the sum of squares that we get are 2.64 from crab type
and 3.47 for residuals. There are 3 degrees of freedom in crab type and
80 degrees of freedom in the residual. The SE of the residual is 0.21.
The mean squared value is 0.88 for the crabType and 0.04 for the
residuals. The F value is 20.31 and the Pr(&gt;F) value is 7e-10 \*\*\*.

Based on the ANOVA you can reject the null hypothesis due to P being so
small that regardless of the alpha value you choose your p value (7e-10)
will be smaller.
