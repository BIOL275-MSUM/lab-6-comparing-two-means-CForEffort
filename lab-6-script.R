
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")


# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

# do stuff ----------------------------------------------------------------

fish_ttest <- t.test(species ~ location, data = fish_long)
fish_ttest$estimate
fish_ttest

## Graph both

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

## Read crab data

crab <- read_csv("chap15q27FiddlerCrabFans.csv")
crab

## Graph the crab data

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


## Anova for crabs

ANOVA <- aov(bodyTemperature ~ crabType, data = crab)
ANOVA
sumAN <- summary(ANOVA)
sumAN