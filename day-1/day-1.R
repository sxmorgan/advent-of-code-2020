library(tidyverse)

input <- read.delim('~/Desktop/advent-of-code-20/day-1/input.txt', 
                    header = FALSE, sep = '\n')

# which 2 numbers add up to 2020?
nums <- input %>%
  as_tibble() %>%
  mutate(diff = 2020-V1) %>%
  mutate(presence = (diff %in% V1)) %>%
  filter(presence) %>%
  magrittr::use_series(V1)

# answer
nums[1]*nums[2]