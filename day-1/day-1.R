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

# which 3 numbers add up to 2020?
nums <- input %>% 
  as_tibble() %>% 
  mutate(V2 = V1) %>% 
  mutate(V3 = V1) %>% 
  expand(V1, V2, V3) %>%
  filter(V1+V2+V3 == 2020) %>%
  magrittr::use_series(V1) %>% 
  unique()

# answer
nums[1]*nums[2]*nums[3]
