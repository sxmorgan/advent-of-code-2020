library(tidyverse)

input <- read_delim('~/Desktop/advent-of-code-20/day-2/input.txt', 
                    delim = ' ', col_names = FALSE)

# how many passwords are valid?
input %>%
    separate(X1, c('min','max'), sep = '[-]') %>%
    mutate_at(vars(min, max), ~ as.numeric(.)) %>%
    mutate(X2 = str_remove_all(X2,'[:]')) %>%
    mutate(count = str_count(X3, X2)) %>%
    mutate(valid = (count >= min) & (count <= max)) %>%
    filter(valid) %>%
    magrittr::use_series(X3) %>%
    length()

# new policies, how many now?
input %>%
    separate(X1, c('pos1','pos2'), sep = '[-]') %>%
    mutate_at(vars(pos1, pos2), ~ as.integer(.)) %>%
    mutate(X2 = str_remove_all(X2,'[:]')) %>%
    mutate(locs = str_locate_all(X3, X2) %>% 
               map(., as_tibble)) %>%
    unnest(locs) %>%
    select(-end) %>%
    mutate(potential.match = (start == pos1) | (start == pos2)) %>%
    filter(potential.match) %>%
    nest(data = -X3) %>% 
    mutate(valid = map_lgl(data, ~ nrow(.) == 1)) %>% 
    filter(valid) %>%
    magrittr::use_series(X3) %>%
    length()
