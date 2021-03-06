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
    pull(X3) %>%
    length()

# jakob's, for fun :D
library(here)
input <- read_delim(here('2020', 'input', '2020-12-02_input.txt'), 
                    delim=' ', col_names = FALSE) %>% 
    transmute(word=X3, numbers=X1, letter=X2) %>% 
    mutate(letter=str_remove(letter, ':')) %>% 
    separate(numbers, into = c('min', 'max'), sep = '-') %>% 
    mutate(min=as.numeric(min), max=as.numeric(max))

input <- read_delim('~/Desktop/advent-of-code-20/day-2/input.txt', 
                    delim = ' ', col_names = FALSE) %>%
    transmute(word=X3, numbers=X1, letter=X2) %>% 
    mutate(letter=str_remove(letter, ':')) %>% 
    separate(numbers, into = c('min', 'max'), sep = '-') %>% 
    mutate(min=as.numeric(min), max=as.numeric(max))

input %>%
    mutate(f=str_sub(word, min, min)) %>% 
    mutate(s=str_sub(word, max, max)) %>% 
    mutate(valid=case_when(f==letter & s==letter ~ FALSE,
                           f!=letter & s!=letter ~ FALSE,
                           f==letter & s!=letter ~ TRUE,
                           f!=letter & s==letter ~ TRUE)) %>% 
    pull(valid) %>% 
    sum