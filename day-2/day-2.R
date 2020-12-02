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