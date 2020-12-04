library(tidyverse)

input <- read_file('~/Desktop/advent-of-code-20/day-4/input.txt')

# how many passwords valid (missing cid field only)?
input %>%
    str_split('\n\n') %>%
    magrittr::extract2(1) %>%
    as_tibble() %>%
    mutate_at(vars(value), ~ str_replace_all(., '\n', ' ')) %>%
    mutate(num.fields = str_count(value, ':')) %>%
    mutate(cid = str_detect(value, 'cid')) %>%
    mutate(valid = case_when(num.fields==7 & !cid ~ TRUE,
                             num.fields==7 & cid ~ FALSE,
                             num.fields==8 ~ TRUE)) %>%
    filter(valid) %>%
    nrow()
