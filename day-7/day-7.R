library(tidyverse)
library(here)

input <- here('day-7','input.txt') %>%
    read_delim(delim = '.\n', col_names = FALSE) %>%
    select(-X2) %>%
    mutate(X1 = str_split(X1, fixed('contain'))) %>%
    mutate(X1 = map(X1, ~ tibble('outer.bag' = .[1], 
                                 'contents' = .[2]))) %>%
    unnest(X1) %>%
    mutate_all(~ trimws(., 'both'))

# part 1
find.bags <- function(input) {
    
    directly <- input %>%
        mutate(can.hold = map_lgl(contents, 
                                  ~ str_detect(., fixed('shiny gold bag')))) %>%
        filter(can.hold) %>%
        pull(outer.bag)
    indirectly <- input %>%
        mutate(can.hold = map_int(contents, 
                                  ~ str_count(., fixed(directly)) %>% 
                                      sum)) %>%
        filter(can.hold > 0) %>%
        pull(outer.bag)
    union(directly, indirectly)
}

input %>%
    find.bags() %>%
    length # not 24
