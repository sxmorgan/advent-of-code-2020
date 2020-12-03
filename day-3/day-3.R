library(tidyverse)

input <- read_lines('~/Desktop/advent-of-code-20/day-3/input.txt') 

unique(map_int(input, str_length)) # 31 elements per row
length(input) # 323 lines long

# how many trees will be hit?
input %>%
    map(~ str_dup(., length(input))) %>%
    imap_chr(function(.x, .y) { 
        substr(.x, 3*.y-2, 3*.y-2) }) %>%
    str_flatten() %>%
    str_count('#')

