library(tidyverse)
library(magrittr)

input <- '~/Desktop/advent-of-code-20/day-6/input.txt' %>%
    readLines() %>% 
    paste(collapse = "\n") %>% 
    strsplit("\n\n") %>% 
    .[[1]] %>%
    gsub("\n", " ", .) %>%
    as.list()

# part 1 - any
test <- list('abc','a b c','ab ac','a a a a','b')

unique.answers.any <- function(str) {
    letters %>%
        map_dbl(~ str_count(str, .)) %>%
        map_dbl(~ ifelse(. > 0, 1, 0)) %>%
        sum()
}

input %>%
    map_dbl(~ unique.answers.any(.)) %>%
    sum()

# part 2 - all; 3579
unique.answers.all <- function(str) {
    n.ppl <- str %>%
        str_count(' ') %>%
        add(1)
    letters %>%
        map_dbl(~ str_count(str, .)) %>%
        map_lgl(~ . == n.ppl) %>%
        sum()
}

input %>% 
    map_dbl(~ unique.answers.all(.)) %>%
    sum()
