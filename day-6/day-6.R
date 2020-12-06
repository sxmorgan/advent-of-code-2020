library(tidyverse)

input <- '~/Desktop/advent-of-code-20/day-6/input.txt' %>%
    readLines() %>% 
    paste(collapse = "\n") %>% 
    strsplit("\n\n") %>% 
    .[[1]] %>%
    gsub("\n", " ", .) %>%
    as.list()

# part 1
test <- list('abc','a b c','ab ac','a a a a')
str <- 'ab ac'

unique.answers <- function(str) {
    letters %>%
        map_dbl(~ str_count(str, .)) %>%
        map_dbl(~ ifelse(. > 0, 1, 0)) %>%
        sum()
}

input %>%
    map_dbl(~ unique.answers(.)) %>%
    sum()
