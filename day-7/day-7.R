library(tidyverse)
library(magrittr)
library(here)

input <- here('day-7','input.txt') %>%
    read_delim(delim = '.\n', col_names = FALSE) %>%
    select(-X2) %>%
    mutate(X1 = str_split(X1, fixed('contain'))) %>%
    mutate(X1 = map(X1, ~ tibble('outer.bag' = .[1], 
                                 'contents' = .[2]))) %>%
    unnest(X1) %>%
    mutate_all(~ trimws(., 'both'))

input.test <- slice(input, 1:100)

# part 1
find.bags <- function(df, pattern) {
    
    # browser()
    print(paste0('# current options ', length(bags.glob)))
    print(paste0(nrow(df), ' rows; recursively looking for ', pattern))
    
    # holds the bag type (pattern) we're looking for
    possible.bags <- df %>%
        mutate(can.hold = map_lgl(contents, 
                                  ~ str_detect(., fixed(pattern)))) %>%
        filter(can.hold) %>%
        pull(outer.bag)
    print(paste0(length(possible.bags), ' bags can hold ', pattern))
    
    # base case; no bags contain pattern (bag type) we're looking for
    # and we know current bag type can already hold our bag (pattern %in% bags.glob)
    if (length(possible.bags) == 0) {
        print(paste0('BASE CASE; no other bags contain ', pattern, '; current options:'))
        if ((pattern != 'shiny gold bag') & (!(pattern %in% bags.glob))) {
            bags.glob <<- c(bags.glob, pattern)
            print(paste0('added ', pattern, ' to current options')) }
        print(paste(bags.glob, sep = ' ')) }
    
    # recursive case
    else { 
        # doesn't appear to be doing anything.
        df %<>%
            filter(!(outer.bag %in% possible.bags))
        print(paste(possible.bags, sep = ' '))
        for (i in seq_along(possible.bags)) {
            if (!(possible.bags[i] %in% bags.glob)) {
                bags.glob <<- c(bags.glob, possible.bags[i])
                print(paste0('added ', possible.bags[i], ' to current options')) }
            find.bags(df, possible.bags[i]) } }
}

bags.glob <<- vector()
find.bags(input, 'shiny gold bag')
length(unique(bags.glob))

# 114 too low, 115 too...
# 60, 68, 78, 56, 192 are also wrong FUCK IT HAS TO BE 114