library(tidyverse)
library(magrittr)
library(here)

clean.input <- function(filename) {
    filename %>%
        read_delim(delim = '.\n', col_names = FALSE) %>%
        select(-X2) %>%
        mutate(X1 = str_split(X1, fixed('contain'))) %>%
        mutate(X1 = map(X1, ~ tibble('outer.bag' = .[1], 
                                     'contents' = .[2]))) %>%
        unnest(X1) %>%
        mutate_at(vars(outer.bag), ~ str_remove_all(., 'bags|bag')) %>%
        mutate_all(~ trimws(., 'both')) 
}

input <- clean.input('~/Desktop/advent-of-code-20/day-7/input.txt')
input <- clean.input('~/Desktop/advent-of-code-20/day-7/test-input.txt')
input <- clean.input('~/Desktop/advent-of-code-20/day-7/test-input-2.txt')

### part 1
find.bags <- function(df, pattern) {
    
    # browser()
    # print(paste0('# current options ', length(bags.glob)))
    # print(paste0(nrow(df), ' rows; recursively looking for ', pattern))
    
    ## holds the bag type (pattern) we're looking for
    possible.bags <- df %>%
        mutate(can.hold = map_lgl(contents, 
                                  ~ str_detect(., fixed(pattern)))) %>%
        filter(can.hold) %>%
        pull(outer.bag)
    # print(paste0(length(possible.bags), ' bags can hold ', pattern))
    
    ## base case; no bags contain pattern (bag type) we're looking for
    ## and we know current bag type can already hold our bag (pattern %in% bags.glob)
    if (length(possible.bags) == 0) {
        # print(paste0('BASE CASE; no other bags contain ', pattern))
        if ((pattern != 'shiny gold bag') & (!(pattern %in% bags.glob))) {
            bags.glob <<- c(bags.glob, pattern)
            # print(paste0('added ', pattern, ' to current options')) }
            # print(paste(bags.glob, sep = ' ')) } 
        }}
    
    ## recursive case
    else { 
        # doesn't appear to be doing anything.
        # df %<>%
        #     filter(!(outer.bag %in% bags.glob) & !(outer.bag %in% possible.bags))
        # print(paste(possible.bags, sep = ' '))
        for (i in seq_along(possible.bags)) {
            if (!(possible.bags[i] %in% bags.glob)) {
                bags.glob <<- c(bags.glob, possible.bags[i])
                # print(paste0('added ', possible.bags[i], ' to current options'))
            }
            find.bags(df, possible.bags[i]) } }
}

bags.glob <<- vector()
find.bags(input, 'shiny gold')
length(unique(bags.glob))

## part 2
clean.differently <- function(filename) {
    
    filename %>%
        read_delim(delim = '.\n', col_names = FALSE) %>%
        select(-X2) %>%
        mutate(X1 = str_split(X1, fixed('contain'))) %>%
        mutate(X1 = map(X1, ~ tibble('bag' = .[1], 
                                     'contents' = .[2]))) %>%
        unnest(X1) %>%
        mutate_at(vars(contents), ~ str_split(., ', ')) %>%
        unnest_longer(contents) %>%
        mutate(quantity = parse_number(contents, na = "no other")) %>%
        mutate_at(vars(contents), ~ str_remove_all(., '[0-9]')) %>%
        mutate_if(is.character, ~ str_remove_all(., 'bags|bag')) %>%
        mutate_if(is.character, ~ trimws(., 'both')) 
}

input <- clean.differently('~/Desktop/advent-of-code-20/day-7/input.txt')
input <- clean.differently('~/Desktop/advent-of-code-20/day-7/test-input.txt')
input <- clean.differently('~/Desktop/advent-of-code-20/day-7/test-input-2.txt')

get.contents <- function(input, bag.type) {
    
    curr.contents <- filter(input, bag == bag.type)
    
    # base case
    if (is.na(curr.contents$quantity)) return(tibble())
    
    curr.contents %<>%
        mutate(contents = map2(contents, quantity, 
                               ~ rep_len(.x, .y))) %>%
        unnest_longer(contents) 
    
    # recursive case
    more.contents <- map_dfr(curr.contents$contents,
                             ~ get.contents(input, .))
    
    bind_rows(curr.contents, more.contents)
    
}

out <- get.contents(input, 'shiny gold')

out %>%
    nest(data = -bag) %>%
    mutate(to.sum = map_dbl(data, ~ nrow(.))) %>%
    summarize_if(is.double, sum) %>%
    pull(to.sum)

