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
test.input <- clean.input('~/Desktop/advent-of-code-20/day-7/test-input.txt')
test.input <- clean.input('~/Desktop/advent-of-code-20/day-7/test-input-2.txt')

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
empty.bags <- test.input %>%
    filter(str_detect(contents, fixed('no other bags'))) %>%
    pull(outer.bag)

get.contents <- function(input, bag.type, empty.bags) {

    if (bag.type %in% empty.bags) return(tibble(bag = bag.type,
                                                reps = 0,
                                                sum = 0))
    else {
        reps <- input %>%
            filter(outer.bag == bag.type) %>%
            pull(contents) %>%
            str_split(', ') %>%
            extract2(1) %>%
            str_extract_all('[0-9]') %>%
            flatten() %>%
            as.numeric()
        names <- input %>%
            filter(outer.bag == bag.type) %>%
            pull(contents) %>%
            str_split(', ') %>%
            extract2(1) %>%
            str_remove_all('[0-9]') %>%
            str_remove_all('bags|bag') %>%
            trimws('both')
        return(tibble(bag = names,
                      reps = reps,
                      sum = 0)) }
}

sum.contents <- function(input, contents, empty.bags) {
    
    browser()
    bag.type <- contents %>%
        str_remove_all(' bags| bag') %>%
        str_remove_all('[0-9] ')
    num.bags <- as.numeric(str_extract(contents, '[0-9]'))
    
    # base case
    if (bag.type %in% empty.bags) {
        #browser()
        print(paste0('BASE CASE: ', bag.type, ' contains no more bags'))
        return(num.bags)
    }
    # recursive case
    else {
        contents <- get.contents(input, bag.type, empty.bags)
    }
}

top.level <- test.input %>%
    get.contents('shiny gold', empty.bags) 
contents <- test.input %>%
    filter(outer.bag %in% top.level$bag)
start <- top.level %>%
    left_join(contents, by = c('bag' = 'outer.bag')) %>%
    mutate(contents = map(contents, ~ str_split(., ', ')[[1]])) %>%
    mutate(contents = map(contents, ~ str_remove_all(., 'bags |bag '))) %>% 
    unnest(contents)
start %>%
    mutate(content.total = map_dbl(contents, 
                                   ~ sum.contents(test.input, ., empty.bags))) %>%
    group_by(bag) %>%
    mutate(sum = sum(content.total))


###

top.level <<- character()
sum.contents <- function(input, bag.type, multiplier, empty.bags) {
    
    browser()
    
    # base case: this bag doesn't hold any bags
    if (bag.type %in% empty.bags) {
        #browser()
        print(paste0('BASE CASE: ', bag.type, ' contains no more bags'))
        bag.count <- my.bags %>%
            filter(bag == bag.type) %>%
            pull(reps)
        my.bags <<- my.bags %>%
            filter(bag != bag.type) %>%
            mutate(sum = sum + bag.count)
    }
    
    # recursive case: this bag holds more bags
    else {
        contents <- get.contents(input, bag.type, empty.bags)
        if (bag.type == 'shiny gold') {
            top.level <<- contents %>%
                pull(bag) %>%
                c(top.level) }
        print(paste0(bag.type, ' contains:'))
        print(contents$bag)
        #my.bags <<- bind_rows(my.bags, contents)
        for (i in seq_len(nrow(contents))) {
            my.bags <<- contents %>%
                slice(i) %>%
                bind_rows(my.bags)
            sum.contents(input, contents$bag[i], contents$reps[i], empty.bags)
        }}

}

# shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
empty.bags <- test.input %>%
    filter(str_detect(contents, fixed('no other bags'))) %>%
    pull(outer.bag)
my.bags <<- tibble()
sum.contents(test.input, 'shiny gold', 0, empty.bags)
