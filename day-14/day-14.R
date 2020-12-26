library(tidyverse)
library(magrittr)
library(rlist)

setwd('~/Desktop/advent-of-code-20/')
library(here)

input <- read_delim(here('day-14','input.txt'),
                    delim = '=', col_names = FALSE) %>%
    mutate_all(trimws) %>% 
    mutate_at(vars(X1), ~ str_remove_all(., 'mem')) %>%
    mutate_at(vars(X1), ~ str_remove_all(., fixed('['))) %>%
    mutate_at(vars(X1), ~ str_remove_all(., fixed(']')))

mask <<- character()
execution <- input$X1 %>%
    map2(input$X2, ~ read.program(.x, .y)) %>%
    discard(is.null) %>% 
    set_names(map_chr(., ~ extract(., 1))) %>%
    discard(duplicated(names(.), fromLast = TRUE)) %>%
    map(~ .[-1]) 
execution %>%
    map(~ as.double(.)) %>%
    #map(~ to.integer(.)) %>%
    reduce(sum) 
# 6513444000000 too high


read.program <- function(x, y) {
    
    if (x == 'mask') {
        mask <<- strsplit(y, split = '')[[1]]
        return(NULL) }
    
    else {
        # browser()
        y %>%
            to.binary() %>%
            as.character() %>%
            apply.mask(mask) %>%
            to.integer() %>%
            c(x, .) }
}

to.binary <- function(num) {
    
    num %>%
        intToBits() %>%
        as.character() %>%
        map_chr(~ str_sub(., 2, 2)) %>%
        c(rep(0, 36-length(.))) %>%
        rev()
}

apply.mask <- function(bit, mask) {
    
    map2_dbl(bit, 
             mask, ~ switch(.y,
                            '0' = 0,
                            '1' = 1,
                            'X' = as.integer(.x))) 
}

to.integer <- function(bit) {
    
    pows <- seq(35, 0, -1)
    bitsums <- map2_dbl(bit, pows, ~ .x * (2^.y))
    return(sum(bitsums))
}

