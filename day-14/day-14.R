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

read.program <- function(X1, X2, memory) {
    
    for (i in seq_len(nrow(input))) {
        if (X1[i] == 'mask') {
            mask <<- strsplit(X2[i], split = '')[[1]]
            next }
        else {
            address <- as.character(X1[i])
            masked.bit <- X2[i] %>%
                to.binary() %>%
                as.character() %>%
                apply.mask(mask) %>%
                to.integer()
            memory[[address]] <- masked.bit } }
    
    return(memory)

}

to.binary <- function(num) {

    # num %>%
    #     intToBits() %>%
    #     as.character() %>%
    #     map_chr(~ str_sub(., 2, 2)) %>%
    #     c(rep(0, 36-length(.))) %>%
    #     rev()
    
    num %>%
        as.numeric() %>%
        as.binary() %>%
        c(rep(0, 36-length(.)), .)
        
}

apply.mask <- function(bit, mask) {
    
    map2_dbl(bit, 
             mask, ~ switch(.y,
                            '0' = 0,
                            '1' = 1,
                            'X' = as.numeric(.x))) 
}

to.integer <- function(bit) {

    pows <- seq(length(bit)-1, 0, -1)
    bitsums <- map2_dbl(bit, pows, ~ .x * (2^.y))
    return(sum(bitsums))
}

# 6513444000000 too high
mask <<- character()
memory <- read.program(input$X1, input$X2, list())
reduce(memory, sum)
