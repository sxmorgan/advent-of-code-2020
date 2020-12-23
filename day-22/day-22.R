library(tidyverse)
library(magrittr)

setwd('~/Desktop/advent-of-code-20/')
library(here)

input <- read_file(here('day-12','input.txt')) %>%
    str_split('[:]') %>%
    pluck(1) %>%
    extract(2:3) %>%
    str_remove_all(fixed('Player 2')) %>%
    map(~ trimws(.)) %>%
    str_split('\n') %>%
    map(~ as.integer(.))


### part 1
play <- function(a, b) {
    
    winner <- play.round(a[1], b[1])
    
    if (winner == 1) {
        a <- c(a[-1], a[1], b[1])
        b <- b[-1] }
    else if (winner == 2) {
        b <- c(b[-1], b[1], a[1]) 
        a <- a[-1] }
    else return('tie occured ?')
    
    if (length(a) == 0) {
        print('player 2 wins')
        return(score(b)) }
    else if (length(b) == 0) {
        print('player 1 wins')
        return(score(a)) }
    else play(a, b)
}

play.round <- function(a, b) {
    if (a > b) 1
    else if (b > a) 2
    else 0
}

score <- function(v) {
    idx <- seq(length(v), 1, -1)
    sum(v*idx)
}

play(input[[1]], input[[2]])
