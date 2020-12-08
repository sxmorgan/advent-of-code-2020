library(tidyverse)
library(magrittr)

clean.input <- function(filename) {
  filename %>%
    read_lines() %>%
    str_split(' ') %>%
    map_dfr(~ tibble(command = .[1], inc = .[2]), .id = NULL) 
}

# part 1
complete.step <- function(df, idx) {

  if (df$exec[idx]) {
    browser()
    return(acc.sum) }
  
  else {
    df$exec[idx] <- TRUE
    switch(df$command[idx],
           'nop' = {complete.step(df, idx + 1)},
           'acc' = {
             acc.sum <<- acc.sum + df$inc[idx]
             complete.step(df, idx + 1)},
           'jmp' = {complete.step(df, idx + df$inc[idx])} )}
}

input <- clean.input('~/Desktop/advent-of-code-20/day-8/input.txt')
input <- clean.input('~/Desktop/advent-of-code-20/day-8/test-input.txt')

input %<>% 
  add_column(exec = FALSE) %>%
  mutate(inc = as.numeric(inc))

acc.sum <<- 0
complete.step(input, 1)
