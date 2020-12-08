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

  # steps been completed already
  if (df$exec[idx]) {
    print('infinite loop condition reached')
    print(paste0('acc.sum = ', acc.sum))
    return() }
  
  # end of program reached
  else if (idx == nrow(df)) {
    print('end of program reached')
    if (df$command[idx] == 'acc') { acc.sum <<- acc.sum + df$inc[idx] }
    print(paste0('acc.sum = ', acc.sum))
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

## part 2
alter.commands <- function(df) {
  nops <- df %>%
    pull(command) %>%
    equals('nop')
  jmps <- df %>%
    pull(command) %>%
    equals('jmp')
  df %<>%
    add_column(nops, jmps)
  
  possible.fixes <- list()
  
  for (i in which(df$nops)) {
    tmp <- df
    tmp$command[i] <- 'jmp'
    possible.fixes <- c(possible.fixes, list(tmp)) }
  
  for (i in which(df$jmps)) {
    tmp <- df
    tmp$command[i] <- 'nop'
    possible.fixes <- c(possible.fixes, list(tmp)) }
  
  return(possible.fixes)
}

options <- input %>%
  alter.commands() %>%
  map(~ select(., 1:3))

# check all the changes and see which one terminates
for (i in seq_along(options)) {
  acc.sum <<- 0
  out <- complete.step(options[[i]], 1) }
