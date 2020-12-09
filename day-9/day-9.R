library(tidyverse)
library(magrittr)

input <- '~/Desktop/advent-of-code-20/day-9/input.txt' %>%
    read_delim(delim = '\n', col_names = FALSE)
input <- '~/Desktop/advent-of-code-20/day-9/test-input.txt' %>%
    read_delim(delim = '\n', col_names = FALSE)

# part 1
for (i in seq_len(nrow(input))) {

    target <- input$X1[i]

    if (i > 25) {
        tmp <- input %>%
            slice((i-25):(i-1)) %>%
            mutate(X2 = X1) %>%
            expand(X1,X2) %>% 
            filter(X1 != X2) %>% 
            mutate(valid = ifelse(X1+X2 == target, TRUE, FALSE)) %>%
            pull(valid) %>%
            any() 
        
        if (!tmp) print(target) }
}

# part 2
target <- 21806024
chunk.sums <- function(input, chunk.size) {
    
    out <- NULL
    for (i in seq_len(nrow(input))) {
        if (i > chunk.size) {
            tmp <- input %>%
                slice((i-chunk.size+1):i) %>%
                pull(X1) %>%
                sum() %>%
                equals(target)
            if (tmp) {
                print('found!')
                out <- slice(input, (i-chunk.size+1):i) }
        }
    }
    return(out)
}

range <- seq(2,25,1)
library(furrr)
plan(multisession)
contiguous <- range %>% 
    future_map(~ chunk.sums(input, .)) %>%
    keep(~ !is.null(.)) 

stats <- contiguous %>%
    extract2(1) %>%
    pull(X1) %>%
    summary() 

stats[1]+stats[6]
