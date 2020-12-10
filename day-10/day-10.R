library(tidyverse)
library(magrittr)

input <- '~/Desktop/advent-of-code-20/day-10/input.txt' %>%
    read_delim('\n', col_names = FALSE)
input <- '~/Desktop/advent-of-code-20/day-10/test-input.txt' %>%
    read_delim('\n', col_names = FALSE)
input <- '~/Desktop/advent-of-code-20/day-10/test-input-2.txt' %>%
    read_delim('\n', col_names = FALSE)

# part 1
build.adapter <- function(v, start, stop, current) {
    
    opt.idx <- which(v %in% seq(current+start, current+stop, 1))
    
    # base case - no options
    if (length(opt.idx) == 0) return(store)
        
    # recursive case 1 - multiple options, take first
    else if (length(opt.idx) > 1) {
        val <- min(v[opt.idx])
        opt.idx <- which(v == val)
        store <<- c(store, v[opt.idx])
        build.adapter(v[-opt.idx], start, stop, v[opt.idx]) }
    
    # recursive case 2 - one option
    else {
        store <<- c(store, v[opt.idx])
        build.adapter(v[-opt.idx], start, stop, v[opt.idx]) }
}

calc.diff <- function(v, steps) {
    v %<>%
        as_tibble() %>%
        mutate(lag = lag(value)) %>%
        filter(!is.na(lag)) %>%
        mutate(diff = value - lag) %>%
        group_by(diff) %>%
        count() %>%
        filter(diff %in% steps) %>%
        pull(n)
}

store <<- numeric()
adapter.order <- input %>%
    pull(X1) %>%
    build.adapter(1, 3, 0) %>%
    c(max(input$X1)+3) %>%
    c(0, .)
diff.dist <- adapter.order %>%
    calc.diff(c(1,3))
diff.dist[1]*diff.dist[2]