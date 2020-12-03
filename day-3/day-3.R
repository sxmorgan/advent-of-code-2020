library(tidyverse)

input <- read_lines('~/Desktop/advent-of-code-20/day-3/input.txt') 

unique(map_int(input, str_length)) # 31 elements per row
length(input) # 323 lines long
full.map <- map(input, ~ str_dup(., length(input)))

# how many trees will be hit?
full.map %>%
    imap_chr(function(.x, .y) { 
        substr(.x, 3*.y-2, 3*.y-2) }) %>%
    str_flatten() %>%
    str_count('#')

# optimize and try different slopes
slopes <- c(1, 3, 5, 7) 

trees <- slopes %>%
    map(function(r) {
        full.map %>%
            imap_chr(function(.x, .y) { 
                substr(.x, r*.y-(r-1), r*.y-(r-1)) }) %>%
            str_flatten() %>%
            str_count('#') })

# delete every other row -> same as slope right 1 down 1
keep.rows <- seq(1, length(full.map), 2)
altered.map <- full.map[keep.rows]
last.route <- altered.map %>%
    imap_chr(function(.x, .y) { 
        # browser()
        substr(.x, 1*.y, 1*.y) }) %>%
    str_flatten() %>%
    str_count('#') 

all.trees <- c(trees, last.route)
reduce(all.trees, `*`)
