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
target <- 127#21806024
range <- seq(1,60,1)
for (i in range) {
    for (j in seq_len(nrow(input))) {
        if (j <= (nrow(input)-i)) {
            tmp <- input %>%
                slice(j:(j+i)) %>%
                pull(X1) %>%
                sum() %>%
                equals(target)
            if (tmp) {
                print('found!')
                out <- slice(input, j:(j+i))
                print(out) }
        }
    }
}
