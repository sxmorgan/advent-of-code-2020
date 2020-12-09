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
