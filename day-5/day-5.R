library(tidyverse)

input <- read_lines('~/Desktop/advent-of-code-20/day-5/input.txt')

# part 1
# str <- 'FBFBBFFRLR'
decode.seat.row <- function(str) {
  
  row <- seq(0,127,1) #1-128
  for (i in seq_len(7)) {
    switch(substr(str,i,i),
           'F' = { row <- row[1:(length(row)/2)] },
           'B' = { row <- row[ ((length(row)/2)+1):length(row)] }) }
  return(row)
}

decode.seat.col <- function(str) {
  
  col <- seq(0,7,1) #1-8
  for (i in seq(8,10,1)) {
    switch(substr(str,i,i),
           'L' = { col <- col[1:(length(col)/2)] },
           'R' = { col <- col[ ((length(col)/2)+1):length(col)] }) }
  return(col)
}

calc.seat.id <- function(row, col) { (row*8)+col }

rows <- map(input, ~ decode.seat.row(.))
cols <- map(input, ~ decode.seat.col(.))

# what is the highest seat ID?
seat.ids <- map2_dbl(rows, cols, ~ calc.seat.id(.x, .y))
max(seat.ids)

# part 2 not 210
possible <- seq(min(seat.ids), max(seat.ids), 1)
seat.ids[which(!(possible %in% seat.ids))]

# anew
locate <- tibble('seat.ids' = seat.ids,
                 'rows' = as.numeric(rows),
                 'cols' = as.numeric(cols)) %>% 
  arrange(rows, cols) %>% 
  filter((rows > min(rows)) & (rows < max(rows))) %>% 
  nest(data = -rows) %>%
  mutate(full = map_lgl(data, ~ ifelse(nrow(.) == 8, TRUE, FALSE))) %>% 
  unnest(data) %>%
  filter(!full) # %>% deduce() ;)
