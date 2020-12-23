library(tidyverse)
library(magrittr)

setwd('~/Desktop/advent-of-code-20/')
library(here)

input <- read_lines(here('day-12','input.txt')) 
input <- c('F10',
           'N3',
           'F7',
           'R90',
           'F11')

get.unit.vector <- function(dir) {
    
    if (dir == 'N') return(c(0,-1))
    else if (dir == 'E') return(c(1,0))
    else if (dir == 'S') return(c(0,1))
    else return(c(-1,0))
}

get.new.dir <- function(v) {
    
    if (all(v == c(0,-1))) return('N')
    else if (all(v == c(0,1))) return('S')
    else if (all(v ==  c(1,0))) return('E')
    else return('W')
}

change.dir <- function(dir, deg, clock.dir) {
    
    # N S E W
    # 0,-1 / 0,1 // 1,0 / -1,0
    v <- get.unit.vector(dir) 
    # browser()
    
    cond1 <- (clock.dir == 'R') & (deg == 90)
    cond2 <- (clock.dir == 'L') & (deg == 270)
    cond3 <- (clock.dir == 'R') & (deg == 270)
    cond4 <- (clock.dir == 'L') & (deg == 90)
    
    if (cond1 | cond2) {
        if (str_detect(dir, 'N|S')) v <- v + 1
        else v <- c(v[2], v[1]) }
    else if (cond3 | cond4) {
        if (str_detect(dir, 'N|S')) v <- c(v[2], v[1])
        else v <- v - 1 }
    else v <- -1 * v
    
    return(get.new.dir(v))
}

navigate <- function(dir, x, y, nav) {
    
   # browser()
    print(paste(dir, x, y, nav[1]))
    i <- as.integer(str_extract(nav[1], '[0-9]+'))
 
    switch(str_sub(nav[1],1,1),
           'N' = { y <- y - i },
           'E' = { x <- x + i },
           'S' = { y <- y + i },
           'W' = { x <- x - i },
           'L' = { dir <- change.dir(dir, i, 'L')},
           'R' = { dir <- change.dir(dir, i, 'R')},
           'F' = {
               v <- get.unit.vector(dir)*i
               x <- x + v[1]
               y <- y + v[2] } )
    
    if (length(nav) == 0) {
        print('route finished')
        sum(abs(x), abs(y)) }
    
    else (navigate(dir, x, y, nav[-1]))
    
}

navigate('E', 0, 0, input)
# not 1768, too low
# nor 2008, 1692