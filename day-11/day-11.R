library(tidyverse)
library(magrittr)

input <- read_lines('~/Desktop/advent-of-code-20/day-11/input.txt')
input <- read_lines('~/Desktop/advent-of-code-20/day-11/input-test.txt')

#### set up S4 classes and methods; http://adv-r.had.co.nz/S4.html
setClass('position', representation(x = 'numeric', 
                                    y = 'numeric', 
                                    # floor (.), empty (L), occupied (#)
                                    status = 'character',
                                    adj.occ = 'numeric',
                                    grid.x = 'numeric',
                                    grid.y = 'numeric'))

setGeneric('get.position', 
           function(object) { standardGeneric('get.position') })
setMethod('get.position', 
          signature(object = 'position'), 
          function(object) { c(object@x, object@y) })

setGeneric('get.status', 
           function(object) { standardGeneric('get.status') })
setMethod('get.status', 
          signature(object = 'position'), 
          function(object) { object@status })

setGeneric('get.grid.dim', 
           function(object) { standardGeneric('get.grid.dim') })
setMethod('get.grid.dim', 
          signature(object = 'position'), 
          function(object) { c(object@grid.x, object@grid.y) })

setGeneric('get.adjacent', 
           function(object) { standardGeneric('get.adjacent') })
setMethod('get.adjacent', 
          signature(object = 'position'), 
          function(object) { object@adj.occ })

setGeneric('calc.adjacent', 
           function(object) { standardGeneric('calc.adjacent') })
setMethod('calc.adjacent', 
          signature(object = 'position'), 
          function(object) {
              browser()
          })

setGeneric('update.status', 
           function(object) { standardGeneric('update.status') })
setMethod('update.status', signature(object = 'position'), 
          function(object) {
              
              status.curr <- get.status(object)
              adj.occ.curr <- get.adjacent(object)
              
              # initial iteration
              if (length(adj.occ.curr) == 0) { 
                  object@adj.occ <- calc.adjacent(object)
                  adj.occ.curr <- get.adjacent(object) }
              
              if (status.curr == '.') 
                  status.loc <- FALSE
              else if ((status.curr == 'L') & (adj.occ.curr == 0)) {
                  object@status <- '#'
                  status.loc <- TRUE }
              else if ((status.curr == '#') & (adj.occ.curr >= 4)) {
                  object@status <- 'L'
                  status.loc <- TRUE }
              else (status.loc <- FALSE)
              
              return(list(object, status.loc))
})

#### MAIN METHOD

# width of the grid
n.pos <- nchar(input[1])
n.rows <- length(input)

# create an object for each position in the grid
grid <- input %>%
    imap(function(row, i) {
        this.row <- list()
        for (pos in seq_len(n.pos)) {
            this.row[[pos]] <- new('position', x = pos, y = i, 
                                 status = str_sub(row, pos, pos),
                                 adj.occ = numeric(0),
                                 grid.x = n.pos,
                                 grid.y = n.rows) }
        return(this.row)} ) %>%
    unlist()

# main method
simulate.round <- function(grid) {
    
    new.grid <- grid %>%
        map(~ update.status(.)) %>%
        map(~ extract2(., 1))
 
    stability <- grid %>%
        map(~ update.status(.)) %>%
        map_lgl(~ extract2(., 2))
    
    # base case: no statuses changed
    if (!any(stability)) return(grid) 
    
    # recursive case: statuses changed
    else simulate.round(grid)
}

# part 1: final arrangement - how many are occupied?
grid <- simulate.round(grid)