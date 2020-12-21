library(tidyverse)
library(magrittr)
library(future)
library(furrr)

input <- read_lines('~/Desktop/advent-of-code-20/day-11/input.txt')
# input <- read_lines('~/Desktop/advent-of-code-20/day-11/input-test.txt')

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

setGeneric('get.n.adj', 
           function(object) { standardGeneric('get.n.adj') })
setMethod('get.n.adj', 
          signature(object = 'position'), 
          function(object) { object@adj.occ })

setGeneric('calc.adj.idx', 
           function(object) { standardGeneric('calc.adj.idx') })
setMethod('calc.adj.idx', 
          signature(object = 'position'), 
          function(object) {
              
              # browser()
              x.border <- character()
              y.border <- character()
              
              if (object@x == 1) x.border <- 'left'
              else if (object@x == object@grid.x) x.border <- 'right'
              else x.border <- 'none'
              
              if (object@y == 1) y.border <- 'top'
              else if (object@y == object@grid.y) y.border <- 'bottom'
              else y.border <- 'none'
              
              if ((y.border == 'top') & (x.border == 'left')) {
                  x.rng <- seq(object@x, object@x+1, 1)
                  y.rng <- seq(object@y, object@y+1, 1) }
              else if ((y.border == 'top') & (x.border == 'none')) {
                  x.rng <- seq(object@x-1, object@x+1, 1)
                  y.rng <- seq(object@y, object@y+1, 1) }
              else if ((y.border == 'top') & (x.border == 'right')) {
                  x.rng <- seq(object@x-1, object@x, 1)
                  y.rng <- seq(object@y, object@y+1, 1) }
              else if ((y.border == 'none') & (x.border == 'left')) {
                  x.rng <- seq(object@x, object@x+1, 1)
                  y.rng <- seq(object@y-1, object@y+1, 1) }
              else if ((y.border == 'none') & (x.border == 'right')) {
                  x.rng <- seq(object@x-1, object@x, 1)
                  y.rng <- seq(object@y-1, object@y+1, 1) }
              else if ((y.border == 'bottom') & (x.border == 'left')) {
                  x.rng <- seq(object@x, object@x+1, 1)
                  y.rng <- seq(object@y-1, object@y, 1) }
              else if ((y.border == 'bottom') & (x.border == 'none')) {
                  x.rng <- seq(object@x-1, object@x+1, 1)
                  y.rng <- seq(object@y-1, object@y, 1) }
              else if ((y.border == 'bottom') & (x.border == 'right')) {
                  x.rng <- seq(object@x-1, object@x, 1)
                  y.rng <- seq(object@y-1, object@y, 1) }
              else {
                  x.rng <- seq(object@x-1, object@x+1, 1)
                  y.rng <- seq(object@y-1, object@y+1, 1) }

              return(list('x.rng' = x.rng,
                          'y.rng' = y.rng))
          })

setGeneric('update.adj.occ',
           function(object, ...) { standardGeneric('update.adj.occ') })
setMethod('update.adj.occ',
          signature(object = 'position'),
          function(object, n.adj) { 
              object@adj.occ <- n.adj
              return(object) })

setGeneric('update.status', 
           function(object) { standardGeneric('update.status') })
setMethod('update.status', signature(object = 'position'), 
          function(object) {
              
              # status.curr <- object@status
              # adj.occ.curr <- object@adj.occ
              
              if (object@status == '.') 
                  status.loc <- FALSE
              else if ((object@status == 'L') & (object@adj.occ == 0)) {
                  object@status <- '#'
                  status.loc <- TRUE }
              else if ((object@status == '#') & (object@adj.occ >= 4)) {
                  object@status <- 'L'
                  status.loc <- TRUE }
              else (status.loc <- FALSE)
              
              return(list(object, status.loc))
})


setClass('grid', representation(objects = 'list',
                                x.pos = 'numeric',
                                y.pos = 'numeric'))
         

#### MAIN METHODS

# returns a list of adjacent position objects
collect.adjacent <- function(grid.obj, target.idx) {
    # browser()
    list.idx <- tibble(target.idx) %>%
        mutate(list.idx = future_map2_dbl(Var1, Var2, 
                                   ~ intersect(which(grid.obj@x.pos == .x),
                                    which(grid.obj@y.pos == .y)))) %>%
        pull(list.idx)
    
    return(grid.obj@objects[list.idx])
}

update.adjacent <- function(grid) {
    
    # browser()
    grid.pos <- map(grid, ~ get.position(.))
    
    adj.idx <- grid %>%
        map(~ calc.adj.idx(.)) %>%
        map(~ expand.grid(.$x.rng, .$y.rng)) %>%
        future_map2(grid.pos, 
                    ~ filter(.x, !((Var1 == .y[1]) & (Var2 == .y[2]))))
    
    grid.obj <- new('grid', objects = grid,
                    x.pos = map_dbl(grid.pos, ~ head(., 1)),
                    y.pos = map_dbl(grid.pos, ~ tail(., 1)))

    adj.obj.cts <- adj.idx %>%
        future_map(~ collect.adjacent(grid.obj, .)) %>%
        map(~ map(., ~ get.status(.))) %>%
        map(~ unlist(.)) %>% 
        map(~ paste(., collapse = '')) %>%
        map(~ str_count(., '#'))
    
    new.grid <- map2(grid, adj.obj.cts, ~ update.adj.occ(.x, .y))
    
    return(new.grid)
}

# main method
simulate.round <- function(grid) {
    
    # browser()
    new.grid <- grid %>%
        update.adjacent() %>%
        map(~ update.status(.)) %>%
        map(~ extract2(., 1))
 
    stability <- grid %>%
        update.adjacent() %>%
        map(~ update.status(.)) %>%
        map_lgl(~ extract2(., 2))
    
    # base case: no statuses changed
    if (!any(stability)) return(grid) 
    
    # recursive case: statuses changed
    else {
        print('simulating again')
        simulate.round(new.grid) }
}

### part 1
plan(multisession)

# grid dimensions
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

# final grid should be returned
last.grid <- simulate.round(grid)

# final arrangement - how many are occupied?
occupied <- last.grid %>%
    map_chr(~ get.status(.)) %>%
    paste(., collapse = '') %>%
    str_count('#')
print(occupied)