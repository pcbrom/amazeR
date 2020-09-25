# get functions ---------------------------------------------------------------

source('mazeR.R')
source('amazeR_functions.R')
library(tidyverse)
library(doSNOW)


# labyrinth settings ----------------------------------------------------------

# defining the number of simulations
N <-1000

# defining the size of the maze
xx <- 3
yy <- 3

# creating the maze
board <- matrix(0, xx, yy)
board_cells <- labyrinth(start_coord_x = 1, start_coord_y = 1)
board_cells <- board_cells[nrow(board_cells):1,]
boxes <- which(board_cells == 2, arr.ind = T)

# simplifying the maze
nb <- board_cells - 1

# creating possible actions
actions <- c("up", "down", "left", "right")


# defining initial positions --------------------------------------------------

# random entry, exit and player positions
ini <- sample(1:nrow(boxes), 2, replace = F)
input = position = posInit <- boxes[ini[1],]
exit <- boxes[ini[2],]
names(exit) <- NULL

# visualization
first_plot <- plot_maze(board_cells, input, exit, boxes)
first_plot + geom_point(aes(y = position[1], x = position[2]), col = 'red')

# creating results
result_ini <- data.frame(
  'rew' = 0, 
  'row' = position[1], 
  'col' = position[2],
  'state' = paste0('s', position[1], position[2]),
  'next_step' = 'start'
)

# configuring clusters --------------------------------------------------------

ncores <- parallel::detectCores()
cl = makeSOCKcluster(ncores)
registerDoSNOW(cl)
pb = txtProgressBar(min = 0, max = N, style = 3)
progress = function(x) setTxtProgressBar(pb, x)
opts = list(progress = progress)

# running some matches
result <- foreach(
  i = 1:N, 
  .options.snow = opts, 
  .combine = 'rbind.data.frame',
  .multicombine = T,
  .packages = c('dplyr')
) %dopar% {
  get_game(nb, input, exit, position)
}

# consolidating results
result <- rbind.data.frame(result_ini, result)

# closing the progress bar
close(pb)

# closing the clusters
stopCluster(cl)


# adjust game results and data ------------------------------------------------

results <- get_result_state(result)
game <- list(
  board_cells = board_cells, 
  input = input, 
  exit = exit, 
  position = posInit, 
  boxes = boxes
)

# save
saveRDS(game, 'game.RDS')
saveRDS(results, 'results.RDS')
