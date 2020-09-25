# get game
get_game <- function(nb = nb, input = input, exit = exit, position = position) {
  
  results <- NULL
  
  n = test <- 1
  
  while (test != 0) {
    
    # take the possible moves
    mov <- get_moves(position = position, nb = nb)
    
    # objective function
    fobj <- function(position, exit, input, n) {
      distance <- sum(abs(exit - position))
      points <- (n * distance)^-1
      rew <- ifelse(distance != 0, points, 10)
      return(rew)
    }
    rew <- fobj(position, exit, input, n) * 2^nrow(mov)
    
    # draw an action among the available ones and update the next position
    next_step <- sample(rownames(mov), 1)
    position <- mov[mov$mov == next_step, c('row', 'col')]
    position <- unlist(position)
    
    # list the results
    results <- rbind.data.frame(
      results,
      cbind.data.frame(
        rew, 
        'row' = position[1], 
        'col' = position[2], 
        'estado' = paste0('s', position[1], position[2]),
        next_step
      )
    )
    
    # update stop test information
    test <- sum(abs(exit - c(position)))
  }
  
  # remove labels from lines
  rownames(results) <- NULL
  
  # return
  return(results)
  
}


# tidy states
get_result_state <- function(results = results) {
  
  # tidy states
  transition <- results %>% 
    mutate(
      State = paste0('s', row, col),
      Action = c(next_step[2:length(next_step)], 'exit'),
      NextState = c(State[2:length(State)], State[length(State)])
    ) %>% 
    rename(Reward = rew) %>% 
    select(State, Action, Reward, NextState)
  
  # return list
  end <- list(transition = transition, results = results)
  return(end)
  
}


# visualization
plot_maze <- function(board_cells, input, exit, boxes) {
  
  cas <- boxes %>% 
    as.data.frame() %>% 
    mutate(State = paste0('s', row, col))
  nbPlot <- board_cells %>% 
    as.data.frame() %>% 
    rownames_to_column(var = 'row') %>% 
    pivot_longer(-row, names_to = 'col', values_to = 'point') %>% 
    mutate(
      row = factor(row, levels = 1:nrow(board_cells)),
      col = str_remove(col, 'V') %>% 
        factor(levels = 1:ncol(board_cells))
    ) %>% 
    ggplot(aes(x = col, y = row, fill = point)) +
    geom_tile(aes(fill = point), colour = "white") +
    scale_fill_gradient(low = "black", high = "white") +
    guides(fill = F) +
    xlab('') +
    ylab('') +
    # input location
    geom_point(aes(y = input[1], x = input[2], size = 2), col = 'green') + 
    # exit location
    geom_point(aes(y = exit[1], x = exit[2], size = 2), col = 'aquamarine') + 
    annotate('text', x = cas$col, y = cas$row, label = cas$State) +
    theme(legend.position = "none")
  
  return(nbPlot)
}

plot_update <- function(first_plot, position) {
  first_plot +
    geom_point(aes(y = position[1], x = position[2]), col = 'red')
}

# take new position
get_pos <- function(position) return(as.matrix(mov[next_step,-3]))


# pick up next steps moves
get_moves <- function(position, nb) {
  
  # Note: In the graph the y-axis is in Cartesian coordinate so 
  # the representation of the matrix lines is in reverse order 
  # compared to the graph.
  
  suppressWarnings({
    
    # up
    res <- try({
      if (nb[position[1] - 1, position[2]] == 1) {
        up <- c(position[1] - 1, position[2])
      } else {
        up <- c(NA, NA)
      }
    })
    if (grepl('Error', res)) up <- c(NA, NA)
    
    # down
    res <- try({
      if (nb[position[1] + 1, position[2]] == 1) {
        down <- c(position[1] + 1, position[2])
      } else {
        down <- c(NA, NA)
      }
    })
    if (grepl('Error', res)) down <- c(NA, NA)
    
    # left
    res <- try({
      if (nb[position[1], position[2] - 1] == 1) {
        left <- c(position[1], position[2] - 1)
      } else {
        left <- c(NA, NA)
      }
    })
    if (grepl('Error', res)) left <- c(NA, NA)
    
    # right
    res <- try({
      if (nb[position[1], position[2] + 1] == 1) {
        right <- c(position[1], position[2] + 1)
      } else {
        right <- c(NA, NA)
      }
    })
    if (grepl('Error', res)) right <- c(NA, NA)
    
  })
  
  x <- rbind(up, down, left, right)
  x <- cbind.data.frame(x, 'mov' = c('up', 'down', 'left', 'right'))
  x <- x[complete.cases(x),]
  
  return(x)
}


# update the labyrinth print
nb_update <- function(nb, input, exit, position) {
  tmp <- nb
  tmp[input[1], input[2]] <- 333
  tmp[exit[1], exit[2]] <- 555
  tmp[position[1], position[2]] <- 999
  return(tmp)
}