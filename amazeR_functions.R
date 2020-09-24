get_game <- function(nb = nb, entrada = entrada, saida = saida, posicao = posicao) {
  
  resultados <- NULL
  
  n = teste <- 1
  
  while (teste != 0) {
    
    # tomar os movimentos possiveis
    mov <- get_moves(posicao = posicao, nb = nb)
    
    # funcao objetivo
    fobj <- function(posicao, saida, entrada, n) {
      distancia <- sum(abs(saida - posicao))
      pontuacao <- (n * distancia)^-1
      rew <- ifelse(distancia != 0, pontuacao, 10)
      return(rew)
    }
    rew <- fobj(posicao, saida, entrada, n) * 2^nrow(mov)
    
    # sortear uma acao dentre as disponiveis e atualiza a proxima posicao
    proximo <- sample(rownames(mov), 1)
    posicao <- mov[mov$mov == proximo, c('row', 'col')]
    posicao <- unlist(posicao)
    
    # listar os resultados
    resultados <- rbind.data.frame(
      resultados,
      cbind.data.frame(
        rew, 
        'row' = posicao[1], 
        'col' = posicao[2], 
        'estado' = paste0('s', posicao[1], posicao[2]),
        proximo
      )
    )
    
    # atualizar o grafico e dar pausa
    # print(plot_update(first_plot, posicao))
    # Sys.sleep(.25)
    
    # atualiza informacao do teste de parada
    teste <- sum(abs(saida - c(posicao)))
  }
  
  # remover rotulos das linhas
  rownames(resultados) <- NULL
  
  # retornar
  return(resultados)
  
}


# arrumar_estados
get_result_state <- function(resultados = resultados) {
  transicao <- resultados %>% 
    mutate(
      State = paste0('s', row, col),
      Action = c(proximo[2:length(proximo)], 'exit'),
      NextState = c(State[2:length(State)], State[length(State)])
    ) %>% 
    rename(Reward = rew) %>% 
    select(State, Action, Reward, NextState)
  fim <- list(transicao = transicao, resultados = resultados)
  return(fim)
}


# visualizacao
plot_maze <- function(board_cells, entrada, saida, caselas) {
  cas <- caselas %>% 
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
    #theme_void() +
    xlab('') +
    ylab('') +
    geom_point(aes(y = entrada[1], x = entrada[2], size = 2), col = 'green') + # local de entrada
    geom_point(aes(y = saida[1], x = saida[2], size = 2), col = 'aquamarine') + # local de saida
    annotate('text', x = cas$col, y = cas$row, label = cas$State) +
    theme(legend.position = "none")
  return(nbPlot)
}

plot_update <- function(first_plot, posicao) {
  first_plot +
    geom_point(aes(y = posicao[1], x = posicao[2]), col = 'red')
}

# pegar nova posicao
get_pos <- function(posicao) return(as.matrix(mov[proximo,-3]))


# pegar proximos movimentos
get_moves <- function(posicao, nb) {
  
  suppressWarnings({
    
    # up
    res <- try({
      if (nb[posicao[1] - 1, posicao[2]] == 1) {
        up <- c(posicao[1] - 1, posicao[2])
      } else {
        up <- c(NA, NA)
      }
    })
    if (grepl('Error', res)) up <- c(NA, NA)
    
    # down
    res <- try({
      if (nb[posicao[1] + 1, posicao[2]] == 1) {
        down <- c(posicao[1] + 1, posicao[2])
      } else {
        down <- c(NA, NA)
      }
    })
    if (grepl('Error', res)) down <- c(NA, NA)
    
    # left
    res <- try({
      if (nb[posicao[1], posicao[2] - 1] == 1) {
        left <- c(posicao[1], posicao[2] - 1)
      } else {
        left <- c(NA, NA)
      }
    })
    if (grepl('Error', res)) left <- c(NA, NA)
    
    # right
    res <- try({
      if (nb[posicao[1], posicao[2] + 1] == 1) {
        right <- c(posicao[1], posicao[2] + 1)
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


# atualizar o print do labirinto
nb_update <- function(nb, entrada, saida, posicao) {
  tmp <- nb
  tmp[entrada[1], entrada[2]] <- 333
  tmp[saida[1], saida[2]] <- 555
  tmp[posicao[1], posicao[2]] <- 999
  return(tmp)
}
