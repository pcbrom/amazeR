# pegar funcoes ---------------------------------------------------------------

suppressMessages({
  suppressWarnings({
    source('mazeR.R')
    source('amazeR_functions.R')
    library(tidyverse)
    library(doSNOW)
  })
})


# configuracoes do labirinto --------------------------------------------------

# definino onumero de simulacoes
N <-10000

# definindo o tamanho do labirinto
xx <- 3
yy <- 3

# criando o labirinto
board <- matrix(0, xx, yy)
board_cells <- labyrinth(start_coord_x = 1, start_coord_y = 1)
board_cells <- board_cells[nrow(board_cells):1,]
caselas <- which(board_cells == 2, arr.ind = T)
ini <- sample(1:nrow(caselas), 2, replace = F)

# definindo posicoes iniciais -------------------------------------------------

entrada = posicao = posInit <- caselas[ini[1],]
saida <- caselas[ini[2],]
names(saida) <- NULL

# visualizacao
first_plot <- plot_maze(board_cells, entrada, saida, caselas)
first_plot + geom_point(aes(y = posicao[1], x = posicao[2]), col = 'red')

# simplificando o labirinto
nb <- board_cells - 1

# criando acoes possiveis
actions <- c("up", "down", "left", "right")

# criando resulados
result_ini <- data.frame(
  'rew' = 0, 
  'row' = posicao[1], 
  'col' = posicao[2],
  'estado' = paste0('s', posicao[1], posicao[2]),
  'proximo' = 'start'
)

# criar lista de acoes disponiveis --------------------------------------------

cl = makeSOCKcluster(40)
registerDoSNOW(cl)
pb = txtProgressBar(min = 0, max = N, style = 3)
progress = function(x) setTxtProgressBar(pb, x)
opts = list(progress = progress)

# rodando umas partidas
result <- foreach(
  i = 1:N, 
  .options.snow = opts, 
  .combine = 'rbind.data.frame',
  .multicombine = T,
  .packages = c('dplyr')
) %dopar% {
  get_game(
    nb, 
    entrada, 
    saida, 
    posicao
  )
}
result <- rbind.data.frame(result_ini, result)

close(pb)
stopCluster(cl)


# # funcao de recompensa
# fobj <- function(posicao, saida, entrada, n) {
#   # calcular distancia da saida dado a posicao atual
#   distancia <- sum(abs(saida - posicao))
#   pontuacao <- (n * distancia)^-1
#   rew <- ifelse(distancia != 0, pontuacao, 10)
#   return(rew)
# }
# pb = txtProgressBar(min = 0, max = N, style = 3) 
# for (k in 1:N) {
#   result <- rbind.data.frame(
#     result, 
#     get_game(
#       nb, 
#       entrada, 
#       saida, 
#       posicao, 
#       fobj(posicao, saida, entrada, n)
#     )
#   )
#   setTxtProgressBar(pb, k)
# }


# ajustar resultados e dados do jogo ------------------------------------------

resultados <- get_result_state(result)
jogo <- list(
  board_cells = board_cells, 
  entrada = entrada, 
  saida = saida, 
  posicao = posInit, 
  caselas = caselas
)

# salvar
saveRDS(jogo, 'jogo.RDS')
saveRDS(resultados, 'resultados.RDS')
