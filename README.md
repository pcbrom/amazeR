# amazeR

## mazeR.R

- Script original de https://github.com/matfmc/mazegenerator.
- Foi modificado para deixar o labirinto mais estreito


## gerar_resultados.R

- Finalidade: Simular passo aletório de um jogador no labirinto.

**Objetos de interesse**

- N: Total de simulações.
- xx e yy: Tamanho do tabuleiro.
- ini: Amostra aleatória simples ser reposição das condições iniciais do jogo.
- first_plot: Plot inicial do tabuleiro.
- resultados: Saídas simulações organizadas para serem utilizadas em Aprendizagem de Máquinas.
- jogo: Condições jogadas.


## amazeR_functions.R

Funções:

**get_game(nb, entrada, saida, posicao)**
+ Finalidade: Criar o labirinto adicionando entrada, saída e posição do jogador.
+ nb: Tabuleiro simplificado do labirinto.
+ entrada: Coordenada de entrada do labirinto.
+ saida: Coordenada de sída do labirinto.
+ posicao: Posição atual do personagem.
+ Dentro de get_game() tem fobj(), função objetivo, que faz a pontuação da recompensa.

**get_result_state(resultados)**
+ Finalidade: Organizar informações do jogo.
+ resultados: Coleção de algumas partidas para o mesmo tabuleiro.

**plot_maze(board_cells, entrada, saida, caselas)**
+ Finalidade: Plotar o jogo usando ggplot. No gráfico gerado as linhas estão em coordenada reversa do tabuleiro. Eixo x representa colunas e eixo y linhas.
+ board_cells: Tabuleiro gerado pelo mazeR.R.
+ entrada: Coordenada de entrada do labirinto.
+ saida: Coordenada de sída do labirinto.
+ caselas: Matriz contendo coordenadas válidas em passeio aleatório.

**plot_update(first_plot, posicao)**
+ Finalidade: Update da posição do jogador no gráfico.
+ first_plot: Objeto que armazena o primeiro plot_maze().
+ posicao: Posição atual do jogador.

**get_moves(posicao, nb)**
+ Finalidade: Dada a posição atual, retornar os movimentos possíveis.
+ posicao: Posição atual do personagem.
+ nb: Tabuleiro simplificado do labirinto.

**nb_update(nb, entrada, saida, posicao)**
+ Finalidade: Update da posição do jogador no tabuleiro.
+ nb: Tabuleiro simplificado do labirinto.
+ entrada: Coordenada de entrada do labirinto.
+ saida: Coordenada de sída do labirinto.
+ posicao: Posição atual do personagem.
