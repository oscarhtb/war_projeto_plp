# war_projeto_plp

 README

O projeto, desenvolvido em haskell, consiste em um jogo de estratégia baseado do clássico War. O jogo permite partidas com até 4 jogadores, que podem ser tanto humanos, quando bots (nesse caso, no máximo 3, pois deve haver, no mínimo, um jogador real em cada partida), e segue um sistema de rodadas, nas quais os jogadores devem fortaleces os seus territórios e conquistar outros, a fim de alcançarem os seus objetivos. 
O mapa possui um total de 24 territórios, divididos entre cinco continentes, conforme a seguinte distribuição:

América: AL, CA, GL, NY, MX, BR, AR

Ásia: TU, MO, CH, SI, VL,IN, VI, JP

Europa: UK, GE, SP

África: MR, EG, SA, MA

Oceania: AU, NZ 

Cada território possui, inicialmente, 01 exército, e uma cor que representa a qual jogador ele pertence. 
Os objetivos são secretos e cada jogador lê o seu individualmente. Os objetivos dos bots nunca são mostrados no terminal.


 ANTES DE COMEÇAR A JOGAR

É necessário ter um terminal, o GHC (Glasgow Haskell Compiler) instalado, para compilar o código, e o Cabal, para gerenciar pacotes e dependências.
Recomenda-se executar, no terminal, o seguinte comando, para garantir que as cores e o mapa serão exibidos corretamente:
    
    #Windows
    [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
    chcp 65001

    #Linux
    export LANG=en_US.UTF-8
    export LC_ALL=en_US.UTF-8
    locale

    #Mac
    defaults write -g AppleLocale en_US.UTF-8

Além disso, é importante executar, também:
    
    cabal clean //para limpar arquivos antigos que possam atrapalhar a execução do jogo
    cabal run //para compilar e executar o jogo

 DURANTE O JOGO

A primeira rodada é de fortalecimento. Cada jogador recebe um total de 5 exércitos e pode alocar eles em quaisquer territórios que possua, não sendo possível alocá-los em territórios de adversários.
As rodadas seguintes possuem, também, a etapa de fortalecimento, mas permitem que após a alocação dos exércitos, os jogadores possam atacar territórios vizinhos e, possivelmente, conquistá-los. O jogo verifica automaticamente a adjacência dos territórios antes de permitir o ataque, e cada ataque faz uso de uma quantidade de dados escolhida pelo jogador para definir a conquista e a quantidade de exércitos que foram perdidos na ação. Após um ataque, exércitos podem ser deslocados para o território conquistado ou para outro que também pertença ao jogador, caso ele deseje.

 VITÓRIA 

A cada rodada, o  jogo verifica se algum jogador cumpriu seu objetivo. Quando um jogador vence, seu objetivo é exibido na tela e a partida é finalizada.

 SALVANDO E CONTINUANDO O JOGO 

O jogo pode ser salvo no início de cada rodada, permitindo que seja retomado depois.
Ao escolher a opção de continuar um jogo salvo, o mapa e os objetivos dos jogadores permanecerão inalterados. 

REGRAS E FLUXO DO JOGO

1. MENU INICIAL: é escolhida a quantidade de jogadores da partida, que deve ser maior ou igual a 2 e menor ou igual a 4.
   
 1.1 BOTS: você pode escolher bots para jogar também, só não é permitido jogar exclusivamente com bots, pelo menos um dos jogadores deve ser real.
   
2. INÍCIO DO JOGO: depois do menu inicial, os territórios são divididos aleatoriamente entre os jogadores, cada jogador vai receber "24 / quantidade de jogadores" territórios com 1 exército em cada um deles.
   
3. RODADA DE ALOCAÇÃO: na primeira rodada, cada jogador vai receber 5 exércitos e pode colocá-los em territórios que o pertencem da forma que escolher.
 
4. DEMAIS RODADAS:
   
    4.1: ALOCAÇÃO: da mesma forma que a primeira rodada, os jogadores terão que alocar seus 5 novos territórios a cada rodada.
   
    4.2: ATAQUE: após a alocação, cada jogador vai poder atacar, para atacar, o jogador seleciona um território pertencente a ele e um território vizinho (ver ponto 5) pertencente a outro jogador. O jogador deve escolher quantos exércitos ele vai usar para atacar o vizinho, o mínimo é 1 e máximo é 3, no entanto, você só pode atacar caso o seu território vá ficar com pelo menos 1 exército sobrando após o ataque, portanto, não se pode atacar se seu território possuir apenas 1 exército e, caso tenha apenas 2 ou 3, só poderá usar, respectivamente, 1 ou 2 para o ataque. O jogador pode atacar quantas vezes quiser desde que tenha exércitos suficientes para isso.
   
      4.2.1: DADOS: a decisão de se o ataque foi bem sucedido ou não é através de dados, o atacante vai jogar a quantidade de dados referente à quantidade de exércitos que ele usou para atacar, já o defensor vai jogar a quantidade de dados referente à quantidade total de exércitos que ele possui no território atacado (máximo de 3 dados), a partir disso, os dados são comparados 1 a 1, da seguinte forma: "maior dado de ataque x maior dado de defesa; segundo maior de ataque x segundo maior de defesa; ..." a cada comparação, um dos exércitos vai ser eliminado, aquele que tem o menor dado vai perder um exército, em caso de empate o território defensor tem vantagem. Caso a quantidade de dados seja desigual, serão considerados apenas os maiores dados do lado que tiver mais.
   
      4.2.2: CONQUISTA: caso o território alvo perca todos os exércitos, o atacante pode transferir de 1 a 3 exércitos do território atacante para o seu novo território, mas claro que ele não pode deixar seu território original com menos de 1 exército.
   
    4.3: MOVIMENTO: após terminar seus ataques, o jogador terá a opção de mover exércitos para territórios vizinhos que também pertencem a ele, respeitando o máximo de 3 exércitos por movimento e respeitando que não pode deixar um território seu com menos de 1 exército. Além disso, há outra restrição, caso o jogador mova exércitos de A para B, ele não pode realizar nenhum movimento partindo de B para qualquer outro território naquela rodada. Respeitando essas regras, o jogador pode mover quantas vezes for necessário na rodada.

5. VIZINHANÇAS: os territórios considerados vizinhos e que podem interagir diretamente, em formato de lista de adjacência:
AL : (NA, VL)
NA : (AL, MX, NY, GL)
GL : (UK, NA, NY)
NY : (GL, NA, MX)
MX : (NY, NA, BR)
BR : (MX, AR, MR)
AR : (BR)
UK : (GL, GE)
GE : (UK, MR, SP)
SP : (GE, TU, MO, SI)
TU : (EG, SP, MO)
MR : (BR, GE, EG)
EG : (MA, SA)
SA : (MA, EG)
MA : (SA, EG)
MO : (TU, SP, SI, CH, IN)
SI : (MO, VL, SP, CH)
VL : (AL, SI, JP)
CH : (IN, VI, JP, MO, SI)
JP : (VL, CH)
IN : (MO, CH, VI)
VI : (AU, IN, CH)
AU : (NZ, VI)
NZ : (AU)

6. OBJETIVOS: no início de cada partida, cada jogador recebe um objetivo diferente dentre os 7 disponíveis, o jogo checa a cada ação relevante se algum dos objetivos foram cumpridos, aquele que cumpre seu objetivo ganha o jogo e ele se encerra imediatamente.
 6.1: objetivos existentes:
   eliminar o jogador 2 ou (caso o jogador 2 o receba), conquistar 14 territórios
   conquistar 14 territórios
   conquistar 12 territórios tendo 2 ou mais exércitos em cada um
   conquistar América e Europa inteiras
   conquistar América e Oceania inteiras
   conquistar Ásia e Europa inteiras
   conquistar Europa, Oceania e África inteiras
   




