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

É necessário ter o GHC (Glasgow Haskell Compiler) instalado, para compilar o código, e o Cabal, para gerenciar pacotes e dependências.
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


