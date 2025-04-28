#' Cosa manca:
#' 
#' - implementazione en passant (sara anche questa mossa speciale definita in make_move5 come l'arrocco perche 
#'    il pedone si muove normalmente ma il pedone avversario va eliminato dalla scacchiera in modo atipico e quindi manuale)
#'      
#' - opening_name() function che dice quale e il nome dell'apertura in game
#' 
#' - Funzione che in caso di "not valid move" vada a cercare di diagnosticare perche la mossa non e valida (
#'   re sotto scacco? non corretto il pezzo in startingposition? non puo arrivare dove dice finalposition? inchiodatura?)
#'   
#' - funzione che semplifichi l'iserimento della mossa giocata del tipo easymove("Bd5"),
#'    sulla base della lunghezza della mossa (l=2 in caso di pedone che non mangia, l=3 in caso di pezzo che non mangia,
#'    l = 4/5 in caso di omonimie o catture) e del contenuto (prima lettera minuscola o maiuscola?, ci sono x a indicare catture? arrocco?)
#'    riscriviamo make_move5(Piece, startingposition, finalposition)

game <- newgame()

game <- make_move4(Pawn, "e2", "e4")
game <- make_move4(Pawn, "d7", "d5")
game <- make_move4(Pawn, "e4", "d5")
game <- make_move4(Pawn, "c7", "c6")
game <- make_move4(Pawn, "d5", "c6")
game <- make_move4(Knight, "g8", "f6")
game <- make_move4(Pawn, "c6", "b7")
game <- make_move4(Knight, "b8", "c6")
game <- make_move4(Pawn, "b7", "c8")
game <- make_move4(Rook, "a8", "c8")

game <- make_move4(Bishop, "f1", "b5")
game <- make_move4(Queen, "d8", "d5")

game <- make_move4(Bishop, "b5", "c6")
game <- make_move4(Rook, "c8", "c6")

game <- make_move4(Knight, "g1", "f3")
game <- make_move4(Queen, "d5", "b5")

all_possibilities()[["w"]][["Kw_e1"]] # The program spots that the king has no legal moves!
game <- make_move4(Pawn, "d2", "d3")
game <- make_move4(Queen, "b5", "b4")

all_possibilities()[["w"]]

game <- make_move4(Pawn, "c2", "c3")
game <- make_move4(Queen, "b4", "e4")

game <- make_move4(Pawn, "d3", "e4")
game <- make_move4(Pawn, "g7", "g6")

all_possibilities()[["w"]][["Kw_e1"]] # Now short castle is available! :)
game <- make_move4(King, "e1", "0-0")

moves_scoresheet()
###

game <- newgame()

game <- make_move4(Pawn, "e2", "e4")
game <- make_move4(Pawn, "d7", "d5")
game <- make_move4(Bishop, "f1", "b5")
game <- make_move4(Bishop, "c8", "d7")
game <- make_move4(Knight, "g1", "f3")
game <- make_move4(Bishop, "d7", "b5")
game <- make_move4(Pawn, "d2", "d3")
game <- make_move4(Knight, "b8", "c6")


game <- make_move4(King, "e1","0-0")

###

game <- newgame()

game<- make_move4(Pawn, "e2", "e4")
game <- make_move4(Pawn, "e7", "e6")
game<- make_move4(Pawn, "d2", "d4")
game <- make_move4(Pawn, "d7", "d5")
game<- make_move4(Pawn, "e4", "d5")
game <- make_move4(Pawn, "e6", "d5")

game <- make_move4(Knight, "g1", "f3")
game <- make_move4(Knight, "g8", "f6")

game <- make_move4(Bishop, "f1", "b5")
game <- make_move4(Knight, "b8", "c6")

game <- make_move4(King, "e1" ,"0-0")
game <- make_move4(Pawn, "a7" ,"a6")

game <- make_move4(Rook, "f1", "e1")
game <- make_move4(Queen, "d8", "e7")

game <- make_move4(Pawn, "h2", "h3")

all_possibilities()[["b"]] # Le inchiodature funzionano: il cavallo in c6 non ha caselle e la donna ha solo quelle sulla linea della torre


###

game <- newgame()

game <- make_move4(Pawn, "e2", "e4")
game <- make_move4(Pawn, "d7", "d5")
game <- make_move4(Bishop, "f1", "b5")
game <- make_move4(Bishop, "c8", "f5") # The program spots that this move is not valid!! Yuppi
game <- make_move4(Knight, "b8", "c6")
game <- make_move4(Queen, "d1", "e2")

game

####

game <- newgame()

game <- make_move4(Pawn, "e2", "e4")
game <- make_move4(Pawn, "c7", "c6")

game <- make_move4(Pawn, "d2", "d4")
game <- make_move4(Pawn, "d7", "d5")

game <- make_move4(Knight, "b1", "c3")
game <- make_move4(Pawn, "d5", "e4")

game <- make_move4(Knight, "c3", "e4")
game <- make_move4(Knight, "g8", "f6")

game <- make_move4(Knight, "e4", "f6")
game <- make_move4(Queen, "d8", "c7") # Invalid!
game <- make_move4(King, "e8", "d7") # Invalid!
game <- make_move4(Pawn, "e7", "f6")

###
game <- newgame()

game <- make_move4(Pawn, "f2", "f4")
game <- make_move4(Pawn, "e7", "e5")
game <- make_move4(Pawn, "g2", "g3")
game <- make_move4(Pawn, "e5", "f4")
game <- make_move4(Pawn, "g3", "f4")

game <- make_move4(Queen, "d8", "h4")

game_result()
