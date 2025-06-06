check_obstacles(alldiags$"8", "b7")
check_obstacles(alldiags$"8", "c6")

check_obstacles(alltravs$"8", "e1")
check_obstacles(alltravs$"8", "b1")
check_obstacles(alltravs$"10", "b3")
check_obstacles(alltravs$"6", "b3")


check_obstacles(alltravs$"10", "b1")

board

defmoves(Bishop, "b7")
defmoves(Bishop, "c6")
defmoves(King, "b1")

defmoves(Rook, "b3")
defmoves(Queen, "e1")


####

game <- newgame()

game <- make_move3(Pawn, "e2", "e4")
game <- make_move3(Pawn, "d7", "d5")
game <- make_move3(Pawn, "e4", "d5")
game <- make_move3(Pawn, "c7", "c6")
game <- make_move3(Pawn, "d5", "c6")
game <- make_move3(Knight, "g8", "f6")
game <- make_move3(Pawn, "c6", "b7")
game <- make_move3(Knight, "b8", "c6")
game <- make_move3(Pawn, "b7", "c8")
game <- make_move3(Rook, "a8", "c8")


###

game <- newgame()

game <- make_move3(Pawn, "e2", "e4")
game <- make_move3(Pawn, "d7", "d5")
game <- make_move3(Bishop, "f1", "b5")
game <- make_move3(Bishop, "c8", "d7")
game <- make_move3(Knight, "g1", "f3")
game <- make_move3(Bishop, "d7", "b5")
game <- make_move3(Pawn, "d2", "d3")
game <- make_move3(Knight, "b8", "c6")


game <- make_move3(King, "0-0")

###

game <- newgame()

game<- make_move3(Pawn, "e2", "e4")
game <- make_move3(Pawn, "e7", "e6")
game<- make_move3(Pawn, "d2", "d4")
game <- make_move3(Pawn, "d7", "d5")
game<- make_move3(Pawn, "e4", "d5")
game <- make_move3(Pawn, "e6", "d5")

game <- make_move3(Knight, "g1", "f3")
game <- make_move3(Knight, "g8", "f6")

game <- make_move3(Bishop, "f1", "b5")
game <- make_move3(Knight, "b8", "c6")

game <- make_move3(King, "0-0")
game <- make_move3(Pawn, "a7" ,"a6")

game <- make_move3(Rook, "f1", "e1")
game <- make_move3(Queen, "d8", "e7")

game <- make_move3(Pawn, "h2", "h3")

kingcheck()
parrycheck() # Ho ancora il problema di non aver definito inchiodatura, quindi mi dà Nd7-e5 come possibile parata
# A parte questo però sembra funzionare


###

game <- newgame()

game <- make_move3(Pawn, "e2", "e4")
game <- make_move3(Pawn, "d7", "d5")
game <- make_move3(Bishop, "f1", "b5")
game <- make_move3(Bishop, "c8", "f5") # The program spots that this move is not valid!! Yuppi
game <- make_move3(Knight, "b8", "c6")
game <- make_move3(Queen, "d1", "e2")

game

####

game <- newgame()

game <- make_move3(Pawn, "e2", "e4")
game <- make_move3(Pawn, "c7", "c6")

game <- make_move3(Pawn, "d2", "d4")
game <- make_move3(Pawn, "d7", "d5")

game <- make_move3(Knight, "b1", "c3")
game <- make_move3(Pawn, "d5", "e4")

game <- make_move3(Knight, "c3", "e4")
game <- make_move3(Knight, "g8", "f6")

game <- make_move3(Knight, "e4", "f6")
game <- make_move3(Queen, "d8", "c7") # Invalid!
game <- make_move3(King, "e8", "d7") # Invalid!
game <- make_move3(Pawn, "e7", "f6")

x <- all_possibilities() # se guardi, ora è turno del b, e qui la pin correction non c'è, il N_c6 ha diverse mosse possibili
pinned_piece() # qui c'è pin correction, N_c6 non ha mosse