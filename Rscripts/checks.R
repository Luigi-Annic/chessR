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
