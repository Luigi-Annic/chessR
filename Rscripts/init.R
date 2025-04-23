init <- matrix(data = c("Rb", "Nb", "Bb", "Qb", "Kb", "Bb", "Nb", "Rb",
                        rep("pb", 8),
                        rep("", 32),
                        rep("pw", 8),
                        "Rw", "Nw", "Bw", "Qw", "Kw", "Bw", "Nw", "Rw"),
               nrow = 8, ncol = 8, byrow = TRUE,
               dimnames = list(c(8:1),c(letters[1:8])))




tilenames <- matrix(data= c(
  unlist(lapply(8:1, function(x) paste0("a",x))),
  unlist(lapply(8:1, function(x) paste0("b",x))),
  unlist(lapply(8:1, function(x) paste0("c",x))),
  unlist(lapply(8:1, function(x) paste0("d",x))),
  unlist(lapply(8:1, function(x) paste0("e",x))),
  unlist(lapply(8:1, function(x) paste0("f",x))),
  unlist(lapply(8:1, function(x) paste0("g",x))),
  unlist(lapply(8:1, function(x) paste0("h",x)))),
  nrow = 8, byrow = F)

#all traverses (Rook and queen)
alltravs <- c(split(tilenames, row(tilenames)),
              split(tilenames, col(tilenames)))
names(alltravs) <- c(1:16)

#all diagonals (bishop and queen)
alldiags <- c(split(tilenames, row(tilenames) - col(tilenames)), 
              split(tilenames, row(tilenames) + col(tilenames)))

names(alldiags) <- c(1:30) 

# remove diagonals with one element only
alldiags<- alldiags[which(as.numeric(lapply(1:length(alldiags), function(i) length(alldiags[[i]])))!=1)]
# King moves for each tile

mat.pad = rbind(NA, cbind(NA, tilenames, NA), NA)

ind = 2:(ncol(tilenames) + 1) # row/column indices of the "middle"
neigh = rbind(N  = as.vector(mat.pad[ind - 1, ind    ]),
              NE = as.vector(mat.pad[ind - 1, ind + 1]),
              E  = as.vector(mat.pad[ind    , ind + 1]),
              SE = as.vector(mat.pad[ind + 1, ind + 1]),
              S  = as.vector(mat.pad[ind + 1, ind    ]),
              SW = as.vector(mat.pad[ind + 1, ind - 1]),
              W  = as.vector(mat.pad[ind    , ind - 1]),
              NW = as.vector(mat.pad[ind - 1, ind - 1]))


colnames(neigh) <- as.character(tilenames)

# Knight moves
mat.pad2 = rbind(NA, NA, cbind(NA, NA, tilenames, NA, NA), NA, NA)

ind2 = 3:(ncol(tilenames)+2)
nighty= rbind(NNE = as.vector(mat.pad2[ind2 - 2, ind2 - 1]),
              NEE = as.vector(mat.pad2[ind2 - 1, ind2 - 2]),
              SEE = as.vector(mat.pad2[ind2 - 2, ind2 + 1]),
              SSE = as.vector(mat.pad2[ind2 - 1, ind2 + 2]),
              SSW = as.vector(mat.pad2[ind2 + 1, ind2 - 2]),
              SWW = as.vector(mat.pad2[ind2 + 2, ind2 - 1]),
              NWW = as.vector(mat.pad2[ind2 + 1, ind2 + 2]),
              NNW = as.vector(mat.pad2[ind2 + 2, ind2 + 1]))

colnames(nighty) <- as.character(tilenames)

# pawn
mat.pad3 = rbind(NA, NA, cbind(NA, NA, tilenames,NA, NA), NA, NA)

ind = 3:(ncol(tilenames) + 2) # row/column indices of the "middle"
whitepawns = rbind(
              N  = as.vector(mat.pad3[ind - 1, ind    ]),
              dN = as.vector(mat.pad3[ind - 2, ind    ]),
              NE = as.vector(mat.pad3[ind - 1, ind + 1]),
              NW = as.vector(mat.pad3[ind - 1, ind - 1]))

colnames(whitepawns) <- as.character(tilenames)

whitepawns["dN",] <- ifelse(colnames(whitepawns) %in% c("a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2"),
                            whitepawns["dN",], NA)

wpmoves <- whitepawns[c("N", "dN"),]
wpcaptures <- whitepawns[c("NE", "NW"),]


blackpawns = rbind(S  = as.vector(mat.pad3[ind + 1, ind    ]),
                   dS = as.vector(mat.pad3[ind + 2, ind    ]),
                   SE = as.vector(mat.pad3[ind + 1, ind + 1]),
                   SW = as.vector(mat.pad3[ind + 1, ind - 1]))
  
colnames(blackpawns) <- as.character(tilenames)

blackpawns["dS",] <- ifelse(colnames(blackpawns) %in% c("a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7"),
                            blackpawns["dS",], NA)

bpmoves <- blackpawns[c("S", "dS"),]
bpcaptures <- blackpawns[c("SE", "SW"),]

rm(list = c("ind", "ind2", "mat.pad", "mat.pad2", "mat.pad3"))

# pieces
Rook <- list(label = "R",
             value = 4.5,
             moverange = 8,
             movedirection = c("l"),
             never_moved = TRUE) # to check for castling rights

King <- list(label = "K",
             value = NA,
             moverange = 1,
             movedirection = c("k"),
             never_moved = TRUE) # to check for castling rights

Bishop <- list(label = "B",
               value = 3,
               moverange = 8,
               movedirection = "d")

Queen <- list(label = "Q",
              value = 9,
              moverange = 8,
              movedirection = c("d", "l"))

Knight <- list(label = "N",
               value = 3,
               moverange = 3,
               movedirection = "n") # need to implement knght movement as did with King

Pawn <- list(label = "p",
             value = 3,
             moverange = 1,
             movedirection = "p")

#emptyboard <- matrix(data = rep("", 64),
#                     nrow = 8, ncol = 8, byrow = TRUE,
#                     dimnames = list(as.character(c(8:1)),c(letters[1:8])))

#emptyboard[which(tilenames == "b7")] <- "Bb"
#emptyboard[which(tilenames == "b1")] <- "Kw"
#emptyboard[which(tilenames == "d5")] <- "pw"
#emptyboard[which(tilenames == "e1")] <- "Qw"
#emptyboard[which(tilenames == "d7")] <- "Rb"
#emptyboard[which(tilenames == "c6")] <- "Bw"
#emptyboard[which(tilenames == "b3")] <- "Rw"
#emptyboard[which(tilenames == "c7")] <- "Kb"
#emptyboard[which(tilenames == "e5")] <- "Nb"
#emptyboard[which(tilenames == "e6")] <- "pb"



#board <- emptyboard
#piece <- Rook
#initialposition <- "b7"
#m0 <- alldiags$'8'

# check obstacles for long range pieces (Rook, Bishop, Queen)
check_obstacles <- function(m0, initialposition, board = game$board) {
  occupied_tiles <- c()
  tile_index <- c()
  for (tile in m0){
    if (tile != initialposition & board[which(tilenames == tile)] != "") {
      occupied_tiles <- c(occupied_tiles, tile)
      tile_index <- c(tile_index, unlist(strsplit(tile, ""))[2])
    }
  }
  
  if (length(occupied_tiles) == 0) { # if no occupied tiles along the vector be happy
    m1 <- m0
  } else { # otherwise we need to calculate
  
  init_index <- unlist(strsplit(initialposition, ""))[2]
  
  #if (length(tile_index) > 1 & length(unique(tile_index)) ==1) {
  if (unlist(strsplit(m0, ""))[2] == unlist(strsplit(m0, ""))[4]) {
    #tile_index <- c(tile_index, unlist(strsplit(tile, ""))[1])
    tile_index <- unlist(strsplit(occupied_tiles, ""))[c(TRUE,FALSE)]
    init_index <- unlist(strsplit(initialposition, ""))[1]
  }
  
  # Find the two most proximate occupied squares (if existent)
  greater_than_index <- suppressWarnings(min(tile_index[tile_index>init_index]))
  smaller_than_index <- suppressWarnings(max(tile_index[tile_index<init_index]))
  great_tile <- ifelse(!greater_than_index %in% c(Inf, NA), occupied_tiles[which(tile_index == greater_than_index)],
                  ifelse(unlist(strsplit(m0, ""))[2]<=unlist(strsplit(m0, ""))[4], m0[length(m0)], m0[1]))
  small_tile <- ifelse(!smaller_than_index %in% c(-Inf, NA), occupied_tiles[which(tile_index == smaller_than_index)],
                  ifelse(unlist(strsplit(m0, ""))[2]<=unlist(strsplit(m0, ""))[4], m0[1], m0[length(m0)]))
  
  m1 <- m0[which(m0 == small_tile):which(m0==great_tile)]
  
  # remove tiles with pieces of the same colour if these are the great_tile and small_tile
  if (unlist(strsplit(board[which(tilenames==initialposition)], ""))[2] == unlist(strsplit(board[which(tilenames==great_tile)], ""))[2] &
      board[which(tilenames==great_tile)] != "") {
    m1 <- m1[! m1 ==great_tile]
  }
  if (unlist(strsplit(board[which(tilenames==initialposition)], ""))[2] == unlist(strsplit(board[which(tilenames==small_tile)], ""))[2] &
      board[which(tilenames==small_tile)] != "") {
    m1 <- m1[! m1 ==small_tile]
  }
  
  }
  
  m1 <- m1[! m1 == initialposition] #remove the initial tile as a possible movement
  return(m1)
}


# remove tiles with pieces of the same colour (for King, Knight movement and maybe also pawn)
# King will require additional check for removing tiles where it is not legal to move (controlled by enemy pieces)

check_occupied_tile <- function(m0, initialposition, board = game$board) { 
  m1 <- as.character(m0)
  for (tile in m0) {
  if (unlist(strsplit(board[which(tilenames==initialposition)], ""))[2] == unlist(strsplit(board[which(tilenames==tile)], ""))[2] &
      board[which(tilenames==tile)] != "") {
    m1 <- m1[! m1 == tile]
  }
  }
  return(m1)
}

# Allow for pawn capture in diagonal if enemy piece stands there
check_pawn_capture <- function(initialposition, board = game$board, turn = game$turn) {
  if (turn == 1) pawnmoves <- whitepawns else pawnmoves <- blackpawns
  
  capturecandidates <- as.character(na.omit(pawnmoves[c(3,4), initialposition]))
  c1 <- capturecandidates
  
  for (tile in capturecandidates) {
    if (unlist(strsplit(board[which(tilenames==initialposition)], ""))[2] == unlist(strsplit(board[which(tilenames==tile)], ""))[2] |
        board[which(tilenames==tile)] == "") {
      c1 <- c1[!c1 == tile]
    }
  }
  
  return(c1)
}


# turn tells if it is white turn(1) or black turn (-1)
defmoves <- function(piece, initialposition, turn = 1) {
  moves0 <- c()

  # Rook and Queen move
  if ("l" %in% piece$movedirection) {
    for (l in names(alltravs)) {
      if (initialposition %in% alltravs[[l]]){
        m0 <- alltravs[[l]]
        m1 <- check_obstacles(m0, initialposition)
        moves0 <- c(moves0, m1) 
      }
    }
  }
  
  # Bishop and Queen move
  if ("d" %in% piece$movedirection) {
    for (d in names(alldiags)) {
      if (initialposition %in% alldiags[[d]]){
        m0 <- alldiags[[d]]
        m1 <- check_obstacles(m0, initialposition)
        moves0 <- c(moves0, m1)
      }
    }
  }
  
  # King move
  if ("k" %in% piece$movedirection) {
    m0 <- as.character(na.omit(neigh[, initialposition]))
    moves0 <- check_occupied_tile(m0, initialposition)
  }
  
  # Knight move
  if ("n" %in% piece$movedirection) {
    m0 <- as.character(na.omit(nighty[, initialposition]))
    moves0 <- check_occupied_tile(m0, initialposition)
  }
  
  # Pawn move
  if ("p" %in% piece$movedirection) {
    if (turn == 1) pawnmoves <- whitepawns else pawnmoves <- blackpawns
    m0moves <- as.character(na.omit(pawnmoves[c(1,2), initialposition]))
    moves0a <- check_occupied_tile(m0moves, initialposition)
    
    c1 <- check_pawn_capture(initialposition)
    
    moves0 <- c(moves0a, c1)
  }
  
  return(moves0)
}



#legalmoves <- function(piece, initialposition) {
#  initialcoords <- unlist(strsplit(initialposition, ""))
#  emptyboard[initialcoords[2], initialcoords[1]] <- piece$label
  
#}

#unlist(strsplit(initialposition, ""))

#newgame <- function() {
#  rm(board)
#  board <- init
#  turn <- 1
#}

#make_move <- function(piece, initialposition, finalposition, currentboard = board) {
#  if (finalposition %in% defmoves(piece, initialposition)) {
#    currentboard[which(tilenames == finalposition)] <- currentboard[which(tilenames == initialposition)]
#    currentboard[which(tilenames == initialposition)] <- ""
#  } else {
#    message("Move not valid")
#  }
#  return(currentboard)
#}

#board <- make_move(Pawn, "e2", "e4")
#board <- make_move(Knight, "g8", "f6")
#board <- make_move(Knight, "b1", "c3")
#board <- make_move(Pawn, "d7", "d6")


#' in make_move, the if clause checks that:
#' - that move is allowed by definitions
#' - the piece is indeed in initial position and of rigth color

#make_move <- function(piece, initialposition, finalposition, currentboard = game$board,
#                      turn = game$turn) {
#  if (finalposition %in% defmoves(piece, initialposition, turn = game$turn) & 
#      paste0(piece$label, ifelse(turn == 1, "w", "b")) == game$board[which(tilenames == initialposition)]) { 
#    
#    currentboard[which(tilenames == finalposition)] <- currentboard[which(tilenames == initialposition)]
#    currentboard[which(tilenames == initialposition)] <- ""
#    move <- paste0(piece$label, initialposition, "-", finalposition)
#    history <- c(game$history, move)
#    turn <- ifelse(length(history)%%2 == 0, 1, -1)
#  } else {
#    message("Move not valid")
#    history <- game$history
#  }
#  return(list(board = currentboard, turn = turn, history = history))
#}

#game <- list(board = init,
#             turn = 1,
#             history = c())

#game <- make_move(Pawn, "d2", "d4")
#game <- make_move(Pawn, "f7", "f5")
#game <- make_move(Knight, "b1", "c3")
#game <- make_move(Knight, "g8", "f6")
#game <- make_move(Bishop, "c1", "g5")
#game <- make_move(Pawn, "e7", "e6")
#game <- make_move(Bishop, "g5", "f6")
#game <- make_move(Queen, "d8", "f6") 
#game <- make_move(Queen, "d1", "d3")
#game <- make_move(Bishop, "f8", "b4")
#game <- make_move(Knight, "g1", "f3")
## Still need to
#
#' - define checks and checkmate
#' - en passant
#' - verification for pins (inchiodature) (Lo facciamo verificando dopo la mossa e prima di passare il turno
#'    che il re non sia in scacco con kingcheck: se il re dopo aver mosso risulta essere in scacco vuol dire che abbiamo
#'    mosso un pezzo inchiodato e non va bene)


# finds all legal moves
all_possibilities <- function() {
  
  legalmoves <- list()
  
  for (j in (1 : length(game$board))) {
    if (game$board[j] != "") {
      pl <- unlist(strsplit(game$board[j], ""))[1]
      
    if (pl == "K") piece <- King
    if (pl == "Q") piece <- Queen
    if (pl == "R") piece <- Rook
    if (pl == "B") piece <- Bishop
    if (pl == "N") piece <- Knight
    if (pl == "p") piece <- Pawn
    
    turn <- ifelse(unlist(strsplit(game$board[j], ""))[2] == "w", 1, -1)
    mv0 <- defmoves(piece, initialposition = tilenames[j], turn)
    
    legalmoves[[unlist(strsplit(game$board[j], ""))[2]]][[paste0(game$board[j], "_", tilenames[j])]] <- mv0
    }
  }
  
  return(legalmoves)
}


castling <- function(side, currentboard = game$board) {
  
  castlingrow <- ifelse(game$turn == 1, "1", "8")
  enemy <- ifelse(game$turn == 1, "b", "w")
  enemy_moves <- all_possibilities()[[enemy]]
  
  if (side %in% c("0-0", "O-O")) {
    
    if (!grepl(paste0("e", castlingrow), paste0(game$history, collapse = "_"), fixed = T) &
        !grepl(paste0("h", castlingrow), paste0(game$history, collapse = "_"), fixed = T) &
        !(paste0("e", castlingrow) %in% unique(Reduce(c, enemy_moves))) &
        !(paste0("f", castlingrow) %in% unique(Reduce(c, enemy_moves))) &
        !(paste0("g", castlingrow) %in% unique(Reduce(c, enemy_moves))) &
        currentboard[tilenames == paste0("f", castlingrow)] == "" &
        currentboard[tilenames == paste0("g", castlingrow)] == ""
        ) {
      
      currentboard[which(tilenames == paste0("g", castlingrow))] <- currentboard[which(tilenames == paste0("e", castlingrow))]
      currentboard[which(tilenames == paste0("f", castlingrow))] <- currentboard[which(tilenames == paste0("h", castlingrow))]
      
      currentboard[which(tilenames == paste0("e", castlingrow))] <- ""
      currentboard[which(tilenames == paste0("h", castlingrow))] <- ""
      
      move <- paste0("Ke", castlingrow, "_0-0")
      history <- c(game$history, move)
      turn <- ifelse(length(history)%%2 == 0, 1, -1)
    } else {
      message("Castle not allowed")
      history <- game$history
    }
  }
  
  if (side %in% c("0-0-0", "O-O-O")) {
    
    if (!grepl(paste0("e", castlingrow), paste0(game$history, collapse = "_"), fixed = T) &
        !grepl(paste0("a", castlingrow), paste0(game$history, collapse = "_"), fixed = T) &
        !(paste0("c", castlingrow) %in% unique(Reduce(c, enemy_moves))) &
        !(paste0("d", castlingrow) %in% unique(Reduce(c, enemy_moves))) &
        !(paste0("e", castlingrow) %in% unique(Reduce(c, enemy_moves))) &
        currentboard[tilenames == paste0("b", castlingrow)] == "" &
        currentboard[tilenames == paste0("c", castlingrow)] == "" &
        currentboard[tilenames == paste0("d", castlingrow)] == ""
    ) {
      
      currentboard[which(tilenames == paste0("c", castlingrow))] <- currentboard[which(tilenames == paste0("e", castlingrow))]
      currentboard[which(tilenames == paste0("d", castlingrow))] <- currentboard[which(tilenames == paste0("a", castlingrow))]
      
      currentboard[which(tilenames == paste0("a", castlingrow))] <- ""
      currentboard[which(tilenames == paste0("e", castlingrow))] <- ""
      
      move <- paste0("Ke", castlingrow, "_0-0-0")
      history <- c(game$history, move)
      turn <- ifelse(length(history)%%2 == 0, 1, -1)
    } else {
      message("Castle not allowed")
      history <- game$history
    }
  }
  return(list(board = currentboard, turn = turn, history = history))
}


make_move2 <- function(piece, initialposition = "", finalposition = "", currentboard = game$board,
                      turn = game$turn) {
  
  if (initialposition %in% c("0-0", "O-O", "0-0-0", "O-O-O")) {
    
    castled <- castling(initialposition)
    
    currentboard <- castled$board
    turn <- castled$turn
    history <- castled$history
    
  } else if (finalposition %in% defmoves(piece, initialposition, turn = game$turn) & 
      paste0(piece$label, ifelse(turn == 1, "w", "b")) == game$board[which(tilenames == initialposition)]) { 
    
    currentboard[which(tilenames == finalposition)] <- currentboard[which(tilenames == initialposition)]
    currentboard[which(tilenames == initialposition)] <- ""
    
    # promotion of pawns in 1st/8th row (always to queen for now)
    if (piece$label == "p" & unlist(strsplit(finalposition, ""))[2] %in% c(1,8)) {
      currentboard[which(tilenames == finalposition)] <- paste0("Q", ifelse(turn == 1, "w", "b"))
    }
    
    #move <- paste0(piece$label, initialposition, "-", finalposition)
    move <- if (game$board[tilenames == finalposition] == "") {
      paste0(piece$label, initialposition, "-", finalposition)
    } else {
      paste0(piece$label, initialposition, "x", finalposition)
    }
    
    if (piece$label == "p" & unlist(strsplit(finalposition, ""))[2] %in% c(1,8)) move <-paste0(move, "=Q")
  
    
    history <- c(game$history, move)
    turn <- ifelse(length(history)%%2 == 0, 1, -1)
  } else {
    message("Move not valid")
    history <- game$history
  }
  return(list(board = currentboard, turn = turn, history = history))
}


newgame <- function(){
  list(board = init,
       turn = 1,
       history = c())
}

game <- newgame()

game <- make_move2(Pawn, "d2", "d4")
game <- make_move2(Pawn, "f7", "f5")
game <- make_move2(Knight, "b1", "c3")
game <- make_move2(Knight, "g8", "f6")
game <- make_move2(Bishop, "c1", "g5")
game <- make_move2(Pawn, "e7", "e6")
game <- make_move2(Bishop, "g5", "f6")
game <- make_move2(Queen, "d8", "f6") 
game <- make_move2(Queen, "d1", "d3")
game <- make_move2(Bishop, "f8", "b4")
game <- make_move2(Knight, "g1", "f3")
game <- make_move2(King, "0-0")
game <- make_move2(King, "0-0-0")

game <- make_move2(Pawn, "f5", "f4")
game <- make_move2(Pawn, "g2", "g3")
game <- make_move2(Pawn, "f4", "g3")
game <- make_move2(King, "c1", "b1")
game <- make_move2(Pawn, "g3", "g2")
game <- make_move2(Pawn, "h2", "h4")

game <- make_move2(Pawn, "g2", "h1")
game <- make_move2(Pawn, "h4", "h5")
game <- make_move2(Queen, "h1", "h5")

###
game <- newgame()

game <- make_move2(Knight, "g1", "f3")
game <- make_move2(Pawn, "d7", "d5")
game <- make_move2(Rook, "h1", "g1")
game <- make_move2(Knight, "g8", "f6")
game <- make_move2(Pawn, "g2", "g3")
game <- make_move2(Bishop, "c8", "f5")
game <- make_move2(Bishop, "f1", "g2")
game <- make_move2(Pawn, "e7", "e6")
game <- make_move2(Rook, "g1", "h1")
game <- make_move2(Bishop, "f8", "e7")
game <- make_move2(King, "e1","f1")
game <- make_move2(King, "0-0")

# Creates move scorelist, in beautiful notation
# Note that shortnotation might incur in lack of clarity when two moves are possible
# e.g. when Knight on b3, other Knight on f3, and short move listed as Nd4
# Anyway, the scorelist saved in history also has the starting square so you
# can look at it in case of doubt
moves_scoresheet <- function(h = game$history, shortnotation = TRUE){
  if (shortnotation == TRUE) {
  h_orig <- h_alt<- h  
  substr(h, 2,4) <- "  "
  h <- gsub(" ", "", h)
  h <- gsub("p|_|-", "", h)
  h <- gsub("000", "0-0-0", h)
  h <- gsub("00", "0-0", h)
  h <- gsub("K0-0", "0-0", h)
  
  substr(h_alt, 1,1) <- substr(h_alt, 3,3) <- " "
  h_alt <- gsub(" ", "", h_alt)
  
  hfin <- ifelse(paste0(substr(h_orig,1,1), substr(h_orig,4,4)) == "px", h_alt ,h)
  } else hfin = h
  
  alt <- unlist(lapply(1:length(h), function(j) ifelse(j %%2 == 0, -1, 1)))
  
  data.frame(n = 1:ceiling(length(h)/2),
             white = hfin[alt == 1],
             black = if (length(h)%%2 == 0) hfin[alt == -1] else c(h[alt == -1], ""))
}

# Kingcheck function returns all pieces giving check to the king
kingcheck <- function(currentboard = game$board, turn = game$turn){
  checkinglines <- list()
  myself <- ifelse(turn == 1, "w", "b")
  enemy <- ifelse(game$turn == 1, "b", "w")
  enemy_moves <- all_possibilities()[[enemy]]
  
  mykingposition <- tilenames[which(currentboard == paste0("K", myself) )]
  
  for (j in names(enemy_moves)) {
    if (mykingposition %in% enemy_moves[[j]]) {
      checkinglines[[j]] <- enemy_moves[[j]]
    }
  }
  
  return(checkinglines)
}

# 
# Trovare linea di attacco in caso di attacco dalla distanza di torre o alfiere o donna
# prima dobbiamo trovare quale è la linea di attacco (diagonale o traversa), perche grazie
# a kingcheck() sappiamo solo quale pezzo dà lo scacco e quali case controlla

parrycheck <- function(currentboard = game$board, turn = game$turn) {

myself <- ifelse(turn == 1, "w", "b")
mykingposition <- tilenames[which(currentboard == paste0("K", myself) )]

checking_item <- names(kingcheck())
checking_tile <- substr(checking_item, 4,5)

# diagonals
for (j in 1 : length(alldiags)) {
 if (checking_tile %in% alldiags[[j]] & mykingposition %in% alldiags[[j]]) checkline <- alldiags[[j]] 
}

# rows/columns
for (j in 1 : length(alltravs)) {
  if (checking_tile %in% alltravs[[j]] & mykingposition %in% alltravs[[j]]) checkline <- alltravs[[j]] 
}

# eliminiamo caselle esterne alle caselle di attacco e del re, e la casella del re (in cui ovviamente non
# possiamo interporre)
checkline <- checkline[which(checkline == checking_tile):which(checkline == mykingposition)]
checkline <- setdiff(checkline, c(mykingposition, checking_tile)) # If we manage to put a piece into the line of fire check is parred

# Troviamo tutte le mosse che si frappongano nelle caselle trovate in checkline
# Ovviamente escludiamo il re dai pezzi paratori :)
checkparry <- list()
for (j in 1:length(names(all_possibilities()[[myself]]))) {
  if (length(intersect(all_possibilities()[[myself]][[j]], checkline)) > 0 & 
      substr(names(all_possibilities()[[myself]])[[j]], 1,1) != "K") {
    checkparry[[names(all_possibilities()[[myself]])[[j]]]] <- intersect(all_possibilities()[[myself]][[j]], checkline)
  } 
}

return(checkparry)
}

parrycheck()

## Escape with the king
# Questa funzione serve quale che sia il tipo di scacco (da una o due linee, o da cavallo/attacco ravvicinato, o da lontano)
escapecheck <- function(currentboard = game$board, turn = game$turn) {
  
  escapes <- list()
  
  myself <- ifelse(turn == 1, "w", "b")
  mykingposition <- tilenames[which(currentboard == paste0("K", myself) )]
  enemy <- ifelse(game$turn == 1, "b", "w")
  enemy_moves <- all_possibilities()[[enemy]]
  
  available_squares <- defmoves(King, initialposition = mykingposition, turn)
  
  escapes[[paste0("K", myself, "_", mykingposition)]] <-subset(available_squares, !available_squares %in% unique(Reduce(c, enemy_moves)))
  
  return(escapes)
}


## Remove attacking piece 
# Questa è da applicare in tutti i casi in cui c'è una sola linea di attacco (in caso di scacco doppio non serve)

removeattacker <- function(currentboard = game$board, turn = game$turn) {
  
  eaters <- list()
  
  if (length(names(kingcheck())) == 1) {
    
    checking_item <- names(kingcheck())
    checking_tile <- substr(checking_item, 4,5)
    
    myself <- ifelse(turn == 1, "w", "b")

    for (j in names(all_possibilities()[[myself]])) {
      if (checking_tile %in% all_possibilities()[[myself]][[j]]) eaters[[j]] <-  checking_tile
    }
    
    return(eaters)
  }
}






make_move3 <- function(piece, initialposition = "", finalposition = "", currentboard = game$board,
                       turn = game$turn) {
  
  if (length(names(kingcheck())) >0) { # what you need to do if you are in check
    parries <- parrycheck()
    escapes <- escapecheck()
    eaters  <- removeattacker()
    myself <- ifelse(turn == 1, "w", "b")
    
    
    keys <- unique(c(names(escapes), names(eaters), names(parries)))
    defendcheckmoves <- setNames(mapply(c, escapes[keys], eaters[keys], parries[keys]), keys)
    
    checkdefender <- paste0(piece$label, myself, "_", initialposition)
    
    if (finalposition %in% defendcheckmoves[[checkdefender]]) {
      
      currentboard[which(tilenames == finalposition)] <- currentboard[which(tilenames == initialposition)]
      currentboard[which(tilenames == initialposition)] <- ""
      
      # promotion of pawns in 1st/8th row (always to queen for now)
      if (piece$label == "p" & unlist(strsplit(finalposition, ""))[2] %in% c(1,8)) {
        currentboard[which(tilenames == finalposition)] <- paste0("Q", ifelse(turn == 1, "w", "b"))
      }
      
      #move <- paste0(piece$label, initialposition, "-", finalposition)
      move <- if (game$board[tilenames == finalposition] == "") {
        paste0(piece$label, initialposition, "-", finalposition)
      } else {
        paste0(piece$label, initialposition, "x", finalposition)
      }
      
      if (piece$label == "p" & unlist(strsplit(finalposition, ""))[2] %in% c(1,8)) move <-paste0(move, "=Q")
      
      
      history <- c(game$history, move)
      turn <- ifelse(length(history)%%2 == 0, 1, -1)
    } else {
      message("Invalid move, your king is in check!")
      history <- game$history
      
    }
  } else if (initialposition %in% c("0-0", "O-O", "0-0-0", "O-O-O")) { # if you are not in check, you may want to castle
    
    castled <- castling(initialposition)
    
    currentboard <- castled$board
    turn <- castled$turn
    history <- castled$history
    
  } else if (finalposition %in% defmoves(piece, initialposition, turn = game$turn) & 
             paste0(piece$label, ifelse(turn == 1, "w", "b")) == game$board[which(tilenames == initialposition)]) { # or any other move!
    
    currentboard[which(tilenames == finalposition)] <- currentboard[which(tilenames == initialposition)]
    currentboard[which(tilenames == initialposition)] <- ""
    
    # promotion of pawns in 1st/8th row (always to queen for now)
    if (piece$label == "p" & unlist(strsplit(finalposition, ""))[2] %in% c(1,8)) {
      currentboard[which(tilenames == finalposition)] <- paste0("Q", ifelse(turn == 1, "w", "b"))
    }
    
    #move <- paste0(piece$label, initialposition, "-", finalposition)
    move <- if (game$board[tilenames == finalposition] == "") {
      paste0(piece$label, initialposition, "-", finalposition)
    } else {
      paste0(piece$label, initialposition, "x", finalposition)
    }
    
    if (piece$label == "p" & unlist(strsplit(finalposition, ""))[2] %in% c(1,8)) move <-paste0(move, "=Q")
    
    
    history <- c(game$history, move)
    turn <- ifelse(length(history)%%2 == 0, 1, -1)
  } else {
    message("Move not valid")
    history <- game$history
  }
  return(list(board = currentboard, turn = turn, history = history))
}
