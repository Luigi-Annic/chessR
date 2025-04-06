init <- matrix(data = c("Rw", "Nw", "Bw", "Qw", "Kw", "Bw", "Nw", "Rw",
                        rep("pw", 8),
                        rep("", 32),
                        rep("pb", 8),
                        "Rb", "Nb", "Bb", "Qb", "Kb", "Bb", "Nb", "Rb"),
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
               movedirectio = "n") # need to implement knght movement as did with King

emptyboard <- matrix(data = rep("", 64),
                     nrow = 8, ncol = 8, byrow = TRUE,
                     dimnames = list(as.character(c(8:1)),c(letters[1:8])))

emptyboard[initialcoords[2], initialcoords[1]] <- "Rw"
emptyboard[which(tilenames == "b7")] <- "Bb"
emptyboard[which(tilenames == "b1")] <- "Kw"
emptyboard[which(tilenames == "d5")] <- "pw"
emptyboard[which(tilenames == "e1")] <- "Qw"
emptyboard[which(tilenames == "d7")] <- "Rb"
emptyboard[which(tilenames == "c6")] <- "Bw"
emptyboard[which(tilenames == "b3")] <- "Rw"


board <- emptyboard
piece <- Rook
initialposition <- "b7"
m0 <- alldiags$'8'

check_obstacles <- function(m0, initialposition) {
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

#the board needs to be defined
defmoves <- function(piece, initialposition) {
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
    moves0 <- as.character(na.omit(neigh[, initialposition]))
  }
  
  return(moves0)
}



#legalmoves <- function(piece, initialposition) {
#  initialcoords <- unlist(strsplit(initialposition, ""))
#  emptyboard[initialcoords[2], initialcoords[1]] <- piece$label
  
#}

#unlist(strsplit(initialposition, ""))


