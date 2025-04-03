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

#all traverses
alltravs <- c(split(tilenames, row(tilenames)),
              split(tilenames, col(tilenames)))
names(alltravs) <- c(1:16)
#all diagonals
alldiags <- c(split(tilenames, row(tilenames) - col(tilenames)), 
              split(tilenames, row(tilenames) + col(tilenames)))

names(alldiags) <- c(1:30)

#for (d in names(alldiags)) {
#  if ("b3" %in% alldiags[[d]]) print(alldiags[[d]])
#}

Rook <- list(label = "R",
             value = 4.5,
             moverange = 8,
             movedirection = c("l"),
             move_as_capture = TRUE)

King <- list(label = "K",
             value = NA,
             moverange = 1,
             movedirection = c("l", "d"),
             move_as_capture = TRUE)

Bishop <- list(label = "B",
               value = 3,
               moverange = 8,
               movedirection = "d",
               move_as_capture = TRUE)

emptyboard <- matrix(data = rep("", 64),
                     nrow = 8, ncol = 8, byrow = TRUE,
                     dimnames = list(as.character(c(8:1)),c(letters[1:8])))

piece <- Bishop
initialposition <- "b3"

defmoves <- function(piece, initialposition) {
  moves0 <- c()
  #X <- unlist(strsplit(initialposition, ""))[1]
  #Y <- unlist(strsplit(initialposition, ""))[2]
  if ("l" %in% piece$movedirection) {
    #  moves0 <- c(unlist(lapply(1:8, function(y) paste0(X, y))),
    #             unlist(lapply(letters[1:8], function(x) paste0(x, Y))))
    for (l in names(alltravs)) {
      if (initialposition %in% alltravs[[l]]){
        m0 <- alltravs[[d]]
        moves0 <- c(moves0, m0) 
      }
    }
  }
  
  if ("d" %in% piece$movedirection) {
    
    for (d in names(alldiags)) {
      if (initialposition %in% alldiags[[d]]){
        m0 <- alldiags[[d]]
        moves0 <- c(moves0, m0)
      }
    }
  }
  
}



legalmoves <- function(piece, initialposition) {
  initialcoords <- unlist(strsplit(initialposition, ""))
  emptyboard[initialcoords[2], initialcoords[1]] <- piece$label
  
}

unlist(strsplit(initialposition, ""))


