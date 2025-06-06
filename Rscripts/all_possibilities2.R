# allpossibilities() alternative method: tutti i controlli si basano su all_possibilities, che ha al suo
#' interno controllo per inchiodature  escacchi al re. Basta controllare dentro all_possibilities() per 
#' valutare se la mossa è legale o no senza distinguere tra scacchi e altro (tranne arrocco per il momento)

# Kingcheck function returns all pieces giving check to the king
kingcheck <- function(currentboard = game$board, turn = game$turn, legalmoves){
  checkinglines <- list()
  myself <- ifelse(turn == 1, "w", "b")
  enemy <- ifelse(game$turn == 1, "b", "w")
  enemy_moves = legalmoves[[enemy]]
  
  mykingposition <- tilenames[which(currentboard == paste0("K", myself) )]
  
  for (j in names(enemy_moves)) {
    if (mykingposition %in% enemy_moves[[j]]) {
      checkinglines[[j]] <- enemy_moves[[j]]
    }
  }
  
  return(checkinglines)
}

parrycheck <- function(currentboard = game$board, turn = game$turn,  legalmoves) {
  
  myself <- ifelse(turn == 1, "w", "b")
  mymoves =legalmoves[[myself]]
  mykingposition <- tilenames[which(currentboard == paste0("K", myself) )]
  
  checking_item <- names(kingcheck(legalmoves = legalmoves))
  checking_tile <- substr(checking_item, 4,5)
  
  if (substr(checking_item,1,1) %in% c("B", "Q", "R") & length(checking_item) == 1) {
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
    for (j in 1:length(names(mymoves))) {
      if (length(intersect(mymoves[[j]], checkline)) > 0 & 
          substr(names(mymoves)[[j]], 1,1) != "K") {
        checkparry[[names(mymoves)[[j]]]] <- intersect(mymoves[[j]], checkline)
      } 
    }
    
  } else checkparry <- list()
  
  
  return(checkparry)
}



escapecheck <- function(currentboard = game$board, turn = game$turn, legalmoves) {
  
  escapes <- list()
  
  myself <- ifelse(turn == 1, "w", "b")
  mykingposition <- tilenames[which(currentboard == paste0("K", myself) )]
  enemy <- ifelse(game$turn == 1, "b", "w")
  
  enemy_moves = legalmoves[[enemy]]
  
  available_squares <- defmoves(King, initialposition = mykingposition, turn)
  
  escapes[[paste0("K", myself, "_", mykingposition)]] <-subset(available_squares, !available_squares %in% unique(Reduce(c, enemy_moves)))
  
  return(escapes)
}


removeattacker <- function(currentboard = game$board, turn = game$turn, legalmoves) {
  
  eaters <- list()
  
  if (length(names(kingcheck(legalmoves = legalmoves))) == 1) {
    
    checking_item <- names(kingcheck(legalmoves = legalmoves))
    checking_tile <- substr(checking_item, 4,5)
    
    myself <- ifelse(turn == 1, "w", "b")
    mymoves = legalmoves[[myself]]
    
    for (j in names(mymoves)) {
      if (checking_tile %in% mymoves[[j]] &
          !(substr(j, 1,1) == "p" & substr(j, 4,4) == substr(checking_tile,1,1)) # necessario perche i pedoni non mangiano dritti!
          ) eaters[[j]] <-  checking_tile
    }
    
    return(eaters)
  }
}






pinned_piece2 <- function(currentboard = game$board, turn = game$turn, legalmoves){
  checkinglines <- list()
  myself <- ifelse(turn == 1, "w", "b")
  enemy <- ifelse(game$turn == 1, "b", "w")
  mykingposition <- tilenames[which(currentboard == paste0("K", myself) )]
  #mymoves <- all_possibilities()[[myself]] # this will be the modified object, excluding unplayable moves due to pins
  mymoves <- legalmoves[[myself]]
  potential_checklines0 <- list()
  
  x <- 1
  # rows/columns
  for (j in 1 : length(alltravs)) {
    if (mykingposition %in% alltravs[[j]]) {
      
      full_checklines <- alltravs[[j]]
      potential_checklines0[["trav"]][[x]] <- full_checklines[1:which(full_checklines == mykingposition)]
      potential_checklines0[["trav"]][[x+1]] <- full_checklines[which(full_checklines == mykingposition):length(full_checklines)]
      
      x <- x+2
    }
  }
  
  x<- 1
  
  # diagonals
  for (j in 1 : length(alldiags)) {
    if (mykingposition %in% alldiags[[j]]) {
      full_checklines <- alldiags[[j]]
      potential_checklines0[["diag"]][[x]] <- full_checklines[1:which(full_checklines == mykingposition)]
      potential_checklines0[["diag"]][[x+1]] <- full_checklines[which(full_checklines == mykingposition):length(full_checklines)]
      
      x <- x+2
    } 
  }
  
  # cancella eventuali semitraverse e semidiagonali con solo la casella del re
  potential_checklines0b_t <- setdiff(potential_checklines0$trav, mykingposition)
  
  #potential_checklines_t <- lapply(1: length(potential_checklines0b_t),
  #                               function(x) setdiff(potential_checklines0b_t[[x]], mykingposition))
  
  potential_checklines0b_diag <- setdiff(potential_checklines0$diag, mykingposition)
  
  #potential_checklines_diag <- lapply(1: length(potential_checklines0b_diag),
  #                                 function(x) setdiff(potential_checklines0b_diag[[x]], mykingposition))
  
  
  pc0 <- list(rowcol = potential_checklines0b_t,
              diags = potential_checklines0b_diag)
  
  # Mi assicuro che in pc ogni stringa abbia mykingposition in unltima casella, tornera utile per le regular expressions
  pc <- pc0
  for (k in names(pc0)) {
    for (w in 1:length(pc0[[k]])) {
      pc[[k]][[w]] <- if (pc0[[k]][[w]][1] == mykingposition) pc0[[k]][[w]][length(pc0[[k]][[w]]):1] else pc0[[k]][[w]]
    }
  }
  
  # Cerco inchiodature in riga
  for (j in 1:length(pc$rowcol)) {
    set <- character(length = length(pc$rowcol[[j]]))
    x <- 1
    for (tile in pc$rowcol[[j]]) {
      set[x] <- currentboard[which(tile == tilenames)]
      x <- x+1
    }
    set_collapsed <- paste0(set, collapse = "")
    
    if (grepl(paste0("(Q|R)", enemy, "(p|N|B|R|Q)", myself, "K", myself), set_collapsed)) {
      
      matching <- regmatches(set_collapsed, gregexpr(paste0("(Q|R)", enemy, "(p|N|B|R|Q)", myself, "K", myself), set_collapsed))
      pinned_piece <- substr(matching,3,4)
      
      pinner <- substr(matching , 1, 2)
      pinner_tile <- pc$rowcol[[j]][max(which(pinner == set))]
      # ora devo ri mappare il pinned_piece per scoprire in che casella fosse
      
      pinned_tile <- pc$rowcol[[j]][max(which(pinned_piece == set))] # max gets the rightmost piece (the nearest to the king), utile se ci sono piu pezzi con lo stesso nome
      
      mymoves[[paste0(pinned_piece, "_", pinned_tile)]] <- intersect(mymoves[[paste0(pinned_piece, "_", pinned_tile)]],
                                                                     pc$rowcol[[j]][which(pc$rowcol[[j]] == pinner_tile):(length(pc$rowcol[[j]])-1)]) 
      # serve perche un alfiere inchiodato sulla diagonale può muoversi dalla casella del pezzo inchiodante alla casella prima del re
    }
  }
  
  # cerco inchiodature in colonna
  for (j in 1:length(pc$diags)) {
    
    set <- character(length = length(pc$diags[[j]]))
    x <- 1
    for (tile in pc$diags[[j]]) {
      set[x] <- currentboard[which(tile == tilenames)]
      x <- x+1
    }
    set_collapsed <- paste0(set, collapse = "")
    
    if (grepl(paste0("(Q|B)", enemy, "(p|N|B|R|Q)", myself, "K", myself), set_collapsed)) {
      
      matching <- regmatches(set_collapsed, gregexpr(paste0("(Q|B)", enemy, "(p|N|B|R|Q)", myself, "K", myself), set_collapsed))
      pinned_piece <- substr(matching,3,4)
      
      pinner <- substr(matching , 1, 2)
      pinner_tile <- pc$diags[[j]][max(which(pinner == set))]
      # ora devo ri mappare il pinned_piece per scoprire in che casella fosse
      
      pinned_tile <- pc$diags[[j]][max(which(pinned_piece == set))] # max gets the rightmost piece (the nearest to the king), utile se ci sono piu pezzi con lo stesso nome
      
      mymoves[[paste0(pinned_piece, "_", pinned_tile)]] <- intersect(mymoves[[paste0(pinned_piece, "_", pinned_tile)]],
                                                                     pc$diags[[j]][which(pc$diags[[j]] == pinner_tile):(length(pc$diags[[j]])-1)])
      
    }
  }
  
  
  return(mymoves)
  
}



# finds all legal moves
all_possibilities <- function(currentboard = game$board) {
  
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
  
  # Now implement pinned piece restriction
  myself <- ifelse(game$turn == 1, "w", "b")
  enemy <- ifelse(game$turn == 1, "b", "w")
  legalmoves[[myself]] <- pinned_piece2(legalmoves = legalmoves)
  
  # Our king can only go in squares which are not controlled by the enemy!
  mykingposition <- tilenames[which(currentboard == paste0("K", myself) )]
  
  available_K_squares <- legalmoves[[myself]][[paste0("K", myself, "_", mykingposition)]]
  legalmoves[[myself]][[paste0("K", myself, "_", mykingposition)]] <- subset(available_K_squares, !available_K_squares %in% unique(Reduce(c, legalmoves[[enemy]])))
  
  # If castle is available, add it as a legal move:
  castlingrow <- ifelse(game$turn == 1, "1", "8")
  
  # short castle
  if (!grepl(paste0("e", castlingrow), paste0(game$history, collapse = "_"), fixed = T) &
      !grepl(paste0("h", castlingrow), paste0(game$history, collapse = "_"), fixed = T) &
      !(paste0("e", castlingrow) %in% unique(Reduce(c, legalmoves[[enemy]]))) &
      !(paste0("f", castlingrow) %in% unique(Reduce(c, legalmoves[[enemy]]))) &
      !(paste0("g", castlingrow) %in% unique(Reduce(c, legalmoves[[enemy]]))) &
      currentboard[tilenames == paste0("f", castlingrow)] == "" &
      currentboard[tilenames == paste0("g", castlingrow)] == "") {
    
    legalmoves[[myself]][[paste0("K", myself, "_", mykingposition)]] <- append(legalmoves[[myself]][[paste0("K", myself, "_", mykingposition)]], "0-0")
  }
  
  #long castle
  if (!grepl(paste0("e", castlingrow), paste0(game$history, collapse = "_"), fixed = T) &
      !grepl(paste0("a", castlingrow), paste0(game$history, collapse = "_"), fixed = T) &
      !(paste0("c", castlingrow) %in% unique(Reduce(c, legalmoves[[enemy]]))) &
      !(paste0("d", castlingrow) %in% unique(Reduce(c, legalmoves[[enemy]]))) &
      !(paste0("e", castlingrow) %in% unique(Reduce(c, legalmoves[[enemy]]))) &
      currentboard[tilenames == paste0("b", castlingrow)] == "" &
      currentboard[tilenames == paste0("c", castlingrow)] == "" &
      currentboard[tilenames == paste0("d", castlingrow)] == "") {
    
    legalmoves[[myself]][[paste0("K", myself, "_", mykingposition)]] <- append(legalmoves[[myself]][[paste0("K", myself, "_", mykingposition)]], "0-0-0")
  }
  
  
  # If we are in check, legalomves[[myself]] is overwritten, and this finds the available moves:
  if (length(names(kingcheck(legalmoves = legalmoves))) >0) { # what you need to do if you are in check
    parries <- parrycheck(legalmoves = legalmoves)
    escapes <- escapecheck(legalmoves = legalmoves)
    eaters  <- removeattacker(legalmoves = legalmoves)

    
    keys <- unique(c(names(escapes), names(eaters), names(parries)))
    legalmoves[[myself]] <- as.list(setNames(mapply(c, escapes[keys], eaters[keys], parries[keys]), keys))
  }
  return(legalmoves)
}


###############

make_move4 <- function(piece, initialposition = "", finalposition = "", currentboard = game$board,
                       turn = game$turn) {
  
  myself <- ifelse(game$turn == 1, "w", "b")

  if (finalposition %in% all_possibilities()[[myself]][[paste0(piece$label, myself, "_", initialposition)]]) {
    
    if (finalposition == "0-0") { # if you are not in check, you may want to castle
      castlingrow <- ifelse(game$turn == 1, "1", "8")
      
      currentboard[which(tilenames == paste0("g", castlingrow))] <- currentboard[which(tilenames == paste0("e", castlingrow))]
      currentboard[which(tilenames == paste0("f", castlingrow))] <- currentboard[which(tilenames == paste0("h", castlingrow))]
      
      currentboard[which(tilenames == paste0("e", castlingrow))] <- ""
      currentboard[which(tilenames == paste0("h", castlingrow))] <- ""
      
      move <- paste0("Ke", castlingrow, "_0-0")
      history <- c(game$history, move)
      turn <- ifelse(length(history)%%2 == 0, 1, -1)
      
    } else if (finalposition == "0-0-0") {
      castlingrow <- ifelse(game$turn == 1, "1", "8")
      
      currentboard[which(tilenames == paste0("c", castlingrow))] <- currentboard[which(tilenames == paste0("e", castlingrow))]
      currentboard[which(tilenames == paste0("d", castlingrow))] <- currentboard[which(tilenames == paste0("a", castlingrow))]
      
      currentboard[which(tilenames == paste0("a", castlingrow))] <- ""
      currentboard[which(tilenames == paste0("e", castlingrow))] <- ""
      
      move <- paste0("Ke", castlingrow, "_0-0-0")
      history <- c(game$history, move)
      turn <- ifelse(length(history)%%2 == 0, 1, -1)
      
    } else { # or any other move!
      
      currentboard[which(tilenames == finalposition)] <- currentboard[which(tilenames == initialposition)]
      currentboard[which(tilenames == initialposition)] <- ""
      
      # promotion of pawns in 1st/8th row (always to queen for now)
      if (piece$label == "p" & unlist(strsplit(finalposition, ""))[2] %in% c(1,8)) {
        currentboard[which(tilenames == finalposition)] <- paste0("Q", ifelse(turn == 1, "w", "b"))
      }
      
      # Enforce capture notation if piece is captured (x)
      #move <- paste0(piece$label, initialposition, "-", finalposition)
      move <- if (game$board[tilenames == finalposition] == "") {
        paste0(piece$label, initialposition, "-", finalposition)
      } else {
        paste0(piece$label, initialposition, "x", finalposition)
      }
      
      if (piece$label == "p" & unlist(strsplit(finalposition, ""))[2] %in% c(1,8)) move <-paste0(move, "=Q")
      
      
      history <- c(game$history, move)
      turn <- ifelse(length(history)%%2 == 0, 1, -1)
    }
    
    
  } else {
    message("Move not valid")
    history <- game$history
  }
  
  
  return(list(board = currentboard, turn = turn, history = history))
}

# game ending check

game_result <- function(currentboard = game$board, turn = game$turn) {
  playersturn <- ifelse(turn == 1, "w", "b")
  color <- ifelse(playersturn == "w", "white", "black")
  other <- ifelse(playersturn == "w", "black", "white")
  
  if (length(as.character(unlist(all_possibilities()[[playersturn]]))) == 0) {
    if (length(kingcheck(currentboard = currentboard, turn = turn, legalmoves = all_possibilities(currentboard = currentboard))) > 0) {
      message("Checkmate! End of the game")
      message(paste0(other, " wins"))
    } else {
      message("Stalemate! Draw")
    }
  } else {
    message("There are still legal moves available, the game is ongoing")
    message(paste0("To move: " , color))
  }
  
}

