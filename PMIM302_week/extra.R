for (i in 1:nrow(vaslinks3)){
  
  if (!is.na(vaslinks3$proc1) & 
      vaslinks3$yearsep < 1988 & 
      vaslinks3$proc1 %in% c(56.36, 59.81)){
    vaslinks3$tag[i] <- 1
    
  } else if (!is.na(vaslinks3$proc1) & 
             vaslinks3$yearsep < 1988 & 
             vaslinks3$proc1 %in% c(56.34, 56.37)) {
    vaslinks3$tag[i] <- 2
    
  } else if (!is.na(vaslinks3$proc2) & 
             vaslinks3$yearsep < 1988 & 
             vaslinks3$proc1 %in% c(56.36, 59.81)){
    vaslinks3$tag[i] <- 1
    
  } else if (!is.na(vaslinks3$proc2) & 
             vaslinks3$yearsep < 1988 & 
             vaslinks3$proc1 %in% c(56.34, 56.37)) {
    vaslinks3$tag[i] <- 2
    
  } else if (!is.na(vaslinks3$proc3) & 
             vaslinks3$yearsep < 1988 & 
             vaslinks3$proc1 %in% c(56.36, 59.81)){
    vaslinks3$tag[i] <- 1
    
  } else if (!is.na(vaslinks3$proc3) & 
             vaslinks3$yearsep < 1988 & 
             vaslinks3$proc1 %in% c(56.34, 56.37)) {
    vaslinks3$tag[i] <- 2
    
  } else if (!is.na(vaslinks3$proc1) & 
             vaslinks3$yearsep >= 1988 & 
             (vaslinks3$proc1 >= 63.70 & vaslinks3$proc1 <= 63.79)){
    vaslinks3$tag[i] <- 1
    
  } else if (!is.na(vaslinks3$proc1) & 
             vaslinks3$yearsep >= 1988 & 
             (vaslinks3$proc1 >= 63.80 & vaslinks3$proc1 <= 63.89)){
    vaslinks3$tag[i] <- 2
    
  } else if (!is.na(vaslinks3$proc2) & 
             vaslinks3$yearsep >= 1988 & 
             (vaslinks3$proc2 >= 63.70 & vaslinks3$proc2 <= 63.79)){
    vaslinks3$tag[i] <- 1
    
  } else if (!is.na(vaslinks3$proc2) & 
             vaslinks3$yearsep >= 1988 & 
             (vaslinks3$proc2 >= 63.80 & vaslinks3$proc2 <= 63.89)){
    vaslinks3$tag[i] <- 2
    
  } else if (!is.na(vaslinks3$proc3) & 
             vaslinks3$yearsep >= 1988 & 
             (vaslinks3$proc3 >= 63.70 & vaslinks3$proc3 <= 63.79)){
    vaslinks3$tag[i] <- 1
    
  } else if (!is.na(vaslinks3$proc3) & 
             vaslinks3$yearsep >= 1988 & 
             (vaslinks3$proc3 >= 63.80 & vaslinks3$proc3 <= 63.89)){
    vaslinks3$tag[i] <- 2
    
  } 
  
}

bphsurgery <- for (i in 1:nrow(bphsurgery)){
  if (bphsurgery$yearsep[i] < 1988){
    if (any(bphsurgery[i, 12:14] %in% 56.01)) {
      bphsurgery$bphsurg[i] <- 1
    } else if (any(bphsurgery[i, 12:14] %in% seq(56.02, 56.05, 0.01))){
      bphsurgery$bphsurg[i] <- 0
    } 
  } else if (bphsurgery$yearsep[i] >= 1988){
    if (any(bphsurgery[i, 12:14] %in% seq(60.20, 60.29, 0.01))) {
      bphsurgery$bphsurg[i] <- 1
    } else if (any(bphsurgery[i, 12:14] %in% seq(60.30, 60.69, 0.01))){
      bphsurgery$bphsurg[i] <- 0
    } 
  }
}

