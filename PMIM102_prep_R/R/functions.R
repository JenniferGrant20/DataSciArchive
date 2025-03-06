library(crayon)

redtext <- function(thing_to_print){
  cat(bgRed(thing_to_print))
}

greentext <- function(thing_to_print){
  cat(bgGreen(thing_to_print))
}
