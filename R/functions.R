clean_names <- function(x){
  names(x) <- gsub("_", " ", names(x), fixed = TRUE)
  return(x)
}