TVHsplit <- function(df, split = c(0.5, 0.25, 0.25),
                     labels = c("T", "V", "H"), iseed = 397){
  #
  set.seed(iseed)
  flags <- sample(labels, size = nrow(df),
                  prob = split, replace = TRUE)
  return(flags)
}