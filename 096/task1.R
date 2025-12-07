reverse_words <- function(string) {
  s <- trimws(string)
  
  split <-
    strsplit(s, split = "\\s")[[1]] |>
    grep("\\w", x = _, value = TRUE)
  
  len <- length(split)
  res <- lapply(len:1, \(x) split[x]) |> paste0(collapse = " ")
  res
}

# -- TESTING --

reverse_words("family same the of part are Raku and Perl")
# [1] "Perl and Raku are part of the same family"

reverse_words("The Weekly Challenge")
# [1] "Challenge Weekly The"
