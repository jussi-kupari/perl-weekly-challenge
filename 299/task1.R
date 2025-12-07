replace_words <- function(words, sentence) {
  # Split sentence to a vector of words
  split <- strsplit(sentence, " ")[[1]]
  
  # Create reducer function
  reducer <- \(acc, nxt) {
    pat <- paste0(nxt, ".*")
    gsub(pattern = pat, replacement = nxt, acc)
  }
  
  # Reduce and collapse to get a vector
  vec <- Reduce(f = reducer, x = words, init = split) 
  res <- paste0(vec, collapse = " ")
  res
}


# -- TEST ---
words <- c("cat", "bat", "rat")
sentence <- "the cattle was rattle by the battery"
replace_words(words, sentence)
# [1] "the cat was rat by the bat"

words2 <- c("a", "b", "c")
sentence2 <- "aab aac and cac bab"
replace_words(words2, sentence2)
# [1] "a a a c b"

words3 <- c("man", "bike")
sentence3 <- "the manager was hit by a biker"
replace_words(words3, sentence3) 
# [1] "the man was hit by a bike"
