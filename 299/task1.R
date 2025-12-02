replace_words <- \(words, sentence) {
  # Split sentence to a vector of words
  split <- strsplit(sentence, " ")[[1]]
  
  # Create function for reduce
  fn <- \(acc, nxt) {
    pat <- paste0(nxt, ".*")
    stringr::str_replace_all(acc, pattern = pat, replacement = nxt)
  }
  
  # Reduce to get s vector
  vec <- purrr::reduce(.x = words, .f = fn, .init = split)
  # Collapse to string
  res <- paste0(vec, collapse = " ")
  res
}

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
