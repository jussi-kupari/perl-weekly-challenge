# This first version becomes very slow at n >= 10

library(tidyverse)

# Generate permutations
create_permutations <- function(n) {
  perms <- gtools::permutations(n = n, r = n, v = 1:n)
  as.data.frame(perms)
}

# Check if number and index is a beautiful combo
is_beautiful_combo <- function(x, i) {
  cond1 <- x %% i == 0
  cond2 <- i %% x == 0
  cond1 || cond2
}

# Is the permutation beautiful?
# This checks all the element-index pairs of the permutation, so slow
is_beautiful <- function(...) {
  vals <- as.numeric(c(...))
  all(imap_lgl(vals, is_beautiful_combo))
}

perms <- create_permutations(10)

perms %>% 
  mutate(beautiful = purrr::pmap_lgl(., is_beautiful)) %>%  
  summarise(sum(beautiful))



# --- Vectorized version that avoids imap_lgl() and uses apply() for speed:

# Generate permutations
create_permutations <- function(n) {
  perms <- gtools::permutations(n = n, r = n, v = 1:n)
  as.data.frame(perms)
}

# Vectorized beautiful check
# This returns FALSE on the first failing element-index pair, faster
is_beautiful_vec <- function(row) {
  i <- seq_along(row)
  all((row %% i == 0) | (i %% row == 0))
}

# --- TESTING

# Slow version
perms %>% 
  mutate(beautiful = purrr::pmap_lgl(., is_beautiful)) %>%  
  summarise(sum(beautiful))

# [1] 700


# Faster version

# Apply the check to each row
perms$beautiful <- apply(perms, 1, is_beautiful_vec)

# Count beautiful permutations
sum(perms$beautiful)

# [1] 700


