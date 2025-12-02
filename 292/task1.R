twice_largest <- \(vec) {
  # Vector length
  vec_len <- length(vec)
  # Biggest value in vector
  biggest <- sort(vec)[vec_len]
  # Index of biggest value
  biggest_ix <- match(biggest, vec)
  # Vector sorted
  vec_sorted <- sort(vec)
  # Which values in sorted vector are half size of less?
  # Logical vector
  half_or_less <- vec_sorted[1:vec_len - 1] * 2 <= biggest
  # Return index of biggest value if all are small enough
  # If not, return -1
  if (all(half_or_less)) {
    largest_value <- vec_sorted[vec_len - 1]
    largest_value_ix <- match(largest_value, vec)
    largest_value_ix
  } else {
    -1
  }
}

vec <- c(2, 4, 1, 0)
vec2 <- c(1, 2, 3, 3, 7)
vec3 <- c(1, 2, 3, 4, 5)

twice_largest(vec)  #  1
twice_largest(vec2) #  3
twice_largest(vec3) # -1
