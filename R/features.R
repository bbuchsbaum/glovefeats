
#' @import data.table
#' @export
glove_features <- function(words) {
  if (!exists("glove50")) {
    data("glove50")
  }

  glove50[words]

}

#' @export
word_sim <- function(words, method="cosine") {
  feats <- glove_features(words)
  fmat <- do.call(rbind, feats$vec)
  ret <- proxy::simil(fmat, method)
  ret <- as.matrix(ret)
  colnames(ret) <- words
  rownames(ret) <- words
  ret
}

#' @export
#' @import assertthat
wordpair_sim <- function(words, method="cosine") {
  assertthat::assert_that(ncol(words) == 2)
  sim <- numeric(nrow(words))
  for (i in 1:nrow(words)) {
    f1 <- glove_features(words[i,1])
    f2 <- glove_features(words[i,2])
    sim[i] <- proxy::simil(rbind(f1$vec[[1]],f2$vec[[1]]), method)[1,1]
  }

  sim
}

