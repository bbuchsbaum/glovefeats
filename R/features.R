
#' @import data.table
glove_features <- function(words) {
  if (!exists("glove50")) {
    data("glove50")
  }

  glove50[words]

}

word_sim <- function(words, method="cosine") {
  feats <- glove_features(words)
  fmat <- do.call(rbind, feats$vec)
  ret <- proxy::simil(fmat, method)
  ret <- as.matrix(ret)
  colnames(ret) <- words
  rownames(ret) <- words
  ret
}

