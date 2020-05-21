
#' @import data.table
#' @export
glove_features <- function(words, feats=NULL) {
  if (is.null(feats)) {
    if (!exists("glove50")) {
      data("glove50")
    }
    feats <- glove50
  }

  feats[words]

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
    if (is.null(f1$vec[[1]]) || is.null(f2$vec[[1]])) {
      sim[i] <- NA
    } else {
      sim[i] <- proxy::simil(rbind(f1$vec[[1]],f2$vec[[1]]), method)
    }
  }

  sim
}

