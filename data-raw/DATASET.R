## code to prepare `DATASET` dataset goes here

##usethis::use_data("DATASET")

dat <- readRDS("data-raw/glove_6B_50.RDS")
glove50 <- as.data.table(dat)
setkey(glove50, "words")

usethis::use_data(glove50)

v1 <- dat2["the"]$vec[[1]]
v2 <- dat2["strong"]$vec[[1]]
v3 <- dat2["stronger"]$vec[[1]]
v4 <- dat2["muscle"]$vec[[1]]
v5 <- dat2["weight"]$vec[[1]]
v6 <- dat2["apple"]$vec[[1]]
v7 <- dat2["pear"]$vec[[1]]
v8 <- dat2["force"]$vec[[1]]
v9 <- dat2["game"]$vec[[1]]
v10 <- dat2["pacman"]$vec[[1]]
v11 <- dat2["arcade"]$vec[[1]]
v12 <- dat2["fever"]$vec[[1]]


V <- rbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)
