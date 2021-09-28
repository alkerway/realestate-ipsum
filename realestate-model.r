library(purrr)
library(progress)
library(keras)

setwd('/Users/aw/Documents/w/py/realestate-ipsum/')

in.file <- 'dataset.csv'
all.x.y <- read.table(in.file, skip=1, header=T, sep=',')
chars <- read.table(in.file, nrows = 1)$V1 %>%
  strsplit('') %>%
  unlist()

calculation.x.y <- all.x.y[1:100000,]

set.seed(1)
train.idc <- sample(1:nrow(calculation.x.y), nrow(calculation.x.y) * 9 / 10)
train.x.y <- calculation.x.y[train.idc,]
test.x.y <- calculation.x.y[-train.idc,]

rm(calculation.x.y)

x.string.length = nchar(train.x.y[1,1])

data_generator <- function(data, batch_size) {
  i <- 1
  function() {
    # reset iterator if already seen all data
    if ((i + batch_size - 1) > nrow(data)) i <<- 1
    # iterate current batch's rows
    row.indeces <- c(i:min(i + batch_size - 1, nrow(data)))
    
    x <- array(0, dim = c(length(row.indeces), maxlen, length(chars)))
    y <- array(0, dim = c(length(row.indeces), length(chars)))
    
    for(each.row in 1:length(row.indeces)){
      pre.chars <- unlist(strsplit(data$x[[i + each.row - 1]], ''))
      x[each.row,,] <- sapply(chars, function(rec.char) as.integer(rec.char == pre.chars))
      y[each.row,] <- as.integer(chars == data$y[[i + each.row - 1]])
    }
    
    # update to next iteration
    i <<- i + batch_size
    
    # return the batch
    list(x, y)
  }
}
gen <- data_generator(data=train.x.y, batch_size = 128)

optimizer <-  optimizer_rmsprop()

model <- keras_model_sequential() %>%
  layer_lstm(128, input_shape = c(x.string.length, length(chars))) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_activation("softmax") %>%
  compile(loss = "categorical_crossentropy", optimizer = optimizer)

num.epochs <- 3
model.history <- model %>%
  fit_generator(gen, steps_per_epoch = nrow(train.x.y) / 128, epochs=num.epochs)
