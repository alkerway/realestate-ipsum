library(purrr)
library(progress)
library(keras)

setwd('/home/aw/Documents/w/r/realestate-ipsum/')

in.file <- 'dataset.csv'
all.x.y <- read.table(in.file, skip=1, header=T, sep=',')
chars <- read.table(in.file, nrows = 1)$V1 %>%
  strsplit('') %>%
  unlist()

calculation.x.y <- all.x.y

set.seed(1)
num.test <- 800
test.idc <- sample(1:nrow(calculation.x.y), num.test)
test.x.y <- calculation.x.y[test.idc,]
train.x.y <- calculation.x.y[-test.idc,]

rm(calculation.x.y)

x.string.length = nchar(train.x.y[1,1])

data_generator <- function(data, batch_size) {
  i <- 1
  function() {
    # reset iterator if already seen all data
    if ((i + batch_size - 1) > nrow(data)) i <<- 1
    # iterate current batch's rows
    row.indeces <- c(i:min(i + batch_size - 1, nrow(data)))
    
    x <- array(0, dim = c(length(row.indeces), x.string.length, length(chars)))
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
gen <- data_generator(data=train.x.y, batch_size = 512)

optimizer <-  optimizer_rmsprop()

model <- keras_model_sequential() %>%
  layer_lstm(512, input_shape = c(x.string.length, length(chars))) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_dense(length(chars)) %>%
  layer_activation("softmax") %>%
  compile(loss = "categorical_crossentropy", optimizer = optimizer)

num.epochs <- 3
model.history <- model %>%
  fit_generator(gen, steps_per_epoch = nrow(train.x.y) / 512, epochs=num.epochs)

model.store <- '1.model'
save_model_hdf5(model, '1.model')
# model <- load_model_hdf5('1.model')

getTestError <- function() {
  scores <- numeric()
  test.progress <- progress_bar$new(total=nrow(test.x.y),format='[:bar] :percent eta: :eta')
  for (idx in 1:nrow(test.x.y)) {
    test.progress$tick()
    
    x.test.sample <- test.x.y$x[idx] %>%
      strsplit('') %>%
      unlist()
    x.pred <- sapply(chars, function(x) as.integer(x == x.test.sample))
    x.pred <- array_reshape(x.pred, c(1, dim(x.pred)))
    
    preds <- predict(model, x.pred)
    names(preds) <- chars
    pred.score <- which(names(sort(preds, decreasing=T)) == test.x.y$y[idx])
    scores <- c(scores, pred.score)
  }
  scores
}
scores <- getTestError()
score.props <- table(scores) / sum(table(scores))
score.props
# pie(score.props)

runPrediction <- function() {
  rand.idx <- sample(1:nrow(test.x.y), 1)
  cur.sentence <- test.x.y$x[rand.idx] %>%
    strsplit('') %>%
    unlist()
  first.sentence <- cur.sentence
  generated <- ''
  gen.length <- 400000
  start.cat <- TRUE
  cur_char <- ''
  i <- 1
  pred.progress <- progress_bar$new(total=gen.length,format='[:bar] :percent eta: :eta')
  while (i < gen.length) { # | cur_char != '.') {
    pred.progress$tick()
    x.pred <- sapply(chars, function(x) as.integer(x == cur.sentence))
    x.pred <- array_reshape(x.pred, c(1, dim(x.pred)))
    
    preds <- predict(model, x.pred)
    cur_char <- sample(chars, 1, prob=preds^2/sum(preds^2))
    # cur_char <- chars[next_index]
    cur.sentence <- c(cur.sentence[-1], cur_char)
    if (cur_char == '□') {
      cur_char <- '50'
    } else if (cur_char == '¢') {
      cur_char <- paste0('$', sample(1:9, 1), paste(rep('0', sample(1:5, 1)), collapse = ''))
    }
    generated <- paste0(generated, cur_char)
    if (start.cat) {
      # cat(cur_char)
    }
    if (cur_char == '.') start.cat = TRUE
    i <- i + 1
  }
  # cat('\n')
  generated
}
pred.output <- runPrediction()

pred.lines <- unlist(strsplit(substring(paste(pred.output, ' ', sep=''), 2), '\\. '))
cap.lines <- paste(toupper(substring(pred.lines, 1, 1)),
                   substring(pred.lines, 2),
                   '.',
                   sep = "")

fileConn<-file(paste0('generated_', '1', '.txt'))
writeLines(cap.lines, fileConn)
close(fileConn)
