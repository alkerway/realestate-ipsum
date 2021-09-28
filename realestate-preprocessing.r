library(rvest)
library(purrr)
library(progress)

setwd('/Users/aw/Documents/w/py/realestate-ipsum/')

listings.base <- 'https://www.trademe.co.nz/a/property/residential/sale/auckland/search?sort_order=expirydesc&page='
num.pages = 55
page.progress = progress_bar$new(total=num.pages,format='[:bar] :percent eta: :eta')
page.responses <- map(1:num.pages, ~{
                            page.progress$tick()
                            read_html(paste(listings.base, .x, sep='')) %>%
                            html_elements('tm-search-card-switcher') %>%
                            html_element('a') %>%
                            html_attr('href')
                            }) %>%
                  unlist() %>%
                  strsplit('\\?') %>%
                  lapply('[[', 1) %>%
                  unlist()

page.responses <- ifelse(substring(page.responses, 1, 1) == '/',
                   paste('https://www.trademe.co.nz', page.responses, sep=''),
                   paste('https://www.trademe.co.nz/a/property/residential/sale/auckland/', page.responses, sep=''))

cleanChars <- function(txt) {
  txt <- gsub('([[:punct:]])[\n]+', '\\1 ', txt)
  txt <- gsub('[\n]+', '\\. ', txt)
  txt <- gsub('\\s+', ' ', txt)
  txt <- gsub('\\.+', '.', txt)
  txt <- gsub(' & ', ' and ', txt)
  txt <- gsub('[\']|[‘]|[’]|[“]|[”]|[″]|["]|[*]', '', txt)
  txt <- gsub('[–]|[—]|[̶]', '-', txt)
  txt <- gsub('[²]', '2', txt)
  # gsub('http\\S+\\s*', '', descriptions[220])
  trimws(txt)
}

description.progress <- progress_bar$new(total=length(page.responses),format='[:bar] :percent eta: :eta')
raw.descriptions <- map(page.responses, ~{
                      description.progress$tick()
                      read_html(.x) %>%
                     html_element('.tm-property-listing-description__text') %>%
                     html_text2()
                    }) # %>%
                # unlist() %>%
                # tolower() %>%
                # cleanChars()

text.block <- raw.descriptions %>%
  cleanChars %>%
  strsplit('[.] |[!] ') %>%
  map(~.x[!grepl('(http)|(\\.co)|( 021)|( 027)|(mailto)', .x)] %>%
        paste(collapse=('. ')) %>%
        paste0('.') %>%
        {gsub('\\.+', '.', .)} %>%
        {gsub('!\\.', '!', .)} %>%
        c('*')) %>%
  unlist() %>%
  paste(collapse='') %>%
  tolower() %>%
  strsplit('') %>%
  unlist()


chars <- text.block %>%
  unique()

maxlen <- 40
start.idx.seq <- seq(1, length(text.block) - maxlen - 1, by = 1)
dataset.progress <- progress_bar$new(total=length(start.idx.seq),format='[:bar] :percent eta: :eta')
dataset <- map(start.idx.seq,
               ~{
                 dataset.progress$tick()
                 list(sentence = paste(text.block[.x:(.x + maxlen - 1)], collapse=''), next_char = text.block[.x + maxlen])
                 }) %>%
  transpose()
all.x.y <- data.frame(x=unlist(dataset[[1]]), y=unlist(dataset[[2]]))
out.file = 'dataset.csv'
writeLines(paste(c('"', chars, '"'), collapse=''), out.file)
write.table(all.x.y, file='dataset.csv', sep=',', row.names = F, append=TRUE)




