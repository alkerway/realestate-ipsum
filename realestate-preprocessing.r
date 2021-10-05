library(rvest)
library(purrr)
library(dplyr)
library(progress)
library(session)


setwd('/home/aw/Documents/w/r/realestate-ipsum/')
restore.session('.Rpreprocessingsession')

listings.base <- 'https://www.trademe.co.nz/a/property/residential/sale/auckland/search?sort_order=expirydesc&page='
num.pages = 320
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


description.progress <- progress_bar$new(total=length(page.responses),format='[:bar] :percent eta: :eta')
raw.descriptions <- character()
for (idx in 1:length(page.responses)) {
  description.progress$tick()
  link <- page.responses[idx]
  # print(link)
  response <- possibly(read_html, FALSE)(link)
  if (class(response[1]) != 'logical') {
    response.text <- response %>%
      html_element('.tm-property-listing-description__text') %>%
      html_text2() # %>%
    raw.descriptions <- c(raw.descriptions, response.text)
  } else {
    print(paste0('Link response error: ', link))
  }
}

cleanChars <- function(txt) {
  txt <- gsub('([[:punct:]])[\n]+', '\\1 ', txt)
  txt <- gsub('[\n]+', '\\. ', txt)
  txt <- gsub('\\s+', ' ', txt)
  txt <- gsub('\\.+', '.', txt)
  txt <- gsub(' & ', ' and ', txt)
  txt <- gsub('[\']|[‘]|[’]|[“]|[”]|[″]|["]|[*]|[™]|[®]|[¦]|[´]|[`]|[°]|[¬]|[<]|[>]|[¢]|[□]', '', txt)
  txt <- gsub('\u0081', '', txt)
  txt <- gsub('\\\\', '', txt)
  txt <- gsub('[–]|[—]|[̶]|[_]', '-', txt)
  txt <- gsub('[²]', '2', txt)
  txt <- gsub('[ç]', 'c', txt)
  txt <- gsub('[é]|[è]|[ê]|[ë]', 'e', txt)
  txt <- gsub('[×]', 'x', txt)
  txt <- gsub('[½]', '1/2', txt)
  txt <- gsub('[¼]', '1/4', txt)
  txt <- gsub('[¾]', '3/4', txt)
  txt <- gsub('[…]', '', txt)
  txt <- gsub(' ?~ ?', '. ', txt)
  txt <- gsub(' ?\\+ ?', ', ', txt)
  txt <- gsub(' ?\\^ ?', ', ', txt)
  txt <- gsub('\\[', '(', txt)
  txt <- gsub('\\]', ')', txt)
  txt <- gsub('\\$ ?[0-9.,]+[0-9]*', '¢', txt)
  txt <- gsub('([[:digit:]]+\\.?[[:digit:]]*) ?m2', '□sqm', txt)
  txt <- gsub('\\$', '', txt)
  txt <- gsub(' ([?,.!])', '\\1', txt)
  # mtr.txt <- unlist(regmatches(txt, gregexpr('[[:digit:]]+ ?sqm', txt)))
  # if (any(grepl("#", txt))) browser()
  # browser()
  trimws(txt)
}
raw.descriptions <- raw.descriptions[which(!is.na(raw.descriptions))]
text.block <- raw.descriptions %>%
  tolower() %>%
  cleanChars %>%
  strsplit('[.] |[!] ') %>%
  map(~{
        .x[!grepl('(http)|(\\.co)|( 021)|( 027)|(@)|(·)|(lot [:digit:])|\\(|\\)', .x)] %>%
        paste(collapse=('. ')) %>%
        paste0('.') %>%
        {gsub('\\.+', '.', .)} %>%
        {gsub('!\\.', '!', .)} %>%
        c('*')
      }) %>%
  unlist() %>%
  paste(collapse='') %>%
  strsplit('') %>%
  unlist()


chars <- text.block %>%
  unique()

maxlen <- 30
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
write.table(all.x.y, file=out.file, sep=',', row.names = F, append=TRUE)


save.session('.Rpreprocessingsession')

