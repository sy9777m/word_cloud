library(KoNLP)
library(tidyverse)
library(dplyr)
library(stringr)
library(googlesheets)
library(wordcloud2)
# library(igraph)
# library(tidygraph)
# library(tidytext)
# library(ggraph)
# library(shiny)
useNIADic()
Sys.setlocale("LC_ALL", "korean")

word_extract <- function(dt) {
  dt <- str_replace_all(string = dt, pattern = '\\W', ' ')
  dt <- str_replace_all(string = dt, pattern = '[0-9]', ' ')
  
  op_nouns <- extractNoun(dt)
  
  op_count <- table(unlist(op_nouns))
  
  op_word <- as.data.frame(op_count, stringsAsFactors = FALSE)
  op_word <- rename(op_word, word = Var1, freq = Freq)
  op_word <- filter(op_word, nchar(word) >= 2)
  
  return(op_word)
}

wcloud <- function(op_word) {
  set.seed(7777)
  
  wordcloud2(op_word, color = 'random-light', size = 2, backgroundColor = 'grey')
}

toron_1 <- gs_title('「경주 문무대왕릉 관광지 활성화 방안」마련을 위한  시민원탁회의 사전조사문(응답)')
df_1 <- gs_read(toron_1, ws = '설문지 응답 시트1')

reason_1 <- df_1[7]
reason_1 <- na.omit(reason_1)
result_reason_1 <- as.character(unlist(result_reason_1))
result_reason_1 <- word_extract(reason_1)

wcloud(result_reason_1)

# wcloud(result_test)

# n <- 1
# ui <- bootstrapPage(
#   numericInput('size', 'Size of wordcloud', n),
#   wordcloud2Output('wordcloud2')
# )
# server <- function(input, output) {
#   output$wordcloud2 <- renderWordcloud2({
#     wordcloud2(result_test, size=input$size, backgroundColor = 'grey', color = 'random-light')
#   })
# }
# shinyApp(ui = ui, server = server)