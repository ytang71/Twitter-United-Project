

library(shiny)
library(tidyverse)
data <- read.csv("tidy_tweetsdf.csv", stringsAsFactors = FALSE)


ui <- shinyUI(fluidPage(
  titlePanel("United Twitter Charts"), 
  navbarPage(title = "Charts",
             tabPanel("tweets frequency chart",plotOutput("tweetsfrequencychart")),         
             tabPanel("word frequency chart",plotOutput("wordfrequencychart")),
             tabPanel("sentiment nrc chart",plotOutput("sentinrc")),
             tabPanel("sentiment bing (pos&neg) chart",plotOutput("sentibing"))


)# end of navbar
)# end fluid page
)# end shiny UI
  
      
server <- function(input, output) {

  
  output$tweetsfrequencychart <- renderPlot({
    data$date<-lubridate::date(data$created)
    dayflowchart<-tweets.df%>%
      group_by(date)%>%
      summarise(numberoftweets=n())
    ggplot(data=dayflowchart, aes(x=date, y=numberoftweets, group=1)) + 
      geom_line(colour="red", linetype="dashed", size=1.5) + 
      geom_point(colour="red", size=4, shape=21, fill="white")
  })
  
  output$wordfrequencychart<-renderPlot({
    tidy_tweets <- data %>% 
      unnest_tokens(word,text)
    #2.remove stop words
    data(stop_words)
    tidy_tweets <-  tidy_tweets %>% 
      anti_join(stop_words)
    #3.word frequency graph
    tidy_tweets %>%
      count(word, sort = TRUE) %>%
      filter(n > 50) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
})
  output$sentinrc<-renderPlot({
    tweets_text<-data$text
    tweets_text<-data_frame(line=1:860, text=tweets_text)
    tweets_text<-tweets_text%>%unnest_tokens(word, text)
    tweets_text<-tweets_text%>%
      inner_join(get_sentiments("nrc")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    tweets_text%>%
      group_by(sentiment) %>%
      top_n(12) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Sentiment Categories",
           x = NULL) +
      coord_flip()
  })
  output$sentibing<-renderPlot({
    tweets_text<-data$text
    tweets_text<-data_frame(line=1:860, text=tweets_text)
    tweets_text<-tweets_text%>%unnest_tokens(word, text)
    tweets_text<-tweets_text%>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    tweets_text%>%
      group_by(sentiment) %>%
      top_n(12) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Positive and Negative words",
           x = NULL) +
      coord_flip()
  })
} 
  
shinyApp(ui = ui, server = server)


