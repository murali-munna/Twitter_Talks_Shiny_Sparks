
# ==================== Twitter Talks, Shiny Sparks! ================================= #
# Contact - Murali                                                                    #

# ============ Install and load the packages =======================
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
#library(twitteR)
#library(rtweet)
library(NLP)
library(tm) # text mining
library(stringr)
library(SnowballC) # text stemming
library(RColorBrewer) # Color Palettes
#library(wordcloud)
library(wordcloud2)
#library(topicmodels)
library(tidytext)
library(slam)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)


#setwd("C:/One-Drive/OneDrive - Tredence/Training/Text Analytics/twitter/Twitter Talks Shiny Sparks")


#=== UI Code Starts ===

header <- dashboardHeader(title= img(src = 'tweet_grey.png',
                                     title = "Twitter", height = "30px"))

sidebar <- dashboardSidebar(
  sidebarMenu(id = 'menu',
              menuItem(strong("Token Frequency"),tabName = 'token', icon = icon("th-list", lib = "glyphicon")),
              menuItem(strong("Sentiment"),tabName = 'sentiment', icon = icon("thumbs-up", lib = "glyphicon")),
              menuItem(strong("Network Analysis"),tabName = 'network', icon = icon("line-chart")),
              menuItem(strong("Popular Tweets"),tabName = 'popular', icon = icon("twitter"))
  ),
  hr(),
  htmlOutput("topic_selector")
)

body <- dashboardBody(
  
  # Title 
  tags$head(tags$style(HTML(
    '.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    font-weight: bold;font-size: 16px;
    }
    '))),
  tags$head(tags$style(HTML(
    '.myClass { 
    font-size: 20px;
    line-height: 50px;
    text-align: left;
    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
    padding: 0 15px;
    overflow: hidden;
    color: white;
    }
    '))),
  #tags$head(tags$style(HTML("div.main-header {text-align: center;}"))),
  tags$script(HTML('
                   $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass" style="white-space:pre">                                                        Twitter Talks, Shiny Sparks !</span>\');
                   })
                   ')),
  tags$head(tags$style(HTML('.modal-sm {width: 40px;}'))),
  
  # Set the boxes of all charts
  tags$head(tags$style(HTML(".col-sm-2,.col-sm-12,.col-sm-4,.col-sm-12,.col-sm-6,.col-sm-7,.col-sm-5 {
                            position: relative;
                            min-height: 1px;
                            padding-right: 5px;
                            padding-left: 5px;"))),

  tags$head(tags$style(HTML(".container-fluid {padding-left: 5px; padding-right: 5px;}"))),
  tags$head(tags$style(HTML(".form-group {margin-bottom: -15px;}"))),

  tags$head(tags$style(HTML(".box {margin-bottom: 10px;}"))),
  
  
  tags$head(tags$style(HTML("#col_word_cloud,#col_freq {padding-left:0px;padding-right:0px;} "))),
  #tags$head(tags$style(HTML("#col_emotion,#col_sentiment {padding-left:0px;padding-right:0px;} "))),
  tags$head(tags$style(HTML(".box-header {text-align: center;} "))),
  tags$head(tags$style(HTML("#network_panel {width:100%;} "))),
  
  tabItems(
    tabItem("token",
            fluidPage(
              fluidRow(
                column(7, id = "col_word_cloud",
                       box(width=12, height=550, solidHeader = F, title = strong("The Word Cloud"),
                           radioButtons("word_cloud_gram",NULL, c("Uni-gram","Bi-gram"), selected = "Uni-gram", inline = T),
                           #plotOutput("word_cloud_plot",height = "300px")
                           wordcloud2Output("word_cloud_plot",height = "470px"))
                ),
                column(5, id = "col_freq",
                       box(width=12, height=550, solidHeader = F, title = strong("Here are the frequent words.."),
                           highchartOutput("word_freq_plot", height=500)
                       )
                       
                )
              )
            )
            
    ),
    tabItem("sentiment",
            fluidPage(
              fluidRow(
                column(width = 6, id = "col_emotion",
                       box(width=NULL, height=550, solidHeader = F, title = strong("Emotions Radar"),
                           highchartOutput("emotion_polar_plot",height=500)
                       )
                ),
                column(width = 6, 
                       box(width=NULL, height=270, solidHeader = F, title = strong("Sentiment Polarity"),
                           highchartOutput("sentiment_plot",height = 210)
                       ),
                       box(width=NULL, height=270, solidHeader = F, title = strong("The extreme ones.."),
                           htmlOutput("pos_tweet"),
                           htmlOutput("neg_tweet")
                       )
                )
              )
            )
            
    ),
    tabItem("network",
            fluidPage(
              fluidRow(
                box(width=12, height=500, solidHeader = F,
                    tabsetPanel(type = 'pills',
                    id = 'network_panel',
                    tabPanel("Bi Directional Network", 
                             #sliderInput("bi_freq", "Min Frequency of Bi-grams:",min = 10, max = 200,value = 100, width = '20%'),
                             plotOutput("network_plot1")),
                    tabPanel("Correlation Network", plotOutput("network_plot2"))
                    )
                    )
              )
            )
            
    ),
    tabItem("popular",
            fluidPage(
              fluidRow(
                box(width=12, height=370, title = strong("Let us check some popular tweets !!"),
                radioButtons("fav_rt_button",NULL, c("Most Favorited","Most Retweeted"), selected = "Most Favorited", inline = T),hr(),
                htmlOutput("fav_rt_tweets")),
                box(width=12, height=300, title = strong("Story - Screenplay - Direction ..."),
                    tags$ul(
                      tags$li(strong(tags$a("Murali - LinkedIn Connect", href = "https://www.linkedin.com/in/muralimohanakrishnadandu/", target="_blank"))," | ",
                              strong(tags$a("Medium Article Link", href = "https://www.linkedin.com/in/muralimohanakrishnadandu/", target="_blank")))
                    ),
                    hr(),
                    h5(strong("Twitter Data:"),style = 'color:rgb(0, 112, 192)'),strong("Tweets downloaded during the time period 15th Sep'2018 - 22nd Sep'2018 with following hashtags - "),
                    tags$ul(
                      tags$li("Data Science (#DataScience OR #MachineLearning OR #DeepLearning): 37426 tweets"),
                      tags$li("IPhoneXS (#iPhoneXS OR #iPhoneXSMax): 21876 tweets"),
                      tags$li("Captain Marvel Trailer (#CaptainMarvelTH OR #CaptainMarvelTrailer OR #captainmarvel): 46092 tweets"),
                      tags$li("Section 377 (#Section377Verdict OR #Section377 OR #377Verdict OR #section377scrapped): 1232 tweets"),
                      tags$li("Robo 2.0 Teaser (#2Point0Teaser OR #2Point0 OR #2point0trailer): 5395 tweets"),
                      tags$li("Reliance-Dassault Rafale (#Reliance OR #reliance OR #Dassault OR #Rafale): 7991 tweets")
                    )
                    )
              )
            )
            
    )
    
  )
  
  
  


  
  )

ui <- dashboardPage(title = 'Twitter Analytics App', header , sidebar, body)




#=== Server Code Starts ===
server <- function(input, output, session) {
  
  # Custom Functions for cleaning the twitter data
  tweets_cleaner <- function(tweet.df){
    
    tweets_txt <- unique(tweet.df$text)
    clean_tweet = gsub("&amp", "", tweets_txt) # Remove Amp
    clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet) # Remove Retweet
    clean_tweet = gsub("@\\w+", "", clean_tweet) # Remove @
    clean_tweet = gsub("#", " ", clean_tweet) # Before removing punctuations, add a space before every hashtag
    clean_tweet = gsub("[[:punct:]]", "", clean_tweet) # Remove Punct
    clean_tweet = gsub("[[:digit:]]", "", clean_tweet) # Remove Digit/Numbers
    clean_tweet = gsub("http\\w+", "", clean_tweet) # Remove Links
    clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet) # Remove tabs
    clean_tweet = gsub("^\\s+|\\s+$", " ", clean_tweet) # Remove extra white spaces
    clean_tweet = gsub("^ ", "", clean_tweet)  # remove blank spaces at the beginning
    clean_tweet = gsub(" $", "", clean_tweet) # remove blank spaces at the end
    clean_tweet = gsub("[^[:alnum:][:blank:]?&/\\-]", "", clean_tweet) # Remove Unicode Char
    
    
    clean_tweet <- str_replace_all(clean_tweet," "," ") #get rid of unnecessary spaces
    clean_tweet <- str_replace_all(clean_tweet, "https://t.co/[a-z,A-Z,0-9]*","") # Get rid of URLs
    clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*","")
    clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","") # Take out retweet header, there is only one
    clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","") # Get rid of hashtags
    clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","") # Get rid of references to other screennames
    
    clean_tweet
  }
  
  tweets_cleaner_tm <- function(clean_tweet, custom_stopwords = c("bla bla")){
    
    docs <- Corpus(VectorSource(clean_tweet))
    #inspect(docs)
    
    
    docs <- tm_map(docs, content_transformer(tolower)) # Convert the text to lower case
    docs <- tm_map(docs, removeNumbers) # Remove numbers
    docs <- tm_map(docs, removeWords, stopwords("english")) # Remove english common stopwords
    docs <- tm_map(docs, removeWords, custom_stopwords)  # Remove your own stop word
    docs <- tm_map(docs, removePunctuation) # Remove punctuations
    docs <- tm_map(docs, stripWhitespace) # Eliminate extra white spaces
    # docs <- tm_map(docs, stemDocument) # Text stemming
    docs
  }
  
  
  #== Select the topic ===
  output$topic_selector <- renderUI({
    selectInput("topic_selector",label = "Select Twitter Topic:",
                choices = c("Data Science", "IPhoneXS", "Captain Marvel Trailer", "Section 377", "Robo 2.0 Teaser", "Relaince - Dassault Rafale"),
                selected = "Data Science", width = "100%")
  })
  
  #=== Read Data ===
  tweet_df_ds <- readRDS(file = "data/tweet_df_ds.rds")
  tweet_df_iphone <- readRDS(file = "data/tweet_df_iphone.rds")
  tweet_df_capmarvel <- readRDS(file = "data/tweet_df_capmarvel.rds")
  tweet_df_sec377 <- readRDS(file = "data/tweet_df_sec377.rds")
  tweet_df_robo2 <- readRDS(file = "data/tweet_df_robo2.rds")
  tweet_df_reliance <- readRDS(file = "data/tweet_df_reliance.rds")
  
  
  
  tweet_df_final <- rbind(
    cbind(tweet_df_ds, topic = "Data Science"),
    cbind(tweet_df_iphone, topic = "IPhoneXS"),
    cbind(tweet_df_capmarvel, topic = "Captain Marvel Trailer"),
    cbind(tweet_df_sec377, topic = "Section 377"),
    cbind(tweet_df_robo2, topic = "Robo 2.0 Teaser"),
    cbind(tweet_df_reliance, topic = "Relaince - Dassault Rafale")
    
  )
  
  #=== Get cleaned tweets of selected topic ===
  cleaned_tweets <- reactive({
    progress <- shiny::Progress$new()
    progress$set(message = "Cleaning Tweets", value = 0)
    on.exit(progress$close())
    
    x <- tweet_df_final %>% filter(topic == input$topic_selector)
    progress$set(detail = "Ah! lot to clean...", value = 0.5)
    tweets_cleaner(x)
    
  })
  
  #=== Get docs for the selected topic
  docs <- reactive({
    custom_stopwords <- if(input$topic_selector == "Data Science"){
      c("datascience","machinelearning","deeplearning")
    }else if(input$topic_selector == "IPhoneXS"){
      c("iphonexs","iphonexsmax","iphone")
    }else if(input$topic_selector == "Captain Marvel Trailer"){
      c("captainmarvelth","captainmarveltrailer","captainmarvel", "captain", "marvel", "trailer")
    }else if(input$topic_selector == "Section 377"){
      c("section377verdict","section377","377verdict","section377scrapped", "section")
    }else if(input$topic_selector == "Robo 2.0 Teaser"){
      c("2point0teaser","2point0","2point0trailer","pointteaser","point")
    }else if(input$topic_selector == "Relaince - Dassault Rafale"){
      c("reliance","dassault","rafale")
    }else{
      NULL
    }
    
    progress <- shiny::Progress$new()
    progress$set(message = "Preparing Docs", value = 0)
    on.exit(progress$close())
    
    progress$set(detail = "Docking...", value = 0.5)
    tweets_cleaner_tm(cleaned_tweets(), custom_stopwords = custom_stopwords)
    
  })
  
  tdm <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Creating TDM", value = 0)
    on.exit(progress$close())
    
    progress$set(detail = "Almost there...", value = 0.5)
    TermDocumentMatrix(docs())
    
  })
  
  d <- reactive({
    v <- sort(row_sums(tdm()),decreasing=TRUE)
    data.frame(word = names(v),freq=v)
  })
  
  
  #=== Word Freq plot ===
  output$word_freq_plot <- renderHighchart(
    hc <- highchart() %>%
      #hc_title(text = "Incremental Revenue and Total Cost by Offer Group") %>%
      hc_chart(type = "bar") %>%
      #hc_plotOptions(bar = list(getExtremesFromAll = T)) %>% 
      hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                 formatter = JS(paste0("function() {
                                       //console.log(this);
                                       //console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.x.name+'</span>:<b> '
                                       +Math.round(this.point.y.toFixed(0)/100)/10 + 'K' + '</b>';
                                       return result;
      }"))) %>%
      hc_xAxis(categories = d()[1:100,]$word,
               #labels = list(rotation = 0, step=1), title =list(text="Brand")
               labels = list(style = list(fontSize= '11px')), max=20, scrollbar = list(enabled = T)
                 )    %>%
      hc_add_series(name="Word", data = d()[1:100,]$freq, type ="column",
                    #max=max(d()$freq), tickInterval = max(d()$freq)/4, alignTicks = F,
                    color = "#4472c4", showInLegend= F)
    #hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width=120, itemStyle = list(fontSize= '10px'))
                 )
  
  
  #=== Wordcloud plot ===
  output$word_cloud_plot <- renderWordcloud2({
    
    if(input$word_cloud_gram == "Uni-gram"){
      
      set.seed(1234)
      # wordcloud(words = d()$word, freq = d()$freq, scale = c(3,0.5), min.freq = 3,
      #           max.words=100, random.order=FALSE, rot.per=0.35, 
      #           colors=brewer.pal(8, "Dark2"))
      d1 <- (d() %>% filter(freq>1) %>% arrange(desc(freq)))[1:100,]
      wordcloud2(data = d1, size=0.8, minSize = 0.0, fontWeight = 'bold', 
                 ellipticity = 0.65)
      
    } else if(input$word_cloud_gram == "Bi-gram"){
      
      progress <- shiny::Progress$new()
      progress$set(message = "Bi-gram", value = 0)
      on.exit(progress$close())
      
      progress$set(value = 0.3, detail = "Creating Bitokens...")
      bitoken <- data.frame(text = sapply(docs(), as.character), stringsAsFactors = FALSE) %>% 
        unnest_tokens(bigram, text, token = "ngrams", n = 2)
      two_word <- bitoken %>% count(bigram, sort = TRUE)
      progress$set(value = 0.9, detail = paste("Parsing a dataframe..Almost done"))
      sort_two <- two_word[order(two_word$n,decreasing=TRUE),]
      # wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(3,0.5),
      #           min.freq = 2,colors = brewer.pal(8,"Dark2"),max.words=50)
      names(sort_two) <- c("word", "freq")
      d1 <- (sort_two %>% filter(freq>2) %>% arrange(desc(freq)))[1:50,]
      wordcloud2(data = d1, size=0.8, minSize = 0.0, fontWeight = 'bold', 
                 ellipticity = 0.65)
      
      
    }
    
  })
  
  #===== Sentiment Analysis ======
  # emotion_df_ds <- readRDS(file = "data/emotion_df_ds.rds")
  # emotion_df_iphone <- readRDS(file = "data/emotion_df_iphone.rds")
  # emotion_df_capmarvel <- readRDS(file = "data/emotion_df_capmarvel.rds")
  # emotion_df_sec377 <- readRDS(file = "data/emotion_df_sec377.rds")
  # emotion_df_robo2 <- readRDS(file = "data/emotion_df_robo2.rds")
  # emotion_df_reliance <- readRDS(file = "data/emotion_df_reliance.rds")
  # 
  # emotion_df_final <- rbind(
  #   cbind(emotion_df_ds, topic = "Data Science"),
  #   cbind(emotion_df_iphone, topic = "IPhoneXS"),
  #   cbind(emotion_df_capmarvel, topic = "Captain Marvel Trailer"),
  #   cbind(emotion_df_sec377, topic = "Section 377"),
  #   cbind(emotion_df_robo2, topic = "Robo 2.0 Teaser"),
  #   cbind(emotion_df_reliance, topic = "Relaince - Dassault Rafale")
  #   
  # )
  
  emotion_score <- reactive({
    # emotion_df <- emotion_df_final %>% filter(topic == input$topic_selector)
    # x <- as.data.frame(colSums(emotion_df[,1:10]))
    # x <- data.frame(names = row.names(x), x)
    # names(x) <- c("emotion", "score")
    # x[1:8,]
    progress <- shiny::Progress$new()
    progress$set(message = "Getting Emotions", value = 0.2)
    on.exit(progress$close())
    
    x <- data.frame(tweet_nbr = 1:length(cleaned_tweets()), clean_tweet = cleaned_tweets())
    x$clean_tweet <- as.character(x$clean_tweet)
    
    df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
    df <- df %>% inner_join(get_sentiments("nrc"))
    progress$set(value = 0.8, detail = paste("COllating.."))
    df <- df %>% group_by(sentiment) %>% summarise(score = n())
    names(df) <- c("emotion", "score")
    df[c(1:5,8:10),]
    #df
  })
  
  output$emotion_polar_plot <- renderHighchart(
    hc <- highchart() %>%
      #hc_title(text = "Incremental Revenue and Total Cost by Offer Group") %>% 
      hc_chart(polar = T) %>% 
      # hc_tooltip(crosshairs = T, shared = T,useHTML=T,
      #            formatter = JS(paste0("function() {
      #                                      //console.log(this);
      #                                      //console.log(this.points[0].series.name);
      #                                      var result='';
      #                                      result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+'$'+Math.round(this.point.y.toFixed(0)/100000)/10 + 'M' + '</b>';
      #                                      return result;
      #                                      }"))
      # ) %>%
      hc_xAxis(categories = emotion_score()$emotion, 
               labels = list(style = list(fontSize= '14px')), title =NULL, tickmarkPlacement = "on", lineWidth = 0) %>% 
      hc_plotOptions(series = list(marker = list(enabled = F))) %>% 
      hc_yAxis(gridLineInterpolation = "polygon", lineWidth = 0, min = 0) %>% 
      hc_add_series(name = "Emotions Score", emotion_score()$score, type ="area", color = "#4472c4", pointPlacement = "on")
  )
  
  sentiment_score <- reactive({
    progress <- shiny::Progress$new()
    progress$set(message = "Working on Sentiment", value = 0)
    on.exit(progress$close())
    
    x <- data.frame(tweet_nbr = 1:length(cleaned_tweets()), clean_tweet = cleaned_tweets())
    x$clean_tweet <- as.character(x$clean_tweet)
    
    progress$set(detail = "Getting score...", value = 0.6)
    df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
    df <- df %>% inner_join(get_sentiments("afinn"))
    df <- df %>% group_by(tweet_nbr) %>% summarize(score = sum(score))
    
    progress$set(detail = "Getting score...", value = 0.8)
    df$category_senti <- ifelse(df$score < 0, "Negative", ifelse(df$score > 0, "Positive", "Neutral"))
    df1 <- df %>% left_join(x)
    
    x <- list()
    x[[1]] <- as.data.frame(df1)
    x[[2]] <- as.character(df1[df1$score == max(df1$score),"clean_tweet"][1,1])
    x[[3]] <- as.character(df1[df1$score == min(df1$score),"clean_tweet"][1,1])
    
    x
  })
  
  senti_df <- reactive({
    sentiment_score()[[1]] %>% group_by(category_senti) %>% summarise(score = n()) %>% 
      mutate(score_pct = score/sum(score)*100, coloract = c("#d35400", "#2980b9", "#2ecc71"))
  })
  
  output$sentiment_plot <- renderHighchart(
    
    hc <- highchart() %>%
      #hc_title(text = "Incremental Revenue and Total Cost by Offer Group") %>%
      hc_chart(type = "bar") %>%
      #hc_plotOptions(bar = list(getExtremesFromAll = T)) %>% 
      hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                 formatter = JS(paste0("function() {
                                       console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+Math.round(this.point.y.toFixed(0))/1 + '%' + '</b>';
                                       return result;
                   }")))%>%
      hc_xAxis(categories = senti_df()$category_senti,
               #labels = list(rotation = 0, step=1), title =list(text="Brand")
               labels = list(style = list(fontSize= '12px')) #max=20, scrollbar = list(enabled = T)
                 )    %>%
      hc_colors(colors = senti_df()$coloract) %>% 
      hc_add_series(name="Sentiment", data = senti_df()$score_pct, colorByPoint = TRUE, 
                    type ="column",
                    #max=max(d()$freq), tickInterval = max(d()$freq)/4, alignTicks = F,
                    color = "#4472c4", showInLegend= F) %>% 
      hc_yAxis(labels=list(format = '{value}%'),min=0,
               max=100,showFirstLabel = TRUE,showLastLabel=TRUE)
    #hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width=120, itemStyle = list(fontSize= '10px'))
  )
  
  
  output$network_plot1 <- renderPlot({
    progress <- shiny::Progress$new()
    progress$set(message = "Bi Directional Graph", value = 0.2)
    on.exit(progress$close())
    progress$set(detail = "Creating Bitokens..", value = 0.6)
    
    bitoken <- data.frame(text = sapply(docs(), as.character), stringsAsFactors = FALSE) %>% 
      unnest_tokens(bigram, text, token = "ngrams", n = 2)
    two_word <- bitoken %>% count(bigram, sort = TRUE)
    
    bigrams_separated <- two_word %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    progress$set(detail = "Plotting..", value = 0.8)
    bigram_graph <- head(bigrams_separated %>% arrange(desc(n)),150) %>% graph_from_data_frame()
    
    #bigram_graph
    #library(ggraph)
    set.seed(2018)
    # ggraph(bigram_graph, layout = "fr") +
    #   geom_edge_link() +
    #   geom_node_point() +
    #   geom_node_text(aes(label = name), vjust = 1, hjust = 1)
    
    a <- grid::arrow(type = "closed", length = unit(.08, "inches"))
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "lightblue", size = 3) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=5) +
      theme_void()
    
    
  })
  
  output$network_plot2 <- renderPlot({
    progress <- shiny::Progress$new()
    progress$set(message = "Correlation Plot", value = 0.2)
    on.exit(progress$close())
    progress$set(message = "Creating Pairwise words", value = 0.4)
    
    x <- data.frame(text = sapply(docs(), as.character), stringsAsFactors = FALSE)
    x$tweet_nbr <- 1:nrow(x)
    tweet_word <- x %>% unnest_tokens(word, text)
    
    tweet_word_cors <- tweet_word %>% group_by(word) %>% filter(n() >= 50) %>% 
      pairwise_cor(word, tweet_nbr, sort = TRUE, upper = FALSE)
    
    progress$set(detail = "Plotting..", value = 0.8)
    set.seed(1234)
    tweet_word_cors %>%
      filter(correlation > .6) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE,
                     point.padding = unit(0.2, "lines")) +
      theme_void()
    
    
  })
  
  output$pos_tweet <- renderUI({
    HTML(paste("<b>","Most positive tweet: ","</b>"),"<br>","<i style = 'font-weight: bold'>","\"",sentiment_score()[[2]][1],"\"","</i>","<hr>")
  })
  output$neg_tweet <- renderText({
    HTML(paste("<b>","Most negative tweet: ","</b>"),"<br>","<i style = 'font-weight: bold'>","\"",sentiment_score()[[3]][1],"\"","</i>")
  })
  
  
  fav_rt_tweets <- reactive({
    
    tweets_df <- tweet_df_final %>% filter(topic == input$topic_selector)
    x <- list()
    x[[1]] <- head(tweets_df %>% select(text, favorite_count) %>% arrange(desc(favorite_count)))
    x[[2]] <- head(tweets_df %>% select(text, retweet_count) %>% arrange(desc(retweet_count)))
    x
  })
  
  output$fav_rt_tweets <- renderUI({
    if(input$fav_rt_button == "Most Favorited"){
      tags$ul(
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][1,]$text,"-",paste(fav_rt_tweets()[[1]][1,]$favorite_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][2,]$text,"-",paste(fav_rt_tweets()[[1]][2,]$favorite_count))), style = "margin-bottom: 5px; color: #F1931B; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][3,]$text,"-",paste(fav_rt_tweets()[[1]][3,]$favorite_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][4,]$text,"-",paste(fav_rt_tweets()[[1]][4,]$favorite_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][5,]$text,"-",paste(fav_rt_tweets()[[1]][5,]$favorite_count))), style = "margin-bottom: 0px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][6,]$text,"-",paste(fav_rt_tweets()[[1]][6,]$favorite_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold")
      )
    } else if(input$fav_rt_button == "Most Retweeted"){
      tags$ul(
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][1,]$text,"-",paste(fav_rt_tweets()[[2]][1,]$retweet_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][2,]$text,"-",paste(fav_rt_tweets()[[2]][2,]$retweet_count))), style = "margin-bottom: 5px; color: #F1931B; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][3,]$text,"-",paste(fav_rt_tweets()[[2]][3,]$retweet_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][4,]$text,"-",paste(fav_rt_tweets()[[2]][4,]$retweet_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][5,]$text,"-",paste(fav_rt_tweets()[[2]][5,]$retweet_count))), style = "margin-bottom: 0px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][6,]$text,"-",paste(fav_rt_tweets()[[2]][6,]$retweet_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold")
        
      )
    }
    
  })
  
  
  }



shinyApp(ui = ui, server = server)