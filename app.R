library(shiny)



server <- function(input, output) {
  output$distPlot <- renderPlot({
    library(twitteR)
    library(ROAuth)
    library(tm)
 
    api_key <-       "6M0UlYMcAKwEKTemVhpncZUqK"
    api_secret <-   "kjHcODg4AEgM9pChZRUyENgAdJrACmOiZkWhrfgnoO6DdCCPbb"
    access_token <- "558595755-YTcAFgolY8qxzMpmLzkng7ML0NUVlYMtYxdc4CM1"
    access_token_secret <-  "JSPW0eRDLNcow8BFYMhNiVwgSdxj9qhE2uvP0a2RMCMfg"
    setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
    tweets <- searchTwitter(input$text, n = 1000, lang = "en")
    tweetsDF <- twListToDF(tweets)
    myCorpus <- Corpus(VectorSource(tweetsDF$text))
    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
    myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
    # conver to lowercase
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
    # remove URLs
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
    # remove anything other than English letters or space
    
    # remove stopwords
    myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                     "use", "see", "used", "via", "amp", "retweet", "follow", "notifications", "rt")
    myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    # remove extra whitespace
    myCorpus <- tm_map(myCorpus, stripWhitespace)
    # keep a copy for stem completion later
    myCorpusCopy <- myCorpus
    wordFreq <- function(corpus, word) {
      results <- lapply(corpus,
                        function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
      )
      sum(unlist(results))
    }
    replaceWord <- function(corpus, oldword, newword) {
      tm_map(corpus, content_transformer(gsub),
             pattern=oldword, replacement=newword)
    }
    tdm <- TermDocumentMatrix(myCorpus,
                              control = list(wordLengths = c(1, Inf)))
    tdm
    idx <- which(dimnames(tdm)$Terms %in% c("alienware", "dell"))
    idx
    (freq.terms <- findFreqTerms(tdm, lowfreq = 10))
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq >= 10)
    df <- data.frame(term = names(term.freq), freq = term.freq)
    df
    library(RColorBrewer)
    m <- as.matrix(tdm)
    # calculate the frequency of words and sort it by frequency
    word.freq <- sort(rowSums(m), decreasing = T)
    # colors
    pal <- brewer.pal(9, "BuGn")[-(1:4)]
    # plot word cloud
    library(wordcloud)
    
    ?wordcloud
     wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10, rot.per = 0.35,
               random.order = F, max.words = 1000, colors = brewer.pal(8, "Dark2"), scale = c(6,1))
    
  })
}

ui <- fluidPage(
  
  titlePanel("Twitter"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", label = h3("Search"), 
                value = "alienware"),
                submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("histPlot")
    )
  )
)

shinyApp(ui = ui, server = server)

