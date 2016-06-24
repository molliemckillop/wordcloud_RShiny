library(shiny)

function(input, output, session) {
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$word_cloud <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(7,1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })

  candData <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        getCandDf(input$selection)
      })
    })
  })
  # output$freq_plot<- renderPlot({
  #   
  #     df <- candData()
  #     
  #     df$numeric <- as.numeric(df$created_at)
  # 
  #   hist(df$created_at,
  #        #probability = TRUE,
  #        #breaks = "mins",
  #        breaks = as.numeric(input$n_breaks),
  #        xlab = "Duration (minutes)",
  #        main = "Tweet Density Over 30 Minutes")
  #   
  #   if (input$individual_obs) {
  #     rug(as.numeric(df$created_at))
  #   }
  #   
  #   if (input$density) {
  #     dens <- density(df$numeric,
  #                     adjust = input$bw_adjust)
  #     lines(dens, col = "blue")
  #  }
    
  # })
  
}