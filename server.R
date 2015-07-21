m  <- runif(4840)
m <- matrix(m, ncol=110)
d <- runif(110)
d <- matrix(d,110,1)
f <- runif(44)
f <- matrix(f,44,1)

shinyServer(function(input, output) {
  output$myChart <- renderIHeatmap(
    iHeatmap(m,
             colAnnote = round(d,1),
             rowAnnote = round(f,1),
             Rowv=input$y,Colv=input$z,distM = input$v,ClustM = input$x)
  )
})
