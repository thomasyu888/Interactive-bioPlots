m  <- runif(1000)
m <- matrix(m, ncol=10)
d <- runif(10)
d <- matrix(d,10,1)
f <- runif(100)
f <- matrix(f,100,1)
shinyServer(function(input, output) {
  output$myChart <- renderIHeatmap(
    iHeatmap(m,colAnnote = round(d,1),Rowv=input$y,Colv=input$z,distM = input$v,ClustM = input$x)
  )
})

