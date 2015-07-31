shinyServer(function(input, output) {
  output$heatmap <- renderIHeatmap({
    col_annot <- matrix(runif(11),1,11)
    colnames(col_annot)<-colnames(mtcars)
    col_annot <- col_annot[,sample.int(11)]

    row_annot <- matrix(runif(32),32,1)
    iHeatmap(
      mtcars,colAnnote=round(col_annot,1),rowAnnote= round(row_annot,1),
      scale="column",colors = input$palette,
      ClustM= input$cluster_method, distM= input$dist,
      Colv = input$cluster_col,Rowv = input$cluster_row
    )
  })
})
