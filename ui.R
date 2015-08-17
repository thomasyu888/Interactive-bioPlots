require(shiny)
require(htmlwidgets)
require(iHeatmap)


shinyUI(fluidPage(
  headerPanel("A heatmap demo"),
  sidebarPanel(
    selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
    selectInput("cluster_method", "Clustering Method", c('complete', 'average','ward.D2','single','mcquitty','median','centroid')),
    selectInput("cluster_row","Cluster rows",c('TRUE', 'FALSE')),
    selectInput("cluster_col","Cluster columns",c('TRUE','FALSE')),
    selectInput("dist","Distance method",c('euclidean','correlation','maximum','manhattan','canberra','binary','minkowski')),
    selectInput("probs","Quantile",c("0-100"=100,"10-90"=90,"20-80"=80))
  ),
  mainPanel(
    iHeatmapOutput("heatmap")
  )
))
