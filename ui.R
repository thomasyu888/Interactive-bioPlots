require(shiny)
require(htmlwidgets)
require(iHeatmap)
library(flashClust)

shinyUI(fluidPage(
  headerPanel("Interactive Heatmap"),

  sidebarPanel(
   selectInput(inputId = "x",
               label = "Clustering method",
               choices = c('complete', 'average','ward','single','mcquitty','median','centroid'),
               selected = "complete"),

   selectInput(inputId = "y",
               label = "Cluster rows",
               choices = c('TRUE', 'FALSE'),
               selected = 'TRUE'),

   selectInput(inputId = "z",
               label = "Cluster columns",
               choices = c('TRUE','FALSE'),
                selected = 'TRUE'),

    selectInput(inputId = "v",
                label = "Distance method",
                choices = c('correlation','euclidean','maximum','manhattan','canberra','binary','minkowski'),
                selected = 'euclidean')
  ),

  mainPanel(
    iHeatmapOutput('myChart')
  )
))
