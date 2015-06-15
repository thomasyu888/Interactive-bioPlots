#' Interactive Heatmap
#'
#' EXTENSION OFF JOE CHENG'S d3heatmap/rstudio.  THIS BY NO MEANS IS COMPLETELY
#' MY OWN WORK.  CREDITS TO JOE CHENG!!!
#'
#' @import htmlwidgets
#'
#' @export
#'
NULL
##This defines the function that does returns a if a isn't null, and b if a is null
`%||%` <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

iHeatmap <- function(x,
                     colAnnote=NULL,
                     rowAnnote=NULL,
                     width = NULL,
                     height = NULL,
                     colors = "RdYlBu",
                     ClustM = "complete",
                     distM = "euclidean",
                     Colv = TRUE,
                     Rowv = TRUE,
                     xaxis_height = 120,
                     yaxis_width = 120,
                     anim_duration=500,
                     showHeat = TRUE,
                     addOnInfo = NULL,
                     scale = FALSE,
                     col_scale = TRUE,
                     cor_method = "pearson",
                     font_size = 10,
                     ...) {
  ## Define the variables
  mainData <- as.matrix(x)
  options<-NULL
  addonHead <- NULL
  add <- NULL
  rowDend <- NULL
  rowHead <- NULL
  rowAnnotes <- rowAnnote
  colDend <- NULL
  colHead <- NULL
  colAnnotes <- colAnnote
##Scale before the dendrogram grouping or else it makes no sense.
### ### ### #####  ######## ### ### #####  ######## ### ### #####  ######## ### ### #####
#########FIX THIS!!!####
#########FIX THIS!!!####

##Dealing with outliers.. For now this works?
#rng <- range(mainData[!mainData %in% boxplot.stats(mainData)$out])
#The red should be really red and the blue should just be really blue
### FIX THE COLOR SCALE#### FIX the scale function tooo...

#domain <- seq.int(ceiling(quantile(temp)[["75%"]]), floor(quantile(temp)[["25%"]]), length.out = 100)
#rng <- range(mainData[!mainData %in% boxplot.stats(mainData)$out])
#temp <- t(rescale_mid(t(mainData),mid = mean(mainData)))

  if (scale) {
    if (col_scale) {
      mainData <- scale(mainData)
    } else {
      mainData <- t(scale(t(mainData)))
    }
    rng <- range(mainData)
    #domain <- seq.int(quantile(mainData)[["75%"]], quantile(mainData)[["25%"]], length.out = 100)
  } else {
    rng <- range(mainData[!mainData %in% boxplot.stats(mainData)$out])

  }
  domain <- seq.int(rng[2], rng[1], length.out = 100)
  #domain <- seq.int(ceiling(rng[2]), floor(rng[1]), length.out = 100)
  colors <- leaflet::colorNumeric(colors, 1:100)(1:100)
#Mid point as median
#White is the midpoint

######  ### ### ### #####  ######## ### ### #####  ######## ### ### #####  #####
######################################
######################

  if (!is.null(rowAnnote)) {
      if (is.null(colnames(rowAnnote))) {
        colnames(rowAnnote) = c(1:dim(rowAnnote)[2])
      }
      if (length(rowAnnote[,1])==dim(mainData)[1]) {
        #rowAnnotes <- matrix(rowAnnotes)
        rowHead <- matrix(colnames(rowAnnote))
      } else { ## If the length of annotations are different don't display it
        rowAnnotes <- NULL
        print("row annotations not the same dimension")
      }
  }

  if (!is.null(colAnnote)) {
    if (is.null(colnames(colAnnote))) {
      colnames(colAnnote) = c(1:dim(colAnnote)[2])
    }
    if (length(colAnnote[,1])==dim(mainData)[2]) {
      #colAnnotes <- matrix(colAnnotes)
      colHead <- matrix(colnames(colAnnote))
    } else { ##If the length of annotations are different don't display it
      colAnnotes <- NULL
      print("col annotations not the same dimension")
    }
  }
##Cluster_mat returns flashClusted matrix
  if (Rowv) {
    rowClust <- cluster_mat(mainData, distM, ClustM, cor_method)
    mainData <- mainData[rowClust$order,]
    if (!is.null(rowAnnotes)) {
      rowAnnotes <- rowAnnote[rowClust$order,]
    }
    rowDend <- HCtoJSON(rowClust)
  }

  if (Colv) {
    colClust <- cluster_mat(t(mainData), distM, ClustM, cor_method)
    mainData <- mainData[,colClust$order]
    if (!is.null(colAnnotes)) {
      colAnnotes <- colAnnote[colClust$order,]
    }
    ##Addon Info is only the for (X) side (So patients...)
    if (!is.null(addOnInfo)) {
      addonHead <- matrix(colnames(addOnInfo))
      add <- addOnInfo[colClust$order,]
      add <- matrix(add)
    }
    colDend <- HCtoJSON(colClust)
  }

  options <- c(options, list(
    xaxis_height = xaxis_height ,
    yaxis_width = yaxis_width,
    anim_duration = anim_duration,
    showheat = showHeat,
    font_size = font_size))


  colMeta <- list(data = colAnnotes,
                  header = colHead)
  rowMeta <- list(data = rowAnnotes,
                  header = rowHead)
  addon <- list(data = add,
                 header = addonHead)

  if (showHeat) {
    matrix <- list(data = as.numeric(t(mainData)),
                   dim = dim(mainData),
                   ##defined %||% function which does paste(..) if row.names is NULL
                   rows = row.names(mainData)%||% paste(1:nrow(mainData)),
                   cols = colnames(mainData)%||% paste(1:ncol(mainData)),
                   colors = colors,
                   domain = domain)
  } else {
    matrix <- list(dim = dim(mainData),
                   cols = colnames(mainData))
  }

  x <- list(rows = rowDend, cols = colDend, colMeta = colMeta,rowMeta = rowMeta, matrix = matrix,addon = addon,options = options)

  # create widget
  htmlwidgets::createWidget(
    name = 'iHeatmap',
    x,
    width = width,
    height = height,
    package = 'iHeatmap',
    sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE)
  )
}

#' Widget output function for use in Shiny
#'
#' @export
iHeatmapOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'iHeatmap', width, height, package = 'iHeatmap')
}

#' Widget render function for use in Shiny
#'
#' @export
renderIHeatmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, iHeatmapOutput, env, quoted = TRUE)
}
