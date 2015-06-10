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

  ## sees if rownames/ col names exist for entered matrix
  if (length(row.names(mainData))==0) {
    row.names(mainData) = c(1:dim(mainData)[1])
  }
  if (length(colnames(mainData))== 0) {
    colnames(mainData) = c(1:dim(mainData)[2])
  }

  if (!is.null(rowAnnote)) {
      if (is.null(colnames(rowAnnote))) {
        colnames(rowAnnote) = c(1:dim(rowAnnote)[2])
      }
      if (length(rowAnnote[,1])==dim(mainData)[1]) {
        #rowAnnotes <- matrix(rowAnnotes)
        rowHead <- matrix(colnames(rowAnnote))
      } else { ## If the length of annotations are different don't display it
        rowAnnotes <- NULL
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
    }
  }

  if (Rowv) {
    rowClust <- hclust(dist(mainData,distM),ClustM)
    mainData <- mainData[rowClust$order,]
    if (!is.null(rowAnnotes)) {
      rowAnnotes <- rowAnnote[rowClust$order,]
    }
    rowDend <- HCtoJSON(rowClust)
  }

  if (Colv) {
    colClust <- hclust(dist(t(mainData),distM),ClustM)
    mainData <- mainData[,colClust$order]
    if (!is.null(colAnnotes)) {
      colAnnotes <- colAnnote[colClust$order,]
    }
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
    showheat = showHeat))

  #########FIX THIS!!!
  #########FIX THIS!!!
  ##Dealing with outliers.. Simple boxplot$out
  ##rng <- range(mainData[abs(mainData)<min(abs(boxplot(mainData)$out))])
  rng <- range(mainData)
  domain <- seq.int(ceiling(rng[2]), floor(rng[1]), length.out = 100)
  colors <- leaflet::colorNumeric(colors, 1:100)(1:100)
#Mid point as median
#White is the midpoint
  colMeta <- list(data = colAnnotes,
                  header = colHead)
  rowMeta <- list(data = rowAnnotes,
                  header = rowHead)
  addon <- list(data = add,
                 header = addonHead)

  if (showHeat) {
    matrix <- list(data = as.numeric(t(mainData)),
                   dim = dim(mainData),
                   rows = row.names(mainData),
                   cols = colnames(mainData),
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
