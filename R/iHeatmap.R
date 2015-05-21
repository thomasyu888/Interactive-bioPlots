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

iHeatmap <- function(mainData,
                     colAnnote=NULL,
                     rowAnnote=NULL,
                     theme = NULL,
                     width = NULL,
                     height = NULL,
                     ClustM = "complete",
                     distM = "euclidean",
                     Colv = TRUE,
                     Rowv = TRUE,
                     showHeat = TRUE,...) {

  ## sees if rownames/ col names exist for entered matrix
  if (length(row.names(mainData))==0) {
    row.names(mainData) = c(1:dim(mainData)[1])
  }
  if (length(colnames(mainData))== 0) {
    colnames(mainData) = c(1:dim(mainData)[2])
  }

  #########FIX THIS!!!
  #########FIX THIS!!!
  #########FIX THIS!!!

  if (Rowv) {
    rowClust <- hclust(dist(mainData,distM),ClustM)
    mainData <- mainData[rowClust$order,]
    if (!is.null(rowAnnote)) {
      rowAnnotes <- rowAnnote[rowClust$order,]
    }
    rowDend <- HCtoJSON(rowClust)
  } else {
    rowDend = NULL
    rowAnnotes <- rowAnnote
  }
  ### NEED TO RUN EVEN IF METADATA is different dimensions
  if (Colv) {
    colClust <- hclust(dist(t(mainData),distM),ClustM)
    mainData <- mainData[,colClust$order]
    if (!is.null(colAnnote)) {
      colAnnotes <- colAnnote[colClust$order,]
    }
    colDend <- HCtoJSON(colClust)
  } else {
    colDend = NULL
    colAnnotes <- colAnnote
  }

  if (!is.null(rowAnnote)) {
    if (length(row.names(rowAnnote))==0) {
      row.names(rowAnnote) = c(1:dim(rowAnnote)[1])
      colnames(rowAnnote) = c(1:dim(rowAnnote)[2])
    }
    if (length(rowAnnote[,1])==dim(mainData)[1]) {
      rowAnnotes <- matrix(rowAnnotes)
      rowHead <- colnames(rowAnnote)
    } else {
      rowAnnotes <- NULL
      rowHead <- NULL
    }
  } else {
    rowAnnotes <- rowAnnote
    rowHead <- NULL
  }

  if (!is.null(colAnnote)) {
    if(length(row.names(colAnnote))==0) {
      row.names(colAnnote) = c(1:dim(colAnnote)[1])
      colnames(colAnnote) = c(1:dim(colAnnote)[2])
    }
    if (length(colAnnote[,1])==dim(mainData)[2]) {
      colAnnotes <- matrix(colAnnotes)
      colHead <- colnames(colAnnote)
    } else {
      colAnnotes <- NULL
      colHead <- NULL
    }
  } else {
    colAnnotes <- colAnnote
    colHead <- NULL
  }
  #########FIX THIS!!!
  #########FIX THIS!!!

  ##Dealing with outliers.. Simple boxplot$out
  ##rng <- range(mainData[abs(mainData)<min(abs(boxplot(mainData)$out))])
  rng <- range(mainData)
  domain <- seq.int(ceiling(rng[2]), floor(rng[1]), length.out = 100)
  colors <- heat.colors(100)
  colors <- sub('FF$', '', colors)

  colMeta <- list(data = colAnnotes,
                  header = colHead)
  rowMeta <- list(data = rowAnnotes,
                  header = rowHead)
  if (showHeat) {
    matrix <- list(data = as.numeric(t(mainData)),
                   dim = dim(mainData),
                   rows = row.names(mainData),
                   cols = colnames(mainData),
                   colors = colors,
                   domain = domain)
  } else {
    matrix <-list(dim = dim(mainData),
                  cols = colnames(mainData))
  }

  x <- list(rows = rowDend, cols = colDend, theme = theme, colMeta = colMeta,rowMeta = rowMeta, matrix = matrix)

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
