#' @import htmlwidgets
#' @import shiny
NULL
`%||%` <- function(a, b) {
  ##This defines the function that does returns a if a isn't null, and b if a is null
  if (!is.null(a))
    a
  else
    b
}

#' Interactive Heatmap
#'
#'Creates a D3.js-based heatmap widget.
#'
#'
#'@param x A numeric \code{m x n} matrix.
#'@param colAnnote Column annotations-
#'  Takes a \code{n x 1} matrix.
#'@param rowAnnote Row annotations-
#'  Takes a \code{m x 1} matrix.
#'@param width Width in pixels (optional, defaults to automatic sizing).
#'@param height Height in pixels (optional, defaults to automatic sizing).
#'@param colors Either a colorbrewer2.org palette name (e.g. \code{"RdYlBu"} or
#'   \code{"Blues"}).
#'@param ClustM The agglomeration (Linkage) method to be used.
#'  This should be (an unambiguous abbreviation of) one of \code{"ward.D"}, \code{"ward.D2"},
#'  \code{"single"}, \code{"complete"}, \code{"average"} (= UPGMA), \code{"mcquitty"} (= WPGMA),
#'  \code{"median"} (= WPGMC) or \code{"centroid"} (= UPGMC).
#'@param distM The distance measure to be used. This must be one of \code{"correlation"},\code{"euclidean"},
#'  \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"} or \code{"minkowski"}.
#'@param Colv Determines if columns should be clustered (defaults to \code{TRUE})
#'@param Rowv Determines if rows should be clustered (defaults to \code{TRUE})
#'@param showHeat Only show the dendrogram.
#'@param addOnInfo For adding on extra information
#'@param scale Determine if the data is to be scaled by \code{"row"} or \code{"column"}, defaults to \code{"none"}
#'@param cor_method Determins correlation method, defaults to \code{"pearson"}
#'@param probs Determines the quantile, defaults to \code{100}
#'
#' @export
#' @source
#' The interface was designed based on Joe cheng's \link{d3heatmap}.
#'
#'@examples
#' library(iHeatmap)
#' iHeatmap(mtcars, scale = TRUE, col_scale=TRUE, colors = "Blues")


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
                     xaxis_height = 100,
                     yaxis_width = 100,
                     anim_duration=500,
                     showHeat = TRUE,
                     addOnInfo = NULL,
                     scale = c("none","column","row"),
                     cor_method = "pearson",
                     font_size = 10,
                     probs = 100,
                     annote_pad = 7,
                     legend_width=50,
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
## Since the data is split into quantiles of 0.1. this will only take multiples of 10
  if (!(probs %in% c(100,90,80,70,60,50,40,30,20,10,0))) {
    stop("probs needs to be a multiple of 10 from 0 to 100")
  }
##====================
  if(!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if(!is.matrix(x)) stop("x must be a matrix")
##====================
  #if(!is.matrix(colAnnote)) {
  #  colAnnote <- as.matrix(colAnnote)
  #}
  #if(!is.matrix(colAnnote)) stop("colAnnote must be a matrix")
##====================
  #if(!is.matrix(rowAnnote)) {
  #  rowAnnote <- as.matrix(rowAnnote)
  #}
  #if(!is.matrix(rowAnnote)) stop("rowAnnote must be a matrix")


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
  scale = match.arg(scale)
  if (scale=="column") {
      mainData <- scale(mainData)
  } else if (scale=="row") {
      mainData <- t(scale(t(mainData)))
  }
  #rng <- range(prepared[paste(probs,'%',sep=""], prepared[paste((1-probs),'%',sep="")])
  #domain <- seq.int(quantile(mainData)[["75%"]], quantile(mainData)[["25%"]], length.out = 100)
   #else {
    #rng <- range(mainData[!mainData %in% boxplot.stats(mainData)$out])
    #rng <- range(quantile(mainData)[["75%"]], quantile(mainData)[["25%"]])
  #}
  prepared <- quantile(mainData,seq(0,1,0.1))
  rng <- range(prepared[paste(probs,'%',sep="")],
               prepared[paste((100-as.integer(probs)),'%',sep="")])
  domain <- seq.int(rng[2], rng[1], length.out = 100)

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
    xaxis_height = xaxis_height,
    yaxis_width = yaxis_width,
    anim_duration = anim_duration,
    showheat = showHeat,
    font_size = font_size,
    annote_pad = annote_pad,
    legend_width = legend_width))


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

#' Wrapper functions for using d3heatmap in shiny
#'
#' Use \code{iHeatmapOutput} to create a UI element, and \code{renderIHeatmap}
#' to render the heatmap.
#'
#' @param outputId Output variable to read from
#' @param width,height The width and height of the map (see
#'   \link[htmlwidgets]{shinyWidgetOutput})
#' @param expr An expression that generates a \code{\link{iHeatmap}} object
#' @param env The environment in which to evaluate \code{expr}
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @examples
#' \donttest{
#' library(iHeatmap)
#' library(shiny)
#'
#' ui <- fluidPage(
#'   h1("A heatmap demo"),
#'   selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
#'   selectInput("cluster_method", "Clustering Method", c('complete', 'average','ward','single','mcquitty','median','centroid'))
#'   selectInput("cluster_row","Cluster rows",c('TRUE', 'FALSE'))
#'   selectInput("cluster_col","Cluster columns",c('TRUE','FALSE'))
#'   selectInput("dist","Distance method",c('euclidean','correlation','maximum','manhattan','canberra','binary','minkowski'))
#'   iHeatmapOutput("heatmap")
#' )
#'
#' server <- function(input, output, session) {
#'   output$heatmap <- renderIHeatmap({
#'    col_annot <- matrix(runif(11),11,1)
#'    row_annot <- matrix(runif(32),32,1)
#'     iHeatmap(
#'       mtcars,colAnnote=col_annot,rowAnnote=row_annot,
#'       scale="column",colors = input$palette,
#'       ClustM= input$cluster_method, distM= input$dist,
#'       Colv = input$cluster_col,Rowv = input$cluster_row
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
iHeatmapOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'iHeatmap', width, height, package = 'iHeatmap')
}

#' @rdname iHeatmapOutput
#' @export
renderIHeatmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, iHeatmapOutput, env, quoted = TRUE)
}
