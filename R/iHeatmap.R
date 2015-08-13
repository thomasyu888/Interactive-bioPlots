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
                     colors = "YlOrRd",
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
  #----------------------------------------------------
  ## Define the variables
  #----------------------------------------------------
  mainData<-x
  options<-NULL
  addonHead <- NULL
  add <- NULL
  rowDend <- NULL
  rowHead <- NULL
  rowAnnotes <- rowAnnote
  colDend <- NULL
  colHead <- NULL
  colAnnotes <- colAnnote

  #----------------------------------------------------
  ## Catching errors
  #----------------------------------------------------

  ## Since the data is split into quantiles of 0.1. this will only take multiples of 10
  if (!(probs %in% c(100,90,80,70,60,50,40,30,20,10,0))) {
    stop("probs needs to be a multiple of 10 from 0 to 100")
  }
  ## If input data isnt matrix, make it a matrix
  if(!is.matrix(mainData)) {
    mainData <- as.matrix(x)
  }
  if(!is.matrix(mainData)) stop("x (mainData) must be a matrix")

  ## Matrix should have row/column names
  if (is.null(colnames(mainData))) {
    colnames(mainData)<-c(1:dim(mainData)[2])
  }

  if (is.null(rownames(mainData))) {
    rownames(mainData)<-c(1:dim(mainData)[1])
  }

  # scale before the dendrogram
  scale = match.arg(scale)
  if (scale=="column") {
    mainData <- scale(mainData)
  } else if (scale=="row") {
    mainData <- t(scale(t(mainData)))
  }
  ##setting quantiles, so that the there can be more contrast in colors
  prepared <- quantile(mainData,seq(0,1,0.1))
  rng <- range(prepared[paste((100-as.integer(probs)),'%',sep="")],prepared[paste(probs,'%',sep="")])
  domain <- seq.int(rng[2], rng[1], length.out = 100)

  hackcolors <- leaflet::colorNumeric(colors, 1:100)(100:1)

  #----------------------------------------------------
  ## Row annotations
  #----------------------------------------------------

  if (!is.null(rowAnnote)) {
    #Convert to matrix
    if(!is.matrix(rowAnnote)) {
      rowAnnote <- as.matrix(rowAnnote)
    }
    #Check if matrix is in the correct format
    if (dim(rowAnnote)[2]==dim(mainData)[1]) {
      rowAnnote <- t(rowAnnote)
    }

    #check to see if the same dimensions
    if (dim(rowAnnote)[1]!=dim(mainData)[1]) {
      rowAnnotes<-NULL
      print("Incorrect row annotation dimensions, your annotations will not display.")
    } else {
      #Check it colnames exist
      if (is.null(colnames(rowAnnote))) {
        colnames(rowAnnote) = c(1:dim(rowAnnote)[2])
        print("row annotations don't have title (ie. weight, height...)")
      }
      rowtitle <- colnames(rowAnnote)

      #If rownames do not exist, if not set rownames = to mainData rownames
      if (is.null(rownames(rowAnnote))) {
        print("No row annotation names, so can't be matched to matrix row names
              (default assumes user passes in annotations that matches the matrix)")
      } else { #Make sure row annotations match the mainData
        rowAnnote<- as.matrix(rowAnnote[rownames(mainData),])
        colnames(rowAnnote)<-rowtitle
        rownames(rowAnnote)<-NULL
      }
      rowHead <- matrix(colnames(rowAnnote))
    }
  }
  #----------------------------------------------------
  ## Column annotations
  #----------------------------------------------------
  if (!is.null(colAnnote)) {
    #Convert to matrix
    if(!is.matrix(colAnnote)) {
      colAnnote <- as.matrix(colAnnote)
    }
    #Check if matrix is in the correct format
    if (dim(colAnnote)[2]==dim(mainData)[2]) {
      colAnnote <- t(colAnnote)
    }

    #check to see if the same dimensions
    if (dim(colAnnote)[1]!=dim(mainData)[2]) {
      colAnnotes<-NULL
      print("Incorrect column annotation dimensions, your annotations will not display.")
    } else {
      #Check it colnames exist
      if (is.null(colnames(colAnnote))) {
        colnames(colAnnote) = c(1:dim(colAnnote)[2])
        print("column annotations don't have title (ie. weight, height...)")
      }
      coltitle <- colnames(colAnnote)

      #If rownames do not exist, if not set rownames = to mainData rownames
      if (is.null(rownames(colAnnote))) {
        print("No column annotation names, so can't be matched to matrix column names
              (default assumes user passes in annotations that matches the matrix)")
      } else { #Make sure row annotations match the mainData
        colAnnote<- as.matrix(colAnnote[colnames(mainData),])
        colnames(colAnnote)<-coltitle
        rownames(colAnnote)<-NULL
      }
      colHead <- matrix(colnames(colAnnote))
    }
  }

  #----------------------------------------------------
  ## ROWV -> To cluster row or not to cluster row
  #----------------------------------------------------
  ##Cluster_mat returns hclust object
  if (Rowv) {
    rowClust <- cluster_mat(mainData, distM, ClustM, cor_method)
    mainData <- mainData[rowClust$order,]
    if (!is.null(rowAnnotes)) {
      rowAnnotes <- rowAnnote[rowClust$order,]
    }
    rowDend <- HCtoJSON(rowClust)
  }

  #----------------------------------------------------
  ## COLV -> To cluster column or not to cluster column
  #----------------------------------------------------
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
  #------------------------
  #COLOR SCHEME FOR HEATMAP
  #------------------------
  colors <- scales::col_numeric(colors, rng, na.color = "transparent")
  imgUri <- encodeAsPNG(t(mainData), colors)

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
  #----------------------------------------------------
  ## Option to not show heatmap
  #----------------------------------------------------
  if (showHeat) {
    matrix <- list(data = as.numeric(t(mainData)),
                   dim = dim(mainData),
                   ##defined %||% function which does paste(..) if row.names is NULL
                   rows = row.names(mainData)%||% paste(1:nrow(mainData)),
                   cols = colnames(mainData)%||% paste(1:ncol(mainData)),
                   colors = hackcolors,
                   domain = domain)
  } else {
    matrix <- list(dim = dim(mainData),
                   cols = colnames(mainData))
  }

  x <- list(rows = rowDend, cols = colDend, colMeta = colMeta,rowMeta = rowMeta, image=imgUri,matrix = matrix,addon = addon,options = options)

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
#' @import png base64enc
encodeAsPNG <- function(x, colors) {
  colorData <- as.raw(col2rgb(colors(x), alpha = TRUE))
  dim(colorData) <- c(4, ncol(x), nrow(x))
  pngData <- png::writePNG(colorData)
  encoded <- base64enc::base64encode(pngData)
  paste0("data:image/png;base64,", encoded)
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
#' headerPanel("A heatmap demo"),
#' sidebarPanel(
#'   selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
#'   selectInput("cluster_method", "Clustering Method", c('complete', 'average','ward.D2','single','mcquitty','median','centroid')),
#'   selectInput("cluster_row","Cluster rows",c('TRUE', 'FALSE')),
#'   selectInput("cluster_col","Cluster columns",c('TRUE','FALSE')),
#'   selectInput("dist","Distance method",c('euclidean','correlation','maximum','manhattan','canberra','binary','minkowski'))
#' ),
#' mainPanel(
#'   iHeatmapOutput("heatmap")
#' )
#' )
#'
#' server <- function(input, output, session) {
#'   output$heatmap <- renderIHeatmap({
#'     col_annot <- matrix(runif(11),11,1)
#'     row_annot <- matrix(runif(32),32,1)
#'     iHeatmap(
#'       mtcars,colAnnote=round(col_annot,1),rowAnnote= round(row_annot,1),
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
