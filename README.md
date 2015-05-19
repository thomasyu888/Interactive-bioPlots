# Interactive-bioPlots

Displays an interactive heatmap that can zoom into a selected area on the heatmap and dendrograms. To run:

>devtools::install_github("thomasyu888/Interactive-bioPlots")

>d <- runif(32)

>d <- matrix(d,32,1)

>f <- runif(11)

>f <- matrix(f,11,1)

>library(iHeatmap)

>iHeatmap(scale(mtcars),colAnnote = f,rowAnnote = d)

EXTENSION OF JOE CHENG'S rstudio/d3heatmap.  There's no way I would have been able to do this without his work. Thanks to his d3heatmap I was able to put together an extended version with annotations and zoom feature on the dendrograms. Furthermore, thanks to HTML widgets and Shiny as well. Extremely cool.

References:

https://github.com/rstudio/d3heatmap

http://shiny.rstudio.com/

http://www.htmlwidgets.org/
