# Interactive-bioPlots

Displays an interactive heatmap that can zoom into a selected area on the heatmap and dendrograms.  Download the git. To run:

>devtools::install()

>library(iHeatmap)

>iHeatmap(scales(mtcars))

EXTENSION OF JOE CHENG'S rstudio/d3heatmap.  There's no way I would have been able to do this without his work. Thanks to his d3heatmap I was able to put together an extended version with annotations and zoom feature on the dendrograms. Furthermore, thanks to HTML widgets and Shiny as well. Extremely cool.

References:

https://github.com/rstudio/d3heatmap

http://shiny.rstudio.com/

http://www.htmlwidgets.org/
