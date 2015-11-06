#' Plot multiple ggplot2s in one page.
#'
#' Stolen shamelessly from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'
#' @param ... plot objects for plotting.
#' @param plotlist alternative input of plots, as list.
#' @param cols number of columns in viewing window.
#' @param layout layout matrix for plots.
#' @examples
#'
#' rm(list=ls())
#'
#'library(ggplot2)
#'library(hunterr)
#'
#'# This example uses the ChickWeight dataset, which comes with ggplot2
#'# First plot
#'p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
#'  geom_line() +
#'  ggtitle("Growth curve for individual chicks")
#'
#'# Second plot
#'p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
#'  geom_point(alpha=.3) +
#'  geom_smooth(alpha=.2, size=1) +
#'  ggtitle("Fitted growth curve per diet")
#'
#'# Third plot
#'p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
#'  geom_density() +
#'  ggtitle("Final weight, by diet")
#'
#'# Fourth plot
#'p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
#'  geom_histogram(colour="black", binwidth=50) +
#'  facet_grid(Diet ~ .) +
#'  ggtitle("Final weight, by diet") +
#'  theme(legend.position="none")        # No legend (redundant in this graph)
#'
#'multiplot(p1, p2, p3, p4, cols=2)
#'multiplot(p1, p2, p3, p4, layout_mat = matrix(c(1,2,3,4,4,4), nrow=2, byrow=TRUE))
#'
#' @export

multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}
