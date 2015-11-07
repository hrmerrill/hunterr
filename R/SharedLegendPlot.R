#' Plot multiple ggplot2s in one page, with a shared legend.
#'
#' Stolen shamelessly from https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs, with a modification to allow for custom layout.
#'
#' @param ... plot objects for plotting.
#' @param layout_mat layout matrix for plots.
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
#'sharedlegendplot(p1, p2, p3, p4, layout = matrix(c(1,2,3,4,4,4), nrow=2, byrow=TRUE))
#'
#' @export

sharedlegendplot <- function(..., layout = NULL) {

  plots <- list(...)
  g <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  list_of_plots <- lapply(plots, function(x)
    x + ggplot2::theme(legend.position="none"))
  if(!is.null(layout_mat)) list_of_plots$layout_matrix <- layout
  gridExtra::grid.arrange(
    do.call(gridExtra::arrangeGrob, list_of_plots),
    legend,
    ncol = 1,
    heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight))
}

