#' Simple function to plot trace pulled from nwb file. This does not plot the stimulus and
#' returns a void style plot
#'
#' @param x file path
#' @param sweeps numeric or character string
#' @param plot_sweep do you want the sweep plotted?
#' @param return_sweep do you want to return the df used to create the sweep
#'
#' @return
#' @examples
#'
#'
#'
#' @export
plotVoidNWB = function(x, sweeps, plot_sweep = TRUE, return_sweep = FALSE){

    sweep = nphys::extractNWB(x, sweeps = sweeps, slim = TRUE)

    df = data.frame(sweep, zoo::index(sweep))
    names(df) = c("y","x")


    ggSweep = ggplot2::ggplot(df, aes(x, y)) +
        ggplot2::geom_line() +
        ggplot2::theme_void()

    if (plot_sweep) {
      base::plot(ggSweep)
    }

    if (return_sweep) {
      return(df)
    }

}
