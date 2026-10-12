#' Plot traceplots for your samples from extensionSamplers
#' @param samples Input a matrix of samples as outputted by the samplers in this package
#' @param variable Variable which should be plotted (SUPPORTS ONLY ONE VARIABLE)
#' @param extension Is the provided sample an extension sample?
#' @param alpha Parameter toggling level of opacitiy for moves outside the fibre (alpha = 1 is the same as extension = FALSE)
#' @param ... Any parameters added to the plot() function
#'
#' @return Plots the traceplots for any sample from the package.
#' @export
traceplot <- function(samples, variable, extension = FALSE, alpha = 0.1, ...) {
  nVariables <- ncol(samples) - 3
  nChains <- max(samples[, nVariables + 1])
  nSample <- max(samples[, nVariables + 2])

  if (extension && alpha == 1) {
    warning(
      "Since alpha = 1, the non-extension traceplot will be plotted"
    )
    extension <- FALSE
  }

  colors <- grDevices::hcl.colors(nChains, palette = "Dark 3")
  x <- samples[, variable]

  if (extension) {
    inside <- apply(samples, 1, function(row) all(row >= 0))

    xInside <- rep(NA_real_, length(x))
    xInside[inside] <- x[inside]
  }

  for (ii in seq_len(nChains)) {
    idx <- (ii - 1) * nSample + seq_len(nSample)

    lineColor <- if (extension) {
      scales::alpha(colors[ii], alpha)
    } else {
      colors[ii]
    }

    if (ii == 1) {
      graphics::plot(
        x[idx],
        type = "l",
        ylab = bquote(x[.(variable)]),
        xlab = "Iteration",
        col = lineColor,
        lwd = 1,
        ...
      )
    } else {
      graphics::lines(
        x[idx],
        col = lineColor
      )
    }

    if (extension) {
      graphics::lines(
        xInside[idx],
        col = colors[ii],
        lwd = 1
      )
    }
  }

  graphics::legend(
    "top",
    inset = c(0, -0.15),
    xpd = NA,
    legend = paste("Chain", seq_len(nChains)),
    col = colors,
    lty = 1,
    horiz = TRUE,
    lwd = 2,
    bty = "n"
  )
}
