#' Mega plotting function.
#' 
#' Create plots with optional density histograms in the margin, colorized points by 
#' a grouping vector, contours or aggregated means of values.
#' 
#' @author Shawn Driscoll
#' 
#' @param x Vector or matrix.
#' @param y Second vector to plot on y-axis. If omitted then the values in \code{x} are
#' plotted on the y-axis with x-axis values \code{1:length(x)}.
#' @param grid Whether or not to draw a grid in the plot
#' @param col Color for plotted objects. This field can be a little flexible for instance
#' if you want to provide specific colors for each point or if you are specifying \code{groups}
#' and you want to assign colors per group. If you want to assign colors by group then 
#' the length of this vector of colors should match the number of levels in the grouping vector.
#' @param cex See \link{plot}. Size setting for points in point-plots.
#' @param cex.axis Size setting for axis text.
#' @param add Indicates that the call should add to the current plot instead of replace it.
#' @param groups Grouping vector for plotted objects. Each group will be plotted with a
#' separate color.
#' @param groups2 Second grouping for grouping the groups specified in \code{groups}. 
#' Sometimes the average of the averaged values is more appropriate than the overall average.
#' @param main Optional title string for the plot
#' @param xlab String label for x-axis. Default: "x"
#' @param ylab String label for y-axis. Default: "y"
#' @param xlim Limits for x-axis
#' @param ylim Limits for y-axis
#' @param xpad Padding for x-axis. This is calculated as a ratio of the range of values 
#' on the x-axis. Useful for making it so the edges of the plot are further from the data
#' when you're adding labels to points or trying to fit a legend in the plot area.
#' @param ypad See \code{xpad}. By default \code{ypad} is set to the same value as \code{xpad}
#' @param col.grid Color for grid lines. Default: grey
#' @param lty.grid Linetype for grid lines. Default: dashed
#' @param bg.col Background color for the plot area. This is also controlled with the 
#' \code{theme} setting. Default: white.
#' @param theme Either \code{none} or \code{gg}. \code{gg} mimics the default plot style
#' of ggplot2.
#' @param cex.lab Text size setting for axis labels (xlab and ylab).
#' @param pch Point shape setting. If specifying \code{groups} this value can be set to 
#' a vector specifying a specific shape per group.
#' @param symm.axis Forces the x and y-axis ranges to be identical.
#' @param show.density Draws density histogram, using \link{density} in the margin of the plot
#' @param density.mar Margin setting for density.
#' @param scale.density Scales the density's y-axis by the number of input values to the
#'   \code{density} function call.  Sometimes useful when you want the size of a group to 
#'   by represented in the height of the density plot when using \code{groups}
#' @param contour.plot Draws a contour plot instead of a scatter plot.
#' @param contour.kde.n Sets \code{n} in \link{kde2d} function call. Default: 512
#' @param contour.lines Sets \code{nlevels} in \link{contour} function call. Default: 10
#' @param contour.q Used for setting the \code{zlim} setting in \link{contour}. This setting
#'   controls how you might want to clip the full z-range of the density calculated. Default
#'   is to display the full range.
#' @param contour.lwd Lineweight for contour lines.
#' @param contour.col Color for the contour lines. Defaults to the setting for \code{col}
#' @param contour.with.points Set to TRUE to overaly a scatter plot of the x/y data points
#'   on top of the contour plot
#' @param aggr.plot Aggregates data by mean or median and plots the result. See details.
#' @param aggr.fun Either \code{mean} or \code{median}. See \link{aplot.statErr}
#' @param aggr.err.fun Either \code{sd}, \code{sem}, \code{var}, or \code{ci}. See \link{aplot.statErr}
#' @param aggr.bootstraps If set to a value greater than 1 then \code{aggr.fun} is bootstrapped. See \link{aplot.statErr}
#' @param aggr.boot.err.mode See \link{aplot.statErr}
#' @param aggr.grand.stat Plot grand statistic. This only comes into play if you're using \code{groups}
#' @param aggr.pch Point shape for aggregated data points (separate from raw data points). 
#' Note: this only works when you do NOT have \code{groups} specified. Otherwise the point style is 
#' set to match the group's point style.
#' @param aggr.cex Point size for aggregated data points.
#' @param aggr.with.points Plot input data points as well as the aggregated points.
#' @param eb List with error bar high/low values for plot data. List members should be 
#'   named \code{x} and/or \code{y} and each one should be a two-column matrix specifying the 
#'   error bar boundaries. 'low' is first column and 'high' is second column.
#' @param eb.col Color of error bars. Defaults to value of \code{col}
#' @param eb.length Length of the edges of the error bar. Passed to the \code{length} 
#'   setting of the \link{arrows} function.
#' @param eb.code Sets the kind of arrowhead. Default is 3 which draws a typical error bar.
#' @param eb.angle Sets angle from the shaft of the error bar to the edge of the head.
#' @param eb.lwd Line weight of the error bars.
#' @param show.legend Adds a legend into the plot area when \code{groups} is set
#' @param legend.cols Number of columns in the legend
#' @param legend.pos Position of the legend. See \link{legend} for available options.
#' @param legend.cex Size setting for the legend. Controls both point and font sizes.
#' @param legend.horiz Draws the legend horizontally. See \link{legend} for details
#' @param identify Allows you to pick points on an existing plot. When finished picking 
#'   points then point indices or labels are drawn in the plot area. Labels can be set
#'   with \code{labels}
#' @param cex.labels Size setting for point labels
#' @param col.labels Color for point labels
#' @param labels Vector of labels for points. Should be equal in length to the number
#'   of plotted points.
#' @param vline Vector of values specifying x-positions for vertical lines to be drawn.
#' @param vline.col Color for vertical lines.
#' @param vline.lty Linetype for vertical lines.
#' @param vline.lwd Line thickness for vertical lines.
#' @param hline Vector of values specifying y-positions for horizontal lines to be drawn.
#' @param hline.col Color for horizontal lines
#' @param hline.lty Linetype for horizontal lines
#' @param hline.lwd Line thickness for horizontal lines
#' @param overlay If set to TRUE then instead of making a new plot the plot data is 
#'   passed to \link{points} and plotted on top of the current plot. Useful for highlighting
#'   a subset of your data in a different color. Note that you could also accomplish that using
#'   a \code{groups} vector.
#' @param overlay.pch Point shape for overlay points
#' @param overlay.cex Point size for overlay points
#' @param overlay.col Point color for overlay points
#' @param highlight Either a logical vector the same length as \code{x} or a vector of 
#'   indices to \code{x}. Points specified in this way are added like an overlay to
#'   the main plot data with separate settings for point size, color, and shape. You 
#'   may also include separate highlight labels and control their size and color.
#' @param highlight.cex Point size for highlighted points.
#' @param highlight.col Point color for highlighted points.
#' @param highlight.pch Point shape for highlighted points.
#' @param highlight.labels Labels for highlighted points. Should be the same length vector
#'   as \code{highlight}
#' @param highlight.label.cex Font size for highlight labels
#' @param highlight.label.col Font color for highlight labels
#' @param mar.bottom Plot window bottom margin
#' @param mar.left Plot window left margin
#' @param mar.top Plot window top margin. This is adjusted automatically when you add a title
#'   with \code{main}
#' @param mar.right Plot window right margin
#'
#' @details This plotting function provides a lot of options. You can provide values to plot and a grouping 
#' vector with \code{groups} to colorize points by group. If you'd like to plot the group means
#' with error bars specify \code{aggr.plot=TRUE} and then choose your preferred setting for 
#' error bars by setting \code{aggr.err.fun}.
#' 
#' When creating contour plots it is recommended to set \code{xpad} to avoid contour lines 
#' being clipped at the edges of the plot.
#' 
#' To label some points on the plot and not others you can create a single label vector and 
#' set values to NA for those that you do not want called out.
#' 
#' @export
#' @seealso \link{plot}
#' @seealso \link{kde2d}
#' @seealso \link{contour}
#' @seealso \link{pointLabel}
#' 
#' @import RColorBrewer
#' @import maptools
#' @import MASS
#' @examples
#' # create some data for plotting
#' x <- c(rnorm(30, -2, 1), rnorm(30, 2, 1))
#' y <- x * rnorm(60, 0, 0.5)
#' groups <- rep(c("A", "B"), each=30)
#' 
#' # Create scatter plot of points
#' aplot(x, y)
#' 
#' # Plot again showing groups with colors
#' aplot(x, y, groups=groups)
#' 
#' # Plot group means with error bars
#' aplot(x, y, groups=groups, aggr.plot=TRUE)
#' 
#' # Plot bootstrapped group means with 95% confidence interval of the means
#' aplot(x, y, groups=groups, aggr.plot=TRUE, aggr.bootstraps=1e3, 
#'   aggr.err.fun="ci")
#' 
#' # Add the raw data points, increase the point size for the means and 
#' # set group specific point shapes
#' aplot(x, y, groups=groups, aggr.plot=TRUE, aggr.bootstraps=1e3, 
#'   aggr.err.fun="ci", aggr.with.points=TRUE, aggr.cex=2, pch=c(17, 15))
#' 
#' # Add density histograms in the margins
#' aplot(x, y, groups=groups, aggr.plot=TRUE, aggr.bootstraps=1e3, 
#'   aggr.err.fun="ci", aggr.with.points=TRUE, aggr.cex=2, pch=c(17, 15), 
#'   show.density=TRUE)
#' 
#' # Make a contour plot of the raw data. Add some padding so that the
#' # contours do not get clipped at the ends of the plot.
#' aplot(x, y, contour.plot=TRUE, xpad=0.5)
#' 
#' # Split data by groups, add in the raw data points.
#' aplot(x, y, contour.plot=TRUE, xpad=0.5, groups=groups, 
#'   contour.with.points=TRUE)
#' 
#' # Put everything in the plot.
#' aplot(x, y, contour.plot=TRUE, xpad=0.5, groups=groups, 
#'   contour.with.points=TRUE, aggr.plot=TRUE, aggr.cex=2, eb.lwd=2, 
#'   pch=c(17, 15), show.density=TRUE)
aplot <- function(x, y=NULL, grid=TRUE, col="#660000", cex=1, add=FALSE, 
	groups=NULL, groups2=NULL, xlab="x", ylab="y", 
	col.grid="grey", lty.grid="dashed", bg.col="white", theme=c("none", "gg"), 
	cex.axis=0.7, xlim=NULL, ylim=NULL, xpad=0, ypad=xpad, main=NULL,
	cex.lab=1, pch=20, symm.axis=FALSE, 
	show.density=FALSE, density.mar=0.2, scale.density=FALSE, 
	contour.plot=FALSE, contour.kde.n=512, contour.lines=10, contour.q=c(0, 1), 
	contour.lwd=1, contour.col=col, contour.with.points=FALSE, 
	aggr.plot=FALSE, aggr.fun=c("mean", "median"), aggr.err.fun=c("sd", "sem", "var", "ci"), 
	aggr.bootstraps=1, aggr.boot.err.mode=1, aggr.grand.stat=FALSE, 
	aggr.pch=19, aggr.cex=1, aggr.with.points=FALSE, 
	eb=NULL, eb.col="#00000044", eb.length=0.05, eb.code=3, eb.angle=90, eb.lwd=1, 
	show.legend=TRUE, legend.cols=1, legend.pos="topleft", legend.cex=0.8, legend.horiz=FALSE, 
	identify=FALSE, cex.labels=0.7, col.labels="black", labels=NULL, 
	vline=NULL, vline.col="black", vline.lty="solid", vline.lwd=1,
	hline=NULL, hline.col=vline.col, hline.lty=vline.lty, hline.lwd=vline.lwd,
	overlay.pch=1, overlay=FALSE, overlay.cex=1, overlay.col="black", 
	highlight=NULL, highlight.cex=cex, highlight.col="dodgerblue", highlight.pch=pch, highlight.labels=NULL, 
	highlight.label.cex=cex.labels, highlight.label.col=highlight.col, 
	mar.bottom=3.1, mar.left=3.1, mar.top=0.8, mar.right=0.8, ...) {

	# need this
	if(!require(RColorBrewer)) stop("Missing package 'RColorBrewer'.\n--> Please run install.packages('RColorBrewer') to install.")
	if(!require(MASS)) stop("Missing package 'MASS'.\n--> Please run install.packages('MASS') to install.")
	if(!require(maptools)) stop("Missing package 'maptools'.\n--> Please run install.packages('maptools') to install.")

	# default margins without a title or anything like that
	mmar <- c(mar.bottom, mar.left, mar.top, mar.right)

	mar0 <- par()$mar

	if(!missing(main)) {
		mmar[3] <- mmar[3]+2
	}

	aggr.fun <- match.arg(aggr.fun)
	aggr.err.fun <- match.arg(aggr.err.fun)

#	print(as.list(match.call(expand.dots=FALSE)))
	#print(list(...))

	has.eb <- c(FALSE, FALSE)
	has.g <- !missing(groups)
	has.g2 <- !missing(groups2)
	density.y <- TRUE
	density.x <- !missing(y)
	x.eb <- NULL
	y.eb <- NULL
	if(!is.null(dim(x))) {
		npoints <- nrow(x)
	} else {
		npoints <- length(x)
	}
	pch.ok <- 1:20

	theme <- match.arg(theme)

	# theme
	if(theme=="gg") {
		bg.col <- "grey90"
		col.grid <- "white"
		lty.grid <- "solid"
	}

	# error bars?
	if(!(missing(eb))) {
		if(!inherits(eb, "list")) {
			stop("Error bar info should be passed as a list with $x and $y members for 'x' and 'y' data error boundaries, respectivly")
		}			
		has.eb <- c(TRUE, TRUE)
		if(!is.null(names(eb))) {
			names(eb) <- tolower(names(eb))
		}
		
	}

	if(missing(y)) {
		if(is.null(dim(x))) {
			# plotting a single vector
			y <- x
			x <- 1:length(x)
			has.eb[1] <- FALSE

			if(any(has.eb)) {
				if(length(eb)==1) {
					y.eb <- eb[[1]]
				} else if("y" %in% names(eb)) {
					y.eb <- eb$y
				} else {
					message("[aplot] unable to resolve error bar data. skipping.")
					has.eb <- c(FALSE, FALSE)
				}
			}
		} else if(ncol(x) > 1) {
			# plotting first two columns of the passed matrix
			density.x <- TRUE

			y <- x[, 2]
			x <- x[, 1]
			if(any(has.eb)) {
				if(length(eb) > 1) {					
					if("x" %in% names(eb)) {
						x.eb <- eb$x
					} else {
						x.eb <- eb[[1]]
					}
					if("y" %in% names(eb)) {
						y.eb <- eb$y
					} else {
						y.eb <- eb[[2]]
					}
				} else {
					message("[aplot] there are not as many error bar data dimensions as data. skipping.")
					has.eb <- c(FALSE, FALSE)
				}
			}
		} else {
			# one column in the matrix

			x <- drop(x)
			y <- x
			x <- 1:length(y)
			has.eb[1] <- FALSE

			if(any(has.eb)) {
				if(length(eb)==1) {
					y.eb <- eb[[1]]
				} else if("y" %in% names(eb)) {
					y.eb <- eb$y
				} else {
					message("[aplot] unable to resolve error bar data. skipping.")
					has.eb <- c(FALSE, FALSE)
				}
			}

		}
	} else {
		if(any(has.eb)) {
			if(length(eb) > 1) {					
				if("x" %in% names(eb)) {
					x.eb <- eb$x
				} else {
					x.eb <- eb[[1]]
				}
				if("y" %in% names(eb)) {
					y.eb <- eb$y
				} else {
					y.eb <- eb[[2]]
				}
			} else {
				message("[aplot] there are not as many error bar data dimensions as data. skipping.")
				has.eb <- c(FALSE, FALSE)
			}
		}
	}

	#
	# groups and colors
	gcol <- col

	if(has.g2) {
		if(!inherits(groups2, "factor")) {
			groups2 <- factor(groups2)
		}
		if(length(groups2) != npoints) {
			stop("'groups2' length is not equal to length of data")
		}
		if(!has.g) {
			# pass groups2 to groups
			message("[aplot] 'groups' is missing but 'groups2' is set. using 'groups2' as 'groups'")
			groups <- groups2
			has.g <- TRUE
			has.g2 <- FALSE
		}
	}

	if(!has.g) {
		# setup single group
		groups <- factor(make.names(as.character(rep(1, npoints))))
		lgroups <- levels(groups)
		ngroups <- length(lgroups)

		has.g <- TRUE

		# check color
		if(length(gcol)==1) {
			# if 'col' is only 1 value rep it out to the data length
			col <- rep(gcol, npoints)
		} else if(length(gcol) < npoints) {
			message("'col', has multiple values but does not match data dimension. using first value.")
			col <- rep(gcol[1], npoints)
		} else if(length(gcol) > npoints) {
			# not sure what to do here
			message("'col' vector is longer than data size, trimming it down.")
			col <- gcol[1:npoints]
		} else {
			col <- gcol
		}
		
		# check 'pch'
		if(length(pch)==1) {
			# if 'pch' is only 1 value rep it out to the data length
			pch <- rep(pch, npoints)
		} else if(length(pch) < npoints) {
			message("'pch', has multiple values but does not match data dimension. using first value.")
			pch <- rep(pch[1], npoints)
		} else if(length(pch) > npoints) {
			# not sure what to do here
			message("'pch' vector is longer than data size, trimming it down.")
			pch <- pch[1:npoints]
		}

		# check 'cex'
		if(length(cex)==1) {
			# if 'cex' is only 1 value rep it out to the data length
			cex <- rep(cex, npoints)
		} else if(length(cex) < npoints) {
			message("'cex', has multiple values but does not match data dimension. using first value.")
			cex <- rep(cex[1], npoints)
		} else if(length(cex) > npoints) {
			# not sure what to do here
			message("'cex' vector is longer than data size, trimming it down.")
			cex <- cex[1:npoints]
		}

	} else {
		##
		# we have groups so here we get the group vector setup and we setup the colors
		# for each group.
		##
		if(!inherits(groups, "factor")) {
			if(is.numeric(groups)) {
				groups <- factor(groups, levels=sort(unique(groups)))
			} else {
				groups <- factor(groups)
			}
		}

		lgroups <- levels(groups)
		ngroups <- length(lgroups)

		# colors
		if(length(gcol)==1 || length(gcol) < ngroups) {
			gcol <- aplot.brewerSets(ngroups)
#			if(ngroups <= 8) {
#				gcol <- rev(brewer.pal(max(c(ngroups, 3)), "Dark2"))[1:ngroups]
#			} else if(ngroups <= 12) {
#				gcol <- brewer.pal(max(c(ngroups, 3)), "Set3")[1:ngroups]
#			} else {
#				gcol <- rainbow(ngroups)
#			}
			col <- gcol[as.numeric(groups)]
		} else if(length(gcol)==ngroups) {
			col <- gcol[as.numeric(groups)]
		} else if(length(gcol)==npoints) {
			col <- gcol
		} else if(length(gcol) > ngroups) {
			gcol <- gcol[1:ngroups]
			col <- gcol[as.numeric(groups)]
		}

		#
		# check 'pch'
		# with groups when the pch vector is longer than 1 but shorter than the data it 
		# will be interpreted to intend to be per-group symbols
		if(length(pch)==1) {
			# one point, expand it to match groups
			gpch <- rep(pch, ngroups)
			pch <- gpch[as.numeric(groups)]
		} else if(length(pch)==npoints) {
			# for whatever reason 'pch' is the same length as the data but we also have
			# groups so we'll setup gpch to be the majority type per group
			gpch <- sapply(split(pch, groups), function(x) {
				tt <- unique(x)
				ttn <- sapply(tt, function(a) sum(x==a))
				ix <- which.max(ttn)
				return(tt[ix])
			})
			# keep pch as is
		} else if(length(pch)==ngroups) {
			# pch matches number of groups perfectly!
			gpch <- pch
			pch <- gpch[as.numeric(groups)]
		} else {
			# != 1 && != npoints && != ngroups
			if(length(pch) < ngroups) {
				message("fewer plot symbols than groups. expanding...")
				foo <- setdiff(pch.ok, pch)
				ndiff <- ngroups-length(pch)
				if(length(foo) >= ndiff) {
					gpch <- c(pch, foo[1:ndiff])
					pch <- gpch[as.numeric(groups)]
				} else {
					# too many groups so we'll use all symbols 
					pch <- c(pch, foo)
					ndiff <- ngroups-length(pch)
					pch <- c(pch, letters[1:pmin(ndiff, length(letters))])
					ndiff <- ngroups-length(pch)
					if(ndiff > 0) {
						# last one addition before wrapping
						pch <- c(pch, toupper(letters[1:pmin(ndiff, length(letters))]))
						ndiff <- ngroups-length(pch)
						if(ndiff > 0) {
							# wrap it - we're up to 20+26+26 total groups
							rr <- ceiling(ngroups/length(pch))
							pch <- rep(pch, rr)
							gpch <- pch[1:ngroups]
							pch <- gpch[as.numeric(groups)]
						}
					}
				}
			} else {
				# longer than ngroups
				message("'pch' is longer than number of groups. trimming.")
				gpch <- pch[1:ngroups]
				pch <- gpch[as.numeric(groups)]
			}
		}


		#
		# check 'cex'
		# with groups when the cex vector is longer than 1 but shorter than the data it 
		# will be interpreted to intend to be per-group symbols

		n.cex <- length(cex)
		if(n.cex > 1 & n.cex < ngroups) {
			message("cex vector is not as long as group count. using first value")
			cex <- cex[1]
		} else if(n.cex > 1 & n.cex > ngroups & n.cex < npoints) {
			message("cex vector is longer than number of groups, trimming")
			cex <- cex[1:ngroups]
		} else if(n.cex > npoints) {
			message("cex vector is longer than total number of points, trimming")
			cex <- cex[1:npoints]
		}

		if(length(cex)==1) {
			# one point, expand it to match groups
			gcex <- rep(cex, ngroups)
			cex <- gcex[as.numeric(groups)]
		} else if(length(cex)==npoints) {
			# for whatever reason 'cex' is the same length as the data but we also have
			# groups so we'll setup gcex to be the majority type per group
			gcex <- sapply(split(cex, groups), function(x) {
				tt <- unique(x)
				ttn <- sapply(tt, function(a) sum(x==a))
				ix <- which.max(ttn)
				return(tt[ix])
			})
			# keep cex as is
		} else if(length(cex)==ngroups) {
			# cex matches number of groups perfectly!
			gcex <- cex
			cex <- gcex[as.numeric(groups)]
		} 

	}

	if(length(col) != length(x)) {
		message("[aplot] for some reason the color vector length is not equal to the data size")
		col <- rep(col[1], length(x))
	}

	# split up the point indices by group
	sidx <- split(1:length(x), groups)

	#
	# figure out plot limits
	#

	if(missing(xlim)) {
		xlim <- range(x[is.finite(x)])
		dr <- diff(xlim)
		if(has.eb[1]) {
			# adjust range to error bars
			xlim <- range(finite(x.eb))
		}
		#ur <- dr*xpad/2
		#xlim[1] <- xlim[1]-ur
		#xlim[2] <- xlim[2]+ur
	}
	dr <- diff(xlim)
	ur <- dr*xpad/2
	xlim <- xlim + c(-1, 1)*ur
	
	if(missing(ylim)) {
		ylim <- range(y[is.finite(y)])
		dr <- diff(ylim)
		if(has.eb[2]) {
			ylim <- range(finite(y.eb))
		}
		#ur <- dr*ypad/2
		#ylim <- c(ylim[1]-ur, ylim[2]+ur)
	}
	dr <- diff(ylim)
	ur <- dr*ypad/2
	ylim <- ylim + c(-1, 1)*ur

	if(show.density) {

		if(density.x) {
			ddx <- density(x)
			# make sure x-limits is enough to show the whole density curve
			xlim[1] <- min(xlim, ddx$x)
			xlim[2] <- max(xlim, ddx$x)
		}

		if(density.y) {
			ddy <- density(y)
			# fix y limits
			ylim[1] <- min(ylim, ddy$x)
			ylim[2] <- max(ylim, ddy$x)
		}

	}


	if(identify) {
		# -------------------------------------------------------------------------
		#
		# IDENTIFY MODE
		#
		# -------------------------------------------------------------------------

		if(missing(labels)) {
			labels <- 1:length(x)
		}
		return(identify(x, y, labels, cex=cex.labels, col=col.labels, ...))
	} else {

		# -------------------------------------------------------------------------
		#
		# MAKE THE PLOT
		#
		# -------------------------------------------------------------------------
		
		#
		# make plot
		#

		if(show.density) {
			par(new=FALSE, mar=mmar)
		} else {
			par(new=FALSE, mar=mmar)
		}

		#
		# deal with densities, if plotting them
		#
		if(show.density) {

			# 
			# figure out plot figure boundaries
			#
			if(density.x && density.y) {
				fig.dx <- c(0, 1-density.mar, 1-density.mar, 1)
				fig.dy <- c(1-density.mar, 1, 0, 1-density.mar)
				fig.plot <- c(0, 1-density.mar, 0, 1-density.mar)
			} else if(density.x) {
				fig.dx <- c(0, 1, 1-density.mar, 1)
				fig.plot <- c(0, 1, 0, 1-density.mar)
			} else if(density.y) {
				fig.dy <- c(1-density.mar, 1, 0, 1)
				fig.plot <- c(0, 1-density.mar, 0, 1)
			}

			# plot x-density
			if(density.x) {
				marDx <- mmar
				marDx[1] <- 0
				par(new=FALSE, fig=fig.dx, mar=marDx)

				#
				# loop through groups and make all of the group
				#

				lddx <- lapply(sidx, function(idx) {
					d <- density(x[idx])
					if(scale.density) {
						d$y <- d$y*length(idx)
					}
					return(d)
				})
				# get overall ylimit
				tmp <- max(unlist(lapply(lddx, function(k) k$y)))
				ddylim <- c(0, tmp)

				plot(0, 0, xaxt='n', xlab="", ylab="density", type='n', tck=-0.01, cex.axis=cex.axis, 
					mgp=c(1.6*cex.lab, 0.3*cex.axis, 0), xlim=xlim, ylim=ddylim)

				if(bg.col != "white") {
					lim <- par("usr")
					rect(lim[1], lim[3], lim[2], lim[4], col=bg.col, border=NA)			
				}

				if(grid) {
					grid(col=col.grid, lty=lty.grid)
				}

				for(i in 1:length(sidx)) {
					points(lddx[[i]]$x, lddx[[i]]$y, type='l', lwd=2, col=gcol[i])
				}

				par(mar=mmar)

			}

			# plot y-density
			if(density.y) {
				marDy <- mmar
				marDy[2] <- 0
				# plot y-density
				if(density.x) {
					par(new=TRUE, fig=fig.dy, mar=marDy)
				} else {
					par(new=FALSE, fig=fig.dy, mar=marDy)
				}
				
				lddy <- lapply(sidx, function(idx) {
					d <- density(y[idx])
					if(scale.density) {
						d$y <- d$y*length(idx)
					}
					return(d)
				})
				# get overall ylimit
				tmp <- max(unlist(lapply(lddy, function(k) k$y)))
				ddylim <- c(0, tmp)

				plot(0, 0, yaxt='n', ylab="", xlab="density", type='n', tck=-0.01, cex.axis=cex.axis, 
					mgp=c(1.6*cex.lab, 0.3*cex.axis, 0), ylim=ylim, xlim=ddylim)

				if(bg.col != "white") {
					lim <- par("usr")
					rect(lim[1], lim[3], lim[2], lim[4], col=bg.col, border=NA)			
				}

				if(grid) {
					grid(col=col.grid, lty=lty.grid)
				}

				for(i in 1:length(sidx)) {
					points(lddy[[i]]$y, lddy[[i]]$x, type='l', lwd=2, col=gcol[i])
				}				

#				plot(ddy$y, ddy$x, yaxt='n', xlab="density", ylab="", type='l', tck=-0.01, cex.axis=cex.axis, 
#					lwd=2, mgp=c(1.6*cex.lab, 0.3*cex.axis, 0), ylim=ylim)
#				if(grid) {
#					grid(col=col.grid, lty=lty.grid)
#				}

				par(mar=mmar)
			}

			par(new=TRUE, fig=fig.plot)

		}

		#
		# start the main plot
		#
		plot(x, y, tck=-0.01, mgp=c(1.6*cex.lab, 0.3*cex.axis, 0), cex.axis=cex.axis, 
				xlim=xlim, ylim=ylim, main=main, cex.lab=cex.lab, type='n', xlab=xlab, ylab=ylab)

		# 
		# deal with background color
		if(bg.col != "white") {
			lim <- par("usr")
			rect(lim[1], lim[3], lim[2], lim[4], col=bg.col, border=NA)			
		}

		# add grid
		if(grid) {
			grid(col=col.grid, lty=lty.grid)
		}

		# add vertical or horizontal line(s)
		if(!missing(vline)) {
			abline(v=vline, col=vline.col, lwd=vline.lwd, lty=vline.lty)
		}
		if(!missing(hline)) {
			abline(h=hline, col=hline.col, lwd=hline.lwd, lty=hline.lwd)
		}

		#
		# loop through groups and add plots
		for(i in 1:length(sidx)) {
			# get indices from the current group
			idx <- sidx[[i]]

			# add contours?
			if(contour.plot) {
				# make 2d kernel density
				dd <- MASS::kde2d(x[idx], y[idx], h=c(MASS::width.SJ(x[idx]), MASS::width.SJ(y[idx])), n=contour.kde.n, 
					lims=c(xlim, ylim))
				if(length(contour.q)==1) {
					contour.q <- c(contour.q, 1)
				}
				zq <- min(dd$z)+diff(range(dd$z))*contour.q
				contour(dd, add=TRUE, col=gcol[i], lwd=contour.lwd, zlim=zq, nlevels=contour.lines, 
					drawlabels=FALSE)
			}


			if((!contour.plot && !aggr.plot) || (contour.plot && contour.with.points) || (aggr.plot && aggr.with.points)) {
				#
				# plot data. this will come up for line points or point plots
				#

				if(has.eb[1]) {
					# plot eb for x
					arrows(x.eb[idx, 1], y[idx], x.eb[idx, 2], y[idx], code=eb.code, angle=eb.angle, col=eb.col, length=eb.length, lwd=eb.lwd)
				}
				if(has.eb[2]) {
					# plot eb for y
					arrows(x[idx], y.eb[idx, 1], x[idx], y.eb[idx, 2], code=eb.code, angle=eb.angle, col=eb.col, length=eb.length, lwd=eb.lwd)
				}

				points(x[idx], y[idx], col=col[idx], cex=cex[idx], pch=pch[idx], ...)

			}

			if(aggr.plot) {
				
				# --
				#
				# aggregation data plot
				#
				# --

				if(has.g2) {
					aggr.res.x <- aplot.statErr(x[idx], fun=aggr.fun, err.fun=aggr.err.fun, 
						num.bootstraps=aggr.bootstraps, boot.err.mode=aggr.boot.err.mode, 
						grand.stat=aggr.grand.stat, 
						groups=factor(as.character(groups2[idx])))
					aggr.res.y <- aplot.statErr(y[idx], fun=aggr.fun, err.fun=aggr.err.fun, 
						num.bootstraps=aggr.bootstraps, boot.err.mode=aggr.boot.err.mode, 
						grand.stat=aggr.grand.stat, 
						groups=factor(as.character(groups2[idx])))
				} else {
					aggr.res.x <- aplot.statErr(x[idx], fun=aggr.fun, err.fun=aggr.err.fun, 
						num.bootstraps=aggr.bootstraps, boot.err.mode=aggr.boot.err.mode, 
						grand.stat=aggr.grand.stat)					
					aggr.res.y <- aplot.statErr(y[idx], fun=aggr.fun, err.fun=aggr.err.fun, 
						num.bootstraps=aggr.bootstraps, boot.err.mode=aggr.boot.err.mode, 
						grand.stat=aggr.grand.stat)					
				}


				if(is.null(dim(aggr.res.x))) {
					
					xhat <- aggr.res.x[1]
					xerr <- aggr.res.x[2:3]
					yhat <- aggr.res.y[1]
					yerr <- aggr.res.y[2:3]

					arrows(xerr[1], yhat, xerr[2], yhat, code=eb.code, angle=eb.angle, col=gcol[i], length=eb.length, lwd=eb.lwd)
					arrows(xhat, yerr[1], xhat, yerr[2], code=eb.code, angle=eb.angle, col=gcol[i], length=eb.length, lwd=eb.lwd)
					points(xhat, yhat, cex=aggr.cex, col=gcol[i], pch=gpch[i])

				} else {
					
					aggr.res.x <- t(aggr.res.x)
					xhat <- aggr.res.x[, 1]
					xerr <- aggr.res.x[, 2:3]
					aggr.res.y <- t(aggr.res.y)
					yhat <- aggr.res.y[, 1]
					yerr <- aggr.res.y[, 2:3]

					arrows(xerr[, 1], yhat, xerr[, 2], yhat, code=eb.code, angle=eb.angle, col=gcol[i], length=eb.length, lwd=eb.lwd)
					arrows(xhat, yerr[, 1], xhat, yerr[, 2], code=eb.code, angle=eb.angle, col=gcol[i], length=eb.length, lwd=eb.lwd)
					points(xhat, yhat, cex=aggr.cex, col=gcol[i], pch=gpch[i])

				}


			}

		}

		if(overlay) {
			points(x, y, col=overlay.col, cex=overlay.cex, pch=overlay.pch)
		}	

		if(!missing(highlight)) {
			if(inherits(highlight, "logical")) {
				highlight <- which(highlight)
			}
			if(length(highlight) > 0) {
				data.n <- length(x)
				if(max(highlight) <= data.n & min(highlight) > 0) {
					# ok!
					points(x[highlight], y[highlight], col=highlight.col, pch=highlight.pch, cex=highlight.cex)
				}
				if(!missing(highlight.labels)) {
					if(length(highlight.labels)==length(highlight)) {
						maptools::pointLabel(x[highlight], y[highlight], highlight.labels, cex=highlight.label.cex, col=highlight.label.col)
					}
				}
			}
		}

	}
	

	if(!missing(labels)) {
		# add labels with pointLabel
		maptools::pointLabel(x, y, labels, cex=cex.labels, col=col.labels)
	}

	if(has.g & ngroups > 1) {
		if(show.legend) {

			group.counts <- sapply(lgroups, function(k) sum(groups==k))
			labs <- paste(lgroups, "(", group.counts, ")", sep="")
			if(contour.plot & contour.with.points) {
				legend(legend.pos, legend=labs, col=gcol, lwd=contour.lwd, pch=gpch, cex=legend.cex, bg="white", horiz=legend.horiz, ncol=legend.cols)
			} else if(contour.plot) {
				legend(legend.pos, legend=labs, col=gcol, lwd=contour.lwd, cex=legend.cex, bg="white", horiz=legend.horiz, ncol=legend.cols)
			} else {
				legend(legend.pos, legend=labs, col=gcol, pch=gpch, cex=legend.cex, bg="white", horiz=legend.horiz, ncol=legend.cols)
			}
			
		}
	}

	par(mar=mar0, fig=c(0, 1, 0, 1))
	
}



#' Helper function to stick RColorBrewer palettes Set1, Set2, and Set3 together
#' and remove the bright yellow colors. If you provide a value for \code{n} then
#' it will return that many colors starting from the first. If the value of \code{n}
#' is longer than the total length of the colors then the colors are interpolated
#' with \link{colorRampPalette}.
#' 
#' @param n Number of colors to return. If \code{n == NULL} then all of the colors are returned.
#' @return A vector of colors
#' @export
aplot.brewerSets <- function(n=NULL) {
	if(!require(RColorBrewer)) stop("Please install 'RColorBrewer' package")

	info <- brewer.pal.info
	cc <- lapply(c("Set1", "Set2", "Set3"), function(k) {
		x <- brewer.pal(info[k, "maxcolors"], k)
		return(x)
	})
	cc <- unlist(cc)
	
	idx <- grepl("^#FFFF", cc)
	if(any(idx)) {
		cc <- cc[!idx]
	}
	cc <- c("#333333", cc)

	if(n <= length(cc)) {
		return(cc[1:n])
	}

	cchat <- colorRampPalette(cc)

	return(cchat(n))

}


#' Calculate mean/median and error of values in vector \code{x}
#' 
#' @param x Vector of numbers
#' @param fun Statistic. Either \code{mean} or \code{median}. Default is \code{mean}
#' @param err.fun Error/uncertainty to calculate. May be \code{sd}, \code{sem}, \code{var}, or \code{ci}. \code{ci} returns the quantiles of the input values unless you are bootstrapping in which case the confidence interval of the bootstrap distribution is returned.
#' @param ci Confidence interval to report when using \code{err.fun="ci"}
#' @param num.bootstraps Number of boostrap iterations. If \code{num.bootstraps == 1} then no bootstrapping is performed.
#' @param boot.err.mode Set to either 1, default or 2. Default behavior is to apply the
#'   error function, \code{err.fun}, to the bootstrap distribution. When set to 2
#'   the error is bootstrapped and the mean of it's bootstrap distribution is returned.
#' @param groups Grouping vector for values in \code{x}. If provided then this function operates on each group of values and returns a matrix of results.
#' @param grand.stat If using \code{groups} this calculates the overall statistic and uncertainty by first calculating the group statistic and uncertainties then propagating that into the grand statistic.
#' @return A vector of values or a matrix if \code{groups} was specified.
#' @export
aplot.statErr <- function(x, fun=c("mean", "median"), err.fun=c("sd", "sem", "var", "ci"), 
	ci=0.95, num.bootstraps=1, 
	boot.err.mode=1, groups=NULL, grand.stat=FALSE) {

	fun <- match.arg(fun)
	err.fun <- match.arg(err.fun)

	# setup primary stat function
	xfun <- mean
	if(fun=="median") {
		xfun <- median
	}

	if(num.bootstraps > 1 && boot.err.mode==1) {
		# if running statistics on the bootstrap distribution we should only take the 
		# standard-deviation or the confidence interval
		if(err.fun=="sem" || err.fun=="var") {
			err.fun <- "sd"
		}
	}

	# if we're using median and we are NOT bootstrapping then we have to use 
	# inter-quantile range in place of standard deviation. if we're bootstrapping and 
	# we going to take stats on the bootstrap distribution (boot.err.mode==1) 
	# then we can leave it as is but if we're gonna boot the stat then we have to 
	# switch it to IQR again
	if(fun=="median" && (num.bootstraps==1 || (num.bootstraps > 1 && boot.err.mode==2))) {
		if(err.fun=="sd" || err.fun=="var") {
			err.fun <- "iqr"
		} else if(err.fun=="sem") {
			err.fun <- "iqr.sem"
		}
	}

	# setup error function
	if(err.fun=="sd") {
		xerrFun <- function(x, mu) {
			v <- sd(x)
			return(c(c(-1, 1)*v + mu, v))
		}
	} else if(err.fun=="sem") {
		xerrFun <- function(x, mu) {
			v <- sd(x)/sqrt(length(x)-1)
			return(c(c(-1, 1)*v + mu, v))
		}
	} else if(err.fun=="var") {
		xerrFun <- function(x, mu) {
			v <- var(x)
			return(c(c(-1, 1)*v + mu, v))
		}
	} else if(err.fun=="ci") {
		xerrFun <- function(x, mu, ci=0.95) {
			alpha <- (1-ci)/2
			v <- quantile(x, c(alpha, 1-alpha))
			names(v) <- NULL
			return(c(v, NA))
		}
	} else if(err.fun=="iqr") {
		xerrFun <- function(x, mu) {
			v <- iqr(x)
			return(c(v[3:4], NA))
		}
	} else if(err.fun=="iqr.sem") {
		xerrFun <- function(x, mu) {
			v <- iqr(x)
			return(c(v[3:4]/sqrt(length(x)-1), NA))
		}
	}	

	nx <- length(x)

	if(missing(groups)) {
		groups <- rep(1, nx)
	}

	if(!inherits(groups, "factor")) {
		if(is.numeric(groups)) {
			groups <- factor(paste("G", groups, sep="."), levels=paste("G", sort(unique(groups)), sep="."))
		} else {
			groups <- factor(make.names(groups))
		}
	}

	lgroups <- levels(groups)
	ngroups <- length(lgroups)

	#
	# continue on as though we have groups even if there is only one
	#	

	sidx <- split(1:nx, groups)

	if(grand.stat && num.bootstraps > 1 && ngroups > 1) {
		# if we're bootstrapping then this has to work a little differently
		set.seed(42)
		if(fun=="median") {
			tstar <- replicate(num.bootstraps, 
				bs.median(saggr(x, groups, median, bstrap=TRUE)))
		} else if(fun=="mean") {
			tstar <- replicate(num.bootstraps, 
				bs.mean(saggr(x, groups, mean, bstrap=TRUE)))			
		}

		tmp <- mean(tstar)
		if(err.fun=="ci") {
			tmp <- c(tmp, quantile(tstar, c(0.025, 0.975)), NA)
		} else if(err.fun=="sd") {
			tmp <- c(tmp, c(-1, 1)*sd(tstar)+tmp, sd(tstar))
		}

	} else {


		tmp <- sapply(sidx, function(idx) {
			
			xhat <- x[idx]
			xhat.n <- length(xhat)

			if(num.bootstraps > 1) {
				# bootstrapping
				set.seed(42)
				tstar <- replicate(num.bootstraps, xfun(sample(xhat, xhat.n, replace=TRUE)))
				stat <- mean(tstar)
				# error
				if(boot.err.mode==2) {
					# bootstrap the error statistic
					set.seed(42)
					tstar <- replicate(num.bootstraps, xerrFun(sample(xhat, xhat.n, replace=TRUE), stat))
					stat.err <- rowMeans(tstar)
				} else {
					stat.err <- xerrFun(tstar, stat)
				}
			} else {
				stat <- xfun(xhat)
				stat.err <- xerrFun(xhat, stat)
			}

			rres <- c(stat, stat.err)
			names(rres) <- c("stat", "err.low", "err.high", "err.val")

			return(rres)

		})
		

		if(ngroups == 1) {
			tmp <- drop(tmp)
		} else {

			if(grand.stat) {
				# summarize the group summaries
				stat <- xfun(drop(tmp[1, ]))

				if(err.fun=="sd" || err.fun=="sem") {
					stat.err <- xerrFun(drop(tmp[1, ]), stat)[3]^2

					# propagate
					stat.err.noise <- mean(tmp[4, ]^2)
					stat.err.val <- sqrt(stat.err+stat.err.noise)
					stat.err <- c(-1, 1)*stat.err.val + stat
				} else if(err.fun=="var") {
					stat.err.val <- var(tmp[1, ]) + mean(tmp[4, ])
					stat.err <- c(-1, 1)*stat.err.val + stat
				} else if(err.fun=="ci" || err.fun=="iqr" || err.fun=="iqr.sem") {
					# take min of low and max of high - this seems to come close to something that 
					# makes sense.
					stat.err <- c(min(tmp[2, ]), max(tmp[3, ]))
					stat.err.val <- NA
				}

				tmp <- c(stat, stat.err, stat.err.val)
				names(tmp) <- c("stat", "err.low", "err.high", "err.val")

			}
		}
	}

	set.seed(round(runif(1)*1e9))
	
#fun=c("mean", "median"), err.fun=c("sd", "sem", "var", "ci"), ci=0.95, num.bootstraps=1, 
#	boot.err.mode=1, groups=NULL, grand.stat=FALSE	

	attr(tmp, "settings") <- list(fun=fun, err.fun=err.fun, ci=ci, num.bootstraps=num.bootstraps, 
		boot.err.mode=boot.err.mode, groups=groups, grand.stat=grand.stat)

	return(tmp)

}


