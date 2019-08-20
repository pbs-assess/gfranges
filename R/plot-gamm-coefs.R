#' Visualization of group estimates.
#'
#' @export
plot_parametric <- function(x, pred, cond = list(), 
  parametricOnly = FALSE, rm.ranef=NULL, 
  col = 'black', se = 1.96, print.summary=getOption('itsadug_print'),
  main=NULL, xlab=NULL, ...) {
  
  dnm <- names(list(...))
  parTerms <- NULL
  if(parametricOnly){
    parTerms <- summary(x)$p.t
  }
  v.names <- names(x$var.summary)
  if (sum(names(pred) %in% v.names) != length(pred)) {
    stop(paste(c("Pred variable must be one of", v.names), collapse = ", "))
  }
  for(i in 1:length(names(pred))){
    if (!inherits(x$var.summary[[names(pred)[i]]], c("factor"))){
      stop("Don't know what to do with parametric terms that are not simple grouping variables.")
    }
  }
  if(!is.null(cond)){
    cn <- names(cond)
    test <- sapply(cn, function(x){
      if(length(unique(cond[[x]]))>1){
        stop("Do not specify more than 1 value for conditions listed in the argument cond.")
      }else{
        TRUE
      }
    })
  }
  for(i in names(pred)){
    cond[[i]] <- pred[[i]]
  }
  newd <- NULL
  if(parametricOnly){
    su <- x$var.summary
    new.cond <- list()
    for(i in names(su)){
      if(i %in% names(cond)){
        new.cond[[i]] <- cond[[i]]
      }else{
        if(class(su[[i]])=="factor"){
          new.cond[[i]] <- as.character(su[[i]][1])
        }else if(class(su[[i]])=="numeric"){
          new.cond[[i]] <- su[[i]][2]
        }
      }
    }
    newd <- expand.grid(new.cond)
    p <- mgcv::predict.gam(x, newd, type='lpmatrix')
    rm.col <- colnames(p)[!colnames(p) %in% names(parTerms)]
    p[,rm.col] <- 0
    if(length(rm.col)==0){
      warning("No smooth terms in the model.\n")               
    }
    newd$fit <- p %*% coef(x)
    if(se>0){
      newd$CI <- se*sqrt(rowSums((p%*%vcov(x))*p))
    }
  }else{
    newd <- get_predictions(x, cond=cond, se=ifelse(se>0, TRUE, FALSE), 
      f=ifelse(se>0, se, 1.96), rm.ranef=rm.ranef,
      print.summary=print.summary)
  }
  newd$VnewCol <- NA
  newd <- droplevels(newd)
  if(length(pred)>1){
    newd$VnewCol <- interaction(newd[, names(pred)])
  }else{
    newd$VnewCol <- newd[,names(pred)[1]]
  }
  #browser()
  ### REVERSED ORDER
  newd <- newd[rev(order(newd$VnewCol)),]
  if(is.null(main)){ main <- paste(names(pred), collapse=' x ') }
  if(is.null(xlab)){ xlab <- names(x$model)[!names(x$model) %in% v.names]}
  
  dotplot_error(x=as.vector(newd$fit), se.val=as.vector(newd$CI),
    labels=as.character(newd$VnewCol), 
    main=main, xlab=xlab, ...)
  abline(v=0, lty=3)
  newd$VnewCol <- NULL
  invisible(list(fv = newd))
}




#' Unordered dotplot
#'
#' @export
dotplot_error <- function (x, se.val=NULL, labels = NULL, groups = NULL, 
  gdata = NULL, cex = par("cex"), 
  pch = 21, gpch = 21, bg = "black", color = par("fg"), gcolor = par("fg"), 
  lcolor = "gray", xlim = NULL, main = NULL, 
  xlab = NULL, ylab = NULL, lwd=1, ...) 
{
  opar <- par("mai", "mar", "cex", "yaxs")
  on.exit(par(opar))
  par(cex = cex, yaxs = "i")
  if (!is.numeric(x)) 
    stop("'x' must be a numeric vector or matrix")
  n <- length(x)
  if(!is.null(se.val)){
    if(length(x) != length(se.val)){
      warning("se.val not equal in length as x. se.val will be ignored.")
      se.val <- NULL
    }
  }
  if (is.matrix(x)) {
    if (is.null(labels)) 
      labels <- rownames(x)
    if (is.null(labels)) 
      labels <- as.character(1L:nrow(x))
    labels <- rep_len(labels, n)
    if (is.null(groups)) 
      groups <- col(x, as.factor = TRUE)
    glabels <- levels(groups)
  }
  else {
    if (is.null(labels)) 
      labels <- names(x)
    glabels <- if (!is.null(groups)) 
      levels(groups)
    if (!is.vector(x)) {
      warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
      x <- as.numeric(x)
    }
    if(! is.null(se.val)){
      if (!is.vector(se.val)) {
        warning("'se.val' is neither a vector nor a matrix: using as.numeric(se.val)")
        se.val <- as.numeric(se.val)
      }
    }
    
  }
  if(is.null(xlim)){
    xlim <- range(x[is.finite(x)])
    if(!is.null(se.val)){
      xlim <- range(c(x[is.finite(x)]-se.val[is.finite(se.val)], x[is.finite(x)]+se.val[is.finite(se.val)]))
    }
  }
  plot.new()
  linch <- if (!is.null(labels)) 
    max(strwidth(labels, "inch"), na.rm = TRUE)
  else 0
  if (is.null(glabels)) {
    ginch <- 0
    goffset <- 0
  }
  else {
    ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
    goffset <- 0.4
  }
  if (!(is.null(labels) && is.null(glabels))) {
    nmai <- par("mai")
    nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
      0.1
    par(mai = nmai)
  }
  if (is.null(groups)) {
    o <- sort.list(as.numeric(x), decreasing = TRUE)
    #x <- x[o]
    y <- 1L:n
    ylim <- c(0, n + 1)
  }
  else {
    o <- group_sort(x, group=groups, decreasing = TRUE)
    #x <- x[o]
    if(!is.null(se.val)){
      se.val <- se.val[o]
    }
    #groups <- groups[o]
    color <- rep_len(color, length(groups))[o]
    lcolor <- rep_len(lcolor, length(groups))[o]
    bg <- rep_len(bg, length(groups))[o]
    offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
    y <- 1L:n + 2 * offset
    ylim <- range(0, y + 2)
  }
  plot.window(xlim = xlim, ylim = ylim, log = "")
  lheight <- par("csi")
  if (!is.null(labels)) {
    linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
    loffset <- (linch + 0.1)/lheight
    labs <- labels#[o]
    mtext(labs, side = 2, line = loffset, at = y, adj = 0, 
      col = color, las = 2, cex = cex, ...)
  }
  abline(h = y, lty = "dotted", col = lcolor)
  if(!is.null(se.val)){
    segments(x0=x-se.val, x1=x+se.val, y0=y, y1=y, col=color, lwd=lwd)
  }
  points(x, y, pch = pch, col = color, bg = bg)
  if (!is.null(groups)) {
    gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
        2) - 1)
    ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
    goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
    mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
      col = gcolor, las = 2, cex = cex, ...)
    if (!is.null(gdata)) {
      abline(h = gpos, lty = "dotted")
      points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
        ...)
    }
  }
  axis(1)
  box()
  title(main = main, xlab = xlab, ylab = ylab, ...)
  invisible()
}

