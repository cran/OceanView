## =============================================================================
## Update range, taking into account neg values for log transformed values
## =============================================================================

Range <- function(Range, x, log) {
  if (is.null(x)) 
    return(Range)
  if (log)
    x[x <= 0] <- min(x[x>0])  # remove zeros
   
   RR <- range(Range, as.double(x), na.rm = TRUE)
   RR[is.infinite(RR)]<- NA

   return( RR )
}
SetRange <- function(lim, x2, isub, xWhich, ip, Log) {

  nx <- length (x2)
  if ( is.null (lim)) {
    yrange <- NULL
      for (j in 1:nx){
        ix <- xWhich[[j]][ip]
        if (! all(is.na(x2[[j]][isub[[j]],ix])))
          yrange <- Range(yrange, x2[[j]][isub[[j]],ix], Log)
      }  
  } else
    yrange  <- lim

  return(yrange)
}




setdots <- function(dots, n) 
  lapply(dots, repdots, n)
repdots <- function(dots, n) {
  if (is.function(dots)) { 
    return(dots)
  } else 
    return(rep(dots, length.out = n))
}
expanddots <- function (dots, default, n) {
  dots <- if (is.null(dots)) default else dots
  rep(dots, length.out = n)
}

# lists: e.g. xlim and ylim....
expanddotslist <- function (dots, n) {
  if (is.null(dots)) return(dots)
  dd <- if (!is.list(dots )) list(dots) else dots
  rep(dd, length.out = n)
}

extractdots <- function(dots, index) {
  
  ret <- lapply(dots, "[", index)
  ret <- lapply(ret, unlist) # flatten list
  return(ret)
}


## ============================================================================
## create several lists: x2:   other matrix objects,
##                       dotmain, dotpoints: remaining (plotting) parameters
## ============================================================================

splitdots <- function(ldots, x){
  x2      <- list()
  nother <- 0
  islist <- (! is.data.frame(x) & is.list(x))
  
  if (! islist) {
    x2[[1]] <- x
    names(x2)[1] <-"M"
  } else {
    for(i in 1:length(x))
      x2[[i]] <- x[[i]]
    names(x2) <- names(x)
    nother <- length(x) - 1
  }

  dots   <- list()
  nd     <- 0
  ndots <- names(ldots)
    
  if (length(ldots) > 0)
    for ( i in 1:length(ldots))
      if (inherits(ldots[[i]],"matrix") | inherits(ldots[[i]],"data.frame")) { 
        nother <- nother + 1        
        x2[[nother + 1]] <- ldots[[i]]
        if (is.null(ndots[i]))
          names(x2)[nother+1] <- nother 
        else 
          names(x2)[nother+1] <- ndots[i]
        # a list of matrix objects
      } else if (is.list(ldots[[i]]) & 
        (inherits(ldots[[i]][[1]], "matrix") | 
         inherits(ldots[[i]][[1]],"data.frame"))) {
        for (j in 1:length(ldots[[i]])) {
          nother <- nother + 1        
          x2[[nother+1]] <- ldots[[i]][[j]]
          nn <- names(ldots[[i]])[[j]]
          if (is.null(nn)) 
            nn <- nother
          names(x2)[nother+1] <- nn
        }
      } else if (! is.null(ldots[[i]])) {  # a graphical parameter
        dots[[nd <- nd+1]] <- ldots[[i]]
        names(dots)[nd] <- ndots[i]
      }

  nmdots <- names(dots)

  # plotting parameters : split in plot parameters and point parameters
  plotnames <- c("xlab", "ylab", "xlim", "ylim", "main", "sub", "log", "asp",
                 "ann", "axes", "frame.plot", "panel.first", "panel.last",
                 "cex.lab", "cex.axis", "cex.main")

  # plot.default parameters
  ii <- names(dots) %in% plotnames
  dotmain <- dots[ii]

  # point parameters
  ip <- !names(dots) %in% plotnames
  dotpoints <- dots[ip]
  list (points = dotpoints, main = dotmain, nother = nother, x2 = x2)
}


## =============================================================================
## =============================================================================
## Matrix boxplotting
## =============================================================================
## =============================================================================

## =============================================================================
## Boxplots a (list of) matrices - x (or y) has to be a FACTOR!
## =============================================================================

Mboxplot <- function (M, ..., 
                      x = 1, 
                      select = NULL, which = select, 
                      subset = NULL, ask = NULL, 
                      legend = list(x = "center"),
                      pos.legend = NULL,
                      xyswap = FALSE, rev = "") {

  getnames <- function(x) 
    if (is.null (cn <- colnames(x))) return (1:ncol(x)) else return(cn)

  plotlegend <- function () {

    if (nolegend) return()   

    # Add legend, if legend not equal to NULL or not equal to FALSE
    if (! is.list(legend)) {
      if (legend[1] == FALSE) 
        legend <- list()
        else if (legend[1] == TRUE)  
          legend <- list(x = "top")
    }
  
    if (length(legend) > 0) {
      if (!is.list(legend)) 
        stop ("'legend' should be a list or NULL")

      if (is.null(legend$col))
        legend$fill <- Dotpoints$col
      else
        legend$fill <- legend$col
      
      if (is.null(legend$legend))
        legend$legend <- names(x2)
      if (is.null(legend$x))
        legend$x <- "center"
    
      do.call("legend", legend)
    }
  }

                  
  # The ellipsis
  ldots   <- list(...)
  
  mtext <- ldots$mtext
  ldots$mtext <- NULL
  
  Dots     <- splitdots(ldots, M)
  x2       <- Dots$x2
  nother   <- Dots$nother
  nx       <- nother + 1 # total number of objects to be plotted
  varnames <- getnames(x2[[1]])
  nolegend <- (nother == 0 & is.null(pos.legend))

 # x-variable  
  xPos <- vector()
  xisfactor <- TRUE
  for (i in 1: length(x2)) { 
    xPos[i] <- selectvar(x, getnames(x2[[i]]))
    xisfactor <- xisfactor & is.factor(x2[[i]][,xPos[1]])
  }
   
  if (! xisfactor) {
    XX <- NULL
    warning ("'x' has to be a factor for Mboxplot - creating factors")
    for (i in 1: length(x2))  
      x2[[i]][,xPos[i]] <- as.factor(x2[[i]][,xPos[i]])
  }

  xat <- 0.5 : length(levels(x2[[1]][,xPos[1]]))  
  xname <- varnames[xPos[1]]

 # variables to be plotted
  Which <- which
  if (is.null(Which)) {
    for (i in 1: length(x2))
      Which <- c(Which,getnames(x2[[i]])[- xPos[i]])
    Which <- unique(Which)
  }

  np      <- length(Which)
  if (np == 0)  
    stop ("M cannot be a (list of) vector(s)")
  if (!is.null(pos.legend)) {
    if (is.character(pos.legend)) 
      pos.legend <- which(pos.legend == Which)
    if (pos.legend > np)
      stop("'pos.legend' should be referring to a variable name or number to plot")
  } else
    pos.legend <- np
  
 # Position of variables to be plotted in "M" and other matrices
  xWhich <- list()

  for (i in 1: length(x2))
    xWhich[[i]] <- selectvar(Which, getnames(x2[[i]]))

  if (! is.character(Which)) 
    Which <- varnames[xWhich[[1]]]

  # number of figures in a row and interactively wait if remaining figures
  ask <- setplotpar(ldots, np + (pos.legend == 0), ask)                   
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }

  Dotmain <- setdots(Dots$main, np)  # expand to np for each plot

 # these are different from the default
  Dotmain$xlab <- expanddots(ldots$xlab, xname      , np)
  Dotmain$ylab <- expanddots(ldots$ylab, ""         , np)
  Dotmain$main <- expanddots(ldots$main, Which      , np)

 # ylim and xlim can be lists and are at least two values
  yylim  <- expanddotslist(ldots$ylim, np)
  xxlim  <- expanddotslist(ldots$xlim, np)

 # in case there are more than one data set: adapt the box width and set delta
  doffset <- 0
  if (nother > 0) {
    if (is.null(Dots$points$boxwex))
      Dots$points$boxwex <- 0.3/nother
    doffset <- 0.4 / nother  
  }
  Dotpoints <- setdots(Dots$points, nx)   # expand all dots to nx values

  if (!missing(subset)){
    isub <- list()
    for (i in 1:nx) {
      e <- substitute(subset)
      r <- eval(e, as.data.frame(x2[[i]]), parent.frame())
      if (!is.logical(r))
        stop("'subset' must evaluate to logical")
      isub[[i]] <- r & !is.na(r)
    }  
  } else isub <- rep(TRUE, nx)

  xyswap <- rep(xyswap, length = np)
  rev    <- rep(rev, length = np)

 # LOOP for each output variable (plot)
  for (ip in 1 : np) {

   # plotting parameters for matrix output 1 (opens a plot)
    dotmain   <- extractdots(Dotmain, ip)
    if (xyswap[ip]) {
      dotmain$horizontal <- TRUE
      
      tmp <- dotmain$xlab 
      dotmain$xlab <- dotmain$ylab
      dotmain$ylab <- tmp

    } else
      dotmain$horizontal <- FALSE
      
    dotpoints <- extractdots(Dotpoints, 1)  # 1st dotpoints

    Xlog <- Ylog <- FALSE
    if (! is.null(dotmain$log)) {
      Ylog  <- length(grep("y",dotmain$log))
      Xlog  <- length(grep("x",dotmain$log))
    }

   # first object plotted (new plot created)
    ix <- xWhich[[1]][[ip]]      # position of variable in 'x'

    dotmain$ylim <- yylim[[ip]]
    if (is.null(xxlim[[ip]])) {
        dotmain$xlim <- SetRange(xxlim[[ip]], x2, isub, xPos, 1, Xlog)
        dotmain$xlim <- dotmain$xlim + c(-0.5, 0.5 + 0.5*(nother>0))
    } else
        dotmain$xlim <- xxlim[[ip]]

    if (is.null(yylim[[ip]]))
      dotmain$ylim <- SetRange(yylim[[ip]], x2, isub, xWhich, ip, Ylog)
    else
      dotmain$ylim <- yylim[[ip]]

    if (length(grep("x",rev[ip])))    
      dotmain$xlim <- rev(dotmain$xlim)

    if (length(grep("y",rev[ip])))
      dotmain$ylim <- rev(dotmain$ylim)

    if (all(is.na(x2[[1]][isub[[1]], ix]))) {  # No data
      dotpoints$type <- dotmain$axes <- dotmain$xlab <- dotmain$ylab <-  NULL
      plot(x = 0.5, y = 0.5, type = "n", main = dotmain$main)
      text (0.5, 0.5, labels = "No data")  
    } else {
      xx <- x2[[1]][isub[[1]], xPos[1]]
      yy <- x2[[1]][isub[[1]], ix]
      do.call("boxplot", c(alist(yy~xx, at = xat), dotmain, dotpoints))
    }
    
   
    if (nother > 0)        # if other outputs  
      for (j in 2:nx) {
        ix <- xWhich[[j]][[ip]]      # position of variable in 'x2'
        if (!is.na(ix)) {
          xx <- x2[[j]][isub[[j]], xPos[j]]
          yy <- x2[[j]][isub[[j]], ix]
          ii <- c(which(is.na(yy)), which(is.na(xx)))
          if (length(ii) > 0) {
            xx <- xx[-ii]
            yy <- yy[-ii]
          }
            
          do.call("boxplot", c(alist(yy~xx, at = xat + (j-1)*doffset, 
            add = TRUE, axes = FALSE), extractdots(Dotpoints, j)) )
        }
      }
     if (pos.legend == ip)
       plotlegend()
     
  }
  
  if (! is.null(mtext))
    mtext(outer = TRUE, side = 3, mtext, line = par()$oma[3]-1, 
          cex = par()$cex*1.5)
  if (pos.legend == 0) {
    plot.new()
    plotlegend()
  }   

}


## =============================================================================
## Set the mfrow parameters and whether to "ask" for opening a new device
## =============================================================================

setplotpar <- function(ldots, nv, ask) {
  nmdots <- names(ldots) 
  # nv = number of variables to plot
  if (!any(match(nmdots, c("mfrow", "mfcol"), nomatch = 0))) {
    nc <- min(ceiling(sqrt(nv)), 3)
    nr <- min(ceiling(nv/nc), 3)
    mfrow <- c(nr, nc)
  } else if ("mfcol" %in% nmdots)
    mfrow <- rev(ldots$mfcol)
  else 
    mfrow <- ldots$mfrow

  if (! is.null(mfrow))  
    mf <- par(mfrow = mfrow)

  ## interactively wait if there are remaining figures
  if (is.null(ask))
    ask <- prod(par("mfrow")) < nv && dev.interactive()

  return(ask)
}

## =============================================================================
## find a variable  - and keep the ordering
## =============================================================================

selectvar <- function (Which, var, NAallowed = TRUE) {
  if (!is.numeric(Which)) {
    ln <- length(Which)

   # the loop is necessary so as to keep ordering...
    Select <- NULL
    for ( i in 1:ln) {
      ss <- which(Which[i] == var)
      if (length(ss) ==0 & ! NAallowed)
        stop("variable ", Which[i], " not in variable names")
      else if (length(ss) == 0)
        Select <- c(Select, NA)
      else
        Select <- c(Select, ss)
    }
  } else {
    Select <- Which  # "Select" now refers to the column number
    if (max(Select) > length(var))
      stop("index in 'which' too large: ", max(Select)-1)
    if (min(Select) < 1)
      stop("index in 'which' should be > 0")
  }
  return(Select)
}

## ============================================================================
## create several lists: x2:   other matrix objects,
##                       dotmain, dotpoints: remaining (plotting) parameters
## ============================================================================

splitdots <- function(ldots, x){
  x2      <- list()
  nother <- 0
  islist <- (! is.data.frame(x) & is.list(x))
  
  if (! islist) {
    x2[[1]] <- x
    names(x2)[1] <-"M"
  } else {
    for(i in 1:length(x))
      x2[[i]] <- x[[i]]
    names(x2) <- names(x)
    nother <- length(x) - 1
  }

  dots   <- list()
  nd     <- 0
  ndots <- names(ldots)
    
  if (length(ldots) > 0)
    for ( i in 1:length(ldots))
      if (inherits(ldots[[i]], "matrix") | inherits(ldots[[i]], "data.frame")) { 
        nother <- nother + 1        
        x2[[nother + 1]] <- ldots[[i]]
        if (is.null(ndots[i]))
          names(x2)[nother+1] <- nother 
        else 
          names(x2)[nother+1] <- ndots[i]
        # a list of matrix objects
      } else if (is.list(ldots[[i]]) & 
        (inherits(ldots[[i]][[1]], "matrix") | 
         inherits(ldots[[i]][[1]], "data.frame"))) {
        for (j in 1:length(ldots[[i]])) {
          nother <- nother + 1        
          x2[[nother+1]] <- ldots[[i]][[j]]
          nn <- names(ldots[[i]])[[j]]
          if (is.null(nn)) 
            nn <- nother
          names(x2)[nother+1] <- nn
        }
      } else if (! is.null(ldots[[i]])) {  # a graphical parameter
        dots[[nd <- nd+1]] <- ldots[[i]]
        names(dots)[nd] <- ndots[i]
      }

  nmdots <- names(dots)

  # plotting parameters : split in plot parameters and point parameters
  plotnames <- c("xlab", "ylab", "xlim", "ylim", "main", "sub", "log", "asp",
                 "ann", "axes", "frame.plot", "panel.first", "panel.last",
                 "cex.lab", "cex.axis", "cex.main")

  # plot.default parameters
  ii <- names(dots) %in% plotnames
  dotmain <- dots[ii]

  # point parameters
  ip <- !names(dots) %in% plotnames
  dotpoints <- dots[ip]
  list (points = dotpoints, main = dotmain, nother = nother, x2 = x2)
}
