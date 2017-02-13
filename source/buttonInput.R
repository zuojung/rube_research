##################################################################
# Clickable input on a graph based on a list of button vectors.  #
# Includes option to draw buttons, too.                          #
# INPUT: 
#    buttons=a matrix of character vectors.  If a character
#      vector element is "" then that row/col is blank.  If it is
#      a string, then the string is the label of a button.
#      if it is NA, then the corresponding element of the slider
#      group list is used to create a group of sliders in that
#      location.
#    sliders=a list of lists of lists.  Each lowest level list
#      defines one slider of a group and has optional elements
#      of range, val, maxRange (for auto expansion), minRange
#      (for auto contraction), labX, cex, tickN, and tickHt.
#      Each middle level list corresponds to one row/col location.
#      (labX is fraction reserved at left for the label.)
#    pos=c(x-left, x-right, y-left, y-right) of the button bar 
#      location; defaults to the current par("usr") region
#    returnYX can be set to TRUE to return row and position of
#      button clicked rather than the name of the button
#    draw can be set to FALSE to only read click without
#      (re)drawing the buttons
#    info can be set to TRUE to return calculated info
#      instead of click response or to the list of calculated
#      info so it doesn't need to be recalculated.
#    input can be set to FALSE to draw without inputting a click
#    yadj can be set to lower numbers put text lower in their
#      button boxes
# OUTPUT:
#    the clicked button's name or Y,X position
#
buttonInput <- function(buttons, sliders=NULL, sliderVals=NULL,
      greyButtons=NULL,
      heights=NULL, widths=NULL,
      pos=NULL, returnYX=FALSE, draw=TRUE, 
      new=FALSE,  info=FALSE,
      yadj=-0.35, input=TRUE, xpd=NA, stopReturn=NULL,
      family=c("serif","sans","mono"), font=2,
      moveMark=FALSE,
      slideLabAdj=c(-0.4,0.1), slideLabCex=0.8, slidePadLR=c(0.15,0.05),
      slideTickN=5, slideTickHt=0.3, slideTickLabAdj=c(+0.05,-0.05), 
      slideTickLabCex=0.7, slideWidth=1, borderGuard=0.5, 
      cex.button=max(0.5,1-length(buttons)%/%7*0.1), ...) {

  # Verify input
  if (!is.list(buttons)) 
    stop("The buttons= argument must be a list.")
  if (!all(sapply(buttons,is.character))) 
    stop("The buttons elements must be strings.")
  if (!is.logical(returnYX)) stop("returnYX must be TRUE or FALSE")
  options(locatorBell=FALSE) # Stupid default!
  family <- match.arg(family)

  # Account for upward y-axis direction
  buttons <- rev(buttons) # 1st row should appear at the top of the screen
  
  # Setup grey buttons
  if (is.null(greyButtons)) {
    greyButtons = lapply(buttons, function(x) rep(FALSE,length(x)))
  } else {
    if (length(greyButtons)!=length(buttons)) stop("greyButtons format error")
    if (any(sapply(greyButtons,class)!="logical")) stop("greyButtons format error")
    greyButtons <- rev(greyButtons)
    if (any(sapply(buttons,length)!=sapply(greyButtons,length))) stop("greyButtons format error")
  }

  # Handle button heights and widths
  if (!is.list(info)) {
    nRows <- length(buttons)
    nCols <- sapply(buttons,length)
    if (is.null(heights)) {
      heights <- rep(1/nRows,nRows)
    } else {
      if (!length(heights)==nRows || sum(heights)>1)
        stop("bad heights")
    }
    heights <- rev(heights)
    if (is.null(widths)) {
      widths <- lapply(rev(nCols), function(x)rep(1/x,x))
    } else {
      if (!length(widths)==nRows || any(sapply(widths,sum)>1))
        stop("bad widths")
    }
  
    # Setup area to draw buttons
    if (draw && new) 
      plot(c(0,1), c(0,1), type="n", axes=FALSE, xlab="", ylab="")
    if (is.null(pos)) {
      if (names(dev.cur())=="null device") 
        stop("cannot use pos=NULL without a prior plot")
      pos <- par("usr")
    }

    # Find tuning parameters
    cxy <- par("cxy")
    cex <- par("cex")
    slideLabAdj <- slideLabAdj * cxy/cex
    slideTickHt <- slideTickHt * cxy[2]/cex
    slideTickLabAdj <- slideTickLabAdj * cxy/cex
    borderGuard <- borderGuard*cxy/cex
    slideWidth <- slideWidth*cxy/cex

    # Plot region size and relative to absolute x/y location functions
    xrange <- diff(pos[1:2])
    yrange <- diff(pos[3:4])
    xpos <- function(fracX) {pos[1]+xrange*fracX}
    ypos <- function(fracY) {pos[3]+yrange*fracY}

    # y for each row (from below row 1 to above row n):
    yRow <- ypos(c(0,cumsum(heights)))
    rowHts <- diff(yRow)
    yMid <- yRow[1:nRows] + rowHts/2

    # x for each column (from left of col 1 to right of col n):
    xCol <- NULL
    for (i in nRows:1) xCol <- c(xCol, list(xpos(c(0,cumsum(widths[[i]])))))
    xWd <- lapply(xCol, diff)
    xMid <- lapply(xCol, function(x) {df<-diff(x); dfh=df/2; x[1]+cumsum(df)-dfh})
    if (is.logical(info) && info==TRUE) {
      butInfo <- list(nRows=nRows, nCols=nCols, pos=pos,
                    cxy=cxy, cex.button=cex.button,
                    slideLabAdj=slideLabAdj, slideTickHt=slideTickHt,
                    slideTickLabAdj=slideTickLabAdj, 
                    slideWidth=slideWidth, borderGuard=borderGuard,
                    xpos=xpos, ypos=ypos,
                    yRow=yRow, rowHts=rowHts, yMid=yMid,
                    xCol=xCol, xWd=xWd, xMid=xMid)
    }
  } else { # if info is provided as a list
    nRows <- info$butInfo$nRows; nCols <- info$butInfo$nCols
    pos <- info$butInfo$pos
    cxy <- info$butInfo$cxy; cex <- info$butInfo$cex
    slideLabAdj <- info$butInfo$slideLabAdj
    slideTickHt <- info$butInfo$slideTickHt
    slideTickLabAdj <- info$butInfo$slideTickLabAdj
    slideWidth <- info$butInfo$slideWidth
    borderGuard <- info$butInfo$borderGuard
    xpos <- info$butInfo$xpos; ypos <- info$butInfo$ypos
    yRow <- info$butInfo$yRow
    rowHts <- info$butInfo$rowHts; yMid <- info$butInfo$yMid
    xCol <- info$butInfo$xCol
    xWd <- info$butInfo$xWd; xMid <- info$butInfo$xMid
    sliders <- info$sliders
    sliderGrpID <- info$slidInfo #$sliderGrpID
    if (!is.null(sliderVals)) {
      if (!is.list(sliderVals)) stop("bad sliderVals")
      sgCount <- length(sliders) 
      if (length(sliderVals)!=sgCount) stop("bad sliderVals")
      if (any(rev(sapply(sliderVals,length))!=sapply(sliders,length)-1))
        stop("bad sliderVals")
      for (i in 1:sgCount) {
        for (j in 1:sliders[[i]]$parms$nSlide) {
          sliders[[i]][[j]]$val <- sliderVals[[sgCount-i+1]][j]
        }
      }
    }
  }

  # Get slider info
  if (!is.list(info) && length(sliders)>0) {
    sliders <- rev(sliders) # (because Y axis goes up)
    sgCount <- length(sliders)
    sliderGrpID <- lapply(nCols, function(x) rep(NA,x))
    if (is.null(sliderVals)) {
      sliderVals <- rep(list(), sgCount)
    } else {
      if (!is.list(sliderVals) || length(sliderVals)!=sgCount)
        stop("bad sliderVals")
      sliderVals = rev(sliderVals) # (because Y axis goes up)
    }
    sgNum <- 0
    for (r in 1:nRows) {
       for (c in 1:nCols[r]) {
        if (!is.na(buttons[[r]][c])) next
        sgNum <- sgNum + 1
        if (sgNum>sgCount) stop("more NA buttons then slider groups")
        sliderGrpID[[r]][c] <- sgNum
        thisSlideGroup <- sliders[[sgNum]]
        if (!is.list(thisSlideGroup)) stop("bad button: ", thisSlideGroup) 
        nSlide <- length(thisSlideGroup)
        if (length(sliderVals[[sgNum]])<nSlide) {
          sliderVals[sgNum] <- c(sliderVals[[sgNum]], 
                                 rep(NA, nSlide-length(sliderVals[[sgNum]])))
        }
        slideYs <- yRow[r] + (nSlide:1)/(nSlide+1)*rowHts[r]
        round <- sapply(thisSlideGroup, function(x) 
                  ifelse(is.null(x$round), 2, x$round))
        labX <- sapply(thisSlideGroup, function(x) 
                  ifelse(is.null(x$labX), slidePadLR[1], x$labX))
        labCex <- sapply(thisSlideGroup, function(x) 
                    ifelse(is.null(x$cex), slideLabCex, x$cex))
        lab <- names(thisSlideGroup)
        if (is.null(lab)) lab <- rep("", length(thisSlideGroup))
        Wd <- xWd[[r]][c]
        slideLen <- (1 - labX - slidePadLR[2])*Wd
        slideLeft <- xCol[[r]][c] + labX * Wd
        slideRight <- slideLeft + slideLen
        for (i in 1:nSlide) {
          this <- thisSlideGroup[[i]]
          thisSlideGroup[[i]]$low <- ifelse(is.null(this$range), 0, this$range[1])
          thisSlideGroup[[i]]$hi <- ifelse(is.null(this$range), 1, this$range[2])
          thisSlideGroup[[i]]$val <- ifelse(is.na(sliderVals[[sgNum]][i]), 
                 sum(this$range)/2, sliderVals[[sgNum]][i])
          if (thisSlideGroup[[i]]$val < thisSlideGroup[[i]]$low)
            thisSlideGroup[[i]]$val = thisSlideGroup[[i]]$low
          if (thisSlideGroup[[i]]$val > thisSlideGroup[[i]]$hi)
            thisSlideGroup[[i]]$val = thisSlideGroup[[i]]$hi
          thisSlideGroup[[i]]$drng <- thisSlideGroup[[i]]$hi - thisSlideGroup[[i]]$low
          thisSlideGroup[[i]]$tickN <- ifelse(is.null(this$tickN), slideTickN, this$tickN)
          thisSlideGroup[[i]]$tickHt <- ifelse(is.null(this$tickHt), slideTickHt,
                                        this$tickHt * cxy[2]/cex)
          thisSlideGroup[[i]]$at = pmin(thisSlideGroup[[i]]$hi, 
                              pmax(thisSlideGroup[[i]]$low, 
                              pretty(c(thisSlideGroup[[i]]$low,thisSlideGroup[[i]]$hi),
                                     thisSlideGroup[[i]]$tickN)))
          xf <- paste("function(z)", slideLeft[i], "+ (z-", thisSlideGroup[[i]]$low, ") * ",
                      slideLen[i], "/", thisSlideGroup[[i]]$drng)
          thisSlideGroup[[i]]$x <- eval(parse(text=xf))
          #thisSlideGroup[[i]]$x <- function(z) slideLeft[i] + 
          #                (z -thisSlideGroup[[i]]$low)*slideLen[i]/thisSlideGroup[[i]]$drng
        } # end tick marks for each slider
        thisSlideGroup$parms = list(nSlide=nSlide, slideYs=slideYs,
           labX=labX, labCex=labCex, lab=lab, Wd=Wd, 
           slideLen=slideLen, slideLeft=slideLeft, slideRight=slideRight)
        sliders[sgNum] <- list(thisSlideGroup)
      } # end for each column
    } # end for each row of buttons
  } # end if any sliders


  # Draw
  if (draw) {
    for (r in 1:nRows) {
      for (c in 1:nCols[[r]]) {
        rect(xCol[[r]][c], yRow[r], xCol[[r]][c+1], yRow[r+1], xpd=xpd)
        if (!is.na(buttons[[r]][c])) {
          text(xMid[[r]][c], yMid[r], buttons[[r]][c], xpd=xpd, cex=cex.button, 
               adj=c(0.5,-yadj), family=family, font=font, col=ifelse(greyButtons[[r]][c],"grey","black"))
        } else {
          thisSlideGroup <- sliders[[sliderGrpID[[r]][c]]]
          parms <- thisSlideGroup$parms
          # sliders:
          segments(parms$slideLeft, parms$slideYs, parms$slideRight, parms$slideYs)
          text(parms$slideLeft+slideLabAdj[1], parms$slideYs+slideLabAdj[2],
               parms$lab, adj=c(1, 0.5), cex=parms$labCex)
          for (i in 1:parms$nSlide) {
            this <- thisSlideGroup[[i]]
            # slider ticks:
            at <- this$x(this$at)
            segments(at, parms$slideYs[i]-this$tickHt, at, parms$slideYs[i]+this$tickHt)
            text(at+slideTickLabAdj[1], parms$slideYs[i]-this$tickHt+slideTickLabAdj[2],
                 this$at, adj=c(0.5,1), cex=slideTickLabCex)
            segments(this$x(this$val), parms$slideYs[i]-this$tickHt, 
                     this$x(this$val), parms$slideYs[i]+this$tickHt, col=2, lwd=2)
            # slider value:
          } # end tick marks for each slider
        } # end if slider, not button
      } # end each column
    } # end each row
  } # end if drawing

  if (is.logical(info) && info==TRUE) {
    return(list(butInfo=butInfo,sliders=sliders,slidInfo=sliderGrpID))
  }
  if (!input) return(NULL)

  # Decode input
  val <- NULL
  sgCount <- length(sliders)
  while (TRUE) {
    where <- locator(1)
    if (is.null(where)) return(stopReturn)
    xclick <- where$x
    yclick <- where$y
    if (xclick<pos[1] || xclick>pos[2] || yclick<pos[3] || yclick>pos[4]) next
    if (min(abs(yclick-yRow))<borderGuard[2]) next
    y <- sum(yRow>yclick)
    yIndex <- nRows-y+1
    if (min(abs(xclick-xCol[[yIndex]]))<borderGuard[1]) next
    x <- sum(xclick>xCol[[yIndex]])
    if (greyButtons[[yIndex]][x]) next
    pick <- buttons[[yIndex]][x]
    if (is.na(pick)) {
      slideGrp <- sliderGrpID[[yIndex]][x]
      p <- sliders[[slideGrp]]$parms
      near <- abs(p$slideYs-yclick)<slideWidth[2] # for horiz. scale
      if (!any(near)) next
      sNum <- which(near)
      if (length(sNum)>1) next
      if (xclick<p$slideLeft[sNum] || xclick>p$slideRight[sNum]) next
      frac <- (xclick-p$slideLeft[sNum])/p$slideLen[sNum]
      this <- sliders[[slideGrp]][[sNum]]
      rng <- this$range
      val <- rng[1] + frac*diff(rng)
      val <- max(this$range[1], min(this$range[2], round(val,this$round)))
    }
    break
  }
  # Restore slider and place new mark
  if (moveMark && !is.null(val)) {
    parms <- sliders[[slideGrp]]$parms
    this <- sliders[[slideGrp]][[sNum]]
    segments(this$x(this$val), parms$slideYs[sNum]-this$tickHt, 
             this$x(this$val), parms$slideYs[sNum]+this$tickHt, col="white", lwd=3)
    segments(parms$slideLeft[sNum], parms$slideYs[sNum],
             parms$slideRight[sNum], parms$slideYs[sNum])
    segments(this$x(val), parms$slideYs[sNum]-this$tickHt, 
             this$x(val), parms$slideYs[sNum]+this$tickHt, col=2, lwd=2)
    # slider ticks:
    at <- this$x(this$at)
    segments(at, parms$slideYs[sNum]-this$tickHt, at, parms$slideYs[sNum]+this$tickHt)
    text(at+slideTickLabAdj[sNum], parms$slideYs[sNum]-this$tickHt+slideTickLabAdj[2],
         this$at, adj=c(0.5,1), cex=slideTickLabCex)
  }

  if (!returnYX) {
    if (!(is.null(val))) return(paste(sgCount-slideGrp+1, sNum, round(val,3), sep="/"))
    return(buttons[[yIndex]][x])
  }
  if (!(is.null(val))) return(c(sgCount-slideGrp+1, sNum, val))
  return(c(y,x))
}

# Test example:
#if (!is.null(dev.list())) dev.off()
#plot(0:1, 0:1, type="n", xlab="", ylab="", axes=F)
#BI <- list(LETTERS[1:4], LETTERS[17:21])
#jnk=buttonInput(BI, input=FALSE)
#while((ans=buttonInput(BI, draw=FALSE, input=TRUE))!="Q") {
#  print(ans)
#  flush.console()
#}



#if (!is.null(dev.list())) dev.off()
#plot(0:1, 0:1, type="n", xlab="", ylab="", axes=F)
#Ws <- list(1, 1, c(0.2,0.2,0.2,0.4), c(0.3, rep(0.7/4,4)))
#SGvals <- list(sg1=c(0.2, 0.4), sg2=0.6)
#SGs <- list(SG1=list(sg1a=list(range=c(0,1)), sg1b=list(range=c(0,1))),
#            SG2=list(sg2a=list(range=c(0,1))))
#BI <- list(as.character(NA), as.character(NA),
#           LETTERS[1:4], LETTERS[17:21])
#jnk=buttonInput(BI, SGs, SGvals, heights=c(0.2,0.35,0.25,0.2), widths=Ws, new=T,info=T,input=FALSE)
#while((ans=buttonInput(BI, , SGvals, info=jnk, draw=FALSE, input=TRUE))!="Q") {
#  print(ans)
#  flush.console()
#  ss <- strsplit(ans,"/")[[1]]
#  if (length(ss)==3) {
#    SGvals[[as.numeric(ss[1])]][as.numeric(ss[2])] <- as.numeric(ss[3])
#  }
#}
