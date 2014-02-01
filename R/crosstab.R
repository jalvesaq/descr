
crosstab <- function(x, y, weight = NULL, digits = 3, max.width = NA,
                     expected = FALSE, prop.r = FALSE, prop.c = FALSE,
                     prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE,
                     fisher = FALSE, mcnemar = FALSE, resid = FALSE,
                     sresid = FALSE, asresid = FALSE, missing.include = FALSE,
                     format = "SPSS", cell.layout = TRUE, dnn = NULL,
                     xlab = NULL, ylab = NULL, main = "", user.missing.x,
                     user.missing.y, plot = getOption("descr.plot"), ...)
{
    if(is.null(dnn))
        dnn <- c(deparse(substitute(x)), deparse(substitute(y)))

    if(!missing(user.missing.x)){
        user.missing.x <- paste("^", user.missing.x, "$", sep = "")
        xlevels <- levels(x)
        for(lev in user.missing.x){
            if(length(grep(lev, xlevels))){
                idx <- grep(lev, as.character(x)) 
                if(length(idx))
                    x[idx] <- NA
            }
        }
        x <- factor(x)
    }
    if(!missing(user.missing.y)){
        user.missing.y <- paste("^", user.missing.y, "$", sep = "")
        ylevels <- levels(y)
        for(lev in user.missing.y){
            if(length(grep(lev, ylevels))){
                idx <- grep(lev, as.character(y)) 
                if(length(idx))
                    y[idx] <- NA
            }
        }
        y <- factor(y)
    }
    if (is.null(weight)) 
        tab <- table(x, y)
    else
        tab <- round(xtabs(weight ~ x + y))
    names(dimnames(tab)) <- dnn

    crosstb <- CrossTable(tab, digits = digits, max.width = max.width,
                      expected = expected, prop.r = prop.r, prop.c = prop.c,
                      prop.t = prop.t, prop.chisq = prop.chisq, chisq = chisq,
                      fisher = fisher, mcnemar = mcnemar, resid = resid,
                      sresid = sresid, asresid = asresid,
                      missing.include = missing.include, format = format,
                      dnn = dnn, xlab = xlab, ylab = ylab)

    if(plot == TRUE)
        plot.CrossTable(crosstb, ...)

    crosstb
}


plot.CrossTable <- function(x, xlab, ylab, main = "", col, inv.x = FALSE, inv.y = FALSE, ...)
{
    tabforplot <- x$t
    if(missing(xlab)){
        lab <- attr(x, "xlab")
        if(!is.null(lab))
            xlab <- lab
        else
            xlab <- names(dimnames(tabforplot))[1]
    }
    if(missing(ylab)){
        lab <- attr(x, "ylab")
        if(!is.null(lab))
            ylab <- lab
        else
            ylab <- names(dimnames(tabforplot))[2]
    }
    nxlev <- dim(tabforplot)[1]
    nylev <- dim(tabforplot)[2]
    if(missing(col)){
        col.min <- 0.9 - 0.25 * (nylev - 1)
        if(col.min < 0.3)
            col.min  <- 0.3
        col <- gray.colors(nylev, 0.9, col.min)
    }
    if(inv.x)
        tabforplot <- tabforplot[nxlev:1, ]
    if(inv.y)
        tabforplot <- tabforplot[, nylev:1]
    class(tabforplot) <- "table"
    if(length(grep("^color$", names(list(...)))) == 0)
        mosaicplot(tabforplot, main = main, xlab = xlab, ylab = ylab, col = col, ...)
    else
        mosaicplot(tabforplot, main = main, xlab = xlab, ylab = ylab, ...)
}

