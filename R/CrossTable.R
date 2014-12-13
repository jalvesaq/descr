
# These functions were developed from the function CrossTable of the package
# gmodels.  The original function had the following comments:
#
# Revision 2.2 2006/05/02
# Fix a bug when a matrix is passed as the 'x' argument
# Reported by Prof. Albert Sorribas same day
# Fix involved creating default values for RowData and ColData
# when there are no dimnames for the matrix

# Revision 2.1 2005/06/26
# Added 'dnn' argument to enable specification of dimnames
# as per table()
# Correct bug in SPSS output for 1d table, where proportions
# were being printed and not percentages ('%' output)

# Revision 2.0 2005/04/27
# Added 'format = "d"' to all table count output
# so that large integers do not print in
# scientific notation

CrossTable <- function (x, y, digits = 3, max.width = NA, expected = FALSE,
    prop.r = TRUE, prop.c = TRUE, prop.t = TRUE, prop.chisq = TRUE,
    chisq = FALSE, fisher = FALSE, mcnemar = FALSE, resid = FALSE,
    sresid = FALSE, asresid = FALSE, missing.include = FALSE,
    drop.levels = TRUE, format = c("SAS", "SPSS"), dnn = NULL,
    cell.layout = TRUE, xlab = NULL, ylab = NULL, ...)
{
    format = match.arg(format)

    RowData <- deparse(substitute(x))
    if (missing(y))
	ColData <- NA
    else
	ColData <- deparse(substitute(y))

    if(is.null(xlab)){
        if(is.null(dnn))
            xlab <- deparse(substitute(x))
        else
            xlab <- dnn[1]
    }
    if(is.null(ylab)){
        if(is.null(dnn)){
            if(missing(y))
                ylab <- "y"
            else
                ylab <- deparse(substitute(y))
        } else {
            ylab <- dnn[2]
        }
    }

    ## Ensure that max.width >= 1
    if (!is.na(max.width) && max.width < 1)
	stop("max.width must be >= 1")
    ## Set 'x' vector flag
    vector.x <- FALSE

    if (missing(y))
    {
	## is x a vector?
	if (is.null(dim(x)))
	{
            if (missing.include)
                x <- no.drop.levels(x)
            if (drop.levels)
                x <- factor(x)
            t <- t(as.matrix(table(x)))
	    vector.x <- TRUE
	}
	## is x a matrix?
	else if (length(dim(x) == 2))
	{
	    if(any(x < 0) || any(is.na(x)))
		stop("all entries of x must be nonnegative and finite")

	    ## Check to see if x has names(dimnames) defined. If yes, use these for
	    ## 'RowData' and 'ColData' labels, else create blank ones
	    ## This can be overridden by setting 'dnn' values
	    if (is.null(names(dimnames(x))))
	    {
		RowData <- ""
		ColData <- ""
	    } else {
		RowData <- names(dimnames(x))[1]
		ColData <- names(dimnames(x))[2]
	    }

	    ## Add generic column and rownames if required
	    ## check each separately, in case user has defined one or the other
	    if (is.null(rownames(x)))
		rownames(x) <- paste("[", 1:nrow(x), ",]", sep = "")
	    if (is.null(colnames(x)))
		colnames(x) <- paste("[,", 1:ncol(x), "]", sep = "")

	    t <- x
	}
	else
	    stop("x must be either a vector or a 2 dimensional matrix, if y is not given")
    } else {
        if(missing.include){
            x <- no.drop.levels(x)
            y <- no.drop.levels(y)
        } 
        if(drop.levels){
            x <- factor(x)
            y <- factor(y)
        }

	if(length(x) != length(y))
	    stop("x and y must have the same length")

	## Generate table
	t <- table(x, y)
    }

    ## Create Titles for Table From Vector Names
    ## At least 2 x 2 table only (for now)
    if (all(dim(t) >= 2)) {
	if (!is.null(dnn)) {
	    if (length(dnn) != 2)
		stop("dnn must have length of 2, one element for each table dimension")
	    else {
		RowData <- dnn[1]
		ColData <- dnn[2]
	    }
	}
    }

    ## if t is not at least a 2 x 2, do not do stats
    ## even if any set to TRUE. Do not do col/table props
    if (any(dim(t) < 2))
	prop.c <- prop.chisq <- chisq <- expected <- fisher <- mcnemar <- FALSE

    if (vector.x && dim(t)[2] < 2)
	prop.r <- FALSE
    if (!vector.x && dim(t)[1] < 2)
	prop.r <- FALSE

    CPR <- CPC <- CPT <- GT <- RS <- TotalN <- CSTc <- CST <- ASR <- FTt <-
    FTl <- FTg <- McN <- McNc <- NA

    ## Generate cell proportion of row
    if(prop.r)
	CPR <- prop.table(t, 1)

    ## Generate cell proportion of col
    if(prop.c)
	CPC <- prop.table(t, 2)

    ## Generate cell proportion of total
    if(prop.t)
	CPT <- prop.table(t)

    ## Generate summary counts
    GT <- sum(t)
    RS <- rowSums(t)
    CS <- colSums(t)

    if (length(dim(x) == 2))
	TotalN <- GT
    else
	TotalN <- length(x)

    ## Perform Chi-Square Tests
    if (expected || chisq || prop.chisq || resid || sresid || asresid) {
        if(!chisq && !prop.chisq){
            wv <- getOption("warn")
            options(warn = -1)
        }
	CST <- chisq.test(t, correct = FALSE, ...)
	if (all(dim(t) == 2))
	    CSTc <- chisq.test(t, correct = TRUE, ...)
        if(!chisq && !prop.chisq){
            options(warn = wv)
        }
    }

    if (asresid & !vector.x)
	ASR <- (CST$observed-CST$expected)/sqrt(CST$expected*((1-RS/GT) %*% t(1-CS/GT)))

    if (fisher)
    {
	try(FTt <- fisher.test(t, alternative = "two.sided"))
	if (all(dim(t) == 2))
	{
	    FTl <- fisher.test(t, alternative = "less")
	    FTg <- fisher.test(t, alternative = "greater")
	}
    }

    if (mcnemar)
    {
	if(dim(t)[1] == dim(t)[2])
	    McN <- mcnemar.test(t, correct = FALSE)
	if (all(dim(t) == 2))
	    McNc <- mcnemar.test(t, correct = TRUE)
    }

    res <- list(t = t, prop.row = CPR, prop.col = CPC, prop.tbl = CPT,
                gt = GT, rs = RS, cs = CS, total.n = TotalN, chisq = chisq,
                CST = CST, chisq.corr = CSTc, fisher.ts = FTt,
                fisher.lt = FTl, fisher.gt = FTg, print.mcnemar = mcnemar,
                mcnemar = McN, mcnemar.corr = McNc, asr = ASR,
                RowData = RowData, ColData = ColData, digits = digits,
                max.width = max.width, vector.x = vector.x,
                expected = expected, prop.chisq = prop.chisq, resid = resid,
                sresid = sresid, asresid = asresid, format = format,
                cell.layout = cell.layout)

    # Attributes for plotting
    attr(res, "xlab") <- xlab
    attr(res, "ylab") <- ylab

    class(res) <- "CrossTable"

    res
}

print.CrossTable <- function(x, ...)
{
    t <- x$t
    CPR <- x$prop.row
    CPC <- x$prop.col
    CPT <- x$prop.tbl
    GT <- x$gt
    RS <- x$rs
    CS <- x$cs
    TotalN <- x$total.n
    chisq <- x$chisq
    CST <- x$CST
    CSTc <- x$chisq.corr
    FTt <- x$fisher.ts
    FTl <- x$fisher.lt
    FTg <- x$fisher.gt
    McN <- x$mcnemar
    McNc <- x$mcnemar.corr
    ASR <- x$asr
    RowData <- x$RowData
    ColData <- x$ColData
    digits <- x$digits
    max.width <- x$max.width
    vector.x <- x$vector.x
    expected <- x$expected
    prop.r <- (is.na(CPR[1]) == FALSE)
    prop.c <- (is.na(CPC[1]) == FALSE)
    prop.t <- (is.na(CPT[1]) == FALSE)
    prop.chisq <- x$prop.chisq
    fisher <- (class(FTt) == "htest")
    resid <- x$resid
    sresid <- x$sresid
    asresid <- x$asresid
    mcnemar <- x$print.mcnemar
    format <- x$format
    cell.layout <- x$cell.layout
    outDec <- getOption("OutDec")

    nsep <- "  | " # normal separator
    if(format == "SAS") {
	hdd <- 1
	psep <- "  | " # percent separator
    } else {
	if (format == "SPSS") {
	    hdd <- 100
	    psep <- "% | "
	} else {
	    stop("unknown format")
	}
    }

    if(vector.x)
	expected <- prop.chisq <- prop.c <- prop.t <- resid <- sresid <- asresid <- FALSE

    ## Column and Row Total Headings
    ColTotal <- gettext("Total", domain = "R-descr")
    RowTotal <- ColTotal

    ## Set consistent column widths based upon dimnames and table values
    strt <- formatC(unclass(t), digits = digits, format = "f", width = 0, decimal.mark = outDec)
    CWidth <- max(digits + 2, c(nchar(strt, type = "width"),
                                nchar(dimnames(t)[[2]], type = "width"),
                                nchar(RS, type = "width"),
                                nchar(CS, type = "width"),
                                nchar(RowTotal, type = "width")))
    if(prop.r){
        if(vector.x)
            strt <- formatC(unclass(CPT), digits = digits, format = "f", width = 0, decimal.mark = outDec)
        else
            strt <- formatC(unclass(CPR), digits = digits, format = "f", width = 0, decimal.mark = outDec)
        CWidth <- max(CWidth, nchar(strt, type = "width"))
    }
    RWidth <- max(c(nchar(dimnames(t)[[1]], type = "width"), nchar(ColTotal, type = "width")))

    ## Adjust first column width if Data Titles present
    if (is.na(RowData) == FALSE)
	RWidth <- max(RWidth, nchar(RowData, type = "width"))

    ## Create row separators
    RowSep <- paste(rep("-", CWidth + 2), collapse = "")
    RowSep1 <- paste(rep("-", RWidth + 1), collapse = "")
    SpaceSep1 <- paste(rep(" ", RWidth), collapse = "")
    SpaceSep2 <- paste(rep(" ", CWidth), collapse = "")

    ## Create formatted Names
    FirstCol <- formatC(dimnames(t)[[1]], width = RWidth, format = "s")
    ColTotal <- formatC(ColTotal, width = RWidth, format = "s")
    RowTotal <- formatC(RowTotal, width = CWidth, format = "s")


    #### Printing the tables

    ## Print Cell Layout
    if(cell.layout){
        cat("  ", gettext("Cell Contents", domain = "R-descr"), "\n")
        cat("|-------------------------|\n")
        if (format=="SAS") {
            cat(gettext("|                       N |", domain = "R-descr"), "\n")
            if (expected)
                cat(gettext("|              Expected N |", domain = "R-descr"), "\n")
            if (prop.chisq)                                                     
                cat(gettext("| Chi-square contribution |", domain = "R-descr"), "\n")
            if (prop.r)                                                         
                cat(gettext("|           N / Row Total |", domain = "R-descr"), "\n")
            if (prop.c)                                                         
                cat(gettext("|           N / Col Total |", domain = "R-descr"), "\n")
            if (prop.t)                                                         
                cat(gettext("|         N / Table Total |", domain = "R-descr"), "\n")
        } else if (format == "SPSS") {
            cat(gettext("|                   Count |", domain = "R-descr"), "\n")
            if (expected)
                cat(gettext("|         Expected Values |", domain = "R-descr"), "\n")
            if (prop.chisq)                                                       
                cat(gettext("| Chi-square contribution |", domain = "R-descr"), "\n")
            if (prop.r)                                                           
                cat(gettext("|             Row Percent |", domain = "R-descr"), "\n")
            if (prop.c)                                                           
                cat(gettext("|          Column Percent |", domain = "R-descr"), "\n")
            if (prop.t)                                                           
                cat(gettext("|           Total Percent |", domain = "R-descr"), "\n")
        }
        if (resid)                                                            
            cat(gettext("|                Residual |", domain = "R-descr"), "\n")
        if (sresid)                                                           
            cat(gettext("|            Std Residual |", domain = "R-descr"), "\n")
        if (asresid)                                                          
            cat(gettext("|           Adj Std Resid |", domain = "R-descr"), "\n")
        cat("|-------------------------|\n")
    }

    #cat(gettext("Total Observations in Table:", domain = "R-descr"), GT, "\n\n")

    ## Print 1 X N vector
    if (vector.x) {
        if(is.na(max.width))
            max.width = floor((getOption("width") - 2) / (CWidth + 3))
	if (length(t) > max.width)
	{
	    ## set breakpoints for output based upon max.width
	    final.row <- length(t) %% max.width
	    max <- length(t) - final.row
	    ## Define breakpoint indices for each row
	    start <- seq(1, max, max.width)
	    end <- start + (max.width - 1)
	    ## Add final.row if required
	    if (final.row > 0)
	    {
		start <- c(start, end[length(end)] + 1)
		end <- c(end, end[length(end)] + final.row)
	    }
	}
	else
	{
	    ## Each value printed horizontally in a single row
	    start <- 1
	    end <- length(t)
	}

	SpaceSep3 <- paste(SpaceSep2, " ", sep = "")
        cat("\n")

	for (i in 1:length(start))
	{
	    cat("| ")
            cat(paste(formatC(dimnames(t)[[2]][start[i]:end[i]], width = CWidth, format = "s"), collapse = " | "))
            cat(" |\n|")
	    cat(rep(RowSep, (end[i] - start[i]) + 1), sep = "|")
            cat("|\n| ")
            cat(formatC(t[, start[i]:end[i]], width = CWidth, format = "d"), sep = " | ")
	    if(prop.r){
		cat(" |\n| ")
                cat(formatC(CPT[, start[i]:end[i]] * hdd, width = CWidth,
			    digits = digits, format = "f", decimal.mark = outDec), sep = " | ")
            }
	    cat(" |\n|")
            cat(rep(RowSep, (end[i] - start[i]) + 1), sep = "|")
            cat("|\n\n")

	}  ## End of for (i in 1:length(start))

	if(format == "SPSS" && GT < TotalN)
	    cat("\n", gettext("Number of Missing Observations:", domain = "R-descr"),
		" ", TotalN-GT, " (", 100*(TotalN-GT)/TotalN, "%)\n", sep = "")
	return(invisible(x))
    } ## End of if (vector.x)


    nelements <- 1 + expected + prop.chisq + prop.r + prop.c + prop.t + resid + sresid + asresid
    nr <- nrow(t) * nelements + 1
    nc <- ncol(t)
    m <- matrix(nrow = nr, ncol = nc + 1)
    rnames <- vector(mode = "character", length = nr)
    ## Fill matrix with table cells values converted into character
    k <- 1
    for (i in 1:nrow(t))
    {
	for(l in 1:nc)
	    m[k, l] <- formatC(t[i,l], format = "d")
	m[k, nc + 1] <- RS[i]
	rnames[k] <- rownames(t)[i]
	k <- k + 1

	if(expected){
	    for(l in 1:nc)
		m[k, l] <- formatC(CST$expected[i, l], digits = 1, format = "f",
		    decimal.mark = outDec)
	    m[k, nc + 1] <- " "
	    rnames[k] <- paste("<=>", as.character(k))
	    k <- k + 1
	}

	if(prop.chisq){
	    for(l in 1:nc)
		m[k, l] <- formatC((((CST$expected[i, l]-t[i, l])^2)/CST$expected[i, l]),
		    digits = digits, format = "f", decimal.mark = outDec)
	    m[k, nc + 1] <- " "
	    rnames[k] <- paste("<=>", as.character(k))
	    k <- k + 1
	}

	if(prop.r){
	    for(l in 1:nc)
		m[k, l] <- formatC(unclass(CPR)[i, l]*hdd, digits = digits, format = "f",
		    decimal.mark = outDec)
	    m[k, nc + 1] <- formatC(hdd*RS[i] / GT, digits = digits, format = "f",
		decimal.mark = outDec)
	    rnames[k] <- paste("<=>", as.character(k))
	    k <- k + 1
	}

	if(prop.c){
	    for(l in 1:nc)
		m[k, l] <- formatC(CPC[i, l]*hdd, digits = digits, format = "f",
		    decimal.mark = outDec)
	    m[k, nc + 1] <- " "
	    rnames[k] <- paste("<=>", as.character(k))
	    k <- k + 1
	}

	if(prop.t){
	    for(l in 1:nc)
		m[k, l] <- formatC(CPT[i, l]*hdd, digits = digits, format = "f",
		    decimal.mark = outDec)
	    m[k, nc + 1] <- " "
	    rnames[k] <- paste("<=>", as.character(k))
	    k <- k + 1
	}

	if(resid){
	    for(l in 1:nc)
		m[k, l] <- formatC(CST$observed[i, l]-CST$expected[i, l], digits = digits,
		    format = "f", decimal.mark = outDec)
	    m[k, nc + 1] <- " "
	    rnames[k] <- paste("<=>", as.character(k))
	    k <- k + 1
	}

	if(sresid){
	    for(l in 1:nc)
		m[k, l] <- formatC(CST$residual[i, l], digits = digits,
		    format = "f", decimal.mark = outDec)
	    m[k, nc + 1] <- " "
	    rnames[k] <- paste("<=>", as.character(k))
	    k <- k + 1
	}

	if(asresid){
	    for(l in 1:nc)
		m[k, l] <- formatC(ASR[i, l], digits = digits, format = "f",
		    decimal.mark = outDec)
	    m[k, nc + 1] <- " "
	    rnames[k] <- paste("<=>", as.character(k))
	    k <- k + 1
	}
    }

    ## Fill Column Totals
    ColTotal <- gettext("Total", domain = "R-descr")
    RowTotal <- ColTotal
    rnames[k] <- RowTotal
    for(l in 1:nc)
	m[k, l] <- formatC(c(CS[l]), format = "d")
    m[k, nc + 1] <- formatC(GT, format = "d")
    colnames(m) <- c(colnames(t), ColTotal)

    ## Print table cells
    # cat("\n\n")
    nc <- nc + 1
    colWidths <- vector(mode = "numeric", length = nc)
    mcolnames <- colnames(m)
    for(i in 1:nc)
	colWidths[i] <- max(c(nchar(m[, i], type = "width"), nchar(mcolnames[i], type = "width")))
    labelwidth <- max(nchar(rnames, type = "width"), nchar(RowData, type = "width")) + 1
    dashedline <- rep("-", sum(colWidths) + 3 * nc + labelwidth)
    ddashedline <- gsub("-", "=", dashedline)

    minimumw <- max(digits + 3, 6)
    biggestw <- max(c(labelwidth - 1, colWidths))
    totalwidth <- labelwidth + (sum(colWidths + 3))
    availablewidth <- getOption("width")
    if(totalwidth > availablewidth){
        # Truncate row and column labels. Withdraw one char of the biggest
        # label until the rows fit in the screen
        while(totalwidth > availablewidth && biggestw >= minimumw){
            biggestw <- max(c(labelwidth - 1, colWidths))
            subp <- paste(rep(".", biggestw - 1), collapse = "")
            subp <- paste("(", subp, ").*", sep = "")
            if(labelwidth > biggestw){
                RowData <- sub(subp, "\\1", RowData)
                for(i in 1:nr)
                    rnames[i] <- sub(subp, "\\1", rnames[i])
                labelwidth <- max(nchar(rnames, type = "width"), nchar(RowData, type = "width")) + 1
            } else {
                for(i in 1:nc){
                    mcolnames[i] <- sub(subp, "\\1", mcolnames[i])
                    colWidths[i] <- max(c(nchar(m[, i], type = "width"), nchar(mcolnames[i], type = "width")))
                }
            }
            totalwidth <- labelwidth + (sum(colWidths + 3))
        }
        dashedline <- rep("-", sum(colWidths) + 3 * nc + labelwidth)
        ddashedline <- gsub("-", "=", dashedline)
    }

    cat("\n", ddashedline, "\n", sep = "")
    if(ColData != "")
        cat(formatC(" ", width = labelwidth), "   ", ColData, "\n", sep = "", collapse = "")
    if(RowData == "")
        cat(formatC(" ", width = labelwidth))
    else
        cat(formatC(RowData, width = labelwidth, format = "s", flag = "-"))
    for(j in 1:nc)
	cat("  ", formatC(mcolnames[j], width = colWidths[j]))
    for(i in 1:nr){
	if(length(grep("<=>", rnames[i])) == 0){
	    cat("\n", dashedline, "\n", sep = "")
	    cat(formatC(rnames[i], width = labelwidth, format = "s", flag = "-"), sep = "")
	} else {
	    cat("\n", formatC(" ", width = labelwidth), sep = "")
	}
	for(j in 1:nc){
	    cat("  ", formatC(m[i,j], width = colWidths[j]))
	}
    }
    if(prop.c){
	cat("\n", formatC(" ", width = labelwidth), sep = "")
	for(j in 1:(nc - 1)){
	    cat("  ", formatC(hdd*CS[j]/GT, width = colWidths[j], digits = digits,
		    format = "f", decimal.mark = outDec))
	}
    }
    cat("\n", ddashedline, "\n", sep = "")


    ## Print Statistics
    if (chisq)
    {
	cat("\n")
	cat(gettext("Statistics for All Table Factors", domain = "R-descr"),
	    "\n\n", sep="")

	cat(CST$method, "\n")
	cat("------------------------------------------------------------\n")
	fp <- format.pval(CST$p.value, digits = digits)
        pv <- paste("p", if(substr(fp, 1L, 1L) == "<") fp else paste("=", fp))
	cat(gettext("Chi^2 =", domain = "R-descr"), CST$statistic,
	    "    ", gettext("d.f. =", domain = "R-descr"), CST$parameter,
	    "    ", pv, "\n\n")

	if (all(dim(t) == 2))
	{
	    cat(CSTc$method, "\n")
	    cat("------------------------------------------------------------\n")
            fp <- format.pval(CSTc$p.value, digits = digits)
            pv <- paste("p", if(substr(fp, 1L, 1L) == "<") fp else paste("=", fp))
	    cat(gettext("Chi^2 =", domain = "R-descr"), CSTc$statistic,
		"    ", gettext("d.f. =", domain = "R-descr"), CSTc$parameter,
		"    ", pv, "\n")
	}
    }

    ## Print McNemar tests
    if (is.na(McN[1]) == FALSE)
    {
	cat(rep("\n", 2))
	cat(McN$method, "\n")
	cat("------------------------------------------------------------\n")
	fp <- format.pval(McN$p.value, digits = digits)
        pv <- paste("p", if(substr(fp, 1L, 1L) == "<") fp else paste("=", fp))
	cat(gettext("Chi^2 =", domain = "R-descr"), McN$statistic,
	    "    ", gettext("d.f. =", domain = "R-descr"), McN$parameter,
	    "    ", pv, "\n\n")

	if (is.na(McNc[1]) == FALSE)
	{
	    cat(McNc$method, "\n")
	    cat("------------------------------------------------------------\n")
            fp <- format.pval(McNc$p.value, digits = digits)
            pv <- paste("p", if(substr(fp, 1L, 1L) == "<") fp else paste("=", fp))
	    cat(gettext("Chi^2 =", domain = "R-descr"), McNc$statistic,
		"    ", gettext("d.f. =", domain = "R-descr"), McNc$parameter,
		"    ", pv, "\n")
	}
    }

    ## Pint Fisher Tests
    if (fisher)
    {
	cat(rep("\n", 2))

	cat(gettext("Fisher's Exact Test for Count Data", domain = "R-descr"))
	cat("\n------------------------------------------------------------\n")

	if (all(dim(t) == 2))
	{
	    cat(gettext("Sample estimate odds ratio:", domain = "R-descr"), FTt$estimate, "\n\n")

	    cat(gettext("Alternative hypothesis: true odds ratio is not equal to 1",
		    domain = "R-descr"), "\n")
            fp <- format.pval(FTt$p.value, digits = digits)
            pv <- paste("p", if(substr(fp, 1L, 1L) == "<") fp else paste("=", fp))
	    cat(pv, "\n")
	    cat(gettext("95% confidence interval:", domain = "R-descr"), FTt$conf.int, "\n\n")

	    cat(gettext("Alternative hypothesis: true odds ratio is less than 1",
		    domain = "R-descr"), "\n")
            fp <- format.pval(FTl$p.value, digits = digits)
            pv <- paste("p", if(substr(fp, 1L, 1L) == "<") fp else paste("=", fp))
	    cat(pv, "\n")
	    cat(gettext("95% confidence interval:", domain = "R-descr"), FTl$conf.int, "\n\n")

	    cat(gettext("Alternative hypothesis: true odds ratio is greater than 1",
		    domain = "R-descr"), "\n")
            fp <- format.pval(FTg$p.value, digits = digits)
            pv <- paste("p", if(substr(fp, 1L, 1L) == "<") fp else paste("=", fp))
	    cat(pv, "\n")
	    cat(gettext("95% confidence interval:", domain = "R-descr"), FTg$conf.int, "\n\n")
	}
	else
	{
	    cat(gettext("Alternative hypothesis: two.sided", domain = "R-descr"), "\n")
            fp <- format.pval(FTt$p.value, digits = digits)
            pv <- paste("p", if(substr(fp, 1L, 1L) == "<") fp else paste("=", fp))
	    cat(pv, "\n")
	}
    } ## End Of If(Fisher) Loop

    #  cat(rep("\n", 2))

    if(format == "SPSS"){
	if (any(dim(t) >= 2) & any(chisq, mcnemar, fisher))
	{
	    MinExpF = min(CST$expected)
	    cat("       ", gettext("Minimum expected frequency:", domain = "R-descr"), MinExpF, "\n")
	    NMinExpF = length(CST$expected[which(CST$expected<5)])
	    if (NMinExpF > 0)
	    {
		NCells = length(CST$expected)
		cat(gettext("Cells with Expected Frequency < 5:", domain = "R-descr"),
		    " ", NMinExpF, " ", gettext("of", domain = "R-descr"), " ",
                    NCells, " (", 100*NMinExpF/NCells, "%)\n", sep = "")
	    }
	    cat("\n")

	} ## End of if (any(dim(t)...))
    }
    return(invisible(x))
}

as.data.frame.CrossTable <- function(x, ...) as.data.frame(x$t, ...)


# Needed by tableStyles() of odfWeave package:
dim.CrossTable <- function(x){
    dim(x$t) + 1
}

