
xtable.CrossTable <- function(x, caption = NULL, label = NULL, align = NULL,
                              digits = 1, display = NULL, multirow = FALSE,
                              hline = FALSE, percent, ...)
{
    nr <- nrow(x$tab)
    nc <- ncol(x$t)
    x$digits <- digits

    if(missing(percent)){
        if(multirow || hline)
            percent <- TRUE
        else
            percent <- FALSE
    }
    if(percent){
        nt <- CreateNewTab(x, prct = TRUE)
        for(i in 1:nrow(nt))
            for(j in 1:ncol(nt))
                nt[i, j] <- sub("%", "\\\\%", nt[i, j])
    } else {
        nt <- CreateNewTab(x)
    }

    # Add rownames as first column
    nt <- cbind(rownames(nt), nt)
    colnames(nt)[1] <- x$RowData
    rownames(nt) <- NULL

    if(x$total.c && !is.na(x$prop.col)[1])
        nrnt <- nrow(nt) - 2
    else if(x$total.c)
        nrnt <- nrow(nt) - 1
    else
        nrnt <- nrow(nt)

    n <- nrnt / nr
    idxm <- seq(1, nrnt, n)
    idxh <- seq(n+1, nrnt+1, n)
    idxh <- idxh[idxh < nrow(nt)] # necessary when total.c = FALSE

    if(multirow)
        nt[idxm, 1] <- paste("\\multirow{", n, "}{*}{", nt[idxm, 1], "}", sep = "")
    if(hline)
        nt[idxh, 1] <- paste("\\hline\n", nt[idxh, 1], sep = "")

    if(multirow){
        idxc <- 1:ncol(x$tab) + 1
        colnames(nt)[idxc] <- paste0("\\multicolumn{1}{c}{", colnames(nt)[idxc], "}")
        col1txt <- paste0("\\multirow{2}{*}{", gsub("\\$", "\\\\$", x$RowData), "} & \\multicolumn{", ncol(x$tab), "}{c}{", gsub("\\$", "\\\\$", x$ColData), "}")
        if(x$total.r)
            col1txt <- paste0(col1txt, " & \\multirow{2}{*}{", colnames(nt)[ncol(nt)], "}\\\\\n \\cline{2-", ncol(x$tab)+1,"}", sep = "")
        else
            col1txt <- paste0(col1txt, " \\\\\n \\cline{2-", ncol(x$tab)+1,"}")
        colnames(nt)[1] <- col1txt
        if(x$total.r)
            colnames(nt)[ncol(nt)] <- " "
    }

    if(is.null(align))
        align = paste0("ll", paste(rep("r", ncol(nt) - 1), collapse = ""))
    xtable::xtable(nt, caption=caption, label=label, align=align, display=display, ...)
}

xtable.freqtable <- function(x, caption = NULL, label = NULL, align = NULL,
                             digits = 1, display = NULL, ...)
{
    if(is.null(align))
        align <- paste0("l", paste0(rep("r", ncol(x)), collapse = ""))
    if(is.null(display))
        display <- c("s", "d", rep("f", ncol(x) - 1))
    xtable::xtable(unclass(x), caption=caption, label=label, align=align,
                   display=display, ...)
}

xtable.meanscomp <- function(x, caption = NULL, label = NULL, align = NULL,
                             digits = 1, display = NULL, ...)
{
    if(is.null(align))
        align <- "lrrr"
    if(is.null(display))
        display <- c("s", "f", "d", "f")
    xtable::xtable(unclass(x), caption=caption, label=label, align=align,
                   display=display, ...)
}

