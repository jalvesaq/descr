
xtable.CrossTable <- function(x, caption = NULL, label = NULL, align = NULL, digits = 1,
                              display = NULL, multirow = FALSE, hline = FALSE, ...)
{
    nr <- dim(x$t)[1]
    nc <- dim(x$t)[2]
    nt <- cbind(rownames(x$t), x$t, x$rs)

    if(multirow){
        colname1 <- paste("\\multirow{2}{*}{",
                          gsub("\\$", "\\\\$", x$RowData),
                          "} & \\multicolumn{",
                          length(x$t[1,]), "}{c}{",
                          gsub("\\$", "\\\\$", x$ColData),
                          "} & \\multirow{2}{*}{",
                          gettext("Total", domain = "R-descr"), "}\\\\\n \\cline{2-",
                          length(x$t[1,])+1,"}", sep = "")
        colnames(nt) <- c(colname1, colnames(x$t), " ")
    } else {
        colname1 <- attr(x, "xlab")
        colnames(nt) <- c(attr(x, "xlab"), colnames(x$t), gettext("Total", domain = "R-descr"))
    }


    if(x$format == "SPSS")
        hdd <- 100
    else
        hdd <- 1

    if(!is.na(x$expected) && x$expected == TRUE){
        xex <- outer(x$rs, x$cs, "*")
        xex <- xex / x$gt
        if(is.null(digits))
            digits = 1
        xx <- format(round(xex, digits), ...)
        xx <- cbind(rep("", nr), xx, rep("", nr))
        nt <- rbind(nt, xx)
        idx <- integer()
        for(i in 1:nr)
            idx <- c(idx, i, i + nr)
        nt <- nt[idx, ]
    }

    appendlines <- function(nt, xx)
    {
        xx <- cbind(rep("", nr), xx, rep("", nr))
        n <- dim(nt)[1] / nr
        nt <- rbind(nt, xx)
        idx <- integer()
        k <- 1
        l <- nr * n + 1
        for(i in 1:nr){
            for(j in 1:n){
                idx <- c(idx, k)
                k <- k + 1
            }
            idx <- c(idx, l)
            l <- l + 1
        }
        nt <- nt[idx, ]
        nt
    }

    if(x$prop.chisq){
        xx <- ((x$CST$expected - x$t) ^ 2) / x$CST$expected
        xx <- format(round(xx, digits), trim = TRUE, ...)
        nt <- appendlines(nt, xx)
    }

    if(!is.na(x$prop.row[1])){
        xx <- format(round(x$prop.row * hdd, digits), trim = TRUE, ...)
        if(hdd == 100 && (multirow || hline))
            xx <- matrix(paste(xx, "\\%", sep = ""), nrow = nr, ncol = nc)
        nt <- appendlines(nt, xx)
    }

    if(!is.na(x$prop.col[1])){
        xx <- format(round(x$prop.col * hdd, digits), trim = TRUE, ...)
        if(hdd == 100 && (multirow || hline))
            xx <- matrix(paste(xx, "\\%", sep = ""), nrow = nr, ncol = nc)
        nt <- appendlines(nt, xx)
    }

    if(!is.na(x$prop.tbl[1])){
        xx <- format(round(x$prop.tbl * hdd, digits), trim = TRUE, ...)
        if(hdd == 100 && (multirow || hline))
            xx <- matrix(paste(xx, "\\%", sep = ""), nrow = nr, ncol = nc)
        nt <- appendlines(nt, xx)
    }

    if(!is.na(x$resid) && x$resid == TRUE && x$expected == TRUE){
        xx <- x$t - xex
        xx <- format(round(xx, digits), trim = TRUE, ...)
        nt <- appendlines(nt, xx)
    }

    if(!is.na(x$sresid) && x$sresid == TRUE && x$expected == TRUE){
        xx <- x$chisq$residual
        xx <- format(round(xx, digits), trim = TRUE, ...)
        nt <- appendlines(nt, xx)
    }

    if(!is.na(x$asr[1])){
        xx <- format(round(x$asr, digits), trim = TRUE, ...)
        nt <- appendlines(nt, xx)
    }

    n <- dim(nt)[1] / nr
    idx <- seq(1, dim(nt)[1], n)
    if(multirow)
        nt[idx, 1] <- paste("\\multirow{", n, "}{*}{", nt[idx, 1], "}", sep = "")
    if(hline)
        idx <- c(idx[-1], dim(nt)[1] + 1)

    nt <- rbind(nt, c(gettext("Total", domain = "R-descr"), x$cs, x$gt))
    if(hline)
        nt[idx, 1] <- paste("\\hline\n", nt[idx, 1], sep = "")

    len <- dim(nt)[1]
    rownames(nt) <- as.character(1:len)
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

