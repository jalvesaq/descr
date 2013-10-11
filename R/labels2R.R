
labels2R <- function(lfile, rfile, dfname = "b")
{
    if(missing(lfile))
        stop(gettext("The name of file with labels is required.",
                     domain = "R-descr"))
    if(missing(rfile))
        stop(gettext("The name of file with R code to is required.",
                     domain = "R-descr"))
    if(!is.character(lfile))
      stop("lfile must be of class character.")
    if(!is.character(rfile))
      stop("rfile must be of class character.")
    if(!is.character(dfname))
      stop("dfname must be of class character.")

    infile <- path.expand(lfile[1])
    outfile <- path.expand(rfile[1])
    if(!file.exists(infile)){
        msg <- paste(gettext("File not found:", domain = "R-descr"), lfile)
        stop(msg)
    }

    .C("reallabels2R", c(infile, outfile), dfname, "", 0, PACKAGE="descr")
    return(invisible(NULL))
}

