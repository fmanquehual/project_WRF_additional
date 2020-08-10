identificacion.rangos.corrplot <- function (corr, method = c("circle", "square", "ellipse", "number", 
                                                             "shade", "color", "pie"), type = c("full", "lower", "upper"), 
                                            add = FALSE, col = NULL, bg = "white", title = "", is.corr = TRUE, 
                                            diag = TRUE, outline = FALSE, mar = c(0, 0, 0, 0), addgrid.col = NULL, 
                                            addCoef.col = NULL, addCoefasPercent = FALSE, 
                                            
                                            activar.etiquetas.caracteres=FALSE,
                                            nuevas.etiquetas.de.leyenda=NULL,
                                            nombre.columnas.abajo=FALSE,
                                            mover.grafico.en.y=FALSE,
                                            valor.movimiento.grafico.en.y=c(0,1),
                                            mover.grafico.en.x=FALSE,
                                            valor.movimiento.grafico.en.x=c(0,1),
                                            
                                            order = c("original","AOE", "FPC", "hclust", "alphabet"), 
                                            hclust.method = c("complete","ward", "ward.D", "ward.D2", "single", "average", "mcquitty",
                                                              "median", "centroid"), addrect = NULL, rect.col = "black", 
                                            rect.lwd = 2, tl.pos = NULL, tl.cex = 1, tl.col = "red", tamanho.circulo.fun = 0.45,
                                            tl.row = "red",
                                            tl.offset = 0.4, tl.srt = 90, cl.pos = NULL, cl.lim = NULL, 
                                            cl.length = NULL, cl.cex = 0.8, cl.ratio = 0.15, cl.align.text = "c", 
                                            cl.offset = 0.5, number.cex = 1, number.font = 2, number.digits = NULL, 
                                            addshade = c("negative", "positive", "all"), shade.lwd = 1, 
                                            shade.col = "white", p.mat = NULL, sig.level = 0.05, insig = c("pch", 
                                                                                                           "p-value", "blank", "n", "label_sig"), pch = 4, pch.col = "black", 
                                            pch.cex = 3, plotCI = c("n", "square", "circle", "rect"), 
                                            lowCI.mat = NULL, uppCI.mat = NULL, na.label = "?", na.label.col = "black", 
                                            win.asp = 1, ...) 
{
  method <- match.arg(method)
  type <- match.arg(type)
  order <- match.arg(order)
  hclust.method <- match.arg(hclust.method)
  addshade <- match.arg(addshade)
  insig <- match.arg(insig)
  plotCI <- match.arg(plotCI)
  if (win.asp != 1 && !(method %in% c("circle", "square"))) {
    stop("Parameter 'win.asp' is supported only for circle and square methods.")
  }
  asp_rescale_factor <- min(1, win.asp)/max(1, win.asp)
  stopifnot(asp_rescale_factor >= 0 && asp_rescale_factor <= 
              1)
  if (!is.matrix(corr) && !is.data.frame(corr)) {
    stop("Need a matrix or data frame!")
  }
  if (is.null(addgrid.col)) {
    addgrid.col <- switch(method, color = NA, shade = NA, 
                          "grey")
  }
  if (any(corr < cl.lim[1]) || any(corr > cl.lim[2])) {
    stop("color limits should cover matrix")
  }
  if (is.null(cl.lim)) {
    if (is.corr) {
      cl.lim <- c(-1, 1)
    }
    else {
      corr_tmp <- corr
      diag(corr_tmp) <- ifelse(diag, diag(corr_tmp), NA)
      cl.lim <- c(min(corr_tmp, na.rm = TRUE), max(corr_tmp, 
                                                   na.rm = TRUE))
    }
  }
  intercept <- 0
  zoom <- 1
  if (!is.corr) {
    c_max <- max(corr, na.rm = TRUE)
    c_min <- min(corr, na.rm = TRUE)
    if (c_max <= 0) {
      intercept <- -cl.lim[2]
      zoom <- 1/(diff(cl.lim))
    }
    else if (c_min >= 0) {
      intercept <- -cl.lim[1]
      zoom <- 1/(diff(cl.lim))
    }
    else {
      stopifnot(c_max * c_min < 0)
      stopifnot(c_min < 0 && c_max > 0)
      intercept <- 0
      zoom <- 1/max(abs(cl.lim))
    }
    if (zoom == Inf) {
      stopifnot(cl.lim[1] == 0 && cl.lim[2] == 0)
      zoom <- 0
    }
    corr <- (intercept + corr) * zoom
  }
  cl.lim2 <- (intercept + cl.lim) * zoom
  int <- intercept * zoom
  if (is.corr) {
    if (min(corr, na.rm = TRUE) < -1 - .Machine$double.eps^0.75 || 
        max(corr, na.rm = TRUE) > 1 + .Machine$double.eps^0.75) {
      stop("The matrix is not in [-1, 1]!")
    }
  }
  if (is.null(col)) {
    col <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
                              "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
                              "#4393C3", "#2166AC", "#053061"))(200)
  }
  n <- nrow(corr)
  m <- ncol(corr)
  min.nm <- min(n, m)
  ord <- seq_len(min.nm)
  if (order != "original") {
    ord <- corrMatOrder(corr, order = order, hclust.method = hclust.method)
    corr <- corr[ord, ord]
  }
  if (is.null(rownames(corr))) {
    rownames(corr) <- seq_len(n)
  }
  if (is.null(colnames(corr))) {
    colnames(corr) <- seq_len(m)
  }
  apply_mat_filter <- function(mat) {
    x <- matrix(1:n * m, nrow = n, ncol = m)
    switch(type, upper = mat[row(x) > col(x)] <- Inf, lower = mat[row(x) < 
                                                                    col(x)] <- Inf)
    if (!diag) {
      diag(mat) <- Inf
    }
    return(mat)
  }
  getPos.Dat <- function(mat) {
    tmp <- apply_mat_filter(mat)
    Dat <- tmp[is.finite(tmp)]
    ind <- which(is.finite(tmp), arr.ind = TRUE)
    Pos <- ind
    Pos[, 1] <- ind[, 2]
    Pos[, 2] <- -ind[, 1] + 1 + n
    return(list(Pos, Dat))
  }
  getPos.NAs <- function(mat) {
    tmp <- apply_mat_filter(mat)
    ind <- which(is.na(tmp), arr.ind = TRUE)
    Pos <- ind
    Pos[, 1] <- ind[, 2]
    Pos[, 2] <- -ind[, 1] + 1 + n
    return(Pos)
  }
  Pos <- getPos.Dat(corr)[[1]]
  if (any(is.na(corr)) && is.character(na.label)) {
    PosNA <- getPos.NAs(corr)
  }
  else {
    PosNA <- NULL
  }
  AllCoords <- rbind(Pos, PosNA)
  n2 <- max(AllCoords[, 2])
  n1 <- min(AllCoords[, 2])
  nn <- n2 - n1
  m2 <- max(AllCoords[, 1])
  m1 <- min(AllCoords[, 1])
  mm <- max(1, m2 - m1)
  expand_expression <- function(s) {
    ifelse(grepl("^[:=$]", s), parse(text = substring(s, 
                                                      2)), s)
  }
  newrownames <- sapply(rownames(corr)[(n + 1 - n2):(n + 1 - 
                                                       n1)], expand_expression)
  newcolnames <- sapply(colnames(corr)[m1:m2], expand_expression)
  DAT <- getPos.Dat(corr)[[2]]
  len.DAT <- length(DAT)
  rm(expand_expression)
  assign.color <- function(dat = DAT, color = col) {
    newcorr <- (dat + 1)/2
    newcorr[newcorr <= 0] <- 0
    newcorr[newcorr >= 1] <- 1 - 1e-16
    color[floor(newcorr * length(color)) + 1]
  }
  col.fill <- assign.color()
  isFALSE <- function(x) identical(x, FALSE)
  isTRUE <- function(x) identical(x, TRUE)
  if (isFALSE(tl.pos)) {
    tl.pos <- "n"
  }
  if (is.null(tl.pos) || isTRUE(tl.pos)) {
    tl.pos <- switch(type, full = "lt", lower = "ld", upper = "td")
  }
  if (isFALSE(cl.pos)) {
    cl.pos <- "n"
  }
  if (is.null(cl.pos) || isTRUE(cl.pos)) {
    cl.pos <- switch(type, full = "r", lower = "b", upper = "r")
  }
  if (isFALSE(outline)) {
    col.border <- col.fill
  }
  else if (isTRUE(outline)) {
    col.border <- "black"
  }
  else if (is.character(outline)) {
    col.border <- outline
  }
  else {
    stop("Unsupported value type for parameter outline")
  }
  # oldpar <- par(mar = mar, bg = "white")
  # on.exit(par(oldpar), add = TRUE)
  # if (!add) {
    #plot.new()
    xlabwidth <- 1#max(strwidth(newrownames, cex = tl.cex))
    ylabwidth <- 1#max(strwidth(newcolnames, cex = tl.cex))
    laboffset <- 1#strwidth("W", cex = tl.cex) * tl.offset
    for (i in 1:50) {
      xlim <- c(m1 - 0.5 - laboffset - xlabwidth * (grepl("l", 
                                                          tl.pos) | grepl("d", tl.pos)), m2 + 0.5 + mm * 
                  cl.ratio * (cl.pos == "r") + xlabwidth * abs(cos(tl.srt * 
                                                                     pi/180)) * grepl("d", tl.pos)) + c(-0.35, 0.15) + 
        c(-1, 0) * grepl("l", tl.pos)
      ylim <- c(n1 - 0.5 - nn * cl.ratio * (cl.pos == 
                                              "b") - laboffset, n2 + 0.5 + laboffset + ylabwidth * 
                  abs(sin(tl.srt * pi/180)) * grepl("t", tl.pos)) + 
        c(-0.15, 0) + c(0, -1) * (type == "upper" && 
                                    tl.pos != "n") + c(0, 1) * grepl("d", tl.pos)
      #plot.window(xlim, ylim, asp = 1, xaxs = "i", yaxs = "i")
      # x.tmp <- max(strwidth(newrownames, cex = tl.cex))
      # y.tmp <- max(strwidth(newcolnames, cex = tl.cex))
      # laboffset.tmp <- strwidth("W", cex = tl.cex) * tl.offset
      # if (max(x.tmp - xlabwidth, y.tmp - ylabwidth, laboffset.tmp - 
      #         laboffset) < 0.001) {
      #   break
      # }
      # xlabwidth <- x.tmp
      # ylabwidth <- y.tmp
      # laboffset <- laboffset.tmp
    #   if (i == 50) {
    #     warning(c("Not been able to calculate text gin, ", 
    #               "please try again with a clean new empty window using ", 
    #               "{plot.new(); dev.off()} or reduce tl.cex"))
    #   }
    #   
    # }
  }
  return(c(xlim, ylim))
}
  