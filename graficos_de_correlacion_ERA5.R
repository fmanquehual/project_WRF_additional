require("corrplot")
library('cowplot')
library('VennDiagram')
library('ggplot2')

rm(list=ls())
dev.off()

# 1. Funciones ----
setwd('/home/msomos/Documentos/WRF/proyecto_WRF/')
source('funcion_identificacion_de_rangos_de_ejes_corrplot.R')
source('funcion_identificacion_dimensiones_corrplot.R')

# 1
db_to_matriz <- function(data_base, variable.i){
  
  #data_base <- db2.NSE
  #variable.i <- 'NSE'
  col.i <- colnames(data_base)[-1]
  
  setwd('/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
  temp.i <- read.csv('db_todas_las_instituciones_y_las_usadas.csv')
  temp.i$names.modi.ERA5 <- as.character(temp.i$names.modi.ERA5)
  temp.i$names <- as.character(temp.i$names)
  
  dif.name1 <- setdiff(col.i, temp.i$names.modi.ERA5)
  advertencia1 <- paste('Este nombre difiere: ', dif.name1, sep = '')
  
  if(length(advertencia1)!=0){ print( advertencia1 ) } else(
    print('Nombres coinciden')
  )
  
  col.k <- c()
  for (i in 1:length(col.i)) {
    #i <- 1
    col.j <- as.character(temp.i$est.numero[temp.i$names.modi.ERA5%in%col.i[i]])
    if(i==1){col.k <- c(col.j)} else(col.k <- c(col.k, col.j))  
  }
  
  row.i <- as.character(data_base[,1])
  values.i <- as.matrix(data_base[,-1])
  
  if(variable.i=='NSE'){values.i[values.i<(-1)] <- (-1)}
  if(variable.i=='RSR'){values.i[values.i>(2)] <- (2)}
  
  m <- matrix(values.i, ncol = ncol(values.i), nrow = nrow(values.i), byrow = FALSE, 
              dimnames = list(row.i, col.k))
  return(m)
}

# 2
corrplot_modificado <- function (corr, method = c("circle", "square", "ellipse", "number", 
                                                  "shade", "color", "pie"), type = c("full", "lower", "upper"), 
                                 add = FALSE, col = NULL, bg = "white", title = "", is.corr = TRUE, 
                                 diag = TRUE, outline = FALSE, mar = c(0, 0, 0, 0), addgrid.col = NULL, 
                                 
                                 activar.etiquetas.caracteres=FALSE,
                                 nuevas.etiquetas.de.leyenda=NULL,
                                 
                                 addCoef.col = NULL, addCoefasPercent = FALSE, order = c("original", 
                                                                                         "AOE", "FPC", "hclust", "alphabet"), hclust.method = c("complete", 
                                                                                                                                                "ward", "ward.D", "ward.D2", "single", "average", "mcquitty", 
                                                                                                                                                "median", "centroid"), addrect = NULL, rect.col = "black", 
                                 rect.lwd = 2, tl.pos = NULL, tl.cex = 1, tl.col = "red", tamanho.circulo.fun = 0.45,
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
  oldpar <- par(mar = mar, bg = "white")
  on.exit(par(oldpar), add = TRUE)
  if (!add) {
    plot.new()
    xlabwidth <- max(strwidth(newrownames, cex = tl.cex))
    ylabwidth <- max(strwidth(newcolnames, cex = tl.cex))
    laboffset <- strwidth("W", cex = tl.cex) * tl.offset
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
      plot.window(xlim, ylim, asp = 1, xaxs = "i", yaxs = "i")
      x.tmp <- max(strwidth(newrownames, cex = tl.cex))
      y.tmp <- max(strwidth(newcolnames, cex = tl.cex))
      laboffset.tmp <- strwidth("W", cex = tl.cex) * tl.offset
      if (max(x.tmp - xlabwidth, y.tmp - ylabwidth, laboffset.tmp - 
              laboffset) < 0.001) {
        break
      }
      xlabwidth <- x.tmp
      ylabwidth <- y.tmp
      laboffset <- laboffset.tmp
      if (i == 50) {
        warning(c("Not been able to calculate text margin, ", 
                  "please try again with a clean new empty window using ", 
                  "{plot.new(); dev.off()} or reduce tl.cex"))
      }
    }
    if (.Platform$OS.type == "windows") {
      grDevices::windows.options(width = 7, height = 7 * 
                                   diff(ylim)/diff(xlim))
    }
    plot.window(xlim = xlim, ylim = ylim, asp = win.asp, 
                xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  }
  laboffset <- strwidth("W", cex = tl.cex) * tl.offset
  symbols(Pos, add = TRUE, inches = FALSE, rectangles = matrix(1, 
                                                               len.DAT, 2), bg = bg, fg = bg)
  if (method == "circle" && plotCI == "n") {
    symbols(Pos, add = TRUE, inches = FALSE, circles = abs(DAT)*0+tamanho.circulo.fun, fg = col.border, bg = col.fill)
  }
  if (method == "ellipse" && plotCI == "n") {
    ell.dat <- function(rho, length = 99) {
      k <- seq(0, 2 * pi, length = length)
      x <- cos(k + acos(rho)/2)/2
      y <- cos(k - acos(rho)/2)/2
      cbind(rbind(x, y), c(NA, NA))
    }
    ELL.dat <- lapply(DAT, ell.dat)
    ELL.dat2 <- 0.85 * matrix(unlist(ELL.dat), ncol = 2, 
                              byrow = TRUE)
    ELL.dat2 <- ELL.dat2 + Pos[rep(1:length(DAT), each = 100), 
                               ]
    polygon(ELL.dat2, border = col.border, col = col.fill)
  }
  if (is.null(number.digits)) {
    number.digits <- switch(addCoefasPercent + 1, 2, 0)
  }
  stopifnot(number.digits%%1 == 0)
  stopifnot(number.digits >= 0)
  if (method == "number" && plotCI == "n") {
    text(Pos[, 1], Pos[, 2], font = number.font, col = col.fill, 
         labels = round((DAT - int) * ifelse(addCoefasPercent, 
                                             100, 1)/zoom, number.digits), cex = number.cex)
  }
  NA_LABEL_MAX_CHARS <- 2
  if (is.matrix(PosNA) && nrow(PosNA) > 0) {
    stopifnot(is.matrix(PosNA))
    if (na.label == "square") {
      symbols(PosNA, add = TRUE, inches = FALSE, squares = rep(1, 
                                                               nrow(PosNA)), bg = na.label.col, fg = na.label.col)
    }
    else if (nchar(na.label) %in% 1:NA_LABEL_MAX_CHARS) {
      symbols(PosNA, add = TRUE, inches = FALSE, squares = rep(1, 
                                                               nrow(PosNA)), fg = bg, bg = bg)
      text(PosNA[, 1], PosNA[, 2], font = number.font, 
           col = na.label.col, labels = na.label, cex = number.cex, 
           ...)
    }
    else {
      stop(paste("Maximum number of characters for NA label is:", 
                 NA_LABEL_MAX_CHARS))
    }
  }
  if (method == "pie" && plotCI == "n") {
    symbols(Pos, add = TRUE, inches = FALSE, circles = rep(0.5, 
                                                           len.DAT) * 0.85, fg = col.border)
    pie.dat <- function(theta, length = 100) {
      k <- seq(pi/2, pi/2 - theta, length = 0.5 * length * 
                 abs(theta)/pi)
      x <- c(0, cos(k)/2, 0)
      y <- c(0, sin(k)/2, 0)
      cbind(rbind(x, y), c(NA, NA))
    }
    PIE.dat <- lapply(DAT * 2 * pi, pie.dat)
    len.pie <- unlist(lapply(PIE.dat, length))/2
    PIE.dat2 <- 0.85 * matrix(unlist(PIE.dat), ncol = 2, 
                              byrow = TRUE)
    PIE.dat2 <- PIE.dat2 + Pos[rep(1:length(DAT), len.pie), 
                               ]
    polygon(PIE.dat2, border = "black", col = col.fill)
  }
  if (method == "shade" && plotCI == "n") {
    symbols(Pos, add = TRUE, inches = FALSE, squares = rep(1, 
                                                           len.DAT), bg = col.fill, fg = addgrid.col)
    shade.dat <- function(w) {
      x <- w[1]
      y <- w[2]
      rho <- w[3]
      x1 <- x - 0.5
      x2 <- x + 0.5
      y1 <- y - 0.5
      y2 <- y + 0.5
      dat <- NA
      if ((addshade == "positive" || addshade == "all") && 
          rho > 0) {
        dat <- cbind(c(x1, x1, x), c(y, y1, y1), c(x, 
                                                   x2, x2), c(y2, y2, y))
      }
      if ((addshade == "negative" || addshade == "all") && 
          rho < 0) {
        dat <- cbind(c(x1, x1, x), c(y, y2, y2), c(x, 
                                                   x2, x2), c(y1, y1, y))
      }
      return(t(dat))
    }
    pos_corr <- rbind(cbind(Pos, DAT))
    pos_corr2 <- split(pos_corr, 1:nrow(pos_corr))
    SHADE.dat <- matrix(na.omit(unlist(lapply(pos_corr2, 
                                              shade.dat))), byrow = TRUE, ncol = 4)
    segments(SHADE.dat[, 1], SHADE.dat[, 2], SHADE.dat[, 
                                                       3], SHADE.dat[, 4], col = shade.col, lwd = shade.lwd)
  }
  if (method == "square" && plotCI == "n") {
    draw_method_square(Pos, DAT, asp_rescale_factor, col.border, 
                       col.fill)
  }
  if (method == "color" && plotCI == "n") {
    draw_method_color(Pos, col.border, col.fill)
  }
  draw_grid(AllCoords, addgrid.col)
  if (plotCI != "n") {
    if (is.null(lowCI.mat) || is.null(uppCI.mat)) {
      stop("Need lowCI.mat and uppCI.mat!")
    }
    if (order != "original") {
      lowCI.mat <- lowCI.mat[ord, ord]
      uppCI.mat <- uppCI.mat[ord, ord]
    }
    pos.lowNew <- getPos.Dat(lowCI.mat)[[1]]
    lowNew <- getPos.Dat(lowCI.mat)[[2]]
    pos.uppNew <- getPos.Dat(uppCI.mat)[[1]]
    uppNew <- getPos.Dat(uppCI.mat)[[2]]
    if (!method %in% c("circle", "square")) {
      stop("Method shoud be circle or square if drawing confidence intervals.")
    }
    k1 <- (abs(uppNew) > abs(lowNew))
    bigabs <- uppNew
    bigabs[which(!k1)] <- lowNew[!k1]
    smallabs <- lowNew
    smallabs[which(!k1)] <- uppNew[!k1]
    sig <- sign(uppNew * lowNew)
    color_bigabs <- col[ceiling((bigabs + 1) * length(col)/2)]
    color_smallabs <- col[ceiling((smallabs + 1) * length(col)/2)]
    if (plotCI == "circle") {
      symbols(pos.uppNew[, 1], pos.uppNew[, 2], add = TRUE, 
              inches = FALSE, circles = 0.95 * abs(bigabs)^0.5/2, 
              bg = ifelse(sig > 0, col.fill, color_bigabs), 
              fg = ifelse(sig > 0, col.fill, color_bigabs))
      symbols(pos.lowNew[, 1], pos.lowNew[, 2], add = TRUE, 
              inches = FALSE, circles = 0.95 * abs(smallabs)^0.5/2, 
              bg = ifelse(sig > 0, bg, color_smallabs), fg = ifelse(sig > 
                                                                      0, col.fill, color_smallabs))
    }
    if (plotCI == "square") {
      symbols(pos.uppNew[, 1], pos.uppNew[, 2], add = TRUE, 
              inches = FALSE, squares = abs(bigabs)^0.5, bg = ifelse(sig > 
                                                                       0, col.fill, color_bigabs), fg = ifelse(sig > 
                                                                                                                 0, col.fill, color_bigabs))
      symbols(pos.lowNew[, 1], pos.lowNew[, 2], add = TRUE, 
              inches = FALSE, squares = abs(smallabs)^0.5, 
              bg = ifelse(sig > 0, bg, color_smallabs), fg = ifelse(sig > 
                                                                      0, col.fill, color_smallabs))
    }
    if (plotCI == "rect") {
      rect.width <- 0.25
      rect(pos.uppNew[, 1] - rect.width, pos.uppNew[, 
                                                    2] + smallabs/2, pos.uppNew[, 1] + rect.width, 
           pos.uppNew[, 2] + bigabs/2, col = col.fill, 
           border = col.fill)
      segments(pos.lowNew[, 1] - rect.width, pos.lowNew[, 
                                                        2] + DAT/2, pos.lowNew[, 1] + rect.width, pos.lowNew[, 
                                                                                                             2] + DAT/2, col = "black", lwd = 1)
      segments(pos.uppNew[, 1] - rect.width, pos.uppNew[, 
                                                        2] + uppNew/2, pos.uppNew[, 1] + rect.width, 
               pos.uppNew[, 2] + uppNew/2, col = "black", lwd = 1)
      segments(pos.lowNew[, 1] - rect.width, pos.lowNew[, 
                                                        2] + lowNew/2, pos.lowNew[, 1] + rect.width, 
               pos.lowNew[, 2] + lowNew/2, col = "black", lwd = 1)
      segments(pos.lowNew[, 1] - 0.5, pos.lowNew[, 2], 
               pos.lowNew[, 1] + 0.5, pos.lowNew[, 2], col = "grey70", 
               lty = 3)
    }
  }
  if (!is.null(p.mat) && insig != "n") {
    if (order != "original") {
      p.mat <- p.mat[ord, ord]
    }
    pos.pNew <- getPos.Dat(p.mat)[[1]]
    pNew <- getPos.Dat(p.mat)[[2]]
    if (insig == "label_sig") {
      if (!is.character(pch)) 
        pch <- "*"
      place_points <- function(sig.locs, point) {
        text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], 
             labels = point, col = pch.col, cex = pch.cex, 
             lwd = 2)
      }
      if (length(sig.level) == 1) {
        place_points(sig.locs = which(pNew < sig.level), 
                     point = pch)
      }
      else {
        l <- length(sig.level)
        for (i in seq_along(sig.level)) {
          iter <- l + 1 - i
          pchTmp <- paste(rep(pch, i), collapse = "")
          if (i == length(sig.level)) {
            locs <- which(pNew < sig.level[iter])
            if (length(locs)) {
              place_points(sig.locs = locs, point = pchTmp)
            }
          }
          else {
            locs <- which(pNew < sig.level[iter] & pNew > 
                            sig.level[iter - 1])
            if (length(locs)) {
              place_points(sig.locs = locs, point = pchTmp)
            }
          }
        }
      }
    }
    else {
      ind.p <- which(pNew > sig.level)
      p_inSig <- length(ind.p) > 0
      if (insig == "pch" && p_inSig) {
        points(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], 
               pch = pch, col = pch.col, cex = pch.cex, lwd = 2)
      }
      if (insig == "p-value" && p_inSig) {
        text(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], 
             round(pNew[ind.p], 2), col = pch.col)
      }
      if (insig == "blank" && p_inSig) {
        symbols(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], 
                inches = FALSE, squares = rep(1, length(pos.pNew[, 
                                                                 1][ind.p])), fg = addgrid.col, bg = bg, 
                add = TRUE)
      }
    }
  }
  if (cl.pos != "n") {
    colRange <- assign.color(dat = cl.lim2)
    ind1 <- which(col == colRange[1])
    ind2 <- which(col == colRange[2])
    colbar <- col[ind1:ind2]
    if (is.null(cl.length)) {
      cl.length <- ifelse(length(colbar) > 20, 11, length(colbar) + 
                            1)
    }
    labels <- seq(cl.lim[1], cl.lim[2], length = cl.length)
    if (cl.pos == "r") {
      vertical <- TRUE
      xlim <- c(m2 + 0.5 + mm * 0.02, m2 + 0.5 + mm * 
                  cl.ratio)
      ylim <- c(n1 - 0.5, n2 + 0.5)
    }
    if (cl.pos == "b") {
      vertical <- FALSE
      xlim <- c(m1 - 0.5, m2 + 0.5)
      ylim <- c(n1 - 0.5 - nn * cl.ratio, n1 - 0.5 - nn * 
                  0.02)
    }
    if(activar.etiquetas.caracteres==TRUE){
      colorlegend(colbar = colbar, labels = nuevas.etiquetas.de.leyenda, 
                  offset = cl.offset, ratio.colbar = 0.3, cex = cl.cex,
                  xlim = xlim, ylim = ylim, vertical = vertical, align = cl.align.text)} else(
                    
                    colorlegend(colbar = colbar, labels = round(labels, 
                                                                2), offset = cl.offset, ratio.colbar = 0.3, cex = cl.cex, 
                                xlim = xlim, ylim = ylim, vertical = vertical, align = cl.align.text))
  }
  if (tl.pos != "n") {
    pos.xlabel <- cbind(m1:m2, n2 + 0.5 + laboffset)
    pos.ylabel <- cbind(m1 - 0.5, n2:n1)
    if (tl.pos == "td") {
      if (type != "upper") {
        stop("type should be \"upper\" if tl.pos is \"dt\".")
      }
      pos.ylabel <- cbind(m1:(m1 + nn) - 0.5, n2:n1)
    }
    if (tl.pos == "ld") {
      if (type != "lower") {
        stop("type should be \"lower\" if tl.pos is \"ld\".")
      }
      pos.xlabel <- cbind(m1:m2, n2:(n2 - mm) + 0.5 + 
                            laboffset)
    }
    if (tl.pos == "d") {
      pos.ylabel <- cbind(m1:(m1 + nn) - 0.5, n2:n1)
      pos.ylabel <- pos.ylabel[1:min(n, m), ]
      symbols(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], 
              add = TRUE, bg = bg, fg = addgrid.col, inches = FALSE, 
              squares = rep(1, length(pos.ylabel[, 1])))
      text(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], newcolnames[1:min(n, 
                                                                     m)], col = tl.col, cex = tl.cex, ...)
    }
    else {
      text(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, 
           srt = tl.srt, adj = ifelse(tl.srt == 0, c(0.5, 
                                                     0), c(0, 0)), col = tl.col, cex = tl.cex, 
           offset = tl.offset, ...)
      text(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, 
           col = tl.col, cex = tl.cex, pos = 2, offset = tl.offset, 
           ...)
    }
  }
  title(title, ...)
  if (!is.null(addCoef.col) && method != "number") {
    text(Pos[, 1], Pos[, 2], col = addCoef.col, labels = round((DAT - 
                                                                  int) * ifelse(addCoefasPercent, 100, 1)/zoom, number.digits), 
         cex = number.cex, font = number.font)
  }
  if (type == "full" && plotCI == "n" && !is.null(addgrid.col)) {
    rect(m1 - 0.5, n1 - 0.5, m2 + 0.5, n2 + 0.5, border = addgrid.col)
  }
  if (!is.null(addrect) && order == "hclust" && type == "full") {
    corrRect.hclust(corr, k = addrect, method = hclust.method, 
                    col = rect.col, lwd = rect.lwd)
  }
  invisible(corr)
}
# 3
draw_method_square <- function(coords, values, asp_rescale_factor, fg, bg) {
  symbols(coords, add = TRUE, inches = FALSE,
          squares = asp_rescale_factor * abs(values) ^ 0.5,
          bg = bg, fg = fg)
}

# 4
draw_method_color <- function(coords, fg, bg) {
  symbols(coords, squares = rep(1, nrow(coords)), fg = fg, bg = bg,
          add = TRUE, inches = FALSE)
}

# 5
draw_grid <- function(coords, fg) {
  symbols(coords, add = TRUE, inches = FALSE, fg = fg, bg = NA,
          rectangles = matrix(1, nrow = nrow(coords), ncol = 2))
}

# 6
numero.de.colores.y.etiquetas <- function(var.climatica, estadistico){
  if(var.climatica=='t2'){
    if(estadistico=='NSE'){num.col.label <- c(11, 9)} else(num.col.label <- c(14, 11))
  } else(
    if(estadistico=='NSE'){num.col.label <- c(15, 11)} else(num.col.label <- c(16, 11))
  )
  return(num.col.label)
}

# 7
colores <- function(var.climatica, estadistico, numero.colores){
  if(var.climatica=='t2'){
    if(var.climatica=='t2' & estadistico=='NSE'){
      col.k <- colorRampPalette(c( '#510802', '#a93127', '#b44b42', # rojos: '#510802', '#a93127', '#b44b42'
                                   '#25567b', '#032648', '#032648'))(numero.colores) # azules: '#406b8c', '#25567b', '#032648'
    } else(col.k <- colorRampPalette(c( '#510802', '#a93127', '#b44b42',
                                        '#406b8c', '#25567b', '#032648'))(numero.colores)
    ) } else(
      
      if(var.climatica=='pp' & estadistico=='NSE'){
        col.k <- colorRampPalette(c( '#510802', '#b44b42',
                                     '#25567b', '#032648', '#032648'))(numero.colores)} else(
                                       col.k <- colorRampPalette(c( '#510802', '#a93127', '#b44b42',
                                                                    '#406b8c', '#25567b', '#032648'))(numero.colores)
                                     )
    )
  return(col.k)}

# 8
rangos <- function(var.climatica, estadistico){
  if(var.climatica=='t2'){
    if(estadistico=='NSE'){rango.i <- c(-3, 1)} else(rango.i <- c(-3, 7))} else(
      if(estadistico=='NSE'){rango.i <- c(-4, 1)} else(rango.i <- c(-40, 180))  
    )
  return(rango.i)
}


# fin ---


# 2. Lectura de datos ----
setwd('/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/forzantes_ERA5/MultipleForcing/MultipleForcing/esta_carpeta_resume_las_otras_dos/')

# 2.1 NSE ----
# Nash–Sutcliffe efficiency can range from −∞ to 1. 
# An efficiency of 1 (NSE = 1) corresponds to a perfect match.
# Threshold values to indicate a model of sufficient quality...
# have been suggested between 0.5 < NSE < 0.65. Fuente: https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient

db1.NSE <- read.csv('NSE_precipitationMultipleForcing.csv')
head(db1.NSE)
tail(db1.NSE)

db2.NSE <- read.csv('NSE_T2MultipleForcing.csv')
head(db2.NSE)
dim(db2.NSE)

# 2.2 BIAS ----

db1.BIAS <- read.csv('PCBIAS_precipitationMultipleForcing.csv')
head(db1.BIAS)
tail(db1.BIAS)

db2.BIAS <- read.csv('BIAS_T2MultipleForcing.csv')
head(db2.BIAS)
dim(db2.BIAS)

# fin ---


# 3. DB a matriz ----

# 3.1 Correlacion ----
m1.NSE <- db_to_matriz(db1.NSE, 'NSE') ; m1.NSE
m2.NSE <- db_to_matriz(db2.NSE, 'NSE') ; m2.NSE

# 3.3 BIAS ----
m1.BIAS <- db_to_matriz(db1.BIAS, 'BIAS') ; m1.BIAS
m2.BIAS <- db_to_matriz(db2.BIAS, 'BIAS') ; m2.BIAS

# fin ---

# 4. Plot ----
var.j <- 'pp' # m1 es 'pp', m2 es 't2'
m1 <- m1.NSE
m2 <- m1.BIAS

col.i <- c('#510802', '#a93127', '#b44b42',  '#be655d', '#c97e78', '#d49893', '#dfb2ae', '#e9cbc9', '#f0e5e4', '#f8f3f3',
           '#eff2f5', '#e0ebf0', '#c8d5de', '#adc0ce', '#92abbd', '#7795ad', '#5c809c', '#406b8c', '#25567b', '#032648')

col.labels.NSE <- numero.de.colores.y.etiquetas(var.j, 'NSE')
col.labels.BIAS <- numero.de.colores.y.etiquetas(var.j, 'BIAS')

col.NSE <- colores(var.j, 'NSE', col.labels.NSE[1])
col.BIAS <- colores(var.j, 'BIAS', col.labels.BIAS[1])

n.etiquetas.leyenda.NSE <- col.labels.NSE[2]
n.etiquetas.leyenda.BIAS <- col.labels.BIAS[2]

rango.NSE <- rangos(var.j, 'NSE')
rango.BIAS <- rangos(var.j, 'BIAS')

pos.letra <- 'topleft'
letra <- c('(a)', '(b)')
tipo.letra <- 2 # bold
posicion.letra.eje.y <- 0.8
tamanho.etiquetas <- 0.7
color.etiquetas <- 'black'
tamanho.valores.leyenda <- 0.5
n.etiquetas.leyenda.m1 <- 11
etiquetas.m1 <- c('< -1', '-0.8', '-0.6', '-0.4', '-0.2', '0', '0.2', '0.4', '0.6', '0.8', '1')

setwd('/home/msomos/Documentos/WRF/plots_paper/')

if(var.j=='pp'){
postscript(file = "matriz_precipitacion_ERA5_2.eps", height = 3, width = 10) 
  pos.x.i <- 10.8
  pos.y.i <- 7.3} else( c(
  postscript(file = "matriz_T2_ERA5_2.eps", height = 4, width = 10),
  pos.x.i <- 19.1,
  pos.y.i <- 7.5) )

par(mfrow=c(1,2))

# plot NSE ----
corrplot_modificado(m1, method = 'col', outline="White", col = col.i, tl.cex = tamanho.etiquetas, tl.col = color.etiquetas, 
                    cl.cex = tamanho.valores.leyenda, cl.length = n.etiquetas.leyenda.m1, activar.etiquetas.caracteres=TRUE,
                    nuevas.etiquetas.de.leyenda = etiquetas.m1)
text(pos.x.i, pos.y.i, letra[1], font = 2, cex=0.8)

# plot BIAS ----
corrplot(m2, method = 'col', outline="White", col = col.BIAS, tl.cex = tamanho.etiquetas, tl.col = color.etiquetas, 
         is.corr = FALSE, cl.lim = rango.BIAS, cl.cex = tamanho.valores.leyenda, cl.length = n.etiquetas.leyenda.BIAS)
text(pos.x.i, pos.y.i, letra[2], font = 2, cex=0.8)

dev.off()

# fin ---




# estaciones utilizadas ----

setwd('/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
estaciones <- read.csv('db_todas_las_instituciones_y_las_usadas.csv')
head(estaciones)

n1 <- colnames(db1.BIAS)[-1]
estaciones$usado[estaciones$names.modi.ERA5%in%n1] <- 1
estaciones$PP[estaciones$names.modi.ERA5%in%n1] <- 'X'

n2 <- colnames(db2.BIAS)[-1]
estaciones$usado[estaciones$names.modi.ERA5%in%n2] <- 1
estaciones$T2[estaciones$names.modi.ERA5%in%n2] <- 'X'
estaciones

#write.csv(estaciones, 'db_todas_las_instituciones_y_las_usadas.csv', row.names = FALSE)

# fin ---





# estaciones utilizadas ----

setwd('/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
estaciones <- read.csv('db_todas_las_instituciones.csv', sep = ';')

head(estaciones)
# var.plot: m1 es pp, m2 es rh, m3 es t2, m4 es tmax, m5 es tmin y m6 sp (surface pressure)
n1 <- colnames(db1.CORRE)[-1]
estaciones$usado[estaciones$names_modi%in%n1] <- 1
estaciones$PP[estaciones$names_modi%in%n1] <- 'X'

n2 <- colnames(db2.CORRE)[-1]
estaciones$usado[estaciones$names_modi%in%n2] <- 1
estaciones$RH[estaciones$names_modi%in%n2] <- 'X'

n3 <- colnames(db3.CORRE)[-1]
estaciones$usado[estaciones$names_modi%in%n3] <- 1
estaciones$T2[estaciones$names_modi%in%n3] <- 'X'

n4 <- colnames(db4.CORRE)[-1]
estaciones$usado[estaciones$names_modi%in%n4] <- 1
estaciones$Tmax[estaciones$names_modi%in%n4] <- 'X'

n5 <- colnames(db5.CORRE)[-1]
estaciones$usado[estaciones$names_modi%in%n5] <- 1
estaciones$Tmin[estaciones$names_modi%in%n5] <- 'X'

n6 <- colnames(db6.CORRE)[-1]
estaciones$usado[estaciones$names_modi%in%n6] <- 1
estaciones$SP[estaciones$names_modi%in%n6] <- 'X'

estaciones
#write.csv(estaciones, 'db_todas_las_instituciones_y_las_usadas.csv', row.names = FALSE)

# fin ---





# otros ----
# corrplot_modificado(m2, method = "circle", outline="White", col = col.j, tl.cex = tamanho.etiquetas, 
#                     tl.col = color.etiquetas, is.corr = FALSE, cl.lim = lim2.t2, cl.length = 10, 
#                     tamanho.circulo.fun = 0.45)
