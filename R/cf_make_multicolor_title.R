make.multicolor.title <- function(main, z, pretitle, posttitle, mainminmax_minmax, cex.main=par()$cex.main) {
  if(is.null(main)) {
    title_text <- c(pretitle)
    title_color <- c(1)
    if (mainminmax_minmax) {
      title_text  <- c(title_text, '[','min',      ', ','max',      '] = ')
      title_color <- c(title_color,1,  "#80FFFFFF",1,   "#FF80FFFF",1)
    }
    title_text  <- c(title_text, "[",signif(min(z),3),', ',signif(max(z),3),']',posttitle)
    title_color <- c(title_color,1,  1,               1,   1,               1,  1)
    multicolor.title(title_text,title_color, cex.main=cex.main)
  } else {
    multicolor.title(main, 1, cex.main=cex.main)
  }
}