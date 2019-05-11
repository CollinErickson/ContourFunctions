gg_make_multicolor_title <- function() {
  # https://stackoverflow.com/questions/49735290/ggplot2-color-individual-words-in-title
  t1 <- textGrob(expression("[" * phantom(bold("min")) * "=0," * phantom(bold("max")) * "=1]"),
                 x = 0.5, y = 1.1, gp = gpar(col = "black"))
  
  t2 <- textGrob(expression(phantom("[") * bold("min") * phantom("=0,max=1]")),
                 x = 0.5, y = 1.1, gp = gpar(col = "cyan"))
  
  t3 <- textGrob(expression(phantom("[min=0,") * bold("max") * phantom("=1]")),
                 x = 0.5, y = 1.1, gp = gpar(col = "magenta"))
  annotation_custom(grobTree(t1, t2, t3))
}