## PLAIN THEME FOR MAP
## Source: ggplot mailing list

theme_map = function(size = 12) { 
	
    o = list(## AXIS
			 axis.line         = theme_blank(), 
             axis.text.x       = theme_blank(), 
             axis.text.y       = theme_blank(), 
             axis.ticks        = theme_blank(), 
             axis.ticks.length = unit(0.3, "lines"),
             axis.ticks.margin = unit(0.5, "lines"), 
             axis.title.x      = theme_blank(), 
             axis.title.y      = theme_blank(), 

			 ## LEGEND
             legend.background = theme_rect(fill="white", colour=NA), 
             legend.key        = theme_rect(colour="white"), 
             legend.key.size   = unit(1.2, "lines"), 
             legend.position   = "right", 
             legend.text       = theme_text(size=size*0.8), 
             legend.title      = theme_text(size=size*0.8, face="bold", hjust=0), 

			 ## PANEL
             panel.background  = theme_blank(), 
             panel.border      = theme_blank(), 
             panel.grid.major  = theme_blank(), 
             panel.grid.minor  = theme_blank(), 
             panel.margin      = unit(0, "lines"), 

			 ## PLOT
             plot.background   = theme_blank(), 
			 plot.margin       = unit(c(1, 1, 0.5, 0.5), "lines"), 
             plot.title        = theme_text(size = size*1.2), 
             strip.background  = theme_rect(fill = "grey90", colour = "grey50"), 
             strip.text.x      = theme_text(size = size*0.8), 
             strip.text.y      = theme_text(size = size*0.8, angle = -90)
			)
			
			return(structure(o, class="options")) 
}

## FUNCTION TO SORT COLORS
## Source: http://goo.gl/3lg2

sort.colours <- function(col) {
  require(colorspace)
  c.rgb = col2rgb(col)
  c.RGB = RGB(t(c.rgb) %*% diag(rep(1/255, 3)))
  c.HSV = as(c.RGB, "HSV")@coords
  order(c.HSV[, 1], c.HSV[, 2], c.HSV[, 3])
}

## FUNCTION TO CAPITALIZE FIRST LETTER OF EVERY WORD
## Source: Examples for toupper, tolower

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)),
                  {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


## FUNCTION TO ADD FOOTNOTE TO A PLOT
## Author: Mark Heckmann (http://goo.gl/2H69K)

makeFootnote <- function(footnoteText =
                         format(Sys.time(), "%d %b %Y"),
                         size = .7, color= grey(.5))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(0,"npc") + unit(2, "mm"),
             y= unit(2, "mm"),
             just=c("left", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}

# function to save plot as pdf using the cairo drivers

savePlotAA <- function(p, filename = 'Rplots', width = 7, height = 7, source = NULL){
    require(Cairo)
    CairoPDF(file = filename, width = width, height = height)
    print(p);
    makeFootnote(source);
    dev.off()
}