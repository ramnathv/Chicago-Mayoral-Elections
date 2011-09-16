## LOAD LIBRARIES

library(maptools); gpclibPermit();
library(ggplot2); library(RColorBrewer);
# library(gdata);

## PLAIN THEME FOR MAP
## Source: ggplot mailing list

theme_map = function(size = 12) {o = list(
    ## AXIS
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
    panel.border      = theme_rect(fill = NA, colour = "grey50"), 
    panel.grid.major  = theme_line(colour = 'grey90', size = 0.2),
    panel.grid.minor  = theme_line(colour = 'grey98', size = 0.5), 
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

# FUNCTION TO PLOT A CHOROPLETH MAP
plotChoropleth <- function(.poly, id1, field, 
   .df = .poly@data, id2 = id1, fill = NA, title = "", legtitle = "Values",
   colpal = 'PuRd', fixed = F, ...){
		
	 # load required libraries
   library(maptools); library(spatial); library(RColorBrewer);
   library(classInt); library(ggplot2); gpclibPermit();
	
	 # extract relevant columns of .df if fill is unspecified
   if(all(is.na(fill))) {.df = .df[, c(unlist(id2), field)]};	
   
   # create id to match shapefile with data
   id1 = llply(id1, as.name); id2 = llply(id2, as.name);
  .poly@data$id  = with(.poly@data, do.call('paste', c(id1, sep = "-")));
  .df$id         = with(.df,        do.call('paste', c(id2, sep = "-"))); 
	 
	 # fortify shape file to data frame and merge with data
   map       = fortify(.poly, region = 'id');       
   map.data  = merge(map, .df, by = 'id');
   map.data  = map.data[order(map.data$order),];
	
	 if (!is.na(fill)){
     
    # if fill color is specified, use it for the plot 
		p1 = ggplot(map.data, aes(long, lat, group = group)) +  
		     geom_polygon(aes_string(fill = fill)) + 
		     scale_fill_identity(name = legtitle) +
		     theme_map();
		
 	} else {
     
   # if fill color is not specified, melt the data frame
	 map.data = melt(map.data, measure.vars = field);                      
   map.data = map.data[order(map.data$order),];
    
   # figure out binning intervals and fill colors
   if (fixed == T){
       # treatment of fixed breaks separately is necessitated by
       # a possible bug in classIntervals
       intvl  = cut(x = map.data$value, ...);
       colors = brewer.pal(length(levels(intvl)), colpal);
       fill   = colors[intvl];
       legkey = levels(intvl);
         
    } else {
      
       intvl   = classIntervals(var = map.data$value, ...);
       colors  = brewer.pal(length(intvl$brks) - 1, colpal);
       fill    = findColours(intvl, colors, digits = 4);
       legkey  = names(attr(fill, "table"));
    }
		map.data$fill = fill;
		p1 = ggplot(map.data, aes(long, lat, group = group)) + 
		     geom_polygon(aes(fill = fill)) +
		     scale_fill_identity(name = legtitle, breaks = colors, labels = legkey) +
		     facet_wrap(~ variable) +
		     theme_map();
	}
	
	return(p1 + opts(title = title));
	    
}


## FUNCTION TO ADD FOOTNOTE TO A PLOT
## Author: Mark Heckmann (http://goo.gl/2H69K)

makeFootnote <- function(
  footnoteText = format(Sys.time(), "%d %b %Y"),
  size         = 0.7, 
  color        = grey(.5))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
     x    = unit(0,"npc") + unit(2, "mm"),
     y    = unit(2, "mm"),
     just = c("left", "bottom"),
     gp   = gpar(cex= size, col=color)
   )
   popViewport()
}

# FUNCTION TO SAVE PLOT USING CAIRO DRIVERS

savePlotAA <- function(
  .plot, 
  filename = 'Rplots',
  width    =  7, 
  height   = 7, 
  source   = NULL){
	
    require(Cairo)
    CairoPDF(file = filename, width = width, height = height)
    print(.plot)
    makeFootnote(source)
    dev.off()
}

savePDF <- function(candidate){
  p = plotCandidate(candidate);
  f = file.path('3_figures', paste(candidate, '.pdf', sep = ''));
  savePlotAA(p, f);
}