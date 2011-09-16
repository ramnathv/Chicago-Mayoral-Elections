# FUNCTION TO PLOT A CHOROPLETH MAP
# TODO. Split the function into pieces
# 1. merge_poly_df: merge shapefile and 
# 2. 

# HELPER FUNCTIONS
merge_poly_df <- function(.poly, id1, .df, id2){
	
	 require(ggplot2);
	
	 # create id to match shapefile with data
	 id1 = llply(id1, as.name); 
	 id2 = llply(id2, as.name);
	.poly@data$id = with(.poly@data, do.call('paste', c(id1, sep = "-")));
	.df$id        = with(.df,        do.call('paste', c(id2, sep = "-")));
	 
	 # fortify shape file and merge with data
	   map          = fortify(.poly, region = 'id');       
     map.data     = merge(map, .df, by = 'id');
     map.data     = map.data[order(map.data$order),];
	   return(map.data);
}


find_fill_colors <- function(map.data, field, colpal, ...){
	
	   map.data  = melt(map.data, measure.vars = field);                      
     map.data  = map.data[order(map.data$order),];
     x = list(...); x$var = map.data$value;
 	  intervals = do.call("classIntervals", x);
     colors    = brewer.pal(length(intervals$brks) - 1, colpal);
     map.data$fill = findColours(intervals, colors, digits = 4);
     
     return(map.data);
}

## FUNCTION TO SORT COLORS
#  Source: http://goo.gl/3lg2

sort.colours <- function(col) {
  require(colorspace)
  c.rgb = col2rgb(col)
  c.RGB = RGB(t(c.rgb) %*% diag(rep(1/255, 3)))
  c.HSV = as(c.RGB, "HSV")@coords
  order(c.HSV[, 1], c.HSV[, 2], c.HSV[, 3])
}

plotChoroplethB <- function(.poly, id1, field, 
   .df = .poly@data, id2 = id1, fill = NA, title = "", legtitle = "Values",
   colpal = 'PuRd', ...){
		
   # load required libraries
   library(maptools); library(spatial); library(RColorBrewer);
   library(classInt); library(ggplot2); gpclibPermit();
	
   # extract relevant columns of .df if fill is unspecified
   if(all(is.na(fill))) {.df = .df[, c(unlist(id2), field)]};	
   
   map.data = merge_poly_df(.poly, id1, .df, id2);

   if (all(is.na(fill))){
   	
   	 map.data = find_fill_colors(map.data, field, colpal, ...);
   	 breaks = with(map.data, attr(fill, 'palette'));
      labels = names(with(map.data, attr(fill, 'table')));
      facet  = facet_wrap(~ variable); 
   	
   } else {
   	
   	 breaks = unique(as.character(map.data$fill));
   	 breaks = breaks[sort.colours(colors)];
   	 labels = breaks;
   	 facet  = geom_blank();
   	
   }

   p1 = ggplot(map.data, aes(long, lat, group = group)) + 
         	geom_polygon(aes(fill = fill)) +
         	scale_fill_identity(name = legtitle, breaks = breaks, 
	           labels = labels) +
         	facet +
            theme_map() +
         	opts(title = title)
   return(p1);     
	    
}