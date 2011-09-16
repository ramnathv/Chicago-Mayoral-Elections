# FUNCTION TO PLOT A CHOROPLETH MAP

# HELPER FUNCTIONS
plotChoroplethC <- function(.poly, id1, field,  .df = NA, id2 = id1, 
                            fill = NA, title = "", legtitle = "Values",
                            colpal = 'PuRd', type = 'ggplot', ...){
		
   # load required libraries and utilities
   library(maptools); library(spatial); library(RColorBrewer);
   library(classInt); library(ggplot2); gpclibPermit();
   source('lib/merge_poly_df.R');
   source('lib/sort.colours.R');
   source('lib/find_fill_colors.R');

   if (all(is.na(fill))){
	  .df = .df[, c(unlist(id2), field)];
     map.data   = merge_poly_df(.poly, id1, .df, id2, type);                     
   	map.data   = find_fill_colors(map.data, field, colpal, ...);
   	leg.colors = attr(map.data$fill, 'palette'));
     leg.labels = names(attr(map.data$fill, 'table')));
     facet  = facet_wrap(~ variable); 
   	
   } else {
   	
   	 leg.colors = unique(as.character(map.data$fill));
   	 leg.colors = leg.colours[sort.colours(leg.colors)];
   	 labels     = leg.colors;
   	 facet      = geom_blank();
   	
   }
   
   if (type == 'ggplot'){
      
      # order map.data to maintain polygon order and plot
      map.data  = map.data[order(map.data$order),];     
      p1 = ggplot(map.data, aes(long, lat, group = group)) + 
         	geom_polygon(aes(fill = fill)) +
         	scale_fill_identity(name = legtitle, breaks = breaks, labels = labels) +
         	facet + theme_map() + opts(title = title);
   } else {
     
      
     .at         = attr(map.data$fill, 'at');
     .poly@data  = merge_poly_df(.poly, id1, .df, id2, type);
    
      p1 = spplot(.poly, field, at = .at, col.regions = leg.colors, col = 'transparent',
                  main = title); 
   }
   return(p1);     
	    
}