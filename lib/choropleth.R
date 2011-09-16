choropleth <- function(
	.poly,                  # shape file for plot
	id1,                    # id variables for shape file
	.df      = .poly@data,  # data frame with data to plot
	id2      = id1,         # id variables for data frame 
	field,                  # fields in data frame to plot
 .title    = "",          # title of plot
 .legtitle = "Values",    # title of legend
 .ggplot   = TRUE,        # use ggplot or spplot
  colpal   = 'PuRd',      # colour palette to use
    ...){                 # arguments to pass to classIntervals
                            
     # load required libraries
     library(maptools); library(spatial); library(RColorBrewer);
     library(classInt); library(ggplot2); gpclibPermit();
     
     map.data = merge_poly_df(.poly, id1, .df, id2, .ggplot);
     breaks   = find_fill_breaks(map.data, field, colpal, ...);
     
     # plot choropleth 
     
     if (.ggplot) {
       map.data.m =  melt(map.data, measure.vars = field);
       map.data.m$value_d =  with(map.data.m, cut(value, breaks,incl = T));
       p1 = ggplot(arrange(map.data.m, order), aes(long, lat, group = group)) + 
         geom_polygon(aes(fill = value_d)) +
         scale_fill_brewer(.legtitle, pal = colpal) + 
         facet_wrap(~ variable) +
         theme_map() +
         opts(title = .title);
          
     } else {
       
       colors    = brewer.pal(length(breaks) - 1, colpal);
      .poly@data = map.data;
       p1 = spplot(.poly, field, at = breaks, col.regions = colors, 
	       col = 'transparent', main = .title, aspect = 'fill');
     }
           
     return(p1)
}