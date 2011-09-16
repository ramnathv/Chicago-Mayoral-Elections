find_fill_colors <- function(map.data, field, colpal, ...){
   
	   require(classInt);
     
     # melt the data according to field
      map.data  = melt(map.data, measure.vars = field);
     
     # hack required to make classIntervals work with ... arguments
     x = list(...); x$var = map.data$value;
     
     # figure out intervals based on specified method
 	  intervals      = do.call("classIntervals", x);
     
     # create palette based on number of breaks
     colors         = brewer.pal(length(intervals$brks) - 1, colpal);
     
     # find fill colors based on intervals and palette
     map.data$fill  = findColours(intervals, colors, digits = 4);
     
     # add the breaks to the fill column to use with spplot
     attr(map.data$fill, 'at') = intervals$brks;
     
     return(map.data);
}