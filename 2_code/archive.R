# FUNCTION TO GENERATE PLOT FOR A SPECIFIC CANDIDATE

dfm = melt(map.data, measure.vars = candidates, var = 'candidate')  # melt data by candidate
dfm = dfm[order(dfm$order),];                                       # maintain polygon order

plotCandidate <- function(cand, colPal = 'Blues'){
	
	df = subset(dfm, candidate == cand);
	df = df[order(df$order),]
	p0 = ggplot(df, aes(long, lat, group = group)) +
	     geom_polygon(aes(fill = cut(value, seq(0, 100, by = 20), include.lowest = T))) +
	     scale_fill_brewer(name = paste(capwords(cand), '(% Votes)'),pal = colPal) +
	     theme_map() + 
       opts(legend.position = c(0.25, 0.35), 
       title = 'Chicago Mayoral Elections 2011');
	return(p0)
	
}

# plotEmanuel = plotCandidate('emanuel');
# print(plotEmanuel);

candidate_plots = llply(candidates, plotCandidate, .progress = 'text');

plotChoropleth2 <- function(.poly, id1, value, .df = NA, id2 = id1,  
	                       fill = NA, colpal = 'PuRd', breaks = 5){
		
	if (length(breaks) == 1){n = breaks} else {n = length(breaks) - 1};						
	if(is.na(fill)){
		         
		         .df$intvl = cut(.df[,value], breaks, labels = F, include.lowest = T);
		         .bins     = levels(.df$intvl);
		         .colors   = brewer.pal(n, breaks)
				 .df$fillcol  = colors[.df$intvl];
	} else {
		
		.df$fillcol = .df[, fill];
	}

	# create id to match the shapefile with data
	 id1 = llply(id1, as.name); id2 = llply(id2, as.name)
	.poly@data$id  = with(.poly@data, do.call('paste', c(id1, sep = "-")));
	.df$id        = with(.df, do.call('paste', c(id2, sep = "-"))); 
	
	# fortify shape file to data frame and merge with data
	map       = fortify(.poly, region = 'id');        
	map.data  = merge(map, .df, by = 'id');                      
	map.data  = map.data[order(map.data$order),];
	
	p0 = ggplot(map.data, aes(long, lat, group = group)) +
	     geom_polygon(aes(fill = fillcol)) +
	     scale_fill_identity(breaks = .colors, labels = .bins) +
	     theme_map();

}

plotChoropleth3 <- function(.poly, id1, value, .df = NA, id2 = id1,  
	                        fill = NA, colpal = 'PuRd', breaks = 5){
		
	# create id to match the shapefile with data
	 id1 = llply(id1, as.name); id2 = llply(id2, as.name)
	.poly@data$id  = with(.poly@data, do.call('paste', c(id1, sep = "-")));
	.df$id        = with(.df, do.call('paste', c(id2, sep = "-"))); 
	
	# fortify shape file to data frame and merge with data
	map       = fortify(.poly, region = 'id');        
	map.data  = merge(map, .df, by = 'id');                      
	map.data  = map.data[order(map.data$order),];
	
	if (!is.na(fill)){
		
		p1 = ggplot(map.data, aes(long, lat, group = group)) +  
		     geom_polygon(aes_string(fill = fill)) + 
		     scale_fill_identity();
	} else {
		
		map.data$bin = cut(map.data[, value], breaks, include.lowest = T)
		p1 = ggplot(map.data, aes(long, lat, group = group)) + 
		     geom_polygon(aes(fill = cut(value, breaks))) +
		     scale_fill_brewer(pal = colpal) +
		     theme_map();
	}
	    
}


plotChoropleth4 <- function(.poly, id1, value, .df = .poly@data, id2 = id1,  
	                       fill = NA, colpal = 'PuRd', breaks = 5){
		
   	 # create id to match shapefile with data

	 id1 = llply(id1, as.name); id2 = llply(id2, as.name);
    .poly@data$id  = with(.poly@data, do.call('paste', c(id1, sep = "-")));
    .df$id         = with(.df, do.call('paste', c(id2, sep = "-"))); 
	 
	
	 # fortify shape file to data frame and merge with data
		
     map       = fortify(.poly, region = 'id');       
     map.data  = merge(map, .df, by = 'id');                      
	 map.data  = map.data[order(map.data$order),];
	
	if (!is.na(fill)){
		
		p1 = ggplot(map.data, aes(long, lat, group = group)) +  
		     geom_polygon(aes_string(fill = fill)) + 
		     scale_fill_identity() +
		     theme_map();
	} else {
		
		
		map.data$bin = cut(map.data[, value], breaks, include.lowest = T)
		p1 = ggplot(map.data, aes(long, lat, group = group)) + 
		     geom_polygon(aes(fill = bin)) +
		     scale_fill_brewer(pal = colpal) +
		     theme_map();
	}
	
	return(p1)
	    
}
