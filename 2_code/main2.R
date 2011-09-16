## CHOROPLETH MAP OF CHICAGO MAYORAL ELECTIONS 2011
#  Recently, the Chicago Tribune (Source: http://goo.gl/ve65F) presented a nice 
#  infographic that visualizes the percentage of votes secured by the winner in each
#  precinct. This is an attempt to recreate this infographic using R and ggplot2.


## LOAD LIBRARIES AND CUSTOM FUNCTIONS

library(maptools);
library(RColorBrewer);
library(ggplot2);
library(gridExtra);
gpclibPermit();
source('lib/utilities.R')

## STEP 0: Load Data

# load map data (Source: http://goo.gl/bCUEF)
shpfile = '1_data/maps/Precincts.shp';
poly    = readShapeSpatial(shpfile);          

# load election results data (Source: http://goo.gl/HYwdV)
df        = read.csv('1_data/precincts.csv');

## STEP 2: Process Data 


# remove vote counts and 'pct' from names
df        = df[,-grep('count', names(df))];         
names(df) = sub('_pct', '', names(df));

# find winner and their pct of votes secured for each precinct.
candidates = names(df)[4:9];                                
df$winner  = candidates[apply(df[, candidates], 1, which.max)];    
df$winpct  = apply(df[, candidates], 1, max);



## STEP 4: Set Plot Settings

# assign each winner to a color palette
colpal  = sapply(map.data$winner, switch, 'braun' = 'PuRd', 'chico' = 'Greens',
                    'delvalle' = 'Oranges', 'emanuel'  = 'Purples');

# create discrete bins for pct of votes secured by winner
bin    = cut(map.data$winpct, c(0, 50, 75, 100), labels = F, include.lowest = T);  

# choose color based on palette and bin for pct of votes
fillcol = adply(data.frame(colpal, bins), 1, summarize, 
                fillcol = brewer.pal(5, colpal)[bin + 2]);     
	
 

## STEP 4: Create the plot!!!           

# identify legend colors and sort them by their rgb values
leg.colors = unique(as.character(df$fillcol));
leg.colors = leg.colors[sort.colours(leg.colors)]

# 
leg.colors = leg.colors[c(2,1,3:length(leg.colors))];

#  (b) assigning meaningful labels to each color
          


leg.labels = c('< 50 del Valle', '> 50', '< 50 Chico', '> 50', '> 75', 
               '< 50 Emanuel', '> 50', '> 75', '> 50 Braun');



p0 = ggplot(map.data, aes(long, lat, group = group)) +
     geom_polygon(aes(fill = fillcol)) +
     scale_fill_identity(name   = 'Pct Votes for Winner', 
						 breaks = leg.colors, 
                         labels = leg.labels) +
     theme_map() +
     opts(legend.position = c(0.25, 0.35), 
          title = 'Chicago Mayoral Elections 2011');



plotChoropleth <- function(.poly, id1, value, .df = .poly@data, id2 = id1,  
	                       fill = NA, colpal = 'PuRd', ...){
		
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
		
	    intvl    = classIntervals(var = map.data[, value], ...);
	    colors   = brewer.pal(length(intvl$brks) - 1, colpal);
		fill     = findColours(intvl, colors, digits = 4);
		legkey   = names(attr(fill, "table"));
		map.data$fill = fill;
		p1 = ggplot(map.data, aes(long, lat, group = group)) + 
		     geom_polygon(aes(fill = fill)) +
		     scale_fill_identity(breaks = colors, labels = legkey) +
		     theme_map();
	}
	
	return(p1)
	    
}

plotChoropleth5 <- function(.poly, id1, field, .df = .poly@data, id2 = id1,  
	                       fill = NA, colpal = 'PuRd', ...){
		
	 # load required libraries

     library(maptools); library(spatial); library(RColorBrewer);
     library(classInt); library(ggplot2); gpclibPermit();
	
	  .df = .df[, c(unlist(id2), field)]	
   	 # create id to match shapefile with data

	   id1 = llply(id1, as.name); id2 = llply(id2, as.name);
    .poly@data$id  = with(.poly@data, do.call('paste', c(id1, sep = "-")));
    .df$id         = with(.df, do.call('paste', c(id2, sep = "-"))); 
	 
	
	 # fortify shape file to data frame and merge with data
		
     map       = fortify(.poly, region = 'id');       
     map.data  = merge(map, .df, by = 'id');
     map.data  = melt(map.data, measure.vars = field);                      
	   map.data  = map.data[order(map.data$order),];
	
	if (!is.na(fill)){
		
		p1 = ggplot(map.data, aes(long, lat, group = group)) +  
		     geom_polygon(aes_string(fill = fill)) + 
		     scale_fill_identity() +
		     theme_map();
		
	} else {
		
	    intvl    = classIntervals(var = map.data$value, ...);
	    colors   = brewer.pal(length(intvl$brks) - 1, colpal);
		  fill     = findColours(intvl, colors, digits = 4);
		  legkey   = names(attr(fill, "table"));
		  map.data$fill = fill;
		  p1 = ggplot(map.data, aes(long, lat, group = group)) + 
		       geom_polygon(aes(fill = fill)) +
		       scale_fill_identity(breaks = colors, labels = legkey) +
		       facet_wrap(~ variable) +
		       theme_map();
	}
	
	return(p1)
	    
}



plotChoropleth(poly, id1 = list('WARD', 'PRECINCT'), value = 'emanuel',
               df,   id2 = list('ward', 'precinct'))
