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

## STEP 1: Clean up and Process Data

# remove vote counts and 'pct' from names
df        = df[,-grep('count', names(df))];         
names(df) = sub('_pct', '', names(df));             


# find winner and their pct of votes secured for each precinct
candidates = names(df)[4:9];                                
df$winner  = candidates[apply(df[, candidates], 1, which.max)];    
df$winpct  = apply(df[, candidates], 1, max);                     
 
# assign a color palette to each winner
df$colpal  = sapply(df$winner, switch, 'braun' = 'PuRd', 'chico' = 'Greens',
                    'delvalle' = 'Oranges', 'emanuel'  = 'Purples');

# bin the pct of votes secured by winner into discrete intervals
df$winint  = cut(df$winpct, c(0, 50, 75, 100), labels = F, include.lowest = T);  

# choose color for each precinct based on palette and bin
df = adply(df, 1, transform, fillcol = brewer.pal(5, colpal)[winint + 2]);     
	
## STEP 2: Merge shapefile with election results data by

# create id = ward-precinct to match the shapefile with data
poly@data$id  = with(poly@data, paste(WARD, '-', PRECINCT));
df$id         = with(df, paste(ward, '-', precinct)); 

# fortify shape file to data frame and merge with data
map       = fortify(poly, region = 'id');        
map.data  = merge(map, df, by = 'id');                      
map.data  = map.data[order(map.data$order),];    

## STEP 3a: Create the plot in Chicago Tribute!!!           

# identify legend colors and sort them by their rgb values
leg.colors = unique(as.character(df$fillcol));
leg.colors = leg.colors[sort.colours(leg.colors)];
leg.colors = leg.colors[c(2,1,3:length(leg.colors))];

# assign meaningful labels to each color
leg.labels = c('< 50 del Valle', '> 50', '< 50 Chico', '> 50', '> 75', 
               '< 50 Emanuel', '> 50', '> 75', '> 50 Braun');


# draw the plot
p0 = ggplot(map.data, aes(long, lat, group = group)) +
     geom_polygon(aes(fill = fillcol)) +
     scale_fill_identity(name   = 'Pct Votes for Winner', 
						 breaks = leg.colors, 
                         labels = leg.labels) +
     theme_map() +
     opts(legend.position = c(0.25, 0.35), 
          title = 'Chicago Mayoral Elections 2011');

# save it as png
ggsave('3_figures/winner5.png', p0, width = 7, height = 7);
         

## STEP 3b. Create the plots in the offensive Politics Blog

# function to creaet a plot by candidate
plotChoro <- function(mapdata, cand, colPal = 'PuRd'){
	
	# bin pct of votes secured by candidate into discrete intervals
	mapdata$bin = cut(mapdata[,cand], seq(0, 100, by = 20), include.lowest = T);
	
	p0 = ggplot(mapdata, aes(long, lat, group = group)) +
	     geom_polygon(aes(fill = bin)) +
	     scale_fill_brewer(name = paste(capwords(cand), '(% Votes)'), pal = colPal) +
	     theme_map() + 
         opts(legend.position = c(0.25, 0.35), 
              title = 'Chicago Mayoral Elections 2011');
	return(p0)
	
}

savePNG <- function(candidate){
  p = plotCandidate(candidate);
  f = file.path('3_figures', paste(candidate, '.png', sep = ''));
  ggsave(f, p, width = 7, height = 7)
}
  
l_ply(candidates, savePNG);





