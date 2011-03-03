## Choropleth Map of Chicago Mayoral Elections 2011

## LOAD LIBRARIES

library(maptools);
library(RColorBrewer);
library(ggplot2);
gpclibPermit();

## LOAD MAP DATA (Source: http://goo.gl/bCUEF)

shpfile = '1_data/maps/Precincts.shp';
poly    = readShapeSpatial(shpfile);

poly@data$id1 = with(poly@data, 
                     paste(WARD, '-', PRECINCT));  # create id based on ward-precinct combo
map           = fortify(poly, region = 'id1');     # fortify spatial polygon to dataframe

# LOAD ELECTION DATA (Source: http://goo.gl/HYwdV)

df0     = read.csv('1_data/precincts.csv');
df0$id2 = with(df0, paste(ward, '-', precinct));   # create id based on ward-precinct combo
df0     = df0[,-grep('count', names(df0))];        # remove counts
df0 = subset(df0, total_votes > 0)                 # remove precincts with zero total votes


## RECREATE PLOT IN CHICAGO TRIBUNE: http://goo.gl/ve65F
## TODO: fix legend to match that in the article

df = df0

candidates = sub('_pct', '', names(df)[4:9])            # find list of candidates
df$winner  = candidates[apply(df[,4:9], 1, which.max)]; # find winner of each election
df$winpct  = apply(df[,4:9], 1, max);                   # find pct of votes secured by winner
df$winint  = cut(df$winpct, c(0, 50, 75, 100));         # bin pct votes into intervals

# set winint to numeric values 3, 4 and 5 to use with brewer.pal
df$win_paln = df$winint
levels(df$win_paln) = 3:5
df$win_paln = as.numeric(as.character(df$win_paln))

# create color palette for each winner

df$colpal = as.factor(df$winner);
levels(df$colpal) = c('braun'    = 'PuRd',      'chico' = 'Greens', 
                      'delvalle' = 'Oranges', 'emanuel' = 'Purples');
df$colpal = as.character(df$colpal);

# choose fillcolor for each ward-precinct based on winner and winpct

choose_fill = function(win_paln, colpal){brewer.pal(5, colpal)[win_paln]};
df$fillcol  = mdply(df[,14:15], choose_fill)$V1;

## add winner and winner percentage to the data
## fill color based on winner and winner percentage

  	
map.data  = merge(map, df, by.x = 'id', by.y = 'id2');
map.data  = map.data[order(map.data$order),];

source('~/Desktop/R Projects/Code Snippets/ggplot_themes.R');
   
p0 = ggplot(map.data, aes(long, lat, group = group)) +
     geom_polygon(aes(fill = fillcol)) +
     scale_fill_identity(name = 'Pct Votes for Winner', breaks = f1) +
     theme_map() +
     opts(legend.position = c(0.25, 0.35), 
          title = 'Chicago Mayoral Elections 2011');


print(p0);

## function to sort colors sourced from http://goo.gl/3lg2
sort.colours <- function(col) {
  require(colorspace)
  c.rgb = col2rgb(col)
  c.RGB = RGB(t(c.rgb) %*% diag(rep(1/255, 3)))
  c.HSV = as(c.RGB, "HSV")@coords
  order(c.HSV[, 1], c.HSV[, 2], c.HSV[, 3])
}

labs = c('< 50 del Valle', 
         '> 50', 
         '< 50 Chico', 
         '50 - 75', 
         '> 75', 
         '< 50 Emanuel', 
         '50 - 75', 
         '> 75', 
         '> 50 Braun')
## RECREATE OFFENSIVE POLITICS PLOTS USING FACET

# merge fortified map with data on elections
map.data2 = merge(map, df0, by.x = 'id', by.y = 'id2');

# melt the data and clean up the variable names
dfm = melt(map.data2, measure.vars = grep('pct', names(map.data2)))
names(dfm)[names(dfm) == 'variable'] = 'candidate';
names(dfm)[names(dfm) == 'value']    = 'vote_pct';
dfm$candidate = with(dfm, sub('_pct', '', candidate));

# order the dataframe so that ggplot can handle it correctly
dfm = dfm[order(dfm$order),];


# create a function that generates a plot for a specified candidate
plotCandidate <- function(cand){
	
	df = subset(dfm, candidate == cand);
	df = df[order(df$order),]
	p0 = ggplot(df, aes(long, lat, group = group)) +
	     geom_polygon(aes(fill = cut(vote_pct, seq(0, 100, by = 20)))) +
	     scale_fill_brewer(name = paste(cand, '(% Votes)'),pal = 'Blues') +
	     opts(title = 'Chicago Mayoral Race') +
	     theme_map();
	return(p0)
	
}

## NEXT STEPS
## 1. clean up the code
## 2. create a blog entry and post it
## 3. figure out how to overlay on top of google maps
## 4. 






