## Choropleth Map of Chicago Mayoral Elections 2011

## LOAD LIBRARIES

library(maptools);
library(RColorBrewer);
library(ggplot2);
library(gridExtra);
gpclibPermit();
source('lib/utilities.R')

## LOAD MAP DATA (Source: http://goo.gl/bCUEF)

shpfile = '1_data/maps/Precincts.shp';
poly    = readShapeSpatial(shpfile);

poly@data$id  = with(poly@data, paste(WARD, '-', PRECINCT)); # create id =  ward-precinct
map           = fortify(poly, region = 'id');                # fortify shp to data frame

# LOAD ELECTION DATA (Source: http://goo.gl/HYwdV)
# 

df0     = read.csv('1_data/precincts.csv');
df0$id  = with(df0, paste(ward, '-', precinct));   # create id = ward-precinct
df0     = df0[,-grep('count', names(df0))];        # remove vote counts
# df0     = subset(df0, total_votes > 0)             # remove precincts with zero votes
names(df0) = sub('_pct', '', names(df0));          # remove pct from names


## RECREATE PLOT IN CHICAGO TRIBUNE: http://goo.gl/ve65F
## TODO: fix legend to match that in the article

df = df0

candidates = names(df)[4:9]                                  # find list of candidates
df$winner  = candidates[apply(df[,4:9], 1, which.max)];      # find winner of each election
df$winpct  = apply(df[,4:9], 1, max);                        # find pct of votes secured by winner
df$winint  = cut(df$winpct, c(0, 50, 75, 100), labels = F,   # bin  pct winner votes
                 include.lowest = T);   
df$colpal  = sapply(df$winner, switch,                       # set palette for each winner
                    'braun'    = 'PuRd', 
                    'chico'    = 'Greens',
                    'delvalle' = 'Oranges', 
                    'emanuel'  = 'Purples');
df = ddply(df, .(id), transform,                             # choose fill colors from colpal
        fillcol = brewer.pal(5, colpal)[winint + 2]);     
	
## MERGE MAP WITH DATA AND PLOT

map.data  = merge(map, df, by = 'id');                       # merge map with data
map.data  = map.data[order(map.data$order),];                # maintain polygon order
          
# sort legend labels and create legend key
          
lcolors = unique(as.character(df$fillcol));
lcolors = lcolors[sort.colours(lcolors)]
lcolors = lcolors[c(2,1,3:length(lcolors))];
labs = c('< 50 del Valle', 
         '> 50', 
         '< 50 Chico', 
         '> 50', 
         '> 75', 
         '< 50 Emanuel', 
         '> 50', 
         '> 75', 
         '> 50 Braun');

p0 = ggplot(map.data, aes(long, lat, group = group)) +
     geom_polygon(aes(fill = fillcol)) +
     scale_fill_identity(name = 'Pct Votes for Winner', breaks = lcolors, labels = labs) +
     theme_map() +
     opts(legend.position = c(0.25, 0.35), 
     title = 'Chicago Mayoral Elections 2011');
         

## RECREATE OFFENSIVE POLITICS PLOTS

dfm = melt(map.data, measure.vars = candidates, var = 'candidate')  # melt data by candidate
dfm = dfm[order(dfm$order),];                                       # maintain polygon order

# FUNCTION TO GENERATE PLOT FOR A SPECIFIC CANDIDATE
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

plotEmanuel = plotCandidate('emanuel');
print(plotEmanuel);

candidate_plots = llply(candidates, plotCandidate, .progress = 'text');
saveCandidateplots <- function(candidate){
  p = plotCandidate(candidate);
  f = file.path('3_figures', paste(candidate, '.pdf', sep = ''));
  savePlotAA(p, f);
}
  
l_ply(candidates, saveCandidateplots);
  
  


 
plotEmanuel = plotCandidate('emanuel');
print(plotEmanuel);

## NEXT STEPS
## 1. clean up the code
## 2. create a blog entry and post it
## 3. figure out how to overlay on top of google maps
## 4. 






