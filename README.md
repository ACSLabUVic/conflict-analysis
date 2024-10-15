# Grizzly bears detected at ecotourism sites are less likely than predicted by chance to encounter conflict

Data and code reproduce results described in 'Grizzly bears detected at ecotourism sites are less likely than predicted by chance to encounter conflict'

## Description of the data and code. 

CJZ_Analysis and figures.R: 
summarizes individiuals by where they were detected (ecotour area, upstream area, or downstream area via conflict), and reproduces analyses described in the manuscript.

individuals.csv: 
these data are called in to the above script, and link samples to individuals, as necessary for species accumulation curve. For the purpose of this analysis, these data are subset from the raw data in a master database of a larger project. 

cummulative_network_distance.csv:
cummulative river network distance (i.e., distance along the river). River segements were measured using the Measure tool in ArcGIS Pro 3.3.

furthest_sites_per_individual.csv:
Lists the two furthests sites at which each recaptured individual was detected. 

dist_from_at25_to_hags.csv:
segments of river measured from the site at25 (ecotour site) to Hagensborg (conflict site) using the Measure tool in ArcGIS Pro 3.3.


## Sharing/Access information

Grizzly Bear Population Unit bear density (bears per area) was derived using the net GBPU area, as done by the Provinice of British Columbia. The gross and net areas are listed in the attributes of the spatial features ‘BC_GBPU_2015_2018_w_Conservation_Rank,’ which is within the ‘Inputs Geodatabase’: https://catalogue.data.gov.bc.ca/dataset/grizzly-bear-data-bc-cumulative-effects-framework-2019-assessment. The ‘net’ area of GBPU's are described as "area of useable habitat" by the province, and excludes area of BC Baseline Thematic Mapping layer of water and Ice ('Fresh Water', 'Salt Water','Glaciers and Snow'). See GBPU polygons under Population Density Estimate tab here: https://www.env.gov.bc.ca/soe/indicators/plants-and-animals/grizzly-bears.html
Raw data generated during this study are not publicly available due to research agreements with the British Columbia Ministry of Forests, Lands and Natural Resource Operations Fish and Wildlife Branch, as well as Nuxalk Nation. 

## Code/Software

Analyses were conducted in R.
R Core Team (2022). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria.
URL https://www.R-project.org/. Version 4.3.2
