
##################
# Grizzly bears detected at ecotourism sites are less likely than predicted by chance to encounter conflict
# Reproduce analysis and figures
##################

setwd("")
data <- read.csv('individuals.csv') 

library(ggplot2)
library(vegan)
library(dplyr)
library(VennDiagram)
library(grid)
library(gridExtra)
citation("ggplot2")
citation("vegan")
citation("dplyr")
citation("VennDiagram")
citation("grid")
citation("gridExtra")

######## Cumulative distribution function and Bayes' theorem 

# How many grizzly bears were detected at least once in the ecotour zone? 
length(unique(data$individual[data$eco.or.upstream=="Ecotour"&data$project=="nt"]))# 34
# Sex
length(unique(data$individual[data$eco.or.upstream=="Ecotour"&data$project=="nt"&data$sex=="female"])) # 24 female
length(unique(data$individual[data$eco.or.upstream=="Ecotour"&data$project=="nt"&data$sex=="male"])) # 10 male
# How many grizzly bears were detected at least once in the upstream zone? 
length(unique(data$individual[data$eco.or.upstream=="Upstream"&data$project=="nt"]))# 97
# How many grizzly bears encountered conflict?
length(unique(data$individual[data$project=="conflict"])) # 30
# Sex of conflict bears
length(unique(data$individual[data$project=="conflict"&data$sex=="male"])) # 11 male 
length(unique(data$individual[data$project=="conflict"&data$sex=="female"])) # 19 female

# Subset individuals detected more than once
filtered <- data %>%
  group_by(individual) %>%
  filter(n_distinct(revisitid)> 1)  # Keeps individuals detected more than once
filtered <- filtered %>%
  group_by(individual) %>%
  summarize(detection_count = n_distinct(revisitid)) %>%
  ungroup()

# Figure S1
ggplot(filtered, aes(x = detection_count)) +
  geom_histogram(binwidth = 1, colour = "grey") +
  labs(title = "",
       x = "Detection frequency",
       y = "Number of individuals") +
  theme_classic() +
  theme_classic(base_size = 14)   # Increase base text size by 1 point

# Which individuals were detected in both eco and upstream?
target_values1 <- c("Ecotour", "Upstream")
result1 <- data %>%
  filter(eco.or.upstream %in% target_values1) %>%
  group_by(individual, sex) %>%
  filter(n_distinct(eco.or.upstream) > 1) %>%
  distinct(individual)

# Which individuals were detected in upstream only?
target_values2 <- "Upstream"
result2 <- data %>%
  group_by(individual, sex) %>%
  summarise(has_target_values2 = all(eco.or.upstream == target_values2)) %>%
  filter(has_target_values2) %>%
  dplyr::select(individual, sex)

# Were there any matches between conflict and hair snag bears?
# Filter data for individuals with conflict_sample = "yes"
conflict_individuals <- data[data$conflict_sample == "yes", "individual"]
# Filter data for individuals with non-NA eco.or.upstream
hair_snag_individuals <- data[!is.na(data$eco.or.upstream), "individual"]
# Find common individuals in both subsets
common_individuals <- intersect(conflict_individuals, hair_snag_individuals)

# There was one genetic match between bears we detected at our hair snags and bears who encountered conflict 
# 49877

# Bayes'

# Pr(ecotour bear| conflict bear) = Pr(conflict bear| ecotour bear)Pr(ecotour bear) / Pr(conflict bear)
# Pr(conflict bear| ecotour bear) = 1/34
# Pr(ecotour bear) = #ecotour bears / total # bears
# Pr(conflict bear) = # conflict bears / total # bears

# This simplifies
# Pr(ecotour bear| conflict bear) = 1/34 * (#ecotour bears / total # bears) / (# conflict bears / total # bears)
# Pr(ecotour bear| conflict bear) = 1/34 * (#ecotour bears/#conflict bears) = 1/34 * #ecotour bears/30 = #ecotour bears/1020.

((1/34) * (34/50))/(30/50)
(1/34) * (34/30)
34/1020

par(mfrow=c(1,2))
plot(34:50,(34:50)/1020,type="l") # probability that a bear is an ecotour bear given that it engaged in conflict (x axis varies over the unknown true number of ecotour bears).
plot(34:50,30*(34:50)/1020,type="l") # expected number of the 30 conflict bears that are ecotour bears.

# Data for the first plot
data1 <- data.frame(
  X = 34:50,
  Y = (34:50)/1020
)

# Data for the second plot
data2 <- data.frame(
  X = 34:50,
  Y = 30*(34:50)/1020
)

plot1 <- ggplot(data1, aes(x = X, y = Y)) +
  geom_line() +
  labs(x = "Number of ecotour bears", y = "Pr(ecotour bear | conflict bear)") +
  theme_classic() +
  annotate("text", x = 34, y = 0.05, label = "A", size = 4.5, color = "black", fontface = "bold")

plot2 <- ggplot(data2, aes(x = X, y = Y)) +
  geom_line() +
  labs(x = "Number of ecotour bears", y = "Estimated number of conflict bears") +
  theme_classic() +
  annotate("text", x = 34, y = 1.5, label = "B", size = 4.5, color = "black", fontface = "bold") +
  geom_segment(aes(x = 50, y = 1, xend = 50, yend = 1.47), linetype = 2, color = "red") +  # Vertical red line
  geom_segment(aes(x = 34, y = 1.47, xend = 50, yend = 1.47), linetype = 2, color = "red") +  # Horizontal red line
  scale_y_continuous(breaks = c(1.0,1.1,1.2,1.3,1.4,1.47,1.5), labels = c("1.0","1.1","1.2","1.3","1.4","1.47","1.5"), limits = c(1.0, 1.5))+
  scale_x_continuous(breaks = c(35,40,45,50), labels = c("35","40","45","50"), limits = c(34, 50))

# Figure 3
# Arranging plots side by side
gridExtra::grid.arrange(plot1, plot2, ncol = 2)
combined_plots <- grid.arrange(plot1, plot2, ncol = 2)

# Probability approach 
# Carry 1.47 to 2 (see Bayes').
# Subtract 2 from 30 to determine the probability that at least 28 non-ecotour bears engaged in conflict

eco.bears <- 34 
obs.eco <- 1 # conflict bear from ecotourism

p.eco <- obs.eco/34

obs.noneco <- 27 # we computed 1-Fx(27) to determine the probability that at least 28 non-ecotour bears engaged in conflict while varying the unknown non-ecotour bear abundance, N.

# What is probability of observing at least as many as obs.noneco bears, as noneco bear population size increases?
# 1 - Pr(of observing less then obs.noneco bears)

plot(1:2000,round(1-pbinom(obs.noneco,size=1:2000,p.eco),5),
     type="l")

# How many bears to have 5% chance of seeing this?
min(which(round(1-pbinom(obs.noneco,size=1:2000,p.eco),2)==0.05)) 

# Identify range of population estimates 
# Plot 4 grizzly bear population estimates on the figure: 
# 1) Tweedsmuir GBPU 
# 2) our area of interest using the same density applied to Tweedsmuir GBPU 
# 3) Artelle et al's SCR estimate to the spatial area of the Tweedsmuir GBPU 
# 4) Artelle et al's SCR estimate to the spatial area of our area of interest

# Area of our area of interest with no water or ice: 2461.29068317460 km2
# The ‘net’ area of GBPU's are described by British Columbia as "area of useable habitat", and excludes area of BC Baseline Thematic Mapping layer of water and Ice ('Fresh Water', 'Salt Water','Glaciers and Snow'). See GBPU polygons under Population Density Estimate tab here: https://www.env.gov.bc.ca/soe/indicators/plants-and-animals/grizzly-bears.html/ 

# Area of the Tweedsmuir GBPU
# 16,663 km2 of useable habitat 
# Population density is 22/1000 km2
16663/1000
16.663*22

# Use the Provincial density estimate of 22 adults/1000km2 and apply to our area
2461.29068317460/1000
2.461291*22

# Artelle et al SCR density esitmate 10.2 grizz/1000km2 (assuming 1:1 sex ratio).
16.663*10.2 # Estimate for Tweedsmuir GBPU using Artelle's density estimate
2.461291*10.2 # Estimate for area of interest using Artelle's density estimate

# Figure 4
# Incorporate population estimates in figure
df <- data.frame(x=1:1550,prob=round(1-pbinom(obs.noneco,size=1:1550,p.eco),5))
ggplot(data = df, aes(x = x, y = prob)) +
  geom_line(size = 1) +
  geom_segment(x = 674, xend=674, y = -Inf, yend=0.05, colour = "black", size = 0.2, linetype = "dashed") +
  geom_segment(x= -Inf, xend=674, y = 0.05, yend = 0.05, colour = "black", size = 0.2, linetype = "dashed") +
  coord_cartesian(xlim=c(0,1550), ylim=c(0,1), clip="off") +
  annotate("segment", x = 15, xend = 368, y = -0.15, yend = -0.15) +
  geom_point(aes(x=675,y=0.045),colour="black", size = 2.5) +
  geom_point(aes(x=25,y=-0.15),pch = 19, size = 2.5) + # Estimate for area of interest using Artelle's density estimate
  geom_point(aes(x=54,y=-0.15),pch = 4, size = 2.5) + # Use the Provincial density estimate of 22 adults/1000km2 and apply to our area
  geom_point(aes(x=170,y=-0.15), pch = 15, size = 2.5) + # Estimate for Tweedsmuir GBPU using Artelle's density estimate
  geom_point(aes(x=368,y=-0.15),pch = 17, size = 2.5) + # Twds GBPU population estimate
  annotate("text", x = 705, y = 0.025, label = "Minimum population for result \nto occur at 5% chance", hjust = 0, size =3) +
  scale_x_continuous(breaks = c(25,54,170,368,500,674,1000,1500), labels = c("25","54","170","368","500", "674","1000","1500"), limits = c(0, 1550), expand= c(0, 0)) +
  #  scale_x_continuous(limits = c(0 ,1550), expand = c(0, 0), minor_breaks=seq(25,25,by=1)) +
  ylab("Probability") +
  xlab("Number of bears") +
  theme_classic() 

######## Venn diagram to illustrate genetic matches

# Figure 2
# Create a new column conflict.eco.or.upstream
data$conflict.eco.or.upstream <- NA
# Populate the conflict.eco.or.upstream column 
data$conflict.eco.or.upstream[data$conflict_sample == "yes"] <- "Conflict"
data$conflict.eco.or.upstream[data$eco.or.upstream == "Ecotour"] <- "Ecotour"
data$conflict.eco.or.upstream[data$eco.or.upstream == "Upstream"] <- "Upstream"
# Select unique combinations of individual and conflict.eco.or.upstream
ind.by.area <- unique(data[, c("individual", "conflict.eco.or.upstream")])
# Remove rows where conflict.eco.or.upstream is NA
ind.by.area <- ind.by.area[!is.na(ind.by.area$conflict.eco.or.upstream), ]

conflict <- ind.by.area[ind.by.area$conflict.eco.or.upstream == "Conflict", "individual"]
ecotour <- ind.by.area[ind.by.area$conflict.eco.or.upstream == "Ecotour", "individual"]
upstream <- ind.by.area[ind.by.area$conflict.eco.or.upstream == "Upstream", "individual"]

v <- venn.diagram(
  x = list(
    conflict = conflict,
    ecotour = ecotour,
    upstream = upstream
  ),
  category.names = c("Conflict" , "Ecotour " , "Upstream"),
  filename = 'matches.png',
  output = TRUE ,
  imagetype="png" ,
  height = 700, 
  width = 700, 
  resolution = 500,
  lwd = .5,
  col=c("#440154ff", '#21908dff', '#fde725ff'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),
  cex = 0.5,
  fontfamily = "sans",
  cat.cex = 0.4,
  cat.default.pos = "outer",
  cat.pos = c(0, 0, 0),
  cat.dist = c(0.055, 0.055, 0.02),
  cat.fontfamily = "sans",
  rotation = 1,
  margin = 0.01,
)

######## Species accumulation curve

# Subset ecotour sites only
eco.grizz.atnarko.snags <- subset(data, eco.or.upstream == "Ecotour")
# Set data up for rarefaction
df <- data.frame(eco.grizz.atnarko.snags$X, eco.grizz.atnarko.snags$individual,values=1) 
rare.df <-reshape(df, idvar=c("eco.grizz.atnarko.snags.X"), timevar="eco.grizz.atnarko.snags.individual", direction="wide")
rare.df[is.na(rare.df)] <- as.numeric(0)
rare.df['eco.grizz.atnarko.snags.X']<- NULL
specpool(rare.df)
# Vignette
# http://www.pelagicos.net/MARS6910_spring2015/manuals/R_vegan.pdf
accum <- specaccum(rare.df, method="random")

# Plot the accumulation curve with updated y-axis limits
plot(accum, add=FALSE, col = "blue", ci.type = "polygon", ci.col = "lightgrey", 
     xlim=c(1,80), ylim=c(0, 60), ci.lty=0, xlab = "Samples", ylab = "Individuals", lwd = 1)

# Add horizontal lines for asymptotes
abline(h=49.76471, col="black", lty=2)  # Asymptote for the first curve
abline(h=41.27725, col="red", lty=2)  # Asymptote for the second curve

text(x=65, y=51.5, labels="49.8", col="black", pos=4, offset=0.5)
text(x=65, y=43, labels="41.3", col="black", pos=4, offset=0.5)

# Add a legend to identify the lines
legend("bottomright", 
       legend = c("Bootstrap estimate", "Jackknife estimate", "Species accumulation curve"),
       col = c("black", "red", "blue"), 
       lty = c(2, 2, 1), # 2 for dashed, 1 for solid
       cex = 0.8)

######## Fisher's exact test

# Create a contingency table
conflict=c(1,rep(0,84+33))
treatment=c(rep("ecotour",34),rep("nonecotour", 84))
table <- table(treatment, conflict)
# Perform Fisher's Exact Test
fisher.test(table)

######## Maximum distances travelled

# Call in distance dataframe and furthest sites per individual. Network distance (distance along the river) was measured using the 'Measure' tool in ArcGIS and exported as csv.
distances <- read.csv("cummulative_network_distance.csv")
at25_hags <- read.csv("dist_from_at25_to_hags.csv")
furthest_sites <- read.csv("furthest_sites_per_individual.csv")

# Join the distances data to furthest_sites for site1
furthest_sites <- furthest_sites %>%
  left_join(distances %>% dplyr::select(site, meters_from_at01), 
            by = c("site1" = "site")) %>%
  rename(meters_from_at01_site1 = meters_from_at01)
# Join the distances data to furthest_sites for site2
furthest_sites <- furthest_sites %>%
  left_join(distances %>% dplyr::select(site, meters_from_at01), 
            by = c("site2" = "site")) %>%
  rename(meters_from_at01_site2 = meters_from_at01)
# Calculate the distance between site1 and site2
furthest_sites <- furthest_sites %>%
  mutate(distance_between_sites = abs(meters_from_at01_site1 - meters_from_at01_site2))

# Distance travelled by the single ecotour/conflict bear 
sum(at25_hags$segment_meters.start_point_at25.end_point_hagensborg,na.rm = TRUE)
# = 47780.12

# Add this distance to furthest sites dataframe, and create histogram. 
final_dist <- furthest_sites %>%
  dplyr::select(individual, distance_between_sites)
final_dist$distance_between_sites[final_dist$individual == 49877] <- 47780.12
final_dist$distance_between_sites <- final_dist$distance_between_sites/1000 # to km

# Figure S2
custom_breaks <- seq(0, 50, by = 5)
# Apply the custom breaks to categorize the distances
final_dist$custom_category <- cut(final_dist$distance_between_sites,
                                  breaks = custom_breaks,
                                  include.lowest = TRUE,
                                  labels = paste0(custom_breaks[-length(custom_breaks)], 
                                                  "-", 
                                                  custom_breaks[-1]))

ggplot(final_dist, aes(x = custom_category)) +
  geom_histogram(stat = "count", aes(fill = as.numeric(custom_category) == max(as.numeric(custom_category))), color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +  # Annotate with counts, increase size by 1
  xlab("Maximum distance between redetection sites (km)") +
  ylab("Number of individuals") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "grey50"), guide = "none") +  # Assign colors
  theme_classic(base_size = 14)   # Increase base text size by 1 point