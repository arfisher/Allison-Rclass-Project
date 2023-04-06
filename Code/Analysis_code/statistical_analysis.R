###############################
# Penguin analysis script
#
# This script loads the processed, cleaned data, 
# does a simple analysis and saves the results
# to the results folder
###############################

## ---- setup -----
#load needed packages. make sure they are installed.
require(ggplot2) #for plotting
require(magrittr) #for piping
require(knitr) #for formatting output

#path to data and results 
data_path <- "../../Data/Processed_data/"
results_path <- "../../Results/"

## ---- functions ----
# function to paste path to output filenames

addpath <- function( filename, path=data_path ) {
    location <- paste( path, filename, sep="")
	return( location )
}

## ---- loaddata ----
# load data. 
dat <- readRDS( addpath("processeddata.rds", data_path) )


## ---- summarize ----
# create summary table of the data using skimr to use in paper
# variables, sample size, mean, standard error

sk <- skimr::skim(dat)  # save skim object
sk <- as.data.frame(sk) # save as data.frame
head(sk)  # see the variable names

nrows <- dim(dat)[1] # total number of rows
sk$N <- nrows - sk$n_missing  # sample size of each variable

## ---- summary.table ----
# select only the variable, N, mean, sd, and category counts

sk.table <- sk[c("skim_variable", "N", "numeric.mean", "numeric.sd", "factor.top_counts")]
names(sk.table) <- c("Variable", "N", "Mean", "SE", "Counts") # rename SD as SE
sk.table$SE <- sk.table$SE/sqrt(sk.table$N) # calculate SE

options(knitr.kable.NA = "")
knitr::kable(sk.table, digits=2)


# save summary table
saveRDS(sk.table, file = addpath("summary_table.rds", results_path))


## ---- header ----
######################################
# Statistical analysis
######################################

############################
#### 


## ---- mass_clutch_boxplot ----
dat$ClutchCompletion <- as.factor(dat$"Clutch Completion")

# plot to screen
with(dat, plot(`Body Mass (g)` ~ ClutchCompletion))

# plot to .png file, can also do pdf using `pdf()` function 
png(filename = addpath("mass_clutch_bars.png", results_path))
  with(dat, plot(`Body Mass (g)` ~ ClutchCompletion))
dev.off()

## ---- mass_clutch_density ----

# create plot and send to screen
p <- dat %>%    # mass density by CC
       ggplot( aes(x=`Body Mass (g)`)) + 
       geom_density( aes(fill=ClutchCompletion), alpha=.5) 
p

# save ggplot objects using `ggsave()` 
ggsave(filename = addpath("mass_clutch_density.png", results_path), plot=p) 

## ---- mass_clutch_ttest ----
ttest <- with(dat, t.test(`Body Mass (g)` ~ ClutchCompletion))

saveRDS(ttest, file = addpath("mass_clutch_ttest.rds", results_path))

## ---- species_clutch_plot ----

#make ClutchCompletion variable categorical
dat$ClutchCompletion <- as.factor(dat$"Clutch Completion")

# plot to screen
with(dat, plot(ClutchCompletion ~ Species))

# plot to .png file, can also do pdf using `pdf()` function 
png(filename = addpath("species_clutch_bars.png", results_path))
  with(dat, plot(ClutchCompletion ~ Species))
dev.off()

## ---- clutchcompletion_species_aov ----

#make clutch completion binary
dat$CC <-ifelse(dat$ClutchCompletion=="Yes",1,0)
head(dat)

lm.fit.c <- lm(`CC` ~ Species, dat)  
anova.table.c <- anova(lm.fit.c)

# print to screen the anova table
print(anova.table.c)

# save anova table to file in Results folder  
saveRDS(anova.table.c, file = addpath("cc_species_anova.rds", results_path))

## ---- mass_species_boxplot ----
# plot to screen
with(dat, plot(`Body Mass (g)` ~ Species))

# plot to .png file, can also do pdf using `pdf()` function 
png(filename = addpath("mass_species_bars.png", results_path))
  with(dat, plot(`Body Mass (g)` ~ Species))
dev.off()
## ---- mass_species_aov ----
# fit linear model using mass as outcome, species as predictor

lm.fit.s <- lm(`Body Mass (g)` ~ Species, dat)  
anova.table.s <- anova(lm.fit.s)

# print to screen the anova table
print(anova.table.s)

# save anova table to file in Results folder  
saveRDS(anova.table.s, file = addpath("mass_species_anova.rds", results_path))

## ---- mass_species_clutch_density ----

# create plot, subseted by clutch completion and faceted by species
q <- dat %>%    # CC by species
       ggplot( aes(x=`Body Mass (g)`)) + 
       geom_density( aes(fill=c(ClutchCompletion)), alpha=.5) +
       facet_grid(Species ~ .)
q

ggsave(filename = addpath("mass_species_clutchcompletion.png", results_path), plot=q) 

 
