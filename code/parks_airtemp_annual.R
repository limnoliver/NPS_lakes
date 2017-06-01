# Air temp fig for all parks
library(prism)
library(httr)
library(RColorBrewer)
library(dplyr)
set_config(config(ssl_verifypeer = 0L))

# fetch prism mean air temp annual data
get_prism_annual(type = 'tmean', years = 1980:2016, keepZip = FALSE)

# create list of long/lats for parks
parks <- list(ACDA = c(-68.25, 44.36), VOYA = c(-93.38, 48.60), 
              ROMO = c(-105.7, 40.4), AMIS = c(-101, 29.5), SEKI = c(-118.5, 36.5), 
              OLYM = c(-123.6, 47.8))


# for yearly data, only use files with YYYY attached to file name
# which distinguishes between monthly (YYYYMM) and annual data files
# also distinguishes mean from tmin
files.keep <- grep('PRISM_tmean_stable_4kmM2_[[:digit:]]{4}_', ls_prism_data()$files)

# extract climate data for each park location
parks.climate.mean <- list()

for (i in 1:length(parks)){
  parks.climate.mean[[i]] <- prism_slice(parks[[i]], ls_prism_data()[files.keep,1])$data
}

# create data frame from list
annual.df <- do.call(rbind, lapply(parks.climate.mean, data.frame))
annual.df$park <- rep(names(parks), each = length(1980:2014)-1) #1981 is missing

# write df
write.csv(annual.df, "data/parks_annual_meanairtemp.csv")

# plot annual tmean data for each park on same plot
# keep temperature scale of each plot
park.cols <- brewer.pal(6, name = 'Dark2')

png('figures/parks_annual_meantemp.png', height = 6, width = 4, units = 'in', res = 300)
par(xpd = TRUE, mar = c(3,3,1,1), mgp = c(2,0.5,0), tcl = -0.3)

plot(data~date, type = 'l', col = park.cols[1], 
     lwd = 3, ylim = c(-2,23), xlab = "Year", ylab = 'Annual Mean Temperature', 
     bty = "L", data = annual.df[annual.df$park == names(parks)[1],]) 

for (i in 2:length(parks)){
  points(data~date, type = 'l', col = park.cols[i], lwd = 3, data = annual.df[annual.df$park == names(parks)[i],])
}
legend(x = as.Date("1980-01-01"), y = 24.7, legend = names(parks), lwd = 3, 
       col = park.cols, ncol = 4, cex = 0.7)

dev.off()



