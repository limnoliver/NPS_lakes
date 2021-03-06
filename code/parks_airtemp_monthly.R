# Air temp fig for all parks
library(prism)
library(httr)
library(RColorBrewer)
library(dplyr)
set_config(config(ssl_verifypeer = 0L))

# fetch prism mean air temp monthly data
get_prism_monthlys(type = 'tmean', years = 1980:2014, mon = c(7:9), keepZip = FALSE)


# create list of long/lats for parks
parks <- list(ACDA = c(-68.25, 44.36), VOYA = c(-93.38, 48.60), 
              ROMO = c(-105.7, 40.4), AMIS = c(-101, 29.5), SEKI = c(-118.5, 36.5), 
              OLYM = c(-123.6, 47.8))


# for monthly data, only use files with month attached to file name
# which distinguishes between monthly and annual data files
files.keep <- grep('_[[:digit:]]{6}_', ls_prism_data()$files)

# extract climate data for each park location
parks.climate.mean <- list()

for (i in 1:length(parks)){
  parks.climate.mean[[i]] <- prism_slice(parks[[i]], ls_prism_data()[files.keep,1])$data
}


# find annual summer averages
summer_annual <- list()
for (i in 1:length(parks)){
  summer_annual[[i]] <- parks.climate.mean[[i]] %>%
    mutate(., year = format(date, "%Y")) %>%
    group_by(year) %>%
    summarise(summer_avg = mean(data))
}

# create data frame from list
summer.df <- do.call(rbind, lapply(summer_annual, data.frame))
summer.df$park <- rep(names(parks), each = length(1980:2014)-1) #1982 is missing

# write df
write.csv(summer.df, "data/parks_summer_meanairtemp.csv")

# plot annual tmean data for each park on same plot
# keep temperature scale of each plot
park.cols <- brewer.pal(8, name = 'Dark2')

png('figures/parks_summer_meantemp_realtemp.png', height = 6, width = 4, units = 'in', res = 300)
par(xpd = TRUE, mar = c(3,3,1,1), mgp = c(2,0.5,0), tcl = -0.3)

plot(summer_avg~year, type = 'l', col = park.cols[1], 
     lwd = 3, ylim = c(7,32), xlab = "Year", ylab = 'Summer Mean Temperature', 
     bty = "L", data = summer.df[summer.df$park == names(parks)[1],]) 

for (i in 2:length(parks)){
  points(summer_avg~year, type = 'l', col = park.cols[i], lwd = 3, data = summer.df[summer.df$park == names(parks)[i],])
}
legend(x = 1980, y = 33, legend = names(parks), lwd = 3, 
       col = park.cols, ncol = 4, cex = 0.5)

dev.off()



