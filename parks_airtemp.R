# Air temp fig for all parks
library(prism)
library(httr)
library(RColorBrewer)
library(dplyr)
set_config(config(ssl_verifypeer = 0L))

# fetch prism mean air temp monthly data
get_prism_monthlys(type = 'tmean', years = 1980:2014, mon = c(7:9), keepZip = FALSE)


# create list of long/lats for parks
parks <- list(acadia = c(-68.25, 44.36), voyageurs = c(-93.38, 48.60), 
              rocky = c(-105.7, 40.4), marjory = c(-80.2, 25.7), 
              amistad = c(-101, 29.5), sequoia = c(-118.5, 36.5), 
              olympic = c(-123.6, 47.8), cuyahoga = c(-81.6, 41.3))

# extract climate data for each park location
# for monthly data, only use files with month attached to file name
files.keep <- grep('_[[:digit:]]{6}_', ls_prism_data()$files)
parks.climate.mean <- list()

for (i in 1:length(parks)){
  parks.climate.mean[[i]] <- prism_slice(parks[[i]], ls_prism_data()[files.keep,1])$data
}

# find monthly averages
summer_annual <- list()
for (i in 1:length(parks)){
  summer_annual[[i]] <- parks.climate.mean[[i]] %>%
    mutate(., year = format(date, "%Y")) %>%
    group_by(year) %>%
    summarise(summer_avg = mean(data))
}

test %>%
  mutate(., year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(summer_avg = mean(data))

# plot annual tmean data for each park on same plot
# keep temperature scale of each plot
park.cols <- brewer.pal(8, name = 'Dark2')

png('parks_mintemp_realtemp.png', height = 6, width = 4, units = 'in', res = 300)
par(xpd = TRUE, mar = c(4,4,2,1))
plot(parks.climate.min[[1]]$data~parks.climate.min[[1]]$date, type = 'l', col = park.cols[1], 
     lwd = 3, ylim = c(-10,24), xlab = "Year", ylab = 'Annual Minimum Temperature', bty = "L") 
for (i in 2:length(parks)){
  points(parks.climate.min[[i]]$data~parks.climate.min[[i]]$date, type = 'l', col = park.cols[i], lwd = 3)
}
legend(x = as.Date('1980-01-01'), y = 26, legend = names(parks), lwd = 3, 
       col = park.cols, ncol = 4, cex = 0.5)

dev.off()

png('parks_meantemp_realtemp.png', height = 6, width = 4, units = 'in', res = 300)
park.cols <- brewer.pal(8, name = 'Dark2')
plot(parks.climate.mean[[1]]$data~parks.climate.mean[[1]]$date, type = 'l', col = park.cols[1], 
     lwd = 3, ylim = c(-2,26),  xlim = c(as.Date("1980-01-01"), as.Date("2020-01-01")), xlab = "Year", ylab = 'Annual Mean Temperature') 
for (i in 2:length(parks)){
  points(parks.climate.mean[[i]]$data~parks.climate.mean[[i]]$date, type = 'l', col = park.cols[i], lwd = 3)
}
text(x = as.Date('2016-01-01'), y = c(parks.climate.mean)))
dev.off()

# now plot temperature that has been normalized to start temp

png('parks_mintemp_realtemp.png', height = 6, width = 4, units = 'in', res = 300)
park.cols <- brewer.pal(8, name = 'Dark2')
plot(park.climate.relative[[1]]$data_rel~park.climate.relative[[1]]$date, type = 'l', col = park.cols[1], 
     lwd = 3, ylim = c(-2,4), xlim = c(as.Date("1980-01-01"), as.Date("2020-01-01")), xlab = "Year", ylab = 'Annual Minimum Temperature') 
for (i in 2:length(parks)){
  points(park.climate.relative[[i]]$data_rel~park.climate.relative[[i]]$date, type = 'l', col = park.cols[i], lwd = 3)
}
dev.off()

