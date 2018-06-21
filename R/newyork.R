library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('alluvial') # visualisation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps
library('xgboost') # modelling
library('caret') # modelling
library('magrittr') # Pipingdffsdkl


# Multiplot Function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

getwd()
list.files()
setwd("E:/Kaggle/New York City Taxi Trip Duration")
traindata <- as.tibble(fread("train.csv"))
testdata <- as.tibble(fread("test.csv"))
sample_submit <- as.tibble(fread("sample_submission.csv"))

# data Structure
summary(traindata);glimpse(traindata)
summary(testdata);glimpse(testdata)

# Missing values
sum(is.na(traindata));sum(is.na(testdata))
sapply(traindata,function(x)table(is.na(x)));sapply(testdata,function(x)table(is.na(x)))


# Combine the both datasets
combinedata <- bind_rows(traindata%>%mutate(dset = "train"),
                         testdata%>%mutate(dset = "test",
                                           dropoff_datetime = NA,
                                           trip_duration  = NA))
combinedata%<>%mutate(dset = factor(dset))


# Reformating the Features
traindata%<>%mutate(pickup_datetime = ymd_hms(pickup_datetime),
                    dropoff_datetime = ymd_hms(dropoff_datetime),
                    vendor_id = factor(vendor_id),
                    passenger_count = factor(passenger_count))


# Consistency check for trip_duration
 traindata%>%mutate(check = abs(int_length(interval(dropoff_datetime,pickup_datetime))+trip_duration)>0)%>%
      select(check,pickup_datetime,dropoff_datetime,trip_duration)%>%
   group_by(check)%>%count()


# individual feature visualisations
 set.seed(1234)
 foo <- sample_n(traindata, 8e3)

 leaflet(data = foo)%>%addProviderTiles("Esri.NatGeoWorldMap")%>%
   addCircleMarkers(~pickup_longitude,~pickup_latitude,radius = 1,color = "blue",fillOpacity = 0.3)

 traindata%>%ggplot(aes(trip_duration))+geom_histogram(colour = 'yellow',fill = 'red',bins = 100)+scale_x_log10()+
   scale_y_sqrt()

 traindata %>%
   arrange(desc(trip_duration)) %>%
   select(trip_duration, pickup_datetime, dropoff_datetime, everything()) %>%
   head(10)


 p1 <- traindata %>%
   ggplot(aes(pickup_datetime)) +
   geom_histogram(fill = "red", bins = 120) +
   labs(x = "Pickup dates")

 p2 <- traindata %>%
   ggplot(aes(dropoff_datetime)) +
   geom_histogram(fill = "blue", bins = 120) +
   labs(x = "Dropoff dates")

 layout <- matrix(c(1,2),2,1,byrow=FALSE)
 multiplot(p1, p2, layout=layout)


 p1 <- 1; p2 <- 1

 traindata %>%
   filter(pickup_datetime > ymd("2016-01-20") & pickup_datetime < ymd("2016-02-10")) %>%
   ggplot(aes(pickup_datetime)) +
   geom_histogram(fill = "red", bins = 120)

 p1 <- traindata %>%
   group_by(passenger_count) %>%
   count() %>%
   ggplot(aes(passenger_count, n, fill = passenger_count)) +
   geom_col() +
   scale_y_sqrt() +
   theme(legend.position = "none")


 p2 <- traindata %>%
   ggplot(aes(vendor_id, fill = vendor_id)) +
   geom_bar() +
   theme(legend.position = "none")

 p3 <- traindata %>%
   ggplot(aes(store_and_fwd_flag)) +
   geom_bar() +
   theme(legend.position = "none") +
   scale_y_log10()

 p4 <- traindata %>%
   mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
   group_by(wday, vendor_id) %>%
   count() %>%
   ggplot(aes(wday, n, colour = vendor_id)) +
   geom_point(size = 4) +
   labs(x = "Day of the week", y = "Total number of pickups") +
   theme(legend.position = "none")

 p5 <- traindata %>%
   mutate(hpick = hour(pickup_datetime)) %>%
   group_by(hpick, vendor_id) %>%
   count() %>%
   ggplot(aes(hpick, n, color = vendor_id)) +
   geom_point(size = 4) +
   labs(x = "Hour of the day", y = "Total number of pickups") +
   theme(legend.position = "none")

 layout <- matrix(c(1,2,3,4,5,5),3,2,byrow=TRUE)
 multiplot(p1, p2, p3, p4, p5, layout=layout)

 traindata %>%
   group_by(passenger_count) %>%
   count()

 traindata %>%
   group_by(store_and_fwd_flag) %>%
   count()

 p1 <- traindata %>%
   mutate(hpick = hour(pickup_datetime),
          Month = factor(month(pickup_datetime, label = TRUE))) %>%
   group_by(hpick, Month) %>%
   count() %>%
   ggplot(aes(hpick, n, color = Month)) +
   geom_line(size = 1.5) +
   labs(x = "Hour of the day", y = "count")

 p2 <- traindata %>%
   mutate(hpick = hour(pickup_datetime),
          wday = factor(wday(pickup_datetime, label = TRUE))) %>%
   group_by(hpick, wday) %>%
   count() %>%
   ggplot(aes(hpick, n, color = wday)) +
   geom_line(size = 1.5) +
   labs(x = "Hour of the day", y = "count")

 layout <- matrix(c(1,2),2,1,byrow=FALSE)
 multiplot(p1, p2, layout=layout)


