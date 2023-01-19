## basic munge
library(dplyr)
library(lubridate)
library(cluster)

csvPth <- '~/Desktop/shoe_sales_case/'
dataDF <- list.files(path       = csvPth,
           pattern    = '.csv',
           full.names = T)

allDF <- lapply(dataDF, read.csv)
allDF <- left_join(allDF[[1]],allDF[[2]], by = "uuID")

tmp <- as.Date(allDF$releaseDate)


# Color munge
rbgColors <- data.frame(  hex_1   = t(data.frame(col2rgb(allDF$hex_1,))),
                          hex_2   = t(data.frame(col2rgb(allDF$hex_2,))),
                          hex_3   = t(data.frame(col2rgb(allDF$hex_3,))),
                          hex_4   = t(data.frame(col2rgb(allDF$hex_4,))),
                          hex_5   = t(data.frame(col2rgb(allDF$hex_5,))),
                          hex_6   = t(data.frame(col2rgb(allDF$hex_6,))),
                          hex_7   = t(data.frame(col2rgb(allDF$hex_7,))),
                          hex_8   = t(data.frame(col2rgb(allDF$hex_8,))))
paste(as.vector(col2rgb(allDF$hex_2)), collapse = " ")

# Now cluster and assign clusters to 1000 shoes by colors
fviz_nbclust(rbgColors, kmeans, method = "wss")
fviz_nbclust(rbgColors, kmeans, method = "silhouette")
gap_stat <- clusGap(rbgColors, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

km2 <- kmeans(scale(rbgColors), 2, nstart = 3)
fviz_cluster(km2, data = rbgColors)

km2$cluster

km10 <- kmeans(scale(rbgColors), 10, nstart = 3)
fviz_cluster(km10, data = rbgColors)



modelDF <- data.frame(uuID = allDF$uuID,
                      title = allDF$title,
                      description = allDF$description,
                      model = allDF$model,
                      condition = allDF$condition,
                      retailPrice = allDF$retailPrice,
                      releaseWeekDay = weekdays(tmp),
                      releaseMonth = month.abb[month(tmp)],
                      releaseDayOfYr  = yday(tmp),
                      releaseQtr  = quarters(tmp),
                      hex_1   = allDF$hex_1, 
                      hex_2   = allDF$hex_2,
                      hex_3   = allDF$hex_3,
                      hex_4   = allDF$hex_4,
                      hex_5   = allDF$hex_5,
                      hex_6   = allDF$hex_6,
                      hex_7   = allDF$hex_7, 
                      hex_8   = allDF$hex_8,
                      kMeans2 = LETTERS[km2$cluster],
                      kMeans10 =LETTERS[km10$cluster],
                      rbgColors,
                      y_sold       = allDF$sold)
write.csv(modelDF, 'modelDF.csv', row.names = F)
