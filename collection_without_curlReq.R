require(httr)
library(magick)
library(dplyr)
library(scales)
library(imager)
library(pbapply)

#inputs
originalImgFolder <- "~/Desktop/shoe_sales_case/images/"
saveImgFolder <- '~/Desktop/shoe_sales_case/conceptDrawing/'
csvPth <- '~/Desktop/shoe_sales_case/'


# CURL request e in chrome
headers = c(
  `authority` = 'stockx.com',
  `accept` = '*/*',
  `accept-language` = 'en-US',
  `apollographql-client-name` = 'Iron',
  `apollographql-client-version` = '2023.01.01.01',
  `app-platform` = 'Iron',
  `app-version` = '2023.01.01.01',
  `content-type` = 'application/json',
  `cookie` = '####',
  `origin` = 'https://stockx.com',
  `referer` = '###',
  `sec-ch-ua` = '###',
  `sec-ch-ua-mobile` = '?###',
  `sec-ch-ua-platform` = '"###"',
  `sec-fetch-dest` = '###',
  `sec-fetch-mode` = '###',
  `sec-fetch-site` = '###',
  `selected-country` = '###',
  `user-agent` = '###',
  `x-operation-name` = '###',
  `x-stockx-device-id` = '###'
)

data = '###'

res <- httr::POST(url = 'https://stockx.com/api/p/e', httr::add_headers(.headers=headers), body = data)
xAll <- content(res)
length(xAll$data$browse$results$edges)


x <- xAll$data$browse$results$edges

allShoes <- list()
for(i in 432:length(x)){ #432
  print(i)
  oneShoe <- x[[i]]
  uuID <- x[[i]]$objectId
  title <-  x[[i]]$node$title
  urlKey <-  x[[i]]$node$urlKey
  description <-  x[[i]]$node$description
  model <-  x[[i]]$node$model
  condition <-  x[[i]]$node$condition
  listingType <-  x[[i]]$node$listingType
  retailPrice <-  x[[i]]$node$productTraits[[1]]$value
  if(length(x[[i]]$node$productTraits)==1){
    releaseDate <-  'NULL'
  } else {
    releaseDate <-  x[[i]]$node$productTraits[[2]]$value
  }
  
  lastSale<- x[[i]]$node$market$salesInformation$lastSale
  lastSaleDate<- x[[i]]$node$market$salesInformation$lastSaleDate
  salesThisPeriod<- x[[i]]$node$market$salesInformation$salesThisPeriod
  salesLastPeriod<- x[[i]]$node$market$salesInformation$salesLastPeriod
  changeValue<- x[[i]]$node$market$salesInformation$changeValue
  changePercentage<- x[[i]]$node$market$salesInformation$changePercentage
  volatility<- x[[i]]$node$market$salesInformation$volatility
  pricePremium<- x[[i]]$node$market$salesInformation$pricePremium
  sold<- x[[i]]$node$market$deadStock$sold
  averagePrice<- x[[i]]$node$market$deadStock$averagePrice
  Sys.sleep(1)
  download.file(url = URLencode(x[[i]]$node$media$smallImageUrl),  #x[[i]]$node$media$smallImageUrl, 
                destfile= paste0(originalImgFolder, uuID, '_',make.names(title),'.png'))
  allShoes[[i]] <- data.frame(uuID,urlKey, title,description,model,condition,
                              listingType, retailPrice, releaseDate,
                              lastSale,lastSaleDate,salesThisPeriod,salesLastPeriod,
                              changeValue,changePercentage,volatility,pricePremium,sold, averagePrice) 
}
allShoesDF <- do.call(rbind, allShoes)
write.csv(allShoesDF,paste0(csvPth,'allShoesDF.csv'), row.names = F)

allPics<- list.files(path = originalImgFolder,
                     pattern = 'png',
                     full.names = T)
for(i in 1:length(allPics)){
  img <- image_read(allPics[i])
  charImg <- image_charcoal(img)
  
  nam <- tail(unlist(strsplit(allPics[i],'/')),1)
  nam <- gsub('.png','_conceptDrawing.png', nam)
  #image_convolve(img, 'Laplacian:0')%>% image_negate() # not as good
  nam <- paste0(saveImgFolder,nam)
  image_write(charImg, path = nam, format = "png", density = 300)
  
}

# Color Palettes
getColorPal <- function(im, n=8, cs="RGB"){
  #print(cs) 
  tmp <-im %>% image_resize("100") %>% 
    image_quantize(max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  response <- tmp %>% select(colorspace,hex,hue,sat,value,n)
  return(response) ## I want data frame as a result.
  
}
allImgsFiles <- list.files(path       = originalImgFolder,
                      patter     = 'png',
                      full.names = T)
allImgs <- pblapply(allImgsFiles,image_read)
picColors <- pblapply(allImgs,getColorPal)

resultLst <- list()
for(i in 1:length(picColors)){
  onePicColors <- picColors[[i]]
  tmp <- list()
  for(j in 1:nrow(onePicColors)){
    oneRow <- onePicColors[j,]
    names(oneRow) <- paste0(names(oneRow),'_',j)
    tmp[[j]] <- oneRow
  }
  result <- do.call(cbind, tmp)
  uuID <- tail(unlist(strsplit(allImgsFiles[i],'/')),1)
  uuID <- head(unlist(strsplit(uuID, '_')),1)
  result$uuID <- uuID 
  resultLst[[i]] <- result
}
resultsDF <- plyr::rbind.fill(resultLst)

write.csv(resultsDF, paste0(csvPth,'topColorsDF.csv'), row.names = F)

