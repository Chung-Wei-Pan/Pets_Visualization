install.packages("magrittr")
install.packages("dplyr")
install.packages("curl")
install.packages("AER")
install.packages("RCurl")
install.packages("XML")
install.packages("bitops")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("mapproj")
install.packages("jsonlite")
library("magrittr")
library("dplyr")
library("curl")
library("stringr")
library(jsonlite)
#Ū�����
pet <- fromJSON("https://data.coa.gov.tw/Service/OpenData/AnimalOpenData.aspx")
pet <- pet[,-c(1:4,11:12,14:17,19:21,23:27)]


pet$shelter_locate <- substr(pet$shelter_name,1,3)
pet$animal_year <- substr(pet$animal_opendate,1,4)
pet_year <- pet %>%
  group_by(animal_year,shelter_locate) %>%
  summarise(sum_pet = n())

pet[pet$animal_foundplace=="��L",] <- NA
pet$animal_foundplace <- paste0(pet$shelter_locate,pet$animal_foundplace)
pet<-na.omit(pet)

library(ggplot2)

foundplace <- c(pet$animal_foundplace)
foundplace_kao <- grep("����",foundplace,value=T) 

install.packages("googleway")
library(googleway)
area_kao <- data.frame(lat=double(),lng=double())
#�Ц�https://developers.google.com/maps/?hl=zh-tw ���oapi key
key <- "your api key"
for (i in 1:length(foundplace_kao)) {
  geo_result <- google_geocode(address = foundplace_kao[i], key = key)
  area_kao<-rbind(area_kao,geo_result$results$geometry$location)
  #�ѩ�google api�C���������Ū������A�]���o��]�w�j��0.01�����@��
  Sys.sleep(0.01)
}

library(ggmap)
library(mapproj)

map <- get_map(location = c(lon = 120.37178, lat = 22.695651),
               zoom = 11, language = "zh-TW")
ggmap(map)+
  geom_point(aes(x = lng, y = lat), size = 1, col="red",data = area_kao, alpha = 0.6)+
  geom_density2d(data = area_kao, aes(x = lng, y=lat), size = 0.3, geom = "polygon")+
  scale_fill_gradient(low = "green", high = "red",guide = FALSE)+
  scale_alpha(range = c(0, 0.3), guide = FALSE)
