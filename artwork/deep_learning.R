library(caret)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
require(scales)
library(grid)
library(raster)
library(magick)
library(keras)
library(matrixStats)
library(reshape2)
library(RODBC)           # Provides database connectivity
library(plotly)
library(data.table)

myServer <- "artstats.database.windows.net"
myUser <- "simonbowerbank"
myPassword <- "firstlover1487!"
#myPassword <- "firstlover1487"
myDatabase <- "auctionsales"
#myDriver <- "ODBC Driver 13 for SQL Server" # Must correspond to an entry in the Drivers tab of "ODBC Data Sources"
myDriver <- "SQL Server"

connectionString <- paste0(
  "Driver=", myDriver, 
  ";Server=", myServer, 
  ";Database=", myDatabase, 
  ";Uid=", myUser, 
  ";Pwd=", myPassword)


conn <- odbcDriverConnect(connectionString)


data <- sqlQuery(conn, "select * from FileData")


w <- data
w$YearSold <- as.numeric(format(as.Date(w$YearSold, format="%d/%m/%Y"),"%Y"))

w <- w[sample(nrow(w)),]
w$artworkid <- as.factor(w$artworkid)
w$Substrate_sub <- addNA(w$Substrate_sub)
w$area=w$Height*w$Width
w$age=w$YearSold-w$YearMade

# Eliminate 0 or null values
#wb <- w %>% filter(Price>0 | Estimate_low>0) %>% dplyr::select(-Notes)
wb <- w  %>% dplyr::select(-Notes)
wb$Price[wb$Price==0] <- NA
wb$Estimate_low[wb$Estimate_low==0] <- NA

nrows <- nrow(wb)
y=wb[1:nrows,]

# Predict Estimate_low
ys <- as.matrix(data.frame(y$Price,y$Estimate_low))
ys <- log(ys)
pr.mean <- mean(ys[,1],na.rm=TRUE)
pr.sd <- sd(ys[,1],na.rm=TRUE)
ys[,1] <- ifelse(is.na(ys[,1]),NA,(ys[,1]-pr.mean)/pr.sd)
est.mean <- mean(ys[,2],na.rm=TRUE)
est.sd <- sd(ys[,2],na.rm=TRUE)
ys[,2] <- ifelse(is.na(ys[,2]),NA,(ys[,2]-est.mean)/est.sd)


#ys<-scale(log(ys))
ySD=rowSds(ys, na.rm=TRUE)
ys=data.frame(ys,ySD)
ys$ySD <- ifelse(is.na(ys$y.Estimate_low), max(na.omit(ys$ySD)), ys$ySD) 
ys$ySD <- ifelse(is.na(ys$y.Price), mean(na.omit(ys$ySD)) , ys$ySD) 
ys$weights = 1-ys$ySD/max(ys$ySD)
ys$weights <- ifelse(ys$weights == 0, .03, ys$weights) 
min <- min(wb$YearSold,na.rm = TRUE)
max <- max(wb$YearSold,na.rm=TRUE)
normalize <- function(vec, min, max) {
  (vec-min) / (max-min)
}
normyear <- normalize(y$YearSold, min, max)
normyear[is.na(normyear)] <- mean(normyear,na.rm=TRUE)

ys$weights <- normyear*ys$weights
weights <- as.array(ys$weights)
y <- ifelse(is.na(ys$y.Estimate_low), ifelse(is.na(ys$y.Price),0.1,ys$y.Price), ys$y.Estimate_low)
sdlogy <- sd(log(na.omit(wb$Estimate_low)))
meanlogy <- mean(log(na.omit(wb$Estimate_low)))

wb <- wb[ , !(names(wb) %in% c("Sale_ID","Image","Title","Estimate_low","Estimate_high","Price","Graph",
                               "Current_Value","GraphJson"))]

wb <- wb %>%
  dplyr::select(c(artworkid,Substrate,Medium_Cat,PlaceSold,Substrate_sub,Sale_Id,Artist), everything())


ncolumns<-ncol(wb)
preprocessParams <- preProcess(wb[,c(8,27,29,30,31,32,35,36,37,38)], method=c("center","scale")) #standardize with center/scale, or range
nums <- predict(preprocessParams, wb[,c(8,27,29,30,31,32,35,36,37,38)])
#pre-processing of factors
# Il ne faut sélectionner que les facteurs avec au moins 2 niveaux
columns <- NULL
for (i in 1:ncolumns) {
  if(is.factor(wb[,i]) & nlevels(wb[,i]) >= 2) {columns <- c(columns,paste0("+",names(wb)[i]))}
}

mm <- dummyVars(as.formula(paste("~", paste0(columns, sep="", collapse=""))), fullRank=T, data=wb)

mm <- data.frame(predict(mm, newdata = wb))
mm <- cbind(mm,wb %>% dplyr::select(Medium_WaterColour,Medium_Acrylic,
                            Medium_Oil,Medium_Gouache,Medium_Enamel,Medium_Lithograph,   
                            Medium_Metallicpaint,Medium_Etching,Medium_Fibretippen,Medium_Wood,         
                            Medium_Offset,Medium_Ink,Medium_Goldleaf,Medium_Graphite,     
                            Medium_Pastel,Medium_Collage,Medium_Woodblock,medium_chalk,        
                            Medium_Gelatinsilver,Medium_Inkjetprint,Signed) )
mm[is.na(mm)] <- 0
mm <- mm[, colSums(mm) != 0]
mm <- mm[, colSums(mm) != 1]
x <- as.matrix(data.frame(mm,nums ))  
for(i in 1:ncol(x)){x[is.na(x[,i]), i] <- mean(x[,i], na.rm = TRUE)}


#x data
ntrain <- ncol(x)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 80, activation = 'tanh', input_shape = c(ntrain), regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
    layer_dropout(rate=0.1)  %>%
  layer_dense(units = 80, activation = 'tanh', regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
    layer_dropout(rate=0.1)  %>%
  layer_dense(units = 1, activation='linear', regularizer_l1_l2(l1 = 0.01, l2 = 0.01))
model %>% compile(optimizer = 'adam', loss = 'mse') #1. mse or 2. mae


history <-model %>% fit(x, y, epochs = 100, batch_size = 5, sample_weight=weights) #, validation_split = 0.15) # callbacks = c(early_stopping)

predictions <- predict(model, x)

wb$Current_Value <- as.vector(predictions)
unstandardize <- function(x) {
  
  return (x * sdlogy + meanlogy)
}
wb$Current_Value <- exp(unstandardize(wb$Current_Value))
wb$estimate <- exp(unstandardize(y))

wb <- left_join(wb %>% dplyr::select(Sale_Id,Current_Value),
                w %>% dplyr::select(-Current_Value,-age,-area))

setDT(wb)
setcolorder(wb, as.character(names(data)))


# Write SQL update statement
conn <- odbcDriverConnect(connectionString)

sqlDrop(conn,"FileDataUpdated")
sqlSave(conn, wb,tablename="FileDataUpdated",rownames =FALSE)

sql<-"update FileData set FileData.Current_Value=FileDataUpdated.Current_Value output inserted.Current_Value from FileData inner join FileDataUpdated on FileData.Sale_Id = FileDataUpdated.Sale_Id"
resultset <- sqlQuery(conn,sql)
sqlDrop(conn,"FileDataUpdated")


# for (i in unique(as.character(wb$artworkid))) {
# 
#   plot <- plot_ly(wb %>% filter(artworkid == i), x = ~YearSold,y = ~estimate,name = "Predicted Price", type = 'scatter', mode = 'lines+markers') %>%
#   layout(xaxis = list(title = "Year Sold"),
#          yaxis = list (title = "",ticksuffix= ' NZD'))
# 
# htmlwidgets::saveWidget(plot, paste0("C:/Migration/PERSO/Simon Bowerbank/",i,".html"))
# }

