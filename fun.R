library(zipcode)
library(rvest)

data("zipcode")


getNearestStores<-function(x){
  if(is.null(x)||!nzchar(x)){x=99354}
    if(is.character(x)){
      x = gsub("\\s+","",x)
      store_finder_url<-paste("http://api.walmartlabs.com/v1/stores?apiKey=hmz6gruuzam3wbaacgh76gju&city=",x,"&format=json",sep="")
    }else{
      store_finder_url<-paste("http://api.walmartlabs.com/v1/stores?apiKey=hmz6gruuzam3wbaacgh76gju&zip=",x,"&format=json",sep="") 
    }
  store_txt=read_html(store_finder_url)%>% html_text()
  store=as.data.frame(regmatches(store_txt, gregexpr("(?<=,\"name\":\").*?(?=\")", store_txt, perl=TRUE)))
  if(length(store[,1])!=0){
  store_zip = as.data.frame(regmatches(store_txt, gregexpr("(?<=\"zip\":\").*?(?=\")", store_txt, perl=TRUE)))
  storeID = as.data.frame(regmatches(store_txt, gregexpr("(?<=\"no\":).*?(?=,\")", store_txt, perl=TRUE)))
  cord = as.data.frame(regmatches(store_txt, gregexpr("(?<=\"coordinates\":\\[).*?(?=\\],\")", store_txt, perl=TRUE)))
  cord = data.frame(do.call('rbind', strsplit(as.character(cord[,1]),',',fixed=TRUE)))
  names(cord)=c("long","lat")
  cord$long = as.numeric(as.character(cord$long))
  cord$lat=as.numeric(as.character(cord$lat))
  storeAdd = as.data.frame(regmatches(store_txt, gregexpr("(?<=\"streetAddress\":\").*?(?=\",\")", store_txt, perl=TRUE)))
  storePh = as.data.frame(regmatches(store_txt, gregexpr("(?<=\"phoneNumber\":\").*?(?=\",\")", store_txt, perl=TRUE)))
  store=cbind(store,store_zip,storeID,cord,storeAdd,storePh)
  names(store)=c("name","zip","storeID","long","lat","Address","Phone")
  store$zip=as.numeric(as.character(store$zip))
  store$storeID=as.numeric(as.character(store$storeID))
  store$name=as.character(store$name)
  store=store[!duplicated(store$name),]
  return(store)
  }
  }

upc=function(x){
  xx=sum(as.numeric(substring(x,c(1,3,5,7,9,11),c(1,3,5,7,9,11))))*3 +
    + sum(as.numeric(substring(x,c(2,4,6,8,10),c(2,4,6,8,10))))
  a= xx %% 10
  a = ifelse(a==0,a,10-a)
}

getInstoreItems<-function(storeID,item){
  item = gsub("\\s+","",item)
  input = paste("http://search.mobile.walmart.com/search?store=",storeID,"&query=",item,"&size=50",sep="")
  txt=read_html(input)%>% html_text()
  Product=as.data.frame(regmatches(txt, gregexpr("(?<=},\"name\":\").*?(?=\",\"department\")", txt, perl=TRUE)))
  ItemID = as.data.frame(regmatches(txt, gregexpr("(?<=\"WWWItemId\":\").*?(?=\")", txt, perl=TRUE)))
  UPC = as.data.frame(regmatches(txt, gregexpr("(?<=\"upc\":\").*?(?=\",)", txt, perl=TRUE)))
  images = as.data.frame(regmatches(txt, gregexpr("(?<=\"thumbnailUrl\":\").*?(?=\",)", txt, perl=TRUE)))
  UPC[] <- lapply(UPC, as.character)
  UPC$n = apply(UPC,1,nchar)
  UPC[,1] = ifelse(UPC$n==10,paste("0",UPC[,1],sep=""),UPC[,1])
  UPC=as.data.frame(UPC[,1])
  UPC$chk = apply(UPC,1,upc)
  UPC$new = paste(UPC$UPC,UPC$chk,sep="")
  UPC=as.data.frame(UPC$new)
  qty = as.data.frame(regmatches(txt, gregexpr("(?<=\"quantity\":).*?(?=,\")", txt, perl=TRUE)))
  qty[]=lapply(qty, function(x){as.numeric(as.character(x))})
  aisle=as.data.frame(regmatches(txt, gregexpr("(?<=\"aisle\":\\[).*?(?=\\],\")", txt, perl=TRUE)))
  price=as.data.frame(regmatches(txt, gregexpr("(?<=\"price\":{).*?(?=\"currencyUnit\")", txt, perl=TRUE)))
  price[]=lapply(price, function(x) regmatches(x, gregexpr("(?<=\"priceInCents\":).*?(?=,)", x, perl=TRUE)))
  price[] = lapply(price, as.character)
  price[,1]=ifelse(price[,1]=="character(0)","?",price[,1])
  price = as.data.frame(paste("$",substr(price[,1], 1, nchar(price[,1])-2),".",substr(price[,1],nchar(price[,1])-1,nchar(price[,1])),sep=""))
  #paste("http://api.walmartlabs.com/v1/search?apiKey=hmz6gruuzam3wbaacgh76gju&&query=",x,"&format=json",sep="")
  df = cbind(Product,ItemID,UPC,qty,aisle,price,images)
  names(df)=c("Product","ItemID","UPC","Qty","Aisle","Price","Image")
 # df=df[order(df$qty),]
  return(df)
}
