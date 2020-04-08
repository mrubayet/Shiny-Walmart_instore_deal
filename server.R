#library(shinyIncubator)
library(leaflet)
library(shiny)

shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
  leaflet() %>% 
    addTiles()%>%
    setView(lng =-119, lat = 47.3, zoom = 7)
  })
  

  output$city_selecter <- renderUI({
    selectInput("city", "Select City:",selected= NULL,choices =c("",unique(zipcode[zipcode$state==input$state,]$city)))
  })
    
   output$store_selecter <- renderUI({
     store_dat=as.data.frame(getNearestStores(input$city))
  if(length(store_dat)>0){
  selectInput("stores", "Select Store:",selected= NULL,choices =c("",store_dat$name))
  }else{
    selectInput("stores", "Select Store:","No Store Found!")
  }
  })
 
  
  observe({
      store_dat=as.data.frame(getNearestStores(input$city))
      if(length(store_dat)>0){
    store_select_dat = store_dat[store_dat$name==input$stores,]
   # isolate({
      new_zoom <- 12
  leafletProxy("map", data = store_select_dat) %>%
    clearMarkers() %>%
    addMarkers(~long, ~lat,
               popup = ~paste0("<strong>", name, "</strong><br/>",
                               "Address: ", Address, "<br />",
                               "Phone: ",Phone))%>%
    setView(lng=store_select_dat$long,lat=store_select_dat$lat, zoom = new_zoom)
    
 # })
      }
  })
  
    observe({
      if (input$submit== 0) {return()}
      isolate({
      store_dat=as.data.frame(getNearestStores(input$city))
        store_select_dat = store_dat[store_dat$name==input$stores,]
      item=input$text
      storeID = store_select_dat$storeID
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Retrieving Data.....", value = 0)
      df=getInstoreItems(storeID,item)
     # img1=df$Image
      ItemID = df$ItemID
      #img=paste('tags$img(src="',img1,'")',sep="")
      Link1 = paste("<a href='http://walmart.com/ip/",ItemID,"'target='_blank'>",df$Product,"</a>",sep="")
      Link2 = paste("<a href='http://www.walmart.com/store/",storeID,"/search?dept=3944&dept_name=Electronics&query=",df$UPC,"'target='_blank'>",df$UPC,"</a>",sep="")
      Link3 = paste("<a href='http://www.upcitemdb.com/upc/",df$UPC,"'target='_blank'>",df$Price,"</a>",sep="")
     # df$Image=img
      df$Product=Link1
      df$UPC = Link2
      df$Price=Link3
      df=df[,-ncol(df)]
      #return(df)
      })
      
      output$ProductTable <- renderDataTable({
        if (is.null(df)) {return()}
        df
      },escape = F,
      options = list(lengthMenu = c(10, 15, 20), pageLength = 5))
      
      
    })
  })
  
  
