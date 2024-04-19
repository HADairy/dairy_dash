# leaflet maps

#### leaflet map prep 
office_icon = makeIcon("images/agridat.png","images/agridat.png", 50, 50)
dairy_icon = makeIcon("images/dairy.png", "images/dairy.png", 50, 50 )
robot_icon = makeIcon("images/robot.png","images/robot.png", 50, 50)
beef_icon = makeIcon("images/beef.png","images/beef.png", 50, 50)
sheep_icon = makeIcon("images/sheep.png","images/sheep.png", 50, 50)
pig_icon = makeIcon("images/pig.png","images/pig.png", 50, 50)
uni_icon = makeIcon("images/graduation-hat.png","images/graduation-hat.png",50,50)




dairy_popup <- paste((HTML("<strong>Watch this space! </strong> <br> 
                     Dairy data coming soon")))



beef_popup <- paste((HTML("<strong>Watch this space! </strong> </br> 
                     Beef data coming soon")))




sheep_popup <- paste((HTML("<strong>Watch this space! </strong> </br> 
                     Sheep data coming soon")))




pig_popup <- paste((HTML("<strong>Watch this space! </strong> </br> 
                     Pig data coming soon")))


office_popup <- paste("Click the animal icons for info!")



map <- leaflet() %>%
  fitBounds(lng1 = -2.435130, lat1 = 52.779020, lng2 = -2.429167, lat2 = 52.781648 ) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-2.427, lat=52.779, popup="Harper Adams University", icon = uni_icon) %>%
  addMarkers(lng=-2.434319, lat=52.780052, popup=dairy_popup, icon = dairy_icon ) %>%
  #addMarkers(lng=-2.432867, lat=52.780706, popup="Robot dairy unit", icon = robot_icon ) %>%
  addMarkers(lng=-2.430173, lat=52.779969, popup=pig_popup, icon = pig_icon) %>%
  addMarkers(lng=-2.430597, lat=52.780818, popup=sheep_popup, icon = sheep_icon) %>%
  addMarkers(lng=-2.430253, lat=52.780475, popup=beef_popup, icon = beef_icon) %>%
  
  addMarkers(lng=-2.432260, 
             lat=52.779504, 
             popup = office_popup, 
             icon = office_icon
  ) %>%
  
  
  # addMarkers(lng = -2.433879, lat = 52.780927, label = "Click on an icon for summary information", labelOptions = labelOptions(noHide = T)) %>%
  addScaleBar(position = "bottomright") 