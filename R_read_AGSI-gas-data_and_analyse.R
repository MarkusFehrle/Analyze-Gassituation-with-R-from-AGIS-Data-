

library(curl)
library(ggplot2)

# May set your working directory here
# setwd("C:/Users/Entwickler/Documents/R/Gas")

h <- new_handle()
# handle_setopt(h,httpauth = 1L)

# put here your API KEY
# you can get it from https://agsi.gie.eu
# or may do not hardcode it here and read from a seerate file
handle_setheaders(h, "x-key" = "Y O U R  A P I  KEY")


d=data.frame()

# Fetching data

for(year in 2011:2022) 
{
  yearS= toString(year)
  url1 = "https://agsi.gie.eu/api?country=at&from="
  url2 = "-01-01&to="
  url3 = "-6-30&size=300"
  url = paste(url1,yearS,url2,yearS,url3,sep = "")
  
  print (paste("Fetching year:", yearS))
  
  req <- curl_fetch_memory(url, h )
  
  #str(req)
  parse_headers(req$headers)
  a=jsonlite::prettify(rawToChar(req$content))
  options(max.print = 1000000)
  jsonlite::fromJSON(a) 
  c = jsonlite::fromJSON(a) 
  d2 = as.data.frame(c)
  d <- rbind (d,d2)
  
  url2 = "-07-01&to="
  url3 = "-12-31&size=300"
  
  url = paste(url1,yearS,url2,yearS,url3,sep = "")
  
  req <- curl_fetch_memory(url, h )
  
  #str(req)
  parse_headers(req$headers)
  a=jsonlite::prettify(rawToChar(req$content))
  options(max.print = 1000000)
  jsonlite::fromJSON(a) 
  c = jsonlite::fromJSON(a) 
  d2 = as.data.frame(c)
  d <- rbind (d,d2)
  
}

d <- d[order (d$data.gasDayStart),]
today <- Sys.Date()
datum = format(today, format="%d %b %Y")


 req <- curl_fetch_memory("https://agsi.gie.eu/api?country=de&from=2005-01-01&to=2022-7-31", h )

str(req)
parse_headers(req$headers)
 a=jsonlite::prettify(rawToChar(req$content))
options(max.print = 1000000)
 jsonlite::fromJSON(a) 
 c = jsonlite::fromJSON(a) 
 d1 = as.data.frame(c)

Titel = paste ("Gasfüllstand D komplett (Prozent), ",as.character( datum))
p <- ggplot (data=d,aes(x=as.Date(data.gasDayStart), y= as.double(data.full))) +  geom_point(colour = 'red', size = 1) + geom_line() +
# p + geom_line(aes(y= as.double(data.gasInStorage))) 
#p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
 labs(title =Titel, 
     subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
     y = "F Ü L L S T A N D   [%]", x = "J A H R")
p
ggsave(p, filename = paste(Titel,".jpg") )

Titel = paste ("Gasfüllstand D komplett (TWh), ",as.character( datum))
p <- ggplot (data=d,aes(x=as.Date(data.gasDayStart), y= as.double(data.gasInStorage))) +  geom_point(colour = 'blue', size = 1) + geom_line() +
  # p + geom_line(aes(y= as.double(data.gasInStorage))) 
  #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
  labs(title = Titel, 
       subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
       y = "F Ü L L S T A N D  [TWh]", x =  "J A H R")
p
ggsave(p, filename = paste(Titel,".jpg") )
 

dp = d
 

Titel = paste ("Einspeisung D komplett pro Tag (GWh), ",as.character( datum))
 p <- ggplot (data=dp,aes(x=as.Date(data.gasDayStart), y= as.double(data.injection))) +  geom_bar(stat = "identity",fill="#D55E00")  +
   # geom_smooth(method = "lm", formula = y ~ poly(x, 4),aes(color="poly(x, 4)"))  + 
   # geom_smooth(method = "lm", formula = (y ~ x + I(x^2) + I(x^3)), aes (color = "x + I(x^2) + I(x^3)")) +
   # geom_smooth( aes(color="LOESS Default Parameters")) +
   # p + geom_line(aes(y= as.double(data.gasInStorage))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E I N S P E I S U N G [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titel,".jpg") )
 
 Titel = paste ("Entnahme D komplett pro Tag (TWh), ",as.character( datum))
 p <- ggplot (data=dp,aes(x=as.Date(data.gasDayStart), y= as.double(data.withdrawal)))  + geom_bar(stat = "identity",fill="steelblue") +
   # geom_smooth(method = "lm", formula = y ~ poly(x, 4),aes(color="poly(x, 4)"))  + 
   # geom_smooth(method = "lm", formula = (y ~ x + I(x^2) + I(x^3)), aes (color = "x + I(x^2) + I(x^3)")) +
   # geom_smooth( aes(color="LOESS Default Parameters")) +
   
   # p + geom_line(aes(y= as.double(data.gasInStorage))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E N T N A H M E [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titel,".jpg") )
 
 
 dp = subset(d,data.gasDayStart >= "2020-01-01"  )
 
 
 # p <- ggplot (data=dp,aes(x=as.Date(data.gasDayStart), y= as.double(data.injection))) +  geom_bar(stat = "identity",fill="#D55E00")  +
 #   geom_smooth(method = "lm", formula = y ~ poly(x, 4),aes(color="poly(x, 4)"))  + 
 #   # geom_smooth(method = "lm", formula = (y ~ x + I(x^2) + I(x^3)), aes (color = "x + I(x^2) + I(x^3)")) +
 #   geom_smooth( aes(color="LOESS Default Parameters")) +
 #   
 #   
 #   # p + geom_line(aes(y= as.double(data.gasInStorage))) 
 #   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
 #   labs(title = "Einspeisung pro Tag  Deutschland komplett", 
 #        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
 #        y = "E I N S P E I S U N G [GWh]", x = "J A H R")
 # p
 # ggsave(p, filename = "Einspeisung_GWh_D_ab_1-1-2022.jpg")
 # 
 # p <- ggplot (data=dp,aes(x=as.Date(data.gasDayStart), y= as.double(data.withdrawal)))  + geom_bar(stat = "identity",fill="steelblue") +
 #   geom_smooth(method = "lm", formula = y ~ poly(x, 4),aes(color="poly(x, 4)"))  + 
 #   # FROM TIDYQUANT geom_ma(ma_fun = SMA, n = 30) +    
 #   # geom_smooth(method = "lm", formula = (y ~ x + I(x^2) + I(x^3)), aes (color = "x + I(x^2) + I(x^3)")) +
 #   geom_smooth( aes(color="LOESS Default Parameters")) +
 #   
 #   # p + geom_line(aes(y= as.double(data.gasInStorage))) 
 #   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
 #   labs(title = "Entnahme  Deutschland Komplett", 
 #        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
 #        y = "E N T N A H M E [GWh]", x = "J A H R")
 # p
 # ggsave(p, filename = "Entnahme_GWh_Dab_1-1-2022.jpg")
 
 dp = d
 #dp = subset(d,data.gasDayStart >= "2022-01-01"  )
 # samples für gleitenden Durchschnitt
 samples <- 90
 d1 <- dp 
 

 d1['data.injection_average'] <- 0
 d1['data.withdrawal_average'] <- 0
 
 for (i in (samples + 1) : nrow(d1)) {
   for (j in 1:samples){
     d1[i,"data.injection_average"] =  d1[i,"data.injection_average"] + ( as.numeric(d1[i-j,"data.injection"] ) / samples)
     d1[i,"data.withdrawal_average"] =  d1[i,"data.withdrawal_average"] + ( as.numeric(d1[i-j,"data.withdrawal"] ) / samples)
   }
 }
 

 
 Titel = paste("Einspeisung pro Tag D, gleitender Durchschnitt: ", as.character(samples),"Tage ",as.character( datum))
 Titelshort = paste ("Einsp-D-GD", as.character(samples),"Tage-",as.character( datum))
 p <- ggplot (data=d1,aes(x=as.Date(data.gasDayStart))) +
   geom_bar(aes (y =  as.double (data.injection)),  stat = "identity",fill="steelblue") + 
   geom_line(aes ( y = data.injection_average ),colour="darkgreen",size = 2) + 
    # + geom_line(aes(y= as.double(data.injection))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E I N S P E I S U N G [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titelshort,".jpg") )
 
 Titel = paste("Entnahme pro Tag D, gleitender Durchschnitt: ", as.character(samples),"Tage ",as.character( datum))
 Titelshort = paste ("Entna-D-GD", as.character(samples),"Tage-",as.character( datum))
 p <- ggplot (data=d1,aes(x=as.Date(data.gasDayStart))) +
   geom_bar(aes (y =  as.double (data.withdrawal)),  stat = "identity",fill="steelblue") + 
   geom_line(aes ( y = data.withdrawal_average ),colour="firebrick",size = 1.2) + 
   # + geom_line(aes(y= as.double(data.injection))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E N T N A H M E [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titelshort,".jpg") )
 
 dp = subset(d1,data.gasDayStart >= "2020-01-01"  )
 
 
 Titel = paste("Einspeisung pro Tag D, gleitender Durchschnitt: ", as.character(samples),"Tage, ",as.character( datum))
 Titelshort = paste ("Einsp-D-GD", as.character(samples),"Tage-Zoom",as.character( datum))
 p <- ggplot (data=dp,aes(x=as.Date(data.gasDayStart))) +
   geom_bar(aes (y =  as.double (data.injection)),  stat = "identity",fill="steelblue") + 
   geom_line(aes ( y = data.injection_average ),colour="darkgreen",size = 2) + 
   # + geom_line(aes(y= as.double(data.injection))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E I N S P E I S U N G [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titelshort,".jpg") )
 
 Titel = paste("Entnahme pro Tag D, gleitender Durchschnitt: ", as.character(samples),"Tage,  ",as.character( datum))
 Titelshort = paste ("Entna-D-GD", as.character(samples),"Tage-Zoom",as.character( datum))
 p <- ggplot (data=dp,aes(x=as.Date(data.gasDayStart))) +
   geom_bar(aes (y =  as.double (data.withdrawal)),  stat = "identity",fill="steelblue") + 
   geom_line(aes ( y = data.withdrawal_average ),colour="firebrick",size = 2) + 
   # + geom_line(aes(y= as.double(data.injection))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E N T N A H M E [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titelshort,".jpg") )

 dp = d
 #dp = subset(d,data.gasDayStart >= "2022-01-01"  )
 # samples für gleitenden Durchschnitt
 samples <- 30
 d1 <- dp 
 
 
 d1['data.injection_average'] <- 0
 d1['data.withdrawal_average'] <- 0
 
 for (i in (samples + 1) : nrow(d1)) {
   for (j in 1:samples){
     d1[i,"data.injection_average"] =  d1[i,"data.injection_average"] + ( as.numeric(d1[i-j,"data.injection"] ) / samples)
     d1[i,"data.withdrawal_average"] =  d1[i,"data.withdrawal_average"] + ( as.numeric(d1[i-j,"data.withdrawal"] ) / samples)
   }
 }
 
 
 
 Titel = paste("Einspeisung pro Tag D, gleitender Durchschnitt: ", as.character(samples),"Tage ",as.character( datum))
 Titelshort = paste ("Einsp-D-GD", as.character(samples),"Tage-",as.character( datum))
 p <- ggplot (data=d1,aes(x=as.Date(data.gasDayStart))) +
   geom_bar(aes (y =  as.double (data.injection)),  stat = "identity",fill="steelblue") + 
   geom_line(aes ( y = data.injection_average ),colour="darkgreen",size = 2) + 
   # + geom_line(aes(y= as.double(data.injection))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E I N S P E I S U N G [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titelshort,".jpg") )
 
 Titel = paste("Entnahme pro Tag D, gleitender Durchschnitt: ", as.character(samples),"Tage ",as.character( datum))
 Titelshort = paste ("Entna-D-GD", as.character(samples),"Tage-",as.character( datum))
 p <- ggplot (data=d1,aes(x=as.Date(data.gasDayStart))) +
   geom_bar(aes (y =  as.double (data.withdrawal)),  stat = "identity",fill="steelblue") + 
   geom_line(aes ( y = data.withdrawal_average ),colour="firebrick",size = 1.2) + 
   # + geom_line(aes(y= as.double(data.injection))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E N T N A H M E [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titelshort,".jpg") )
 
 dp = subset(d1,data.gasDayStart >= "2020-01-01"  )
 
 
 Titel = paste("Einspeisung pro Tag D, gleitender Durchschnitt: ", as.character(samples),"Tage, ",as.character( datum))
 Titelshort = paste ("Einsp-D-GD", as.character(samples),"Tage-Zoom-",as.character( datum))
 p <- ggplot (data=dp,aes(x=as.Date(data.gasDayStart))) +
   geom_bar(aes (y =  as.double (data.injection)),  stat = "identity",fill="steelblue") + 
   geom_line(aes ( y = data.injection_average ),colour="darkgreen",size = 2) + 
   # + geom_line(aes(y= as.double(data.injection))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E I N S P E I S U N G [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titelshort,".jpg") )
 
 Titel = paste("Entnahme pro Tag D, gleitender Durchschnitt: ", as.character(samples),"Tage,  ",as.character( datum))
 Titelshort = paste ("Entna-D-GD", as.character(samples),"Tage-Zoom",as.character( datum))
 p <- ggplot (data=dp,aes(x=as.Date(data.gasDayStart))) +
   geom_bar(aes (y =  as.double (data.withdrawal)),  stat = "identity",fill="steelblue") + 
   geom_line(aes ( y = data.withdrawal_average ),colour="firebrick",size = 2) + 
   # + geom_line(aes(y= as.double(data.injection))) 
   #p +  geom_point(colour = 'blue', size = 2,aes(y= as.double(data.gasInStorage)) )
   labs(title = Titel, 
        subtitle = "Data provider: agsi.gie.eu  -   (c) Image Processing & Engineering",
        y = "E N T N A H M E [GWh]", x = "J A H R")
 p
 ggsave(p, filename = paste(Titelshort,".jpg") )


 