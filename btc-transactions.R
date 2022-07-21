library(tidyverse)
library(lubridate)
library(riingo)
library(httr)
library(rvest)
library(tidyquant)
library(png)
library(grid) # If you want to add a logo to the chart


# Free API Key to pull stock / crypto data: https://api.tiingo.com/
tidyquant::tiingo_api_key('your_key_here')

##==================================================================================
## Pull Bitcoin price data.
##==================================================================================

price = riingo::riingo_crypto_prices('BTCUSDT', start_date = '2021-11-01', end_date = as.character(Sys.Date()+1)) %>% rename(time=date)

##==================================================================================
## Pull Bitcoin address transaction data; extract data from html.
##==================================================================================

bitcoin.address = '1P5ZEDWTKTFGxQjZphgWPQUpe554WKDfHQ'

url = paste0('https://bitinfocharts.com/bitcoin/address/',bitcoin.address)

transactions = GET(url) %>% 
  content() %>%
  html_nodes('table') %>% html_table() %>% .[[3]] %>% 
  select(time=Time, amount=Amount, price=`Balance, USD @ Price`) %>% 
  separate(price, into=c(NA,'price'), sep='@', remove=F, fill='right') %>% 
  mutate(price = trimws(price)) %>% 
  separate(amount, into=c(NA,'usd'), sep='\\(', remove=F, fill='right') %>% 
  mutate(usd = as.numeric(gsub('[^0-9\\.]','',usd))) %>% 
  mutate(type = ifelse(grepl('-',amount),'Sell','Buy')) %>% 
  filter(time!='...more...') %>% 
  mutate(time = as_datetime(time)) %>%
  riingo::convert_to_local_time(., tz='America/New_York') %>%
  separate(amount,into=c('amount',NA),sep=' ', remove=F, extra='drop') %>%
  mutate(amount = abs(as.numeric(gsub('[^0-9\\.]','',amount)))) %>%
  filter(amount>=100)
  # Filter to relevant data only; note that if you want to include buys, 
  # filter out amount<1 BTC as random people send pennies to these whales.

##==================================================================================
## Summarise transactions by day.
##==================================================================================

transactions.by.day = transactions %>% 
  mutate(time = floor_date(time, 'day')) %>%
  group_by(time, type) %>% 
  summarise(amount=sum(amount)) %>% 
  ungroup

##==================================================================================
## Custom Theme.
## Will default to system default if you don't have Montserrat installed, or
## install Montserrat font family from fonts.google.com/download?family=Montserrat. 
##==================================================================================

custom.theme = theme(legend.position = 'top',
      legend.title = element_blank(),
      text=element_text(family = "Montserrat", color='#ffffff'),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size=11, colour="white"),
      axis.ticks = element_blank(),
      plot.caption = element_text(family = "Montserrat", size = 7, color='#4f4f4f', margin=margin(t=10,b=15)),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_line(linetype=2, size=0.2),
      plot.title = element_text(family = "Montserrat", face='bold', size=24, color='#ffffff', margin = margin(t=5, b = 10, unit = "pt")),
      plot.subtitle = element_text(family = "Montserrat", size=9, color='#ffffff', margin = margin(t=5, b = 5, unit = "pt")),
      axis.text.y=element_text(size=9, colour="white"),
      axis.text.x=element_text(size=10, colour="white"),
      plot.margin = unit(c(1,1.4,1,1), "lines"),
      plot.background=element_rect(fill="black"),
      panel.background=element_rect(fill="#05051c", color='#05051c')
)

##==================================================================================
## Initialize png save.
##==================================================================================

png(filename=paste0(bitcoin.address,'-',Sys.Date(),'.png'),  width=7.5, height=4.8, units='in', res=600)

##==================================================================================
## Plot.
##==================================================================================

ggplot(price, aes(x=time, y=close))+
  ggtitle('Bitcoin Whale Unloading')+
  labs(captions=paste0('whale address: ',bitcoin.address))+
  scale_x_datetime(expand = c(0, 0), limits=c(min(price$time),max(price$time)+days(20)), date_breaks='1 month', date_labels='%b\n%Y')+
  scale_y_continuous(expand=c(0,0), breaks=seq(15000,70000,3000), limits=c(13000,72000), labels=function(x){scales::number(x,big.mark=',')}, oob=scales::oob_keep)+
  geom_vline(key_glyph='point', data=transactions.by.day, aes(xintercept=time, color=type), size=0.1, alpha=1, show.legend = T) +
  geom_point(data=transactions.by.day, aes(x=time, y=44000, color=type, size=amount, alpha=amount, show.legend = F)) +
  geom_candlestick(key_glyph='point', data=price, size=0.2, inherit.aes=F, aes(x=time, open = open, high = high, low = low, close = close), alpha=1, colour_down='#ED474A', colour_up='#72EB48', fill_down='#ED474A', fill_up='#72EB48', na.rm=T, show.legend=FALSE)+
  scale_color_manual(labels=c(' tick up',' tick down',"Buy","Sell"), values=c('#72EB48','#ED474A','lightblue','orange'),guide = guide_legend(size=2))+
  scale_size_continuous(guide = 'none',range = c(1, 50))+
  scale_alpha_continuous(range=c(0.7, 0.35))+ # Want biggest amounts to be more transparent
  guides(size = "none",fill="none", alpha="none", color = guide_legend(override.aes = list(shape=18,size=5,alpha=1), alpha=1,size=10))+
  custom.theme

##==================================================================================
## Add custom logo / watermark.
##==================================================================================

grid::grid.raster(png::readPNG('logo.png'), interpolate = TRUE, just = c('right', 'bottom'), width=unit(5,'lines'),x = unit(0.965,"npc"), y = unit(0.015,"npc"),hjust = 0.965, vjust = 0.015)

##==================================================================================
## Save png.
##==================================================================================

dev.off()


