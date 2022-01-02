## market profile

library(ggplot2)

supportFILE = 'support_crypto_scores.R'
source(supportFILE)


make_mkt_profile_data = function(xts_in, histBins = 40){
  Lsub = xts_in$Low  
  Hsub = xts_in$High 
  last_price = as.numeric(last(xts_in$Close))
  
  breaks  = pretty(seq(min(Lsub), max(Hsub), length=histBins), histBins)
  last_price_bin = max(which(breaks < last_price))
  histMat = cbind(Breaks = breaks, Count = 0, LastPrice =NA) #, Count_Last= 0)

  for(b in 1:(length(breaks)-1)){
    print(b)  
    for(j in 1:length(Lsub)){
      toAdd = as.numeric(Hsub[j]>breaks[b] & Lsub[j]<=breaks[b+1])
      histMat[b, "Count"] = histMat[b, "Count"] + toAdd
      
      
    }
    
  }
  histMat[last_price_bin, "LastPrice"] =  histMat[last_price_bin, "Count"]+2

  df_ret = data.frame(histMat)
  return(df_ret)
  
}

ggplot_mkt_profile = function(df_plot, title1, subTitle, xLab_Text) {
  
  p<-ggplot(data=df_plot, aes(x=Breaks, y=Count)) +
    geom_point(aes(x = Breaks, y = LastPrice), colour = "black", size =4)+ 
    geom_bar(stat="identity", fill="#E4BA12") + coord_flip() +
    theme(axis.title.x=element_blank(), 
          legend.position = 'none', #bottom
          #legend.key.width = unit(4, 'cm'), 
          #legend.title = element_blank(),
          #legend.text = element_blank(),
          plot.margin = margin(1, 1, 1, 1, "cm"),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()
          #title = element_text(size = 14,family= 'Avenir Next')
    ) +
    labs(title=title1, 
         subtitle=subTitle ,
         x = xLab_Text) +
    scale_y_continuous(limits = c(0,max(df_plot$Count)*1.3), expand = c(0, 0))
  
  #axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
  
  return(p)
  
}

doTesting = FALSE

if(doTesting){

btc_60MIN = get_crypto_watch(ticker = 'btcusd',
                             exchange = 'coinbase-pro',
                             timeperiod = 3600)


##prepare Data in
month1_rows = which(as.Date(index(btc_60MIN)) > (Sys.Date() -30 ))
btc_60MIN_1M = btc_60MIN[month1_rows, ]
last_price_text = as.numeric(last(btc_60MIN_1M$Close))

##make market profile chart data
btc_mkt_profile  = make_mkt_profile_data(btc_60MIN_1M) 

## make the ggplot chart
p_mkt_profile_btc_60MIN = ggplot_mkt_profile(btc_mkt_profile, 
                                             title1 = "Bitcoin 30Day Histogram",
                                             subTitle = paste0("Last Price $", last_price_text),
                                             xLab_Text = "Price USD")

## ggplot object
p_mkt_profile_btc_60MIN

ggsave( "btc_market_profile.png", p_mkt_profile_btc_60MIN, width = 6, height = 9, units = "in")

}
