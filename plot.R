library(ggplot2)
library(dplyr)
snpraw <- read.csv("./all_stocks_5yr.csv",stringsAsFactors = F)
snpraw$date <- as.Date(snpraw$date,format = "%m/%d/%Y")
snpraw$year_month <- format(as.Date(snpraw$date), "%Y-%m")


na_sum <- function(x)
{
  if(all(is.na(x))) val <- sum(x/100000,na.rm=F)
  if(!all(is.na(x))) val <- sum(x/100000,na.rm=T)
  return(val)
}


snp <- snpraw %>% group_by(year_month,Name) %>% summarise(mon_vol = na_sum(volume/100000))
snp_heat <- snp %>% group_by(year_month) %>%  
  mutate(ratio_vol = mon_vol / sum(mon_vol)) %>%
  as.data.frame()

summary(snp_heat$ratio_vol)

snp_heat2 <- snp_heat %>%
  # convert ticker to factor and reverse order of levels
  mutate(Name=factor(Name,levels=rev(sort(unique(Name))))) %>%
  # create a new variable from ratio
  mutate(ratiofactor=cut(ratio_vol,breaks=c(0,5e-4,1e-3,2e-3,2.1e-3,2.5e-3,1),
                         labels=c("<0.0005","0.0005 - 0.001","0.001-0.002","0.002-0.0021","0.0021-0.0025",">0.0025"))) %>%
  # change level order
  mutate(ratiofactor=factor(as.character(ratiofactor),levels=rev(levels(ratiofactor))))


textcol <- "grey40"

png(filename="gplot_lowres.png",height=20,width=9,res=500,units="in")
ggplot(snp_heat2,aes(x=year_month,y=Name,fill=ratiofactor))+
  geom_tile(colour="white",size=0.1)+
  guides(fill=guide_legend(title="Monthly volumn ratio"))+
  labs(x="",y="",title="Monthly volumn ratio for all stocks \nlisted in S&P 500")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#ddf1da"),na.value = "grey90")+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=4,colour=textcol,angle = 90),
        axis.text.y=element_text(size=3,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))
dev.off()

# updated 06.23.19
