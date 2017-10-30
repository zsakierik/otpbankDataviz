library(data.table)
library(dplyr)
library(plotly)
tranzakcio_havi <-  fread("otpbank_dataviz_verseny_adatok_199_321/tranzakcio_havi.csv")

ugyfel <- fread("otpbank_dataviz_verseny_adatok_199_321/ugyfel.csv")

ugyfelID <- 2259345
szegmens <- ugyfel[UGYFEL_ID == ugyfelID, .(UGYFEL_NEME,UGYFEL_CSALADI_ALLAPOT,UGYFEL_ISKOLAI_VEGZETTSEGE)]

szegmens_ugyfelID<- ugyfel[UGYFEL_NEME == szegmens$UGYFEL_NEME & 
                          UGYFEL_CSALADI_ALLAPOT == szegmens$UGYFEL_CSALADI_ALLAPOT & 
                          UGYFEL_ISKOLAI_VEGZETTSEGE == szegmens$UGYFEL_ISKOLAI_VEGZETTSEGE,UGYFEL_ID]




tranzakcio_havi$TRX_HONAP <- as.Date(tranzakcio_havi$TRX_HONAP, "%Y-%m-%d")
tranzakcio_havi$year  <- as.numeric(format(tranzakcio_havi$TRX_HONAP, "%Y"))
tranzakcio_havi$month  <- as.numeric(format(tranzakcio_havi$TRX_HONAP, "%m"))


mean_tranzakcio_havi <- tranzakcio_havi %>% group_by(UGYFEL_ID,MCC_CSOPORT) %>% 
                        summarise(sum = sum(TRX_OSSZEG)) %>% group_by(MCC_CSOPORT) %>%
                        summarise(sum = mean(sum))%>%
                        mutate(type = "mean")

mean_szegmens_tranzakcio_havi <- tranzakcio_havi %>% filter(UGYFEL_ID %in% szegmens_ugyfelID) %>% 
                                  group_by(UGYFEL_ID,MCC_CSOPORT) %>% 
                                  summarise(sum = sum(TRX_OSSZEG)) %>%
                                  group_by(MCC_CSOPORT) %>% summarise(sum = mean(sum)) %>% 
                                  mutate(type = "szegmens")

ugyfeltranzakcio_havi <- tranzakcio_havi[UGYFEL_ID == ugyfelID]
ugyfeltranzakcio_havi <- ugyfeltranzakcio_havi %>% group_by(MCC_CSOPORT) %>% summarise(sum = sum(TRX_OSSZEG)) %>% mutate(type = "sajat")

data <- full_join(mean_tranzakcio_havi,ugyfeltranzakcio_havi)
data <- full_join(data,mean_szegmens_tranzakcio_havi)

p <-plot_ly(data, r = ~sum, t = ~MCC_CSOPORT) %>% add_area(color = ~type)
layout(p,  orientation = 270)

install.packages("ggplot2")
install.packages("reshape2")
install.packages("plyr")
install.packages("devtools")
package_version("dplyr")

library(devtools)
library(ggplot2)
library(reshape2)
library(plyr)
library(treemapify)

library(ggplot2) 
install.packages("treemapify")
library(treemapify)

data2 <- tranzakcio_havi[UGYFEL_ID == ugyfelID] %>% group_by(MCC_CSOPORT,PARTNER) %>% summarise(sum = sum(TRX_OSSZEG), ferq = sum(TRX_DB))

ggplot(data2, aes(area = sum, fill = MCC_CSOPORT, label = PARTNER,
                subgroup = MCC_CSOPORT)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)



remove.packages("dplyr")
remove.packages("glue")
install.packages('dplyr', dependencies = TRUE)

write.csv(data, file = "data.csv")
data$MCC_CSOPORT <- as.factor(data$MCC_CSOPORT)
data$type <- as.factor(data$type)
data$sum <- round(data$sum)

data <- as.data.frame(data)

ggplot(data, aes(x = MCC_CSOPORT,y = sum)) + geom_bar(aes(fill = type), stat = "identity", position = "identity", alpha = 0.7, width = 0.8) 
+  coord_polar("y", start=0)




bp<- ggplot(data, aes(x="", y=sum, fill=type))+
geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

pie + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

 p <- ggplot(data, aes(x=MCC_CSOPORT, y=sum, fill=type)) +
    geom_bar( stat="identity", position = "identity", alpha = 0.7) + theme_light() +
    theme(axis.title.y=element_text(angle=0))
p <- p + theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))
p + coord_polar()  + aes(x=reorder(MCC_CSOPORT, sum)) +
    theme(axis.text.x = element_text(angle=-20)) 


