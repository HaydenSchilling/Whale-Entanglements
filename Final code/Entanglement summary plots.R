# Initial entanglement plotting

library(tidyverse)

mydataE <- read_csv("Entanglements clean.csv", lazy = F)
mydataE_long <- mydataE %>% pivot_longer(`Month 1`:`Month 12`, names_to = "Month", values_to = "Whales_Caught") %>%
  mutate(Month = as.numeric(str_remove(Month, "Month ")))

ggplot(mydataE_long, aes(Month, Whales_Caught, col=as.character(Year), group=Year)) + geom_line()


mydataE_long <- mydataE_long %>% group_by(Year) %>% mutate(Perc_caught = Whales_Caught/sum(Whales_Caught, na.rm=T)*100,
                                                           recent = (2021-Year)/30)

ggplot(mydataE_long, aes(Month, Perc_caught, col=as.character(Year), group=Year)) + geom_line() +
  theme_classic() + scale_x_continuous(breaks=seq(1:12)) + facet_wrap(~Year)

ggsave("Percent Whales Caught each month by year.png", dpi = 600, width=21, height=14.8, units="cm")

all_years <- ggplot(mydataE_long, aes(Month, Whales_Caught, col=Year,group=Year)) + geom_line(aes()) +
  theme_classic() + scale_x_continuous(breaks=seq(1:12)) +
  scale_colour_viridis_c(direction = -1)+
  ylab("Monthly Observed\nEntanglements")+
  theme(legend.title = element_text(face="bold", size=12),
        axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        legend.position = c(0.2,0.7))
all_years

ggplot(mydataE_long, aes(Month, Whales_Caught, col=as.character(Year), group=Year)) + geom_line() +
  theme_classic() + scale_x_continuous(breaks=seq(1:12)) + facet_wrap(~Year)

ggsave("Number Whales Caught each month by year.png", dpi = 600, width=21, height=14.8, units="cm")

dat_sum <- mydataE_long %>% group_by(Month) %>% summarise(Mean_perc = mean(Perc_caught, na.rm=T),
                                                          sd_perc = sd(Perc_caught, na.rm=T),
                                                          n=n(),
                                                          SE_perc = sd_perc/sqrt(n))
p1 <- ggplot(dat_sum, aes(Month, Mean_perc)) + geom_line() +
  theme_classic() + scale_x_continuous(breaks=seq(1:12)) +
  geom_errorbar(aes(ymin=Mean_perc-SE_perc, ymax=Mean_perc+SE_perc)) +
  theme(axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12)) +
  ylab("Percent Whales\nEntangled (±SE)")
p1

ggsave("Percent Whales Caught each month summary.png", dpi = 600, width=21, height=14.8, units="cm")


Annual_Caught <- mydataE_long %>% group_by(Year) %>% summarise(Total_caught = sum(Whales_Caught),
                                                               Popn = mean(Rel_Abund))
p2 <- ggplot(Annual_Caught, aes(Year, Total_caught)) + geom_line() +
  theme_classic()  + scale_x_continuous(breaks=seq(1992,2020,4))+
  geom_line(aes(y=Popn/1000), col="red", linetype=2)+
  scale_y_continuous(sec.axis = sec_axis(~.*1000, name="Relative Whale Abundance"))+
  theme(axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        axis.title.y.right = element_text(colour = "red"),
        axis.text.y.right = element_text(colour="red")) +
  ylab("Annual Whale\nEntanglements")
p2

library(patchwork)
p2/(p1+all_years)+plot_annotation(tag_levels = "a")  + plot_layout(nrow=2)

ggsave("Figure Entanglement Summary_3_panel.png", dpi=600, width=21, height=14.8, units="cm")
ggsave("Figure Entanglement Summary_3_panel.pdf", dpi=600, width=21, height=14.8, units="cm")
