library(ggplot2)
library(ggpubr)
show_line_types()

#===========================================================================

# Barcelona

#===========================================================================

# Plot 1 : COVID-19 Cases vs Maximum Temperature and Precipitation

BCN$`Covid-19 Cases` <- as.numeric(BCN$`Covid-19 Cases`)
BCN$`T. max` <- as.numeric(BCN$`T. max`)
BCN$`Precipitation L/m2` <- as.numeric(BCN$`Precipitation L/m2`)
str(BCN)

Barcelona1.plot <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`*20 ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Daily Max. Tempertature")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./20, name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Barcelona2.plot <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2`*15 ,color= "Precipitation L/m2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "Precipitation L/m2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Precipitation L/m2")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./15, name="Precipitation L/m2")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid_BCN1 <- plot_grid(Barcelona1.plot, Barcelona2.plot, labels = NULL, nrow = 1)

title <- ggdraw() + 
  draw_label(
    "Barcelona",
    fontface = 'bold',
    x = 0.35,
    hjust = -0.57
  ) 

plot_grid(
  title, plot_grid_BCN1,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
) 


# Plot 2 : COVID-19 Cases vs no2 i o3

BCN$`Covid-19 Cases` <- as.numeric(BCN$`Covid-19 Cases`)
BCN$`T. max` <- as.numeric(BCN$`T. max`)
BCN$`Precipitation L/m2` <- as.numeric(BCN$`Precipitation L/m2`)
str(BCN)

Barcelona3.plot <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2*40 ,color= "no2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground no2 µm/m3 level")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./20, name="Ground no2 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Barcelona4.plot <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3*25 ,color= "o3" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "o3" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground o3 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./15, name="Ground o3 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid_BCN2 <- plot_grid(Barcelona3.plot, Barcelona4.plot, labels = NULL, nrow = 1)

title <- ggdraw() + 
  draw_label(
    "Barcelona",
    fontface = 'bold',
    x = 0.35,
    hjust = -0.57
  ) 

plot_grid(
  title, plot_grid_BCN2,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)



#===========================================================================

# Bilbao

#===========================================================================

# Plot 1 : COVID-19 Cases vs temperature, no2 and so2

BILBAO$`Covid-19 Cases` <- as.numeric(BILBAO$`Covid-19 Cases`)
BILBAO$`T. max` <- as.numeric(BILBAO$`T. max`)
BILBAO$`Precipitation L/m2` <- as.numeric(BILBAO$`Precipitation L/m2`)
str(BILBAO)

Bilbao1.plot <-  BILBAO  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
            xmax = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`*5 ,color= "temp"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "temp" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Daily Max Temperature in C")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./5, name="Daily Max Temperature in C")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Bilbao") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Bilbao1.plot)

Bilbao2.plot <-  BILBAO  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
            xmax = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2*7 ,color= "no2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground no2 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./7, name="Ground no2 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("Bilbao") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Bilbao2.plot)

Bilbao3.plot <-  BILBAO  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
            xmax = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
            ymin = 0,  
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= so2*50 ,color= "so2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "so2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground so2 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./50, name="Ground so2 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("Bilbao") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Bilbao3.plot)




#===========================================================================

# Ceuta

#===========================================================================

# Plot 1 : COVID-19 Cases vs Maximum Temperature and Precipitation

CEUTA$`Covid-19 Cases` <- as.numeric(CEUTA$`Covid-19 Cases`)
CEUTA$`T. max` <- as.numeric(CEUTA$`T. max`)
CEUTA$`Precipitation L/m2` <- as.numeric(CEUTA$`Precipitation L/m2`)
str(CEUTA)

Ceuta1.plot <-  CEUTA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
            xmax = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max` ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Daily Max. Tempertature")) +
  labs(x="Date, 2021", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~., name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.45, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Ceuta2.plot <-  CEUTA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
            xmax = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2` ,color= "Precipitation L/m2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "Precipitation L/m2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Precipitation L/m2")) +
  labs(x="Date, 2021", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~., name="Precipitation L/m2")) +
  theme_bw() +
  theme(legend.position = c(0.45, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid_CEUTA1 <- plot_grid(Ceuta1.plot, Ceuta2.plot, labels = NULL, nrow = 1)

title <- ggdraw() + 
  draw_label(
    "Ceuta",
    fontface = 'bold',
    x = 0.39,
    hjust = -0.57
  ) 

plot_grid(
  title, plot_grid_CEUTA1,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
) 


# Plot 2 : COVID-19 Cases vs no2 

CEUTA$`Covid-19 Cases` <- as.numeric(CEUTA$`Covid-19 Cases`)
str(CEUTA)

Ceuta3.plot <-  CEUTA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
            xmax = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2 ,color= "no2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground no2 µm level")) +
  labs(x="Date, 2021", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~., name="Ground no2 µm level")) +
  theme_bw() +
  theme(legend.position = c(0.35, 0.88), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Ceuta") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Ceuta3.plot



#===========================================================================

# Donostia

#===========================================================================

# Plot 1 : COVID-19 Cases vs NO2

Donostia1.plot <-  DONOSTIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2020-05-13")]), 
            xmax = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2021-07-22")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2*7 ,color= "no2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground no2 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./7, name="Ground no2 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("Donostia") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2021-07-22")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Donostia1.plot)


#===========================================================================

# Girona

#===========================================================================

# Plot 1 : COVID-19 Cases vs NO2

Girona1.plot <-  GIRONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
            xmax = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2*7 ,color= "no2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground no2 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./7, name="Ground no2 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Girona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Girona1.plot)


# Plot 2 : COVID-19 Deceases vs Precipitation, PM10 and NO2
GIRONA$`Precipitation L/m2` <- as.numeric(GIRONA$`Precipitation L/m2`)
Girona2.plot <-  GIRONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
            xmax = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Deaths`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2`/10 ,color= "Precipitation L/m2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "Precipitation L/m2" = "firebrick"),
                     labels = c("COVID-19 Daily Deaths/100.000 inhabitants", "Precipitation L/m2")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*10, name="Precipitation L/m2")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Girona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Girona2.plot)

Girona3.plot <-  GIRONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
            xmax = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10*7 ,color= "pm10" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "pm10" = "firebrick"),
                     labels = c("COVID-19 Daily Deaths/100.000 inhabitants", "Ground pm10 µm level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./7, name="Ground pm10 µm level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Girona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Girona3.plot)

Girona4.plot <-  GIRONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
            xmax = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Deaths`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2/3 ,color= "no2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Deaths/100.000 inhabitants", "Ground no2 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*3, name="Ground no2 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Girona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Girona4.plot)


#===========================================================================

# Lleida

#===========================================================================

# Plot 1 : COVID-19 Cases vs NO2

Lleida1.plot <-  LLEIDA %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2020-05-13")]), 
            xmax = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2*7 ,color= "no2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground no2 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./7, name="Ground no2 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Lleida") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Lleida1.plot)

# Plot 2 : COVID-19 deaths vs Temperature
LLEIDA$`T. max` <- as.numeric(LLEIDA$`T. max` )
Lleida2.plot <-  LLEIDA %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2020-05-13")]), 
            xmax = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Deaths`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`/7 ,color= "temps" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "temps" = "firebrick"),
                     labels = c("COVID-19 Daily Deaths/100.000 inhabitants", "Maximum temperature in °C")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*7, name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Lleida") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(Lleida2.plot)

#===========================================================================

# Madrid

#===========================================================================

# Plot 1 : COVID-19 Cases vs Maximum Temperature and SO2 ground level

MADRID$`Covid-19 Cases` <- as.numeric(MADRID$`Covid-19 Cases`)
MADRID$`T. max` <- as.numeric(MADRID$`T. max`)
str(MADRID)

Madrid1.plot <-  MADRID  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MADRID$Date[which(MADRID$Date=="2020-05-13")]), 
            xmax = as.numeric(MADRID$Date[which(MADRID$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`*40 ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Daily Max. Tempertature")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./40, name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MADRID$Date[which(MADRID$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MADRID$Date[which(MADRID$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Madrid2.plot <-  MADRID  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MADRID$Date[which(MADRID$Date=="2020-05-13")]), 
            xmax = as.numeric(MADRID$Date[which(MADRID$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= so2*500 ,color= "so2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "so2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground so2 µg/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./500, name="Ground so2 µg/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MADRID$Date[which(MADRID$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MADRID$Date[which(MADRID$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid_MADRID1 <- plot_grid(Madrid1.plot, Madrid2.plot, labels = NULL, nrow = 1)

title <- ggdraw() + 
  draw_label(
    "Madrid",
    fontface = 'bold',
    x = 0.35,
    hjust = -0.57
  ) 

plot_grid(
  title, plot_grid_MADRID1,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
) 


#===========================================================================

# Murcia

#===========================================================================

# Plot 1 : COVID-19 Cases vs Maximum Temperature and Precipitation

MURCIA$`Covid-19 Cases` <- as.numeric(MURCIA$`Covid-19 Cases`)
MURCIA$`T. max` <- as.numeric(MURCIA$`T. max`)
MURCIA$`Precipitation L/m2` <- as.numeric(MURCIA$`Precipitation L/m2`)
str(MURCIA)

Murcia1.plot <-  MURCIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
            xmax = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`/5 ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Maximum temperature in °C")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*5, name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Murcia2.plot <-  MURCIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
            xmax = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2`/2 ,color= "Precipitation L/m2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "Precipitation L/m2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Precipitation L/m2")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*2, name="Precipitation L/m2")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid_MURCIA1 <- plot_grid(Murcia1.plot, Murcia2.plot, labels = NULL, nrow = 1)

title <- ggdraw() + 
  draw_label(
    "Murcia",
    fontface = 'bold',
    x = 0.39,
    hjust = -0.57
  ) 

plot_grid(
  title, plot_grid_MURCIA1,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
) 


# Plot 2 : COVID-19 Cases vs o3


Murcia4.plot <-  MURCIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
            xmax = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3/12 ,color= "o3" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "o3" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground o3 ??g/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*12, name="Ground o3 ??g/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.32, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Murcia") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot(Murcia4.plot)

# Plot 3 : COVID-19 Cases vs no2 i so2

MURCIA$`Covid-19 Cases` <- as.numeric(MURCIA$`Covid-19 Cases`)
str(MURCIA)

Murcia5.plot <-  MURCIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
            xmax = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2/5 ,color= "no2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c("Cases2" = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground no2 µm level")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*5, name="Ground no2 µm level")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Murcia6.plot <-  MURCIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
            xmax = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= so2/2 ,color= "so2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "so2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground so2 µm level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*2, name="Ground so2 µm level")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid_MURCIA3 <- plot_grid(Murcia5.plot, Murcia6.plot, labels = NULL, nrow = 1)

title <- ggdraw() + 
  draw_label(
    "Murcia",
    fontface = 'bold',
    x = 0.39,
    hjust = -0.57
  ) 

plot_grid(
  title, plot_grid_MURCIA3,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
) 



#===========================================================================

# Ourense

#===========================================================================

# Plot 1 : COVID-19 Cases vs Maximum Temperature, PM25, NO2 and CO

OURENSE$`T. max` <- as.numeric(OURENSE$`T. max`)
Ourense1.plot <-  OURENSE  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
            xmax = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`*50 ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Maximum temperature in °C")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./50, name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Ourense") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Ourense1.plot

OURENSE$`T. max` <- as.numeric(OURENSE$`T. max`)
Ourense2.plot <-  OURENSE  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
            xmax = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm25*15 ,color= "pm25"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "pm25" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "PM25 µm level")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./15, name="PM25 µm level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Ourense") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Ourense2.plot

Ourense3.plot <-  OURENSE  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
            xmax = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2*40 ,color= "no2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "NO2 µm/m3 level")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./40, name="NO2 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Ourense") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Ourense3.plot

Ourense4.plot <-  OURENSE  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
            xmax = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`/5, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= co*100 ,color= "co"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "co" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "CO ppm ground level")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./100, name="CO ppm ground level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Ourense") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Ourense4.plot

#===========================================================================

# Pontevedra

#===========================================================================

# Plot 1 : COVID-19 Cases vs Maximum Temperature

PONTEVEDRA$`T. max` <- as.numeric(PONTEVEDRA$`T. max`)
Pontevedra1.plot <-  PONTEVEDRA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2020-05-13")]), 
            xmax = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2021-10-03")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`*50 ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Maximum temperature in °C")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./50, name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Pontevedra") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2021-10-03")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Pontevedra1.plot


#===========================================================================

# Santander

#===========================================================================

# Plot 1 : COVID-19 Cases vs Maximum Temperature 

SANTANDER$`Covid-19 Cases` <- as.numeric(SANTANDER$`Covid-19 Cases`)
SANTANDER$`T. max` <- as.numeric(SANTANDER$`T. max`)
SANTANDER$`Precipitation L/m2` <- as.numeric(SANTANDER$`Precipitation L/m2`)
str(SANTANDER)

Santander1.plot <-  SANTANDER  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
            xmax = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`*2.2 ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Maximum temperature in °C")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./1.5, name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.32, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Santander") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot(Santander1.plot)


# Plot 2 : COVID-19 Cases vs pm10, o3, no2

SANTANDER$`Covid-19 Cases` <- as.numeric(SANTANDER$`Covid-19 Cases`)
str(SANTANDER)

Santander3.plot <-  SANTANDER  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
            xmax = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10 ,color= "pm10"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "pm10" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground pm10 µm level")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~. , name="Ground pm10 µm level")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Santander4.plot <-  SANTANDER  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
            xmax = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3/2 ,color= "o3" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "o3" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground o3 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*2, name="Ground o3 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


plot_grid_SANTANDER2 <- plot_grid(Santander3.plot, Santander4.plot,labels = NULL, nrow = 1, ncol =2)

title <- ggdraw() + 
  draw_label(
    "Santander",
    fontface = 'bold',
    x = 0.57,
    hjust = -0.57
  ) 

plot_grid(
  title, plot_grid_SANTANDER2,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
) 


Santander5.plot <-  SANTANDER  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
            xmax = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2*2.5 ,color= "no2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground no2 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./2.5, name="Ground no2 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid_SANTANDER3 <- plot_grid(Santander5.plot,labels = NULL, nrow = 1, ncol =2)

title <- ggdraw() + 
  draw_label(
    "",
    fontface = 'bold',
    x = 0.08,
    hjust = -0.57
  ) 

plot_grid(
  title, plot_grid_SANTANDER3,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
) 



#===========================================================================

# Tarragona

#===========================================================================

# Plot 1 : COVID-19 Cases vs PM10

Tarragona1.plot <-  TARRAGONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2020-05-13")]), 
            xmax = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2021-09-01")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10*7 ,color= "pm10"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "pm10" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground pm10 µm level")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./7 , name="Ground pm10 µm level")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Tarragona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2021-09-01")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Tarragona1.plot


# Plot 2 : COVID-19 Deaths vs Maximum Temperature

TARRAGONA$`T. max` <- as.numeric(TARRAGONA$`T. max`)
Tarragona2.plot <-  TARRAGONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2020-05-13")]), 
            xmax = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2021-09-01")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Deaths`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`/4 ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Deaths/100.000 inhabitants", "Maximum temperature in °C")) +
  labs(x="Date", y = "COVID-19 Daily Deaths/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*4, name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.32, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Tarragona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2021-09-01")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot(Tarragona2.plot)


#===========================================================================

# Vigo

#===========================================================================

# Plot 1 : COVID-19 Cases vs Humidity

VIGO <- VIGO[VIGO$`Relative humidity 1,5m` > 0, ]
VIGO$`Covid-19 Cases` <- as.numeric(VIGO$`Covid-19 Cases`)
VIGO$`Relative humidity 1,5m` <- as.numeric(VIGO$`Relative humidity 1,5m`)
str(VIGO)

Vigo1.plot <- VIGO  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(VIGO$Date[which(VIGO$Date=="2020-05-13")]), 
            xmax = as.numeric(VIGO$Date[which(VIGO$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Cases`/6, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `Relative humidity 1,5m`*5 ,color= "humidity"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c("Cases2" = "forestgreen", "humidity" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Relative humidity, 1.5m")) +
  labs(x="Date", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~./25, name="Relative humidity, 1.5m")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Vigo") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(VIGO$Date[which(VIGO$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(VIGO$Date[which(VIGO$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Vigo1.plot









## DECEASES

#===========================================================================

# Barcelona

#===========================================================================

# Plot 1 : COVID-19 Deceases vs Maximum Temperature and Precipitation

BCN$`Covid-19 Deaths` <- as.numeric(BCN$`Covid-19 Deaths`)
BCN$`T. max` <- as.numeric(BCN$`T. max`)
BCN$`Precipitation L/m2` <- as.numeric(BCN$`Precipitation L/m2`)
str(BCN)

Barcelona1.plot <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Deaths`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max` ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Deceases/100.000 inhabitants", "Daily Max. Tempertature")) +
  labs(x="Date", y = "COVID-19 Daily Deceases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~., name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Barcelona2.plot <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Deaths`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2` ,color= "Precipitation L/m2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "Precipitation L/m2" = "firebrick"),
                     labels = c("COVID-19 Daily Deceases/100.000 inhabitants", "Precipitation L/m2")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~., name="Precipitation L/m2")) +
  theme_bw() +
  theme(legend.position = c(0.43, 0.98), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=5)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid_BCN1 <- plot_grid(Barcelona1.plot, Barcelona2.plot, labels = NULL, nrow = 1)

title <- ggdraw() + 
  draw_label(
    "Barcelona",
    fontface = 'bold',
    x = 0.35,
    hjust = -0.57
  ) 

plot_grid(
  title, plot_grid_BCN1,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
) 


# Plot 2 : COVID-19 Deceases vs o3

BCN$`Covid-19 Deaths` <- as.numeric(BCN$`Covid-19 Deaths`)
str(BCN)

Barcelona7.plot <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Deaths`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3*2 ,color= "o3" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c("Cases2" = "forestgreen", "o3" = "firebrick"),
                     labels = c("COVID-19 Daily Deceases/100.000 inhabitants", "Ground o3 µm/m3 level")) +
  labs(x="Date", y = "") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*2, name="Ground o3 µm/m3 level")) +
  theme_bw() +
  theme(legend.position = c(0.44, 0.95), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Barcelona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot(Barcelona7.plot)




#===========================================================================

# Santander

#===========================================================================

# Plot 1 : COVID-19 Deceases vs Maximum Temperature and Precipitation

SANTANDER$`Covid-19 Deaths` <- as.numeric(SANTANDER$`Covid-19 Deaths`)
SANTANDER$`T. max` <- as.numeric(SANTANDER$`T. max`)
str(SANTANDER)

Santander7.plot <-  SANTANDER  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
            xmax = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(`Covid-19 Deaths`, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`/25 ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick"),
                     labels = c("COVID-19 Daily Deceases/100.000 inhabitants", "Maximum temperature in °C")) +
  labs(x="Date", y = "COVID-19 Daily Deceases/100.000 inhabitants") +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*25, name="Maximum temperature in °C")) +
  theme_bw() +
  theme(legend.position = c(0.33, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Santander") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Santander7.plot