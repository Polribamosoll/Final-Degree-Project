library(ggplot2)
library(ggpubr)
show_line_types()

#===========================================================================

# Barcelona

#===========================================================================

# Plot 1 : COVID-19 Cases vs Precipitation and Temperature

BCN$Cases2 <- as.numeric(BCN$Cases2)
BCN$`T. max` <- as.numeric(BCN$`T. max`)
BCN$`Precipitation L/m2` <- as.numeric(BCN$`Precipitation L/m2`)
str(BCN)

Barcelona.plot <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max` ,color= "T. max"),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2`*3 ,color= "Precipitation L/m2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick", "Precipitation L/m2" = "mediumslateblue"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Daily Max. Tempertature", "Precipitation L/m2")) +
  labs(x=" ", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.30, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Barcelona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Barcelona.plot



# Plot 2 : COVID-19 Cases vs pm10

BCN$Cases2 <- as.numeric(BCN$Cases2)
BCN$no2 <- as.numeric(BCN$no2)
BCN$o3 <- as.numeric(BCN$o3)
str(BCN)

Barcelona.plot2 <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14)*2 ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2*3 ,color= "no2"),size=0.3) +
  geom_line(aes(y= o3+30 ,color= "o3"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick", "o3" = "mediumslateblue"),
                     labels = c("COVID19 daily cases/100.000 inhabitants", "Ground-level no2 µm", "Ground-level o3 µm")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.31, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Barcelona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Barcelona.plot2

# Plot 3 : COVID-19 Deceases vs Precipitation and Temperature

BCN$Deaths2 <- as.numeric(BCN$Deaths2)
BCN$`T. max` <- as.numeric(BCN$`T. max`)
BCN$`Precipitation L/m2` <- as.numeric(BCN$`Precipitation L/m2`)
str(BCN)

Barcelona.plot3 <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Deaths2, n = 14)*25 ,color= "Deaths2" ),size=0.3) +
  geom_line(aes(y= `T. max` ,color= "T. max"),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2`*2 ,color= "Precipitation L/m2" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c("Deaths2" = "forestgreen", "T. max" = "firebrick", "Precipitation L/m2" = "mediumslateblue"),
                     labels = c("COVID-19 daily deceases/100.000 inhabitants", "Daily maximum temperature", "Precipitation L/m2")) +
  labs(x=" ", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.60, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Barcelona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Barcelona.plot3

# Plot 4 : COVID-19 Deceases vs pm10, no2 and o3

BCN$Deaths2 <- as.numeric(BCN$Deaths2)

str(BCN)

Barcelona.plot4 <-  BCN  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
            xmax = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Deaths2, n = 14)*25 ,color= "Deaths2" ),size=0.3) +
  geom_line(aes(y= pm10*1.5 ,color= "pm10"),size=0.3) +
  geom_line(aes(y= no2 ,color= "no2" ),size=0.3) +
  geom_line(aes(y= o3*1.5 ,color= "o3" ),size=0.3) +
  scale_color_manual(name = " ",
                     values = c("Deaths2" = "forestgreen", "pm10" = "firebrick", "no2" = "mediumslateblue", "o3" ="orange2"),
                     labels = c("COVID-19 daily deceases/100.000 inhabitants", "Ground-level pm10 µm", "Ground-level no2 µm", "Ground-level o3 µm")) +
  labs(x=" ", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.60, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Barcelona") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2020-03-02")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BCN$Date[which(BCN$Date=="2021-10-07")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Barcelona.plot4

#===========================================================================

# Bilbao

#===========================================================================

# Plot 1 : COVID-19 Cases vs no2, o3

Bilbao.plot1 <-  BILBAO  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
            xmax = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3 ,color= "o3"),size=0.3) +
  geom_line(aes(y= no2 ,color= "no2"),size=0.3) +
  geom_line(aes(y= so2 ,color= "so2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "o3" = "firebrick", "no2" = "purple", "so2" = "black"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level ozone", "Ground-level no2", "Ground-level so2")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.31, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Bilbao") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Bilbao.plot1



# Plot 2 : COVID-19 Cases vs pm10

Bilbao2.plot <-  BILBAO  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
            xmax = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10/3 ,color= "pm10"),size=0.3) +
  geom_line(aes(y= pm25/3 ,color= "pm25"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "pm10" = "firebrick", "pm25" = "purple"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level particular matter 10", "Ground-level particular matter 10")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.31, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Bilbao") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(BILBAO$Date[which(BILBAO$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Bilbao2.plot

#===========================================================================

# Ceuta

#===========================================================================

# Plot 1 : COVID-19 Cases vs no2

CEUTA$Cases2 <- as.numeric(CEUTA$Cases2)
CEUTA$`Precipitation L/m2`<- as.numeric(CEUTA$`Precipitation L/m2`)
CEUTA$`T. max`<- as.numeric(CEUTA$`T. max`)
str(CEUTA)

Ceuta.plot1 <-  CEUTA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
            xmax = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2 , n=14), color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2` ,color= "Precipitation L/m2"),size=0.3) +
  geom_line(aes(y= `T. max` ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick", "Precipitation L/m2" = "mediumslateblue"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Daily Max. Tempertature", "Precipitation L/m2")) +
  labs(x=" ", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.31, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Ceuta") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Ceuta.plot1


# Plot 2 : COVID-19 Cases vs no2

CEUTA$Cases2 <- as.numeric(CEUTA$Cases2)
CEUTA$no2 <- as.numeric(CEUTA$no2)
str(CEUTA)

Ceuta.plot1 <-  CEUTA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
            xmax = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2 , n=14), color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2 ,color= "no2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick"),
                     labels = c("COVID-19 Daily Cases/100.000 inhabitants", "Ground level no2 µm")) +
  labs(x=" ", y = "COVID-19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.31, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Ceuta") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-03-01")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(CEUTA$Date[which(CEUTA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Ceuta.plot1



#===========================================================================

# Donostia

#===========================================================================

# Plot 1 : COVID-19 Cases vs o3, no2, so2

Donostia.plot1 <-  DONOSTIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2020-05-13")]), 
            xmax = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2021-07-22")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2 , n=14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3 ,color= "o3"),size=0.3) +
  geom_line(aes(y= no2 ,color= "no2"),size=0.3) +
  geom_line(aes(y= so2 ,color= "so2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "o3" = "firebrick", "no2" = "purple", "so2" = "black"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level ozone", "Ground-level no2", "Ground-level so2")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.31, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("DONOSTIA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2021-07-22")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Donostia.plot1



# Plot 2 : COVID-19 Cases vs pm10 and pm25

Donostia.plot2 <-  DONOSTIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2020-05-13")]), 
            xmax = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2021-07-22")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2 , n=14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm25/2 ,color= "pm25"),size=0.3) +
  geom_line(aes(y= pm10 ,color= "pm10"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "pm25" = "firebrick", "pm10" = "purple"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level pm25", "Ground-level pm10")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.31, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("DONOSTIA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(DONOSTIA$Date[which(DONOSTIA$Date=="2021-07-22")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Donostia.plot2


#===========================================================================

# Girona

#===========================================================================

# Plot 1 : COVID-19 Cases vs no2

Girona.plot1 <-  GIRONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
            xmax = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2*5 ,color= "no2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "no2" = "firebrick"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level no2")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.25, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("GIRONA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Girona.plot1


# Plot 2 : COVID-19 Cases vs pm10 and pm25

Girona.plot2 <-  GIRONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
            xmax = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10 ,color= "pm10"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "pm10" = "firebrick"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level particular matter 10")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.25, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("GIRONA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(GIRONA$Date[which(GIRONA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Girona.plot2


#===========================================================================

# Lleida

#===========================================================================

# Plot 1 : COVID-19 Cases vs o3 and no2

Lleida.plot1 <-  LLEIDA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2020-05-13")]), 
            xmax = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3*2 ,color= "o3"),size=0.3) +
  geom_line(aes(y= no2*2 ,color= "no2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "o3" = "firebrick", "no2" = "purple"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level o3", "Ground-level no2")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.45, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("LLEIDA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Lleida.plot1


# Plot 2 : COVID-19 Cases vs pm10 and pm25

Lleida.plot2 <-  LLEIDA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2020-05-13")]), 
            xmax = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10 ,color= "pm10"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "pm10" = "firebrick"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level particular matter 10")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.44, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("LLEIDA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(LLEIDA$Date[which(LLEIDA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Lleida.plot2


#===========================================================================

# MADRID

#===========================================================================

# Plot 1 : COVID-19 Cases vs o3 and no2
MADRID$Cases2 <- as.numeric(MADRID$Cases2)
MADRID$`T. max`<- as.numeric(MADRID$`T. max`)
MADRID$so2<- as.numeric(MADRID$so2)

Madrid.plot1 <-  MADRID  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MADRID$Date[which(MADRID$Date=="2020-05-13")]), 
            xmax = as.numeric(MADRID$Date[which(MADRID$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max` ,color= "T. max"),size=0.3) +
  geom_line(aes(y= so2*15 ,color= "so2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick", "so2" = "mediumslateblue"),
                     labels = c("COVID19 daily cases/100.000 inhabitants", "Maximum daily temperature" ,"Ground-level SO2 µm")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.73, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("MADRID") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MADRID$Date[which(MADRID$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MADRID$Date[which(MADRID$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Madrid.plot1


# Plot 2 : COVID-19 Cases vs pm10 and pm25

Madrid.plot2 <-  MADRID  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MADRID$Date[which(MADRID$Date=="2020-05-13")]), 
            xmax = as.numeric(MADRID$Date[which(MADRID$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10 ,color= "pm10"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "pm10" = "firebrick"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level particular matter 10")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.3, 0.97), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("MADRID") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MADRID$Date[which(MADRID$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MADRID$Date[which(MADRID$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Madrid.plot2


#===========================================================================

# Murcia

#===========================================================================

# Plot 1 : COVID-19 Cases vs Temperature and Precipitation

Murcia.plot1 <-  MURCIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
            xmax = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14)*3 ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `T. max`/15 ,color= "T. max"),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2`/10 ,color= "Precipitation L/m2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "T. max" = "firebrick", "Precipitation L/m2"="mediumslateblue"),
                     labels = c("COVID19 daily cases/100.000 inhabitants", "Maximum Temperature", "Precipitation L/m2")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.72, 0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("MURCIA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-093")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Murcia.plot1


# Plot 2 : COVID-19 Cases vs pm10 and o3

which.max(MURCIA$pm10)
MURCIA<- MURCIA[-179,]
which.max(MURCIA$pm10)
MURCIA<- MURCIA[-173,]
which.max(MURCIA$pm10)


Murcia.plot2 <-  MURCIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
            xmax = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14)*20 ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10/7 ,color= "pm10"),size=0.3) +
  geom_line(aes(y= o3/2 ,color= "o3"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "pm10" = "firebrick", "o3" = "mediumslateblue"),
                     labels = c("COVID19 daily cases/100.000 inhabitants", "Ground-level particular matter 10 µm", "Ground-level o3 µm")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.8), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("MURCIA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Murcia.plot2



# Plot 3 : COVID-19 Cases vs no2 and so2

Murcia.plot3 <-  MURCIA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-09")]), 
            xmax = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2*10, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= no2/2 ,color= "no2"),size=0.3) +
  geom_line(aes(y= so2 ,color= "so2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "no2" = "firebrick", "so2"="mediumslateblue"),
                     labels = c("COVID19 daily cases/100.000 inhabitants", "Ground-level no2 µm", "Ground-level so2 µm")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.72, 0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("MURCIA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2020-07-093")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(MURCIA$Date[which(MURCIA$Date=="2021-10-29")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Murcia.plot3

#===========================================================================

# Ourense

#===========================================================================

# Plot 1 : COVID-19 Cases vs o3, no2, so2 and co

Ourense.plot1 <-  OURENSE  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
            xmax = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2/30, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3/5 ,color= "o3"),size=0.3) +
  geom_line(aes(y= no2*2 ,color= "no2"),size=0.3) +
  geom_line(aes(y= so2*5 ,color= "so2"),size=0.3) +
  geom_line(aes(y= co*5 ,color= "co"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "o3" = "firebrick", "no2" = "purple", so2 = "green", "co" = "orange"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level o3", "Ground-level no2", "Ground-level so2","Ground-level co" )) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.25, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Ourense") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Ourense.plot1


# Plot 2 : COVID-19 Cases vs pm10 and pm25

Ourense.plot2 <-  OURENSE  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
            xmax = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2/10, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10 ,color= "pm10"),size=0.3) +
  geom_line(aes(y= pm25*1.5 ,color= "pm25"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "pm10" = "firebrick", "pm25"="purple"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level particular matter 10", "Ground-level particular matter 25")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.44, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Ourense") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-03")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Ourense.plot2



#===========================================================================

# Pontevedra

#===========================================================================

# Plot 1 : COVID-19 Cases vs o3, no2 and co

Pontevedra.plot1 <-  PONTEVEDRA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2020-05-13")]), 
            xmax = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2/50, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3/5 ,color= "o3"),size=0.3) +
  geom_line(aes(y= no2*2 ,color= "no2"),size=0.3) +
  geom_line(aes(y= co ,color= "co"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "o3" = "firebrick", "no2" = "purple", "co" = "orange"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level o3", "Ground-level no2","Ground-level co" )) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.25, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("PONTEVEDRA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2021-10-3")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Pontevedra.plot1


# Plot 2 : COVID-19 Cases vs pm10 and pm25

Pontevedra.plot2 <-  PONTEVEDRA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2020-05-13")]), 
            xmax = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2/20, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10/2 ,color= "pm10"),size=0.3) +
  geom_line(aes(y= pm25/2 ,color= "pm25"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "pm10" = "firebrick", "pm25"="purple"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level particular matter 10", "Ground-level particular matter 25")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.25, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("PONTEVEDRA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(PONTEVEDRA$Date[which(PONTEVEDRA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Pontevedra.plot2



#===========================================================================

# Santander

#===========================================================================

# Plot 1 : COVID-19 Cases vs maximum temperature and precipitation 

str(SANTANDER)
SANTANDER$`T. max` <- as.numeric(SANTANDER$`T. max`)
SANTANDER$`Precipitation L/m2` <- as.numeric(SANTANDER$`Precipitation L/m2`)

Santander.plot1 <-  SANTANDER  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
            xmax = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `Precipitation L/m2`/3 ,color= "Precipitation L/m2"),size=0.3) +
  geom_line(aes(y= `T. max` ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "Precipitation L/m2" = "mediumslateblue", "T. max" = "firebrick"),
                     labels = c("COVID19 daily cases/100.000 inhabitants", "Precipitation L/m2", "Maximum daily temperature")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.3, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("SANTANDER") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-3")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Santander.plot1


# Plot 2 : COVID-19 Cases vs pm10, o3 and no2

Santander.plot2 <-  SANTANDER  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
            xmax = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2, n = 14)*2 ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10/2 ,color= "pm10"),size=0.3) +
  geom_line(aes(y= o3 ,color= "o3"),size=0.3) +
  geom_line(aes(y= no2/2 ,color= "no2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "pm10" = "firebrick", "o3" = "mediumslateblue", "no2"="orange2"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level particular matter 10 µm", "Ground-level o3 µm", "Ground-level o2 µm")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.44, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("SANTANDER") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Santander.plot2


# Plot 3 : COVID-19 Deceases vs maximum temperature

Santander.plot3 <-  SANTANDER  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
            xmax = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Deaths2, n = 14)*20 ,color= "Deaths2" ),size=0.3) +
  geom_line(aes(y=`T. max` ,color= "T. max"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( "Deaths2" = "darkseagreen", "T. max" = "firebrick"),
                     labels = c("COVID19 daily deceases/100.000 inhabitants", "Maximum daily temperature")) +
  labs(x=" ", y = "COVID19 Daily deceases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.25, 0.93), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("SANTANDER") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(SANTANDER$Date[which(SANTANDER$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Santander.plot3


#===========================================================================

# Tarragona

#===========================================================================

# Plot 1 : COVID-19 Cases vs o3 and no2 

Tarragona.plot1 <-  TARRAGONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2020-05-13")]), 
            xmax = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2021-09-01")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2/10, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3/2 ,color= "o3"),size=0.3) +
  geom_line(aes(y= no2 ,color= "no2"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "o3" = "firebrick", "no2" = "mediumslateblue"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level o3", "Ground-level no2")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.3, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("TARRAGONA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2021-09-01")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Tarragona.plot1


# Plot 2 : COVID-19 Cases vs pm10 

Tarragona.plot2 <-  TARRAGONA  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2020-05-13")]), 
            xmax = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2021-09-01")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2/5, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= pm10 ,color= "pm10"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "pm10" = "firebrick"),
                     labels = c("COVID19 daily cases/100.000 habitants", "Ground-level particular matter 10")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 habitants") +
  theme_bw() +
  theme(legend.position = c(0.25, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("TARRAGONA") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(TARRAGONA$Date[which(TARRAGONA$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Tarragona.plot2



#===========================================================================

# Vigo

#===========================================================================

# Plot 1 : COVID-19 Cases vs pm25 and o3

Vigo.plot1 <-  VIGO  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(VIGO$Date[which(VIGO$Date=="2020-05-13")]), 
            xmax = as.numeric(VIGO$Date[which(VIGO$Date=="2021-10-31")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2/10, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= o3/5 ,color= "o3"),size=0.3) +
  geom_line(aes(y= pm25 ,color= "pm25"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "forestgreen", "o3" = "firebrick", "pm25" = "mediumslateblue"),
                     labels = c("COVID19 daily cases/100.000 inhabitants", "Ground-level o3", "Ground-level no2", "Ground-level so2","Ground-level co" )) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.25, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Vigo") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(OURENSE$Date[which(OURENSE$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Vigo.plot1


# Plot 2 : COVID-19 Cases vs pm10 and pm25

VIGO <- VIGO[VIGO$`Relative humidity 1,5m` > 0, ]

Vigo.plot2 <-  VIGO  %>% ggplot(aes(x=Date)) +
  geom_rect(xmin = as.numeric(VIGO$Date[which(VIGO$Date=="2020-05-13")]), 
            xmax = as.numeric(VIGO$Date[which(VIGO$Date=="2021-10-01")]), 
            ymin = 0, 
            ymax = Inf, alpha = 0.04, fill="grey90") +
  geom_line(aes(y= lag(Cases2/5, n = 14) ,color= "Cases2" ),size=0.3) +
  geom_line(aes(y= `Relative humidity 1,5m`*1.5 ,color= "humidity"),size=0.3) +
  scale_color_manual(name = " ",
                     values = c( Cases2 = "darkseagreen", "humidity" = "firebrick"),
                     labels = c("COVID19 daily cases/100.000 inhabitants", "Relative Humidity at 1,5 meters")) +
  labs(x=" ", y = "COVID19 Daily Cases/100.000 inhabitants") +
  theme_bw() +
  theme(legend.position = c(0.3, 0.90), 
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10)) +
  ggtitle("Vigo") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))  +
  geom_vline(xintercept = as.numeric(VIGO$Date[which(VIGO$Date=="2020-05-13")]), 
             color = "black", size=0.6) +
  geom_vline(xintercept = as.numeric(VIGO$Date[which(VIGO$Date=="2021-10-31")]), 
             color = "black", size=0.6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Vigo.plot2
