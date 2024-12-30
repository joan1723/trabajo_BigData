#- código R usado en el trabajo


#- pkgs
library(euroleaguer)
library(tidyverse)
library(plotly)
library(knitr)
library(kableExtra)
library(gganimate)
library(DT)
#- datos; en caso de no tener el pkg euroleaguer instalado -> install.packages("euroleaguer")

Competicion <- euroleaguer::getCompetitionHistory("E")


#- Aquí mostramos la clasificación de campeones de la Euroliga en los últimos 20 años a través de una tabla 
Ganadores <- Competicion |> select(Name , Year , WinnerCode , Ganador = WinnerEditorialName) |>
  drop_na() |>
  group_by(Ganador) |>
  summarise(Copas = n()) |>
  arrange(desc(Copas)) |> 
  ungroup()

knitr::kable(Ganadores) |> 
  kableExtra::kable_styling(bootstrap_options = "striped", 
                            full_width = F, 
                            position = "float_right" , 
                            fixed_thead = list(enabled = T , background = "#A43B32"))

#- Aquí lo mostramos en un gráfico de barras

Competicion <- Competicion |> select(Name , Year ,  WinnerCode , WinnerEditorialName) |>
  drop_na()
p <- Competicion |> ggplot(aes(x = WinnerCode)) + 
  geom_bar(aes(fill = WinnerEditorialName)) + 
  labs(title = "Campeonatos Euroliga de Baloncesto 2000-2022" ,
       subtitle = "(diferenciando cada equipo por color)", 
       caption = "Datos provenientes del pkg Euroleaguer" , 
       x = "Equipos" , 
       y = "Campeonatos" ,
       fill = "Nombre de los equipos")

p + theme_classic()


#- Ahora vamos a mostrar la evolución del número de jornada de la Euroliga desde el año de fundación de la liga hasta la temporada 2023-24.

rounds <- getCompetitionRounds(season_code = paste0("E" , 2000:2023))

df <- rounds |> select(SeasonCode , Round) |> 
  separate(SeasonCode , c("Letra" , "Year") , 1) |> 
  select(!Letra) |> 
  group_by(Year) |> 
  slice_max(Round , n = 1) |> 
  ungroup()

años <- c(2000:2023)

df <- df |> cbind(años) |> 
  mutate(Year = NULL)

p <- df |> ggplot(mapping = aes(x = años , y = Round)) + 
  geom_line(color = "#FDAA65" , size = 1.5) +
  geom_point(color = "#EEB78D" , alpha = 0.7 , aes(size = Round) , show.legend = FALSE) + 
  scale_x_continuous(breaks = seq(2000 , 2023 , by = 1)) +
  scale_size(range = c(4, 10)) + 
  labs(title = "Evolución del número de Jornadas por temporada" ,
       subtitle = "(incluyendo la Fase Regular y los playoffs)" ,
       x = "Temporadas" , 
       y = "Número de Jornadas" , 
       caption = "Datos provenientes del pkg Euroleaguer") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(size = 12.5, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 13, hjust = 1, vjust = 0),
        axis.title.y = element_text(size = 13, hjust = 1, vjust = 1),
        plot.caption = element_text(hjust = 0.5), 
        panel.grid.major = element_line(colour = "#F8F7E5", linetype = "dashed"),
        panel.background = element_rect(fill = "#CBE9D6"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(family = "serif", size = 9, colour = "gray17"))

p

p_animated <- p + transition_reveal(años) +
  enter_fade() + 
  exit_fade()

animate(p_animated, nframes = 200, fps = 20) 
  
  
#- Definimos el tema del gráfico anterior como nuestro tema por defecto
my_theme <- theme_minimal() + 
  theme(axis.text.x = element_text(hjust = 1, size = 8),
        plot.title = element_text(size = 12.5, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 13, hjust = 1, vjust = 0),
        axis.title.y = element_text(size = 13, hjust = 1, vjust = 1),
        plot.caption = element_text(hjust = 0.5), 
        panel.grid.major = element_line(colour = "#F8F7E5", linetype = "dashed"),
        panel.background = element_rect(fill = "#CBE9D6"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(family = "serif", size = 9, colour = "gray17"))



#- Una vez mostrados los equipos que más y menos han ganado  y la evolución del número de jornada vamos a pasar a analizar estadísticas individuales y por equipos

rm(list = ls()[!(ls() %in% "my_theme")])

df <- getPlayerStats("E2023")



#- cojos datos de estadísticas individuales y arreglo un poco 

df <- df |> drop_na()


Player_stats <- df |> filter(StatisticMode == "perGame") |> 
  select(StatisticMode , PlayerName , PlayerAge , TeamName , PIR , PTS) |>
  mutate(categorias_por_puntos = as_factor(ntile(PTS , n = 3))) |>
  mutate(categorias_por_puntos = case_when( 
    categorias_por_puntos == 1 ~ "PP<5.5",
    categorias_por_puntos == 2 ~ "5.5<PP<9.3" ,
    categorias_por_puntos == 3 ~ "9.3<PP")) |> 
  arrange(desc(PTS))
  
P <- Player_stats |> ggplot(mapping = aes(x = PlayerAge , y = PIR)) +
  geom_point(aes(color = categorias_por_puntos)) +
  geom_smooth(color = "orange" ) + 
  labs(title = "PIR EN RELACIÓN A LA EDAD DE LOS JUGADORES" ,
       subtitle = "(por categorías de Puntos por partido)" , 
       caption = "Datos provenientes del pkg euroleaguer" , 
       x = "Edad" , 
       y = "PIR" , 
       color = "Puntos por partido") +
  scale_color_manual(values = c("#A85458" , "#7F000D" , "#DAB7B8")) + 
  my_theme
  
       
P

ggplotly(P)


#- Ahora vamos a mostrar un gráfico que relaciona los puntos por partido y los minutos en función de los porcentajes de tiro conjuntos (de dos y de tres)

rm(list = ls()[!(ls() %in% c("df" , "my_theme"))])

df <- df |> filter(StatisticMode == "perGame") |> 
  select(StatisticMode , PlayerName , MIN , PTS , `2PM` , `2PA` , `3PM` , `3PA`) |> 
  mutate(total_intentos = `2PA` + `3PA`) |>
  mutate(total_aciertos = `2PM` + `3PM`) |> 
  mutate(porcentaje_de_acierto = total_aciertos/total_intentos) 
  
df <- df |> select(PlayerName , MIN , PTS , porcentaje_de_acierto) |> 
  mutate(categoria_porcentaje = as_factor(ntile(porcentaje_de_acierto , n = 3))) |> 
  arrange(desc(porcentaje_de_acierto)) |> 
  mutate(comentarios_Andres_Montes = case_when( 
    categoria_porcentaje == 3  ~ "¿Por qué todos los
    jugones sonríen igual?",
    categoria_porcentaje == 2  ~ "Sensacional" , 
    categoria_porcentaje == 1  ~ "El club del ladrillo"))

df2 <- df |> arrange(desc(PTS)) |>
  filter(PTS >= 15.0) |> 
  filter(!(PlayerName == "BALDWIN IV, WADE" | PlayerName == "NUNN, KENDRICK"))

df3 <- df |> filter(PlayerName == "BALDWIN IV, WADE" | PlayerName == "NUNN, KENDRICK")

p <- df |>  ggplot(mapping = aes(x = MIN , y = PTS , color = comentarios_Andres_Montes)) + 
  geom_point() + 
  labs(title = "Gráfico 3: Relación entre Puntos por partido y minutos por partido" , 
       subtitle = "(categorizando los porcentajes de campo con comentarios del famoso periodista Andrés Montés)" , 
       caption = "Datos provenientes del pkg Euroleaguer" , 
       x = "Minutos PP" , 
       y = "Puntos PP" , 
       color = "Porcentajes de campo") + 
  geom_text(data = df2 , aes(label = PlayerName) , color = "black" , size = 2 , hjust = "right" , vjust = "top") + 
  geom_text(data = df3 , aes(label = PlayerName) , color = "black" , size = 2 , hjust = "right" , vjust = "bottom") +
  scale_color_manual(values = c("#7F000D" , "#DAB7B8" , "#A85458")) +
  my_theme + 
  theme(legend.position = "bottom" ,
        legend.title.position = "top" , 
        legend.title = element_text(face = "bold" , hjust = 0.5))

p


#- Asistencias en relación a los puntos

rm(list = ls()[!(ls() %in% "my_theme")])

df <- getPlayerStats(season_code = "E2023" , statistic_mode = "perGame")

Player_stats <- df |> select(SeasonCode , PlayerName , TeamName , PTS , AST) |>
  drop_na() |> 
  separate(SeasonCode , c("E" , "Year") , sep = 1) |> 
  mutate(E = NULL) |> 
  select(PTS , AST) |> 
  arrange(desc(PTS))

p <- Player_stats |> ggplot(mapping = aes(x = PTS , y = AST)) +
  geom_smooth(aes(color = "loess") , method = "loess", se = FALSE) + 
  geom_smooth(aes(color = "lm")    , method = "lm"   , se = FALSE) +
  labs(title = "Relación entre los Puntos y las Asistencias. Temporada 2023-24" , 
       subtitle = "(en términos de puntos y asistencias por partido de los jugadores)" , 
       x = "PUNTOS" , 
       y = "ASISTENCIAS" , 
       caption = "Datos provenientes del pkg euroleaguer") +
  scale_color_manual(values = c("#7C4D79" , "#2A5783")) + 
  my_theme + theme(panel.background = element_rect(fill = "#AADD9C"))

p

#- Tabla de estadísticas de los 10 jugadores con mejor PIR de la temporada 2023-24 

df <- getPlayerStats(season_code = "E2023" , statistic_mode = "perGame")

df <- df |> slice_max(PIR , n = 10) |> 
  select(Jugador = PlayerName , Edad = PlayerAge , Equipo = TeamName , 11 , 13:34)
  
Tabla <- df |> kable(align = "c", 
                     caption = "Estadísticas de los 10 jugadores con mejor PIR. Temporada 2023-24",
                     digits = 2, 
                     format.args = list(decimal.mark = ",", big.mark = ".")) |>
  kable_styling(bootstrap_options = "striped",
                full_width = F, 
                fixed_thead = list(enabled = T , background = "#A43B32")) 


Tabla


#- Equipos 


rm(list = ls())

Team_Stats <- getTeamLeadStats(season_code = paste0("E" , 2016:2023))
Team_Promedio <- Team_Stats$TeamAveragePerGame

df <- Team_Promedio |> select(-c(Subset , PhaseType)) |> 
  group_by(SeasonCode) |> 
  summarise(MEDIA_PUNTOS = mean(PTS)) |> 
  ungroup() |> 
  separate(SeasonCode , c("E" , "Year") , 1) |> 
  mutate(E = NULL) |> 
  mutate(Crecimiento_anual = MEDIA_PUNTOS - lag(MEDIA_PUNTOS)) |> 
  mutate(Crecimiento_anual_2 = ifelse(is.na(Crecimiento_anual), 0, Crecimiento_anual)) |> 
  mutate(Crecimiento_anual = NULL) |> 
  rename(Crecimiento_Anual_Puntos = Crecimiento_anual_2)

DT::datatable(df)




#- Equipos 
#ANÁLISIS DE LAS ESTADÍSTICAS DE LOS EQUIPOS PARA LA TEMPORADA 2023-2024


EuroleagueTeamLeaders <- getTeamLeadStats(season_code = "E2023", phase_type = , subset = )
team_stats <- EuroleagueTeamLeaders$TeamAccumulated
#EQUIPOS CON MÁS PUNTOS ANOTADOS EN EL TOTAL DE LA COMPETICIÓN
MostPointsTotal<-team_stats %>% slice_max(PTS, n = 18) %>% arrange(desc(PTS)) %>%
  select("PTS", TeamCode)
#EQUIPOS CON MÁS REBOTES EN EL TOTAL DE LA COMPETICIÓN
MostReboundsTotal<-team_stats %>%  slice_max(REB, n=18) %>% arrange(desc(REB)) %>%
  select("REB", TeamCode )
#EQUIPOS CON MÁS TIROS LIBRES HECHOS A LO LARGO DE LA COMPETICIÓN
MostFTTotal<-team_stats %>%  slice_max(REB, n=18) %>% arrange(desc(FTT)) %>%
  select("FTM", TeamCode )
#EQUIPOS CON MÁS TRIPLES ANOTADOS EN TODA LA COMPETICIÓN
Most3PMotal<-team_stats %>%  slice_max(REB, n=18) %>% arrange(desc("3PM")) %>%
  select("3PM", TeamCode )
#EQUIPO CON MÁS TIROS DE CAMPO (TIROS DE 2 Y DE 3) ANOTADOS EN TODA LA COMPETICIÓN
MostFGMotal<-team_stats %>%  slice_max(REB, n=18) %>% arrange(desc(FGM)) %>%
  select("FGM", TeamCode )
#EQUIPOS CON MÁS ASISTENCIAS ACUMULADAS EN TODA LA COMPETICIÓN
MostASTotal<-team_stats %>%  slice_max(REB, n=18) %>% arrange(desc(AST)) %>%
  select("AST", TeamCode )
#EQUIPOS CON MÁS ROBOS ACUMULADOS EN TODA LA COMPETICIÓN
MostSTLTotal<-team_stats %>%  slice_max(REB, n=18) %>% arrange(desc(STL)) %>%
  select("STL", TeamCode )
#EQUIPOS CON MÁS PÉRDIDAS REALIZADAS EN TODA LA COMPETICIÓN
MostTOTotal<-team_stats %>%  slice_max(REB, n=18) %>% arrange(desc(TO)) %>%
  select("TO", TeamCode )
#EQUIPOS CON MÁS FALTAS RECIBIDAS EN TODA LA COMPETICIÓN
MostFDTotal<-team_stats %>%  slice_max(REB, n=18) %>% arrange(desc(FD)) %>%
  select("FD", TeamCode )
#EQUIPOS CON MÁS FALTAS REALIZADAS EN TODA LA COMPETICIÓN
MostFCTotal<-team_stats %>%  slice_max(REB, n=18) %>% arrange(desc(FC)) %>%
  select("FC", TeamCode )
#EQUIPOS CON EL MAYOR ÍNDICE PIR
MostPIRTotal<-team_stats %>%  slice_max(PIR, n=18) %>% arrange(desc(PIR)) %>%
  select("PIR", TeamCode )
team_stats_average <- EuroleagueTeamLeaders$TeamAveragePerGame
#EQUIPOS CON LA MEJOR MEDIA DE PUNTOS POR PARTIDO
BestPTSAverage <- team_stats_average %>% slice_max(PTS, n = 18) %>% arrange(desc(PTS)) %>%
  select("PTS", TeamCode)
#EQUIPOS CON LA MAYOR MEDIA DE REBOTES POR PARTIDO
BestREBAverage <- team_stats_average %>%  slice_max(REB, n=18) %>% arrange(desc(REB)) %>%
  select("REB", TeamCode )
#EQUIPOS CON LA MAYOR MEDIA DE TIROS LIBRES REALIZADOS POR PARTIDO
BestFTTAverage <- team_stats_average %>%  slice_max(FTT, n=18) %>% arrange(desc(FTT)) %>%
  select("FTM", TeamCode )
#EQUIPOS CON EL MEJOR PORCENTAJE DE TIROS DE 3
Best3PMAverage <- team_stats_average %>%  slice_max("3PM", n=18) %>% arrange(desc("3PM")) %>%
  select("3PM", TeamCode )
#EQUIPOS CON EL MEJOR PORCENTAJE DE TIROS DE CAMPO
BestFGMAverage <- team_stats_average %>%  slice_max(FGM, n=18) %>% arrange(desc(FGM)) %>%
  select("FGM", TeamCode )
#EQUIPOS CON LA MAYOR MEDIA DE ASISTENCIAS POR PARTIDO
BestASTAverage <- team_stats_average %>%  slice_max(AST, n=18) %>% arrange(desc(AST)) %>%
  select("AST", TeamCode )
#EQUIPOS CON LA MAYOR MEDIA DE ROBOS POR PARTIDO
BestSTLAverage <- team_stats_average %>% slice_max(STL, n=18) %>% arrange(desc(STL)) %>%
  select("STL", TeamCode )
#EQUIPOS CON MÁS PÉRDIDAS POR PARTIDO
BestTOAverage <- team_stats_average %>%  slice_max(TO, n=18) %>% arrange(desc(TO)) %>%
  select("TO", TeamCode )
#EQUIPOS CON MÁS FALTAS RECIBIDAS POR PARTIDO
BestFDAverage <- team_stats_average %>%  slice_max(FD, n=18) %>% arrange(desc(FD)) %>%
  select("FD", TeamCode )
#EQUIPOS CON MÁS FALTAS REALIZADAS POR PARTIDO
BestFCAverage <- team_stats_average %>% slice_max(FC, n=18) %>% arrange(desc(FC)) %>%
  select("FC", TeamCode )




