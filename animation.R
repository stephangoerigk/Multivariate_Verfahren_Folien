library(ggplot2)
library(gganimate)

mytheme = theme_bw() + theme(panel.grid.minor = element_blank(),
                             panel.grid.major = element_blank(), panel.background = element_blank(),
                             strip.background = element_blank(), strip.text.y = element_text(),
                             legend.background = element_blank(), legend.key = element_blank(),
                             panel.border = element_rect(colour = "black", fill = NA))
theme_set(mytheme)

t = 100




# Simple ------------------------------------------------------------------

df = data.frame(Time = 1:t,
                Stress = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1)))
                )
df_long = as.data.frame(tidyr::pivot_longer(df, cols = names(df)[2:ncol(df)], names_to = "Symptom", values_to = "Score"))
df_long$Symptom = factor(df_long$Symptom, levels = c("Stress"))

p1 = ggplot(df_long, aes(x = Time, y = Score)) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  geom_line(aes(group = 1)) +
  facet_grid(rows = vars(Symptom)) +
  labs(x = "Zeit") +
  coord_cartesian(ylim = c(0,10)) +
  theme(text = element_text(size = 14)) +
  transition_reveal(Time)

animate(p1, height = 3, width =6, units = "in", res = 300, renderer = gifski_renderer(loop = TRUE))
anim_save(filename = "bilder/ts_p1.gif")

df = data.frame(Time = 1:t,
                Stress = c(abs(arima.sim(n = t*0.25, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))),
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3.5,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 4,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 5,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 6))
df_long = as.data.frame(tidyr::pivot_longer(df, cols = names(df)[2:ncol(df)], names_to = "Symptom", values_to = "Score"))
df_long$Symptom = factor(df_long$Symptom, levels = c("Stress"))

df_long$Colour = 0
df_long$Colour[(df_long$Symptom == "Stress" | df_long$Symptom == "Schlaf" | df_long$Symptom == "Event") & df_long$Time >= 25] = 1
df_long$Colour = factor(df_long$Colour)

p2 = ggplot(df_long, aes(x = Time, y = Score, colour = Colour)) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  geom_line(aes(group = 1)) +
  facet_grid(rows = vars(Symptom)) +
  scale_color_manual(values = c("0" = "black",
                                "1" = "red")) +
  labs(x = "Zeit") +
  coord_cartesian(ylim = c(0,10)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  transition_reveal(Time)

animate(p2, height = 3, width =6, units = "in", res = 300, renderer = gifski_renderer(loop = TRUE))
anim_save(filename = "bilder/ts_p2.gif")




df = data.frame(Time = 1:t,
                Stress = c(abs(arima.sim(n = t*0.25, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))),
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3.5,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 4,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 5,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 6),
                Schlaf = c(abs(arima.sim(n = t*0.25, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 4,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3.5,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 2.5,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 2))
df_long = as.data.frame(tidyr::pivot_longer(df, cols = names(df)[2:ncol(df)], names_to = "Symptom", values_to = "Score"))
df_long$Symptom = factor(df_long$Symptom, levels = c("Stress", "Schlaf"))

df_long$Colour = 0
df_long$Colour[(df_long$Symptom == "Stress" | df_long$Symptom == "Schlaf" | df_long$Symptom == "Event") & df_long$Time >= 25] = 1
df_long$Colour = factor(df_long$Colour)

p2 = ggplot(df_long, aes(x = Time, y = Score, colour = Colour)) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  geom_line(aes(group = 1)) +
  facet_grid(rows = vars(Symptom)) +
  scale_color_manual(values = c("0" = "black",
                                "1" = "red")) +
  labs(x = "Zeit") +
  coord_cartesian(ylim = c(0,10)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  transition_reveal(Time)

animate(p2, height = 6, width =6, units = "in", res = 300, renderer = gifski_renderer(loop = TRUE))
anim_save(filename = "bilder/ts_p7.gif")




# stable and dormant ------------------------------------------------------


df = data.frame(Time = 1:t,
                Stress = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))),
                Schlaf = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                Stimmung = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                Angst = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))),
                Suizidalität = rep(0, t),
                Event = rep(0, t))
df_long = as.data.frame(tidyr::pivot_longer(df, cols = names(df)[2:ncol(df)], names_to = "Symptom", values_to = "Score"))
df_long$Symptom = factor(df_long$Symptom, levels = c("Stress", "Schlaf", "Stimmung", "Angst", "Suizidalität", "Event"))

p1 = ggplot(df_long, aes(x = Time, y = Score)) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  geom_line(aes(group = 1)) +
  facet_grid(rows = vars(Symptom)) +
  labs(x = "Zeit") +
  coord_cartesian(ylim = c(0,10)) +
transition_reveal(Time)

animate(p1, height = 6, width =3, units = "in", res = 300, renderer = gifski_renderer(loop = TRUE))
anim_save(filename = "bilder/ts_p3.gif")


df = data.frame(Time = 1:t,
                Stress = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))),
                Schlaf = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                Stimmung = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                Angst = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))),
                Suizidalität = rep(0, t),
                Event = rep(0, t))
df_long = as.data.frame(tidyr::pivot_longer(df, cols = names(df)[2:ncol(df)], names_to = "Symptom", values_to = "Score"))
df_long$Symptom = factor(df_long$Symptom, levels = c("Stress", "Schlaf", "Stimmung", "Angst", "Suizidalität", "Event"))
df_long$subject = "1"

df2 = data.frame(Time = 1:t,
                Stress = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) +2,
                Schlaf = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 4,
                Stimmung = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 5,
                Angst = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 2,
                Suizidalität = rep(0, t),
                Event = rep(0, t))
df_long2 = as.data.frame(tidyr::pivot_longer(df2, cols = names(df2)[2:ncol(df2)], names_to = "Symptom", values_to = "Score"))
df_long2$Symptom = factor(df_long2$Symptom, levels = c("Stress", "Schlaf", "Stimmung", "Angst", "Suizidalität", "Event"))
df_long2$subject = "2"

df3 = data.frame(Time = 1:t,
                 Stress = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) +1,
                 Schlaf = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                 Stimmung = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 6,
                 Angst = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                 Suizidalität = rep(0, t),
                 Event = rep(0, t))
df_long3 = as.data.frame(tidyr::pivot_longer(df3, cols = names(df3)[2:ncol(df3)], names_to = "Symptom", values_to = "Score"))
df_long3$Symptom = factor(df_long3$Symptom, levels = c("Stress", "Schlaf", "Stimmung", "Angst", "Suizidalität", "Event"))
df_long3$subject = "3"


df_long = rbind(df_long, df_long2, df_long3)


p1 = ggplot(df_long, aes(x = Time, y = Score, colour = subject)) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  geom_line(aes(group = subject)) +
  facet_grid(rows = vars(Symptom)) +
  labs(x = "Zeit") +
  coord_cartesian(ylim = c(0,10)) +
  theme(legend.position = "none") +
  transition_reveal(Time)

animate(p1, height = 6, width =3, units = "in", res = 300, renderer = gifski_renderer(loop = TRUE))
anim_save(filename = "bilder/ts_p8.gif")


# network activation ------------------------------------------------------


df = data.frame(Time = 1:t,
                Stress = c(abs(arima.sim(n = t*0.25, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))),
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3.5,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 4,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 5,
                           abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 6),
                Schlaf = c(abs(arima.sim(n = t*0.25, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                          abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 4,
                          abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3.5,
                          abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                          abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 2.5,
                          abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 2),
                Stimmung = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                Angst = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))),
                Suizidalität = rep(0, t),
                Event = c(rep(0, t*0.25), rep(5, t*0.75)))
df_long = as.data.frame(tidyr::pivot_longer(df, cols = names(df)[2:ncol(df)], names_to = "Symptom", values_to = "Score"))
df_long$Symptom = factor(df_long$Symptom, levels = c("Stress", "Schlaf", "Stimmung", "Angst", "Suizidalität", "Event"))

df_long$Colour = 0
df_long$Colour[(df_long$Symptom == "Stress" | df_long$Symptom == "Schlaf" | df_long$Symptom == "Event") & df_long$Time >= 25] = 1
df_long$Colour = factor(df_long$Colour)

p2 = ggplot(df_long, aes(x = Time, y = Score, colour = Colour)) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  geom_line(aes(group = 1)) +
  facet_grid(rows = vars(Symptom)) +
  scale_color_manual(values = c("0" = "black",
                                "1" = "red")) +
  labs(x = "Zeit") +
  coord_cartesian(ylim = c(0,10)) +
  theme(legend.position = "none") +
transition_reveal(Time)

animate(p2, height = 6, width =3, units = "in", res = 300, renderer = gifski_renderer(loop = TRUE))
anim_save(filename = "bilder/ts_p4.gif")


# Symptom spread ----------------------------------------------------------

df = data.frame(Time = 1:t,
                Stress = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 6,
                Schlaf = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 2,
                Stimmung = c(abs(arima.sim(n = t*0.25, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                         abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 6,
                         abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 5,
                         abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 4,
                         abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                         abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 2),
                Angst = c(abs(arima.sim(n = t*0.25, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))),
                            abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                            abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3.5,
                            abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 4,
                            abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 5,
                            abs(arima.sim(n = t*0.15, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 6),
                Suizidalität = rep(0, t),
                Event = c(rep(5, t*0.80), rep(0, t*0.20)))
df_long = as.data.frame(tidyr::pivot_longer(df, cols = names(df)[2:ncol(df)], names_to = "Symptom", values_to = "Score"))
df_long$Symptom = factor(df_long$Symptom, levels = c("Stress", "Schlaf", "Stimmung", "Angst", "Suizidalität", "Event"))

df_long$Colour = 0
df_long$Colour[(df_long$Symptom == "Stress" | df_long$Symptom == "Schlaf") ] = 1
df_long$Colour[df_long$Symptom == "Event" & df_long$Time <= 80] = 1
df_long$Colour[(df_long$Symptom == "Stimmung" | df_long$Symptom == "Angst") & df_long$Time >= 25] = 1
df_long$Colour = factor(df_long$Colour)

p3 = ggplot(df_long, aes(x = Time, y = Score, colour = Colour)) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  geom_line(aes(group = 1)) +
  facet_grid(rows = vars(Symptom)) +
  scale_color_manual(values = c("0" = "black",
                                "1" = "red")) +
  labs(x = "Zeit") +
  coord_cartesian(ylim = c(0,10)) +
  theme(legend.position = "none") +
  transition_reveal(Time)

animate(p3, height = 6, width = 3, units = "in", res = 300, renderer = gifski_renderer(loop = TRUE))
anim_save(filename = "bilder/ts_p5.gif")


# stable and active -------------------------------------------------------


df = data.frame(Time = 1:t,
                Stress = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                Schlaf = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 2,
                Stimmung = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 2,
                Angst = abs(arima.sim(n = t, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 7,
                Suizidalität =  c(rep(0, t*0.25),
                        abs(arima.sim(n = t*0.1, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 3,
                        rep(0, t*0.25),
                        abs(arima.sim(n = t*0.05, mean = 0.001, model = list(order = c(1,0,0), ar = 0.1))) + 6,
                        rep(0, t*0.35)),
                Event = rep(0, t))
df_long = as.data.frame(tidyr::pivot_longer(df, cols = names(df)[2:ncol(df)], names_to = "Symptom", values_to = "Score"))
df_long$Symptom = factor(df_long$Symptom, levels = c("Stress", "Schlaf", "Stimmung", "Angst", "Suizidalität", "Event"))

df_long$Colour = 0
df_long$Colour[(df_long$Symptom == "Stress" | df_long$Symptom == "Schlaf" | df_long$Symptom == "Stimmung" | df_long$Symptom == "Angst") ] = 1
df_long$Colour[(df_long$Symptom == "Suizidalität") & df_long$Time >= 25 & df_long$Time <= 35] = 1
df_long$Colour[(df_long$Symptom == "Suizidalität") & df_long$Time >= 60 & df_long$Time <= 65] = 1
df_long$Colour = factor(df_long$Colour)


p4 = ggplot(df_long, aes(x = Time, y = Score, colour = Colour)) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  geom_line(aes(group = 1)) +
  facet_grid(rows = vars(Symptom)) +
  scale_color_manual(values = c("0" = "black",
                                "1" = "red")) +
  labs(x = "Zeit") +
  coord_cartesian(ylim = c(0,10)) +
  theme(legend.position = "none") +
  transition_reveal(Time)

animate(p4, height = 6, width =3, units = "in", res = 300, renderer = gifski_renderer(loop = TRUE))
anim_save(filename = "bilder/ts_p6.gif")

