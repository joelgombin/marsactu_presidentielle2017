library(tidyverse)
library(ggalt)
library(plotly)
library(ggrepel)
library(hrbrthemes)

Sys.setenv("plotly_username"="joelgombin")
Sys.setenv("plotly_api_key"="nlymPXI0THB61oBGFS5i")

df <- read_csv2("./presidentielles_marseille.csv")
df <- df %>% 
  gather(force, score, `extrême gauche`:Divers) %>% 
  mutate(force = recode_factor(force, "extrême gauche" = "Extrême gauche", "PCF/FdG/Melenchon" = "Gauche communiste", "Ecolos" = "Écologistes", "PS" = "Socialistes et DVG", "Centre/UDF" = "Centre droit", "RPR/UMP" = "Droite", "FN/extrême droite" = "Extrême droite"))


## évolution par famille politique

couleurs <- c("Extrême gauche" = "#040404", "Gauche communiste" = "#C31622", "Écologistes" = "#74BC4C", "Socialistes et DVG" = "#EB1E4F", "Centre droit" = "#F39900", "Droite" = "#0052AE", "Extrême droite" = "#0A183D", "Divers" = "#929292")

liens <- rep(NA_character_, 8)
names(liens) <- names(couleurs)

for (i in names(couleurs)) {
  p <- df %>% 
    filter(force %in% i) %>% 
    ggplot(aes(x = Année, y = score)) +
    geom_point(colour = couleurs[i], size = 2) +
    geom_line(aes(linetype = Territoire), color = couleurs[i], alpha = 0.7) +
    geom_label_repel(aes(label = round(score, 1))) +
    scale_linetype(name = "") +
    theme_ipsum(grid = "Y", base_size = 15, axis_title_size = 12) +
    theme(legend.position = "bottom") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "", y = "en % des exprimés", title = i, caption = "Sources : ministère de l'intérieur. Réalisation : JG pour Marsactu.")
  ggsave(paste0(i, ".png"), p, "png", width = 3, height = 2, units = "in", scale = 3)
  # p <- ggplotly(p)
  # plotly_POST(p, filename = paste0("Marsactu_", i))
}

p <- df %>% 
  filter(force %in% c("Droite", "Centre droit")) %>% 
  group_by(Année, Territoire) %>% 
  summarise(score = sum(score)) %>% 
  ggplot(aes(x = Année, y = score)) +
  geom_point(colour = "#0052AE", size = 2) +
  geom_line(aes(linetype = Territoire), colour = "#0052AE", alpha = 0.7) +
  geom_label_repel(aes(label = round(score, 1))) +
  scale_linetype(name = "") +
  theme_ipsum(grid = "Y", base_size = 15, axis_title_size = 12) +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "", y = "en % des exprimés", title = "Droite et centre droite", caption = "Sources : ministère de l'intérieur. Réalisation : JG pour Marsactu.")
ggsave("TotalDroite.png", p, "png", width = 3, height = 2, units = "in", scale = 3)

elections <- unique(df$Année)

for (i in elections) {
  p <- df %>% 
    filter(Année %in% i) %>% 
    ggplot(aes(x = force, y = score)) +
    geom_col(aes(alpha = Territoire, fill = force), position = "dodge") +
    scale_fill_manual(values = couleurs, guide = "none") +
    scale_alpha_manual(values = c("Marseille" = 0.7, "France" = 0.3), name = "") +
    theme_ipsum(grid = "Y", base_size = 15, axis_title_size = 12) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle=45, hjust = 1)) +
    labs(x = "", y = "en % des exprimés", title = paste0("Élection présidentielle de ", i), caption = "Sources : ministère de l'intérieur. Réalisation : JG pour Marsactu.")
  ggsave(paste0(i, ".png"), p, "png", width = 3, height = 2, units = "in", scale = 3)
}

p <- df %>% 
  filter(force %in% c("Gauche communiste", "Socialistes et DVG")) %>% 
  group_by(Année, Territoire) %>% 
  summarise(score = sum(score)) %>% 
  ggplot(aes(x = Année, y = score)) +
  geom_point(colour = "#EB1E4F", size = 2) +
  geom_line(aes(linetype = Territoire), colour = "#EB1E4F", alpha = 0.7) +
  geom_label_repel(aes(label = round(score, 1))) +
  scale_linetype(name = "") +
  theme_ipsum(grid = "Y", base_size = 15, axis_title_size = 12) +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "", y = "en % des exprimés", title = "Gauches radicale, socialiste et communiste", caption = "Sources : ministère de l'intérieur. Réalisation : JG pour Marsactu.")
ggsave("TotalGauche.png", p, "png", width = 3, height = 2, units = "in", scale = 3)
