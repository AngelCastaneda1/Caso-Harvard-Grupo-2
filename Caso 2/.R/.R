# ============================================================
# HOLLYWOOD RULES — Análisis Estadístico 
# ============================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

# ============================================================
# CARGAR DATOS 
# ============================================================
df <- read.csv(file.choose(), header = FALSE, stringsAsFactors = FALSE)

colnames(df) <- c(
  "Movie",
  "Opening Gross",
  "Total U.S. Gross",
  "Total Non-U.S. Gross",
  "Budget",
  "Opening Theatres",
  "Sequel",
  "Holiday",
  "Summer",
  "Genre",
  "Animation",
  "Known Story",
  "Dummy1",
  "MPAA",
  "Dummy2",
  "CriticsScore",
  "Dummy3",
  "Dummy4"
)

# ============================================================
# LIMPIEZA DE DATOS
# ============================================================

df$`Opening Gross` <- as.numeric(gsub(",", "", df$`Opening Gross`))
df$`Total U.S. Gross` <- as.numeric(gsub(",", "", df$`Total U.S. Gross`))
df$`Total Non-U.S. Gross` <- as.numeric(gsub(",", "", df$`Total Non-U.S. Gross`))
df$Budget <- as.numeric(gsub(",", "", df$Budget))
df$`Opening Theatres` <- as.numeric(gsub(",", "", df$`Opening Theatres`))
df$CriticsScore <- as.numeric(df$CriticsScore)

df <- na.omit(df)

df$MPAA_D <- ifelse(df$MPAA == "R", 1, 0)

# ============================================================
# PREGUNTA 1
# ============================================================

cat("\nPREGUNTA 1\n")

summary(df[, c("Opening Gross", "Total U.S. Gross",
               "Total Non-U.S. Gross", "Opening Theatres")])

cat("\nNúmero de comedias:", sum(df$Genre == "Comedy"), "\n")
cat("Películas R-rated:", sum(df$MPAA == "R"), "\n")

# ============================================================
# PREGUNTA 2
# ============================================================

df$ROI_US <- (df$`Total U.S. Gross` - df$Budget) / df$Budget

cat("\nROI promedio:", mean(df$ROI_US), "\n")

print(t.test(df$ROI_US))
print(t.test(df$ROI_US, mu = 0.12, alternative = "greater"))

# ============================================================
# PREGUNTA 3
# ============================================================

df <- df %>%
  mutate(
    is_comedy = if_else(Genre == "Comedy", 1L, 0L),
    roi_us = (`Total U.S. Gross` - Budget) / Budget
  )

# 3A
print(t.test(
  df$`Total U.S. Gross`[df$is_comedy == 1],
  df$`Total U.S. Gross`[df$is_comedy == 0]
))

# 3B
print(t.test(
  df$roi_us[df$is_comedy == 1],
  df$roi_us[df$is_comedy == 0]
))

# ============================================================
# PREGUNTA 4
# ============================================================

print(t.test(
  df$`Total U.S. Gross`[df$MPAA_D == 1],
  df$`Total U.S. Gross`[df$MPAA_D == 0]
))

# ============================================================
# PREGUNTA 5
# ============================================================

modelo_completo <- lm(
  `Total U.S. Gross` ~ Budget + is_comedy + MPAA_D + Sequel + `Known Story`,
  data = df
)

summary(modelo_completo)

# ============================================================
# PREGUNTA 6
# Regresión Opening Weekend
# ============================================================

cat("\nPREGUNTA 6\n")

model_opening <- lm(`Opening Gross` ~ Budget + Sequel + `Opening Theatres` + Summer, data = df)
summary(model_opening)

# ============================================================
# PREGUNTA 7
# Regla del 25%
# ============================================================

cat("\nPREGUNTA 7\n")

model_total <- lm(`Total U.S. Gross` ~ `Opening Gross` + Budget + Sequel, data = df)
summary(model_total)

# ============================================================
# PREGUNTA 8
# Regresión con críticos
# ============================================================

cat("\nPREGUNTA 8\n")

model_critics <- lm(`Total U.S. Gross` ~ `Opening Gross` + Budget + CriticsScore, data = df)
summary(model_critics)

# ============================================================
# PREGUNTA 9
# Interacción críticos × comedia
# ============================================================

cat("\nPREGUNTA 9\n")

model_interaction <- lm(
  `Total U.S. Gross` ~ `Opening Gross` + Budget + CriticsScore * is_comedy,
  data = df
)

summary(model_interaction)

# ============================================================
# PREGUNTA 10
# Star Power (aproximación con Budget)
# ============================================================

cat("\nPREGUNTA 10\n")

model_star <- lm(`Total U.S. Gross` ~ Budget + `Opening Gross`, data = df)
summary(model_star)

cat("\nInterpretación sugerida:\n")
cat("El presupuesto se usa como proxy de 'star power'. Si es significativo,\n")
cat("indica que películas con mayor inversión (actores conocidos, marketing)\n")
cat("tienden a generar mayor taquilla.\n")

# ============================================================
# GRÁFICAS
# ============================================================

tema <- theme_minimal()

g1 <- ggplot(df, aes(`Opening Gross`, `Total U.S. Gross`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Opening vs Total")

g2 <- ggplot(df, aes(Budget, `Opening Gross`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Budget vs Opening")

g3 <- ggplot(df, aes(CriticsScore, `Total U.S. Gross`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Critics vs Total")

g4 <- ggplot(df, aes(factor(is_comedy), `Total U.S. Gross`)) +
  geom_boxplot() +
  ggtitle("Comedy vs Others")

print(g1)
print(g2)
print(g3)
print(g4)

# ============================================================
# EXPORTAR
# ============================================================

ggsave("g1.png", g1)
ggsave("g2.png", g2)
ggsave("g3.png", g3)
ggsave("g4.png", g4)

cat("\nAnalisis completo finalizado\n")