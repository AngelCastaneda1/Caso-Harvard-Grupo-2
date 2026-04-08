# ============================================================
# HOLLYWOOD RULES — Análisis Estadístico (VERSIÓN FINAL)
# ============================================================

# ---- CONFIGURACIÓN ----
options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

# ============================================================
# CARGAR DATOS (CSV SIN HEADER)
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

# Variable dummy MPAA
df$MPAA_D <- ifelse(df$MPAA == "R", 1, 0)

# ============================================================
# PREGUNTA 1
# ============================================================

cat("\nOpening Gross\n")
cat("Mínimo:", min(df$`Opening Gross`), "\n")
cat("Promedio:", mean(df$`Opening Gross`), "\n")
cat("Máximo:", max(df$`Opening Gross`), "\n")

cat("\nTotal U.S. Gross\n")
cat("Mínimo:", min(df$`Total U.S. Gross`), "\n")
cat("Promedio:", mean(df$`Total U.S. Gross`), "\n")
cat("Máximo:", max(df$`Total U.S. Gross`), "\n")

cat("\nTotal Non-U.S. Gross\n")
cat("Mínimo:", min(df$`Total Non-U.S. Gross`), "\n")
cat("Promedio:", mean(df$`Total Non-U.S. Gross`), "\n")
cat("Máximo:", max(df$`Total Non-U.S. Gross`), "\n")

cat("\nOpening Theatres\n")
cat("Mínimo:", min(df$`Opening Theatres`), "\n")
cat("Promedio:", mean(df$`Opening Theatres`), "\n")
cat("Máximo:", max(df$`Opening Theatres`), "\n")

cat("\nNúmero de comedias:", sum(df$Genre == "Comedy"), "\n")
cat("Películas R-rated:", sum(df$MPAA == "R"), "\n")

# ============================================================
# PREGUNTA 2
# ============================================================

df$ROI_US <- (df$`Total U.S. Gross` - df$Budget) / df$Budget

cat("\nROI promedio:", mean(df$ROI_US), "\n")

t_test <- t.test(df$ROI_US, conf.level = 0.95)
print(t_test)

t_test_12 <- t.test(df$ROI_US, mu = 0.12, alternative = "greater")
print(t_test_12)

# ============================================================
# PREGUNTA 3
# ============================================================

df <- df %>%
  mutate(
    is_comedy = if_else(Genre == "Comedy", 1L, 0L),
    roi_us = (`Total U.S. Gross` - Budget) / Budget
  )

# 3A
ttest_3a <- t.test(
  df$`Total U.S. Gross`[df$is_comedy == 1],
  df$`Total U.S. Gross`[df$is_comedy == 0]
)
print(ttest_3a)

# 3B
ttest_3b <- t.test(
  df$roi_us[df$is_comedy == 1],
  df$roi_us[df$is_comedy == 0]
)
print(ttest_3b)

# ============================================================
# PREGUNTA 4
# ============================================================

ttest_4 <- t.test(
  df$`Total U.S. Gross`[df$MPAA_D == 1],
  df$`Total U.S. Gross`[df$MPAA_D == 0]
)
print(ttest_4)

# ============================================================
# PREGUNTA 5
# ============================================================

modelo_completo <- lm(
  `Total U.S. Gross` ~ Budget + is_comedy + MPAA_D + Sequel + `Known Story`,
  data = df
)

summary(modelo_completo)

# ============================================================
# REGRESIONES ADICIONALES (PUNTO 3)
# ============================================================

model_opening <- lm(`Opening Gross` ~ Budget + Sequel + `Opening Theatres` + Summer, data = df)
summary(model_opening)

model_total <- lm(`Total U.S. Gross` ~ `Opening Gross` + Budget + Sequel, data = df)
summary(model_total)

model_critics <- lm(`Total U.S. Gross` ~ `Opening Gross` + Budget + CriticsScore, data = df)
summary(model_critics)

model_interaction <- lm(`Total U.S. Gross` ~ `Opening Gross` + Budget + CriticsScore * is_comedy, data = df)
summary(model_interaction)

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
# EXPORTAR GRÁFICAS
# ============================================================

ggsave("g1.png", g1)
ggsave("g2.png", g2)
ggsave("g3.png", g3)
ggsave("g4.png", g4)

cat("\nAnalisis completado\n")
