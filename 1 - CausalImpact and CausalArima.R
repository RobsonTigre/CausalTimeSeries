# Project: Interrupted time series
# Author: Robson Tigre & Cleyton Farias
# Created: Sep 14 2024 
# Last update: Sep 15 2024 

# Setup -------------------------------------------------------------------
# install.packages("devtools", dependencies = TRUE)
# install.packages("tidybayes", dependencies = TRUE) # I needed to install this one in order to install the CausalArima package
# devtools::install_github("FMenchetti/CausalArima", dependencies = TRUE)
library(tidyverse)
library(forecast)
library(CausalImpact)
library(CausalArima)

rm(list = ls())


# Data generation ---------------------------------------------------------

# EN-US: Set seed for reproducibility
# PT-BR: Defina a semente para reprodutibilidade
set.seed(123)

# EN-US: Define the total number of days in this analysis
# PT-BR: Defina o número total de dias nesta análise
total_days <- 100

# EN-US: Create a sequence of dates for the time series
# PT-BR: Crie uma sequência de datas para a série temporal
dates <- seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = total_days)

# EN-US: Generate autocorrelated predictor x1, with strong autocorrelation (ar = 0.7), a shift of +50, and positive trend [0.2 * (1:total_days))]
# PT-BR: Gere o preditor autocorrelacionado x1, com forte autocorrelação (ar = 0.7), um shift de +50 e tendência positiva [0.2 * (1:total_days)]
x1 <- ts(arima.sim(model = list(ar = 0.7), n = total_days, sd = 5)) + 50 + 0.2 * (1:total_days)

# EN-US: Extract the day of the week for each date in "dates" (we want to create day-of-week seasonality)
# PT-BR: Extraia o dia da semana para cada data em "dates" (queremos criar sazonalidade de dia da semana)
day_of_week <- weekdays(dates)

# EN-US: Assign seasonal effects for the days of the week: custom values for each day
# PT-BR: Atribua efeitos sazonais por dia da semana: valores customizados para cada dia
day_of_week_effect <- ifelse(
  day_of_week == "Saturday", 30, ifelse(
    day_of_week == "Sunday", 25, ifelse(
      day_of_week == "Monday", -5, ifelse(
        day_of_week == "Tuesday", 0, ifelse(
          day_of_week == "Wednesday", 5, ifelse(
            day_of_week == "Thursday", 10, ifelse(
              day_of_week == "Friday", 20, 0)))))))


# EN-US: Calculate week number to induce weekly variation to the data too
# PT-BR: Calcule o número da semana para induzir variação semanal nos dados também
week_number <- as.numeric(format(dates, "%U"))

# EN-US: Weekly variation using a sinusoidal function
# PT-BR: Variação semanal usando uma função senoidal
weekly_effect <- sin(2 * pi * week_number / max(week_number)) * 5

# EN-US: Baseline values for y before intervention
# PT-BR: Valores de linha de base para y antes da intervenção
y <- 100 + 0.5 * x1 + day_of_week_effect + weekly_effect + rnorm(total_days, sd = 3)

# EN_US: Add the day of the week and weekly effects to the predictor x1
# PT-BR: Adicione os efeitos de dia da semana e semanais ao preditor x1
x1 <- x1 + day_of_week_effect + weekly_effect

# EN-US: Define the intervention timepoint (day when the intervention occurs)
# PT-BR: Defina o ponto de intervenção (dia em que a intervenção ocorre)
intervention_time <- 75

# EN-US: Increase the response variable y by 20 units for all days following the intervention timepoint
# PT-BR: Aumente a variável de resposta y em 20 unidades para todos os dias após o ponto de intervenção
y[dates >= dates[intervention_time]] <- y[dates >= dates[intervention_time]] + 20

# EN-US: Build the dataset with all the information we created above
# PT-BR: Construa o dataset com todas as informações que criamos acima
data <- data.frame(
  date = dates,
  y = as.numeric(y),
  x1 = as.numeric(x1),
  day_of_week = day_of_week
)

# EN-US: Plot the y time series over time
# PT-BR: Plote a série de tempo y ao longo do tempo
ggplot(data, aes(x = date, y = y)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = data$date[intervention_time], linetype = "dashed", color = "red") +
  labs(
    title = "Time series y with intervention effect from 15-03-2024 (day 75) onwards",
    x = "Date",
    y = "y") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))

# EN-US: Plot the auxiliary time series x1 over time
# PT-BR: Plote a série temporal auxiliar x1 ao longo do tempo
ggplot(data, aes(x = date, y = x1)) +
  geom_line(color = "orange") +
  geom_vline(xintercept = data$date[intervention_time], linetype = "dashed", color = "red") +
  labs(
    title = "Time series x1 unaffected by the intervention",
    x = "Date",
    y = "x1") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))


# EN-US: Calculate average y by day of week, to show seasonality
# PT-BR: Calcule a média de y por dia da semana, para mostrar a sazonalidade
avg_y_by_day <- data %>%
  group_by(day_of_week) %>%
  summarize(avg_y = mean(y))

avg_y_by_day$day_of_week <- factor(avg_y_by_day$day_of_week, levels = c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plot average y by day of week
ggplot(avg_y_by_day, aes(x = day_of_week, y = avg_y)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Response Variable y by Day of the Week",
    x = "Day of the Week",
    y = "Average y") +
  theme_minimal()



# CausalImpact ------------------------------------------------------------

# EN-US: Define pre-intervention and post-intervention periods
# PT-BR: Defina os períodos pré-intervenção e pós-intervenção
pre_period <- c(1, intervention_time - 1) # Pre-Period: Days 1 to 74 (before the intervention).
post_period <- c(intervention_time, total_days) # Post-Period: Days 75 to 100 (after the intervention).


# EN-US: Run the CausalImpact analysis with seasonal components for day-of-week seasonality
# PT-BR: Execute a análise de CausalImpact com componentes sazonais para a sazonalidade do dia da semana
impact <- CausalImpact(
  data = cbind(y = data$y, x1 = data$x1),
  pre.period = pre_period,
  post.period = post_period,
  model.args = list(
    niter = 5000,
    nseasons = 7,  # Capture weekly seasonality
    season.duration = 1  # Seasonality repeats every 7 days
  )
)

# EN-US: Display a summary of the results
# PT-BR: Exiba um resumo dos resultados
summary(impact)
summary(impact, "report") # descriptive report

# EN-US: Plot the impact
# PT-BR: Plote o impacto
plot(impact)

# ~~~~~~~~~~~~~~~~~~~~~~~~~ 
# EN-US: Robustness Test 1: absence of placebo effect between periods t = 60 and 74 (pre-intervention)
# PT-BR: Teste de robustez 1: ausência de efeito placebo entre os períodos t = 60 and 74 (pré-intervenção)

# EN-US: Subset the data to include only periods up to 70
# PT-BR: Subset os dados para incluir apenas períodos até 70
data_placebo <- data[1:70, ]
placebo_intervention_time <- 55
pre_period_placebo <- c(1, placebo_intervention_time - 1) # Pre-Period: Days 1 to 54 (before the intervention).
post_period_placebo <- c(placebo_intervention_time, 70) # Post-Period: Days 55 to 70 (after the intervention).


impact_placebo <- CausalImpact(
  data = cbind(y = data_placebo$y, x1 = data_placebo$x1),
  pre.period = pre_period_placebo,
  post.period = post_period_placebo,
  model.args = list(
    niter = 5000,
    nseasons = 7,  # Capture weekly seasonality
    season.duration = 1  # Seasonality repeats every 7 days
  )
)

summary(impact_placebo, "report") # descriptive report
plot(impact_placebo)

# CausalArima ------------------------------------------------------------

# EN-US: Create a time series object for y with weekly seasonality
# PT-BR: Crie um objeto de série temporal para y com sazonalidade semanal
y_ts <- ts(data$y, frequency = 7)

# EN-US: Define pre-intervention and post-intervention indices
# PT-BR: Defina os índices pré-intervenção e pós-intervenção
pre_period <- 1:(intervention_time - 1) # Pre-Period: Days 1 to 75 (before the intervention).
post_period <- intervention_time:length(y_ts) # Post-Period: Days 75 to 100 (after the intervention).

# EN-US: Set up the parameters for CausalArima's int.date (the intervention date) and dates (the full date sequence) arguments
# PT-BR: Configure os parâmetros para os argumentos int.date (a data de intervenção) e dates (a sequência completa de datas) do CausalArima
intervention_date <- data[min(post_period), "date"]
all_dates <- data$date

# EN-US: Fit the CausalArima model for causal effect estimation
# PT-BR: Ajuste o modelo CausalArima para estimativa do efeito causal
ce <- CausalArima(y = ts(data$y, frequency = 7),
                  dates = all_dates, int.date = intervention_date,
                   nboot = 1000)

# EN-US: Plot the impact
# PT-BR: Plote o impacto
forecasted <- plot(ce, type = "forecast")
print(forecasted)

impact_p <-plot(ce, type = "impact")
grid.arrange(impact_p$plot, impact_p$cumulative_plot)

# EN-US: Display the normalized impact
# PT-BR: Exiba o impacto normalizado
summary_model <- impact(ce)
summary_model$arima
summary_model$impact_norm

# ~~~~~~~~~~~~~~~~~~~~~~~~~ 
# EN-US Robustness Test 2: Absence of effect in a correlated time series, but not affected by the intervention
# PT-BR Teste de robustez 2: Ausência de efeito em uma série temporal correlacionada, mas não afetada pela intervenção
ce_placebo <- CausalArima(y = ts(data$x, frequency = 7),
                  dates = all_dates, int.date = intervention_date,
                  nboot = 1000)

forecasted_placebo <- plot(ce_placebo, type = "forecast")
print(forecasted_placebo)
summary_model_placebo <- impact(ce_placebo)
summary_model_placebo$impact_norm
