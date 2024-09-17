# Project: Interrupted time series
# Author: Robson Tigre
# Created: Sep 16 2024 
# Last update: Sep 16 2024 

# Setup -------------------------------------------------------------------
# install.packages("devtools", dependencies = TRUE)
# install.packages("tidybayes", dependencies = TRUE) # I needed to install this one in order to install the CausalArima package
# devtools::install_github("FMenchetti/CausalArima", dependencies = TRUE)
library(tidyverse)
library(forecast)
library(CausalImpact)
library(CausalArima)

rm(list = ls())

# EN-US: Define the intervention timepoint (day when the intervention occurs)
# PT-BR: Defina o ponto de intervenção (dia em que a intervenção ocorre)
intervention_time <- 75

# EN-US: Define the total number of days in this analysis
# PT-BR: Defina o número total de dias nesta análise
total_days <- 100

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

forecasted <- plot(ce, type = "forecast")
print(forecasted)

impact_p <-plot(ce, type="impact")
grid.arrange(impact_p$plot, impact_p$cumulative_plot)

summary_model <- impact(ce)
summary_model$arima

summary_model$impact_norm
