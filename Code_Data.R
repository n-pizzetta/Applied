# !!! Run this code first (before Code_Model.R) !!!

# All libraries we use
library(dplyr)
library(zoo)
library(xts)
library(tidyr)
library(magrittr)
library(ggplot2)
library(tseries)
library(forecast)
library(stargazer)

# Format of our dataframes
styled_dt <- function(df, n=5) {
  
  DT::datatable(df, 
                extensions = 'Buttons',
                rownames = FALSE,
                class = 'dataTables_wrapper',
                options = list(
                  scrollX = TRUE, 
                  pageLength = n,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel')
                ))
}
# We import our dataset
traffic <- openxlsx::read.xlsx(xlsxFile="datasets/data_airports_APP.xlsx")

# Simplified label
traffic <- traffic %>% dplyr::rename("Airport" = "REP_AIRP.(Labels)")
# We want to add the country of each airport
airports_names<- read.csv("datasets/airports_by_country.csv")
airports_names$Airport <- paste(airports_names$Airport, "airport", sep = " ")
airports_names <- airports_names %>% mutate(Country = ifelse(Country == "Chile", "Spain", Country))

# We select data from 2002 because before there is not enough data
names_col <- names(traffic)
selected_col <- c(names_col[1], names_col[50:length(names_col)])
traffic <- traffic %>% dplyr::select(all_of(selected_col))

# We add the country of each airport
traffic_mg <- merge(traffic, airports_names, by = "Airport", all.x = TRUE)

# We erase the duplicated airports
airports_dupli <- duplicated(traffic_mg)
length(traffic_mg[airports_dupli,])
traffic_mg <- unique(traffic_mg)

# REMPLACER PAR UN STARGAZER
stargazer(traffic_mg, 5)

# We rotate the dataframe to do the time series
traffic_pivot <- tidyr::pivot_longer(traffic_mg, cols = -c("Airport", "Country"), names_to = "Date", values_to = "Passengers")

# We repalce the NaN values with 0
traffic_pivot$Passengers[traffic_pivot$Passengers == ":"] <- 0

# Numerical values
traffic_pivot$Passengers <- as.numeric(traffic_pivot$Passengers)

# We add a 1 as the number in the month for the date
traffic_pivot$Date <- zoo::as.Date(paste0(traffic_pivot$Date, "-01"), format="%Y-%m-%d")

# We sum the passengers for each airport during the period 2002-2023 to select the airports with the most traffic
traffic_sum <- traffic_pivot %>% group_by(Airport, Country) %>% summarise(sumPassengers = sum(Passengers))

# Selection
airports_best_ranked <- traffic_sum %>% group_by(Country) %>% slice_max(order_by = sumPassengers)

# We erase airports with no data
airports_best_ranked <- airports_best_ranked %>% filter(sumPassengers != 0)

# We erase places which are not true countries
list_countries = c("Faroe Islands (Denmark)", "Fictional/Private", "French Guiana", 
                   "Guadeloupe (France)", "Martinique (France)", "Mayotte (France)", "Reunion (France)",
                   "Saint Barthelemy (France)", "Saint Martin (France)", "Svalbard (Norway)", NA)

airports_best_ranked <- airports_best_ranked %>% filter(!(Country %in% list_countries))

# Final dataset for descriptive statistics
airports_final_list <- unique(airports_best_ranked$Airport)
traffic_checked <- traffic_pivot %>% filter(Airport %in% airports_final_list)
traffic_checked <- traffic_checked[traffic_checked$Date <= as.Date("2023-05-01"),]

# We create a csv file with the cleaned data
write.csv(traffic_checked, file = "datasets/final-dataset.csv", row.names = FALSE, na = "NA")

# WE select 5 representative airports for our statistical analysis and our model
# List of the 5 xairports
airports_to_keep <- c("PARIS-CHARLES DE GAULLE airport", "ADOLFO SUAREZ MADRID-BARAJAS airport", "ROMA/FIUMICINO airport", "KOBENHAVN/KASTRUP airport", "OSLO/GARDERMOEN airport")

# Plot of the ranking of the countries based on their traffic
ggplot2::ggplot(airports_best_ranked, aes(x = sumPassengers, y = reorder(Country, sumPassengers))) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Ranking of Countries based on their traffic",
       x = "Total passengers carried",
       y = "Country")

# List of the airports
# Filter
filtered_traffic_checked <- traffic_checked %>%
  dplyr::filter(Airport %in% airports_to_keep)

# Plot of the traffic between 2002 and 2023 of the 5 airports
ggplot2::ggplot(filtered_traffic_checked, aes(x = Date, y = Passengers, color = Country)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Monthly Passengers per Country", x = "Date", y = "Number of Passengers")

# We plot the average number of passengers per month
traffic_mean <- traffic_checked %>%
  group_by(Date) %>%
  summarize(MeanPassengers = mean(Passengers, na.rm = TRUE))
dygraphs::dygraph(traffic_mean, main = "Average Passengers per Month", xlab = "Date")


# We create the time series for the 5 airports ()
paris <- traffic_checked %>% dplyr::filter(Airport == "PARIS-CHARLES DE GAULLE airport")
paris_ts <- paris %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2023, 5))
dygraphs::dygraph(data=paris_ts, main="Passengers per month at Paris Charles de Gaulle")

madrid <- traffic_checked %>% dplyr::filter(Airport == "ADOLFO SUAREZ MADRID-BARAJAS airport")
madrid_ts <- madrid %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2023, 5))
dygraphs::dygraph(data=madrid_ts, main="Passengers per month at Madrid")

roma <- traffic_checked %>% dplyr::filter(Airport == "ROMA/FIUMICINO airport")
roma_ts <- roma %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2023, 5))
dygraphs::dygraph(data=roma_ts, main="Passengers per month at Roma")

copenhagen <- traffic_checked %>% dplyr::filter(Airport == "KOBENHAVN/KASTRUP airport")
copenhagen_ts <- copenhagen %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2023, 5))
dygraphs::dygraph(data=copenhagen_ts, main="Passengers per month at Copenhagen")

oslo <- traffic_checked %>% dplyr::filter(Airport == "OSLO/GARDERMOEN airport")
oslo_ts <- oslo %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2023, 5))
dygraphs::dygraph(data=oslo_ts, main="Passengers per month at Oslo Gardermoen")

# We cut the time series at the end of 2019 to use it as training data for the SARIMA model
paris_ts19 <- paris %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2019, 12))
paris_ts19 <- na.omit(paris_ts19)

madrid_ts19 <- madrid %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2019, 12))
madrid_ts19 <- na.omit(madrid_ts19)

roma_ts19 <- roma %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2019, 12))
roma_ts19 <- na.omit(roma_ts19)

copenhagen_ts19 <- copenhagen %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2019, 12))
copenhagen_ts19 <- na.omit(copenhagen_ts19)

oslo_ts19 <- oslo %>% dplyr::select(4) %>% ts(frequency = 12, start = c(2002, 1), end = c(2019, 12))
oslo_ts19 <- na.omit(oslo_ts19)

# We do some descriptive statitics on PARIS CDG (as the 5 airport has the same trends)

ts_decomposed <- decompose(paris_ts19)

# Plot of the global trend of PARIS CDG
plot(paris_ts19, col = 'blue', xlab = "Year", ylab = "Passengers",
     main = "CDG passengers before seasonal differencing")

lines(ts_decomposed$trend,  col = 'red')

legend("topright", legend = c("Original", "Trend"), col = c("blue", "red"), 
       lty = c(1, 1), cex = 0.8)

# ADF test (to check for stationarity)

adf_test <- adf.test(paris_ts19, alternative = "stationary")

# Results of the ADF test
print(adf_test)

# p-value < 0.05, we reject the null hypothesis of non-stationarity

# Plot of the Autocorrelation Function (ACF)
forecast::ggAcf(paris_ts19) +
  ggplot2::ggtitle("Sample ACF for CDG airport")
# Plot of the Partial Autocorrelation Function (PACF)
forecast::ggPacf(paris_ts19) +
  ggplot2::ggtitle("Sample PACF for CDG airport")
# Plot of the difference
paris_ts19_diff <- diff(paris_ts19, lag = 12)

# Plot of the difference with the global trend
ts_stl <- stl(paris_ts19_diff, s.window = "periodic")
plot(paris_ts19_diff, col = 'blue', xlab = "Year", ylab = "Passengers",
     main = "CDG passengers after seasonal differencing")

lines(ts_stl$time.series[, "trend"], col = 'red')

legend("topright", legend = c("Original", "Trend"), col = c("blue", "red"), 
       lty = c(1, 1), cex = 0.8)

# ADF test on the diffrerence
adf_test2 <- adf.test(paris_ts19_diff, alternative = "stationary")

# Results of the ADF test
print(adf_test2)

# p-value < 0.05, we reject the null hypothesis of non-stationarity

# Plot of the Autocorrelation Function (ACF)
forecast::ggAcf(paris_ts19_diff) +
  ggplot2::ggtitle("Sample ACF for CDG airport after differencing")
# Plot of the Partial Autocorrelation Function (PACF)
forecast::ggPacf(paris_ts19_diff) +
  ggplot2::ggtitle("Sample PACF for CDG airport after differencing")

# In the next part of the code, we use a dataset vith the 3 politics as dummies to do SARIMAX model
# Importing our dataset containing the policies and the number of passengers
airports_politics <- openxlsx::read.xlsx(xlsxFile="datasets/DATA_POLITICS.xlsx")

# Creating one dataframe for policies and one for airports
col_start <- which(names(airports_politics) == "2002-01")
col_end <- which(names(airports_politics) == "2023-09")

politics <- airports_politics %>% dplyr::select(1, (col_end+1):length(airports_politics))


# Splitting the dataframe policies to get one for each policy
# We need the split to pivot each one separatly and then merge them back

# a - Borders main EU period
col_start_a <- which(names(airports_politics) == "2002-01.a")
col_end_a <- which(names(airports_politics) == "2023-09.a")

policy_a <- airports_politics %>% dplyr::select(1, col_start_a : col_end_a)

# b - Borders non-EU period
col_start_b <- which(names(airports_politics) == "2002-01.b")
col_end_b <- which(names(airports_politics) == "2023-09.b")

policy_b <- airports_politics %>% dplyr::select(1, col_start_b : col_end_b)

# c - Negative tests period
col_start_c <- which(names(airports_politics) == "2002-01.c")
col_end_c <- which(names(airports_politics) == "2023-09.c")

policy_c <- airports_politics %>% dplyr::select(1, col_start_c : col_end_c)

# Pivoting the dataframes
policy_a <- tidyr::pivot_longer(policy_a, cols = -c("Airport"), names_to = "Date", values_to = "Borders main EU period")
policy_b <- tidyr::pivot_longer(policy_b, cols = -c("Airport"), names_to = "Date", values_to = "Borders non-EU period")
policy_c <- tidyr::pivot_longer(policy_c, cols = -c("Airport"), names_to = "Date", values_to = "Negative tests period")

# Modifying the name of the date column
policy_a$Date <- gsub("\\.a$", "", policy_a$Date)
policy_b$Date <- gsub("\\.b$", "", policy_b$Date)
policy_c$Date <- gsub("\\.c$", "", policy_c$Date)

# Merging the four dataframes to get the final one at the good format
politics_formate <- merge(policy_a, policy_b, by = c("Airport", "Date"), all.x = TRUE)
politics_formate <- merge(politics_formate, policy_c, by = c("Airport", "Date"), all.x = TRUE)

# Changing the date format
politics_formate$Date <- zoo::as.Date(paste0(politics_formate$Date, "-01"), format="%Y-%m-%d")
paris_policies <- politics_formate[politics_formate$Airport == "PARIS-CHARLES DE GAULLE airport",]
madrid_policies <- politics_formate[politics_formate$Airport == "ADOLFO SUAREZ MADRID-BARAJAS airport",]
roma_policies <- politics_formate[politics_formate$Airport == "ROMA/FIUMICINO airport",]
copenhagen_policies <- politics_formate[politics_formate$Airport == "KOBENHAVN/KASTRUP airport",]
oslo_policies <- politics_formate[politics_formate$Airport == "OSLO/GARDERMOEN airport",]
styled_dt(paris_policies[paris_policies$Date >= as.Date("2020-01-01") & paris_policies$Date <= as.Date("2022-10-01"),])
styled_dt(madrid_policies[madrid_policies$Date >= as.Date("2020-01-01") & madrid_policies$Date <= as.Date("2022-10-01"),])
styled_dt(roma_policies[roma_policies$Date >= as.Date("2020-01-01") & roma_policies$Date <= as.Date("2022-10-01"),])
styled_dt(copenhagen_policies[copenhagen_policies$Date >= as.Date("2020-01-01") & copenhagen_policies$Date <= as.Date("2022-10-01"),])
styled_dt(oslo_policies[oslo_policies$Date >= as.Date("2020-01-01") & oslo_policies$Date <= as.Date("2022-10-01"),])

# Creating the CovidDummy, we will also use it in the SARIMAX
paris$CovidDummy <- ifelse(paris$Date >= as.Date("2020-03-01") & paris$Date <= as.Date("2020-12-01"), 1, 0)
madrid$CovidDummy <- ifelse(madrid$Date >= as.Date("2020-03-01") & madrid$Date <= as.Date("2020-12-01"), 1, 0)
roma$CovidDummy <- ifelse(roma$Date >= as.Date("2020-03-01") & roma$Date <= as.Date("2020-12-01"), 1, 0)
copenhagen$CovidDummy <- ifelse(copenhagen$Date >= as.Date("2020-03-01") & copenhagen$Date <= as.Date("2020-12-01"), 1, 0)
oslo$CovidDummy <- ifelse(oslo$Date >= as.Date("2020-03-01") & oslo$Date <= as.Date("2020-12-01"), 1, 0)

# Exogenous variables for our SARIMAX models
paris_covid <- paris$CovidDummy
madrid_covid <- madrid$CovidDummy
roma_covid <- roma$CovidDummy
copenhagen_covid <- copenhagen$CovidDummy
oslo_covid <- oslo$CovidDummy

