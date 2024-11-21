df<-read.csv(file.choose(),header = TRUE)
df

# Parse the Date column
df$Date <- as.Date(df$Date)  # Convert to Date type

# ---------------------------------------------
# descriptive statistics
attach(df)
summary(Close)
mean(Close)
median(Close)
range(Close)
min(Close)
max(Close)
var(Close)
sd(Close)
quantile(Close)
IQR(Close)
summary(df)
require(e1071)
skewness(Close)
kurtosis(Close)
# ---------------------------------------------


#MA
require(dplyr)
require(zoo)
df <- df %>% mutate(MA_30 = zoo::rollmean(Close, k = 30, fill = NA, align = "right"), MA_200 = zoo::rollmean(Close, k = 200, fill = NA, align = "right"))
require(ggplot2)
# Plot the Closing Prices and Moving Averages
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), linewidth = 1) +  # Stock closing prices
  geom_line(
    data = df[!is.na(df$MA_30), ],  # Exclude rows with NA for MA_30
    aes(y = MA_30, color = "MA 30"),
    linewidth = 1, linetype = "dashed"
  ) +
  geom_line(
    data = df[!is.na(df$MA_200), ],  # Exclude rows with NA for MA_200
    aes(y = MA_200, color = "MA 200"),
    linewidth = 1, linetype = "dotted"
  ) +
  labs(
    title = "Stock Prices with 30-Day and 200-Day Moving Averages",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("Close" = "blue", "MA 30" = "green", "MA 200" = "red")
  )

# ---------------------------------------------

