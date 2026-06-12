# ==========================================================
# 1. Load Required Libraries
# ==========================================================

library(tidyverse)
library(lubridate)
library(scales)
library(viridis)

# ==========================================================
# 2. Load Dataset
# ==========================================================

# Sales transaction dataset
sales <- Sales_Store1

# ==========================================================
# 3. Data Understanding
# ==========================================================

# First few records
head(sales)

# Statistical summary
summary(sales)

# Data structure review
glimpse(sales)

# ==========================================================
# 4. Data Preparation
# ==========================================================
# Create analytical variables from transaction timestamps
# and calculate total transaction value.

sales <- sales %>%
  mutate(
    opened_dt = mdy_hm(Opened),
    date = as.Date(opened_dt),
    year = year(opened_dt),
    month_num = month(opened_dt),
    
    month = factor(
      month(opened_dt,
            label = TRUE,
            abbr = TRUE),
      levels = month.abb[7:12],
      ordered = TRUE
    ),
    
    wday = factor(
      wday(opened_dt,
           label = TRUE,
           abbr = TRUE),
      levels = c(
        "Mon","Tue","Wed",
        "Thu","Fri","Sat","Sun"
      ),
      ordered = TRUE
    ),
    
    day = day(opened_dt),
    hour = hour(opened_dt),
    
    total_amount =
      Amount + Tax + Tip + Gratuity
  ) %>%
  filter(!is.na(total_amount))

# ==========================================================
# 5. Executive KPI Dashboard
# ==========================================================
# Key business metrics management would monitor.

sales_kpi <- sales %>%
  summarise(
    Total_Revenue = sum(total_amount),
    Transactions = n(),
    Average_Order_Value = mean(total_amount),
    Maximum_Order = max(total_amount)
  )

sales_kpi

# ==========================================================
# 6. Monthly Revenue Analysis
# ==========================================================
# Identify revenue trends and seasonality.

monthly_sales <- sales %>%
  group_by(month) %>%
  summarise(
    total_sales = sum(total_amount),
    .groups = "drop"
  ) %>%
  mutate(
    growth_pct =
      round(
        (total_sales / lag(total_sales) - 1) * 100,
        1
      )
  )

monthly_sales

ggplot(monthly_sales,
       aes(month, total_sales)) +
  geom_col(fill = "#2C6EBA") +
  geom_text(
    aes(label = dollar(total_sales)),
    vjust = -0.3
  ) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Monthly Revenue Performance",
    subtitle = "July to December",
    x = "Month",
    y = "Revenue"
  ) +
  theme_minimal()

# ==========================================================
# 7. Daily Revenue Trend
# ==========================================================
# Monitor fluctuations in daily business performance.

daily_sales <- sales %>%
  group_by(date) %>%
  summarise(
    total_sales = sum(total_amount),
    .groups = "drop"
  )

ggplot(daily_sales,
       aes(date, total_sales)) +
  geom_line(
    color = "#009E73",
    linewidth = 1
  ) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Daily Revenue Trend",
    subtitle = "Revenue fluctuations over time",
    x = "Date",
    y = "Revenue"
  ) +
  theme_minimal()

# ==========================================================
# 8. Day-of-Week Performance
# ==========================================================
# Determine which days generate the most revenue.

weekday_sales <- sales %>%
  group_by(wday) %>%
  summarise(
    total_sales = sum(total_amount),
    .groups = "drop"
  )

ggplot(weekday_sales,
       aes(wday, total_sales)) +
  geom_col(fill = "#F39C12") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Revenue by Day of Week",
    x = "Day",
    y = "Revenue"
  ) +
  theme_minimal()

# ==========================================================
# 9. Hourly Sales Analysis
# ==========================================================
# Identify peak operating hours for staffing decisions.

hourly_sales <- sales %>%
  filter(hour >= 10) %>%
  group_by(hour) %>%
  summarise(
    total_sales = sum(total_amount),
    .groups = "drop"
  )

ggplot(hourly_sales,
       aes(hour, total_sales)) +
  geom_col(fill = "#8E44AD") +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(breaks = 10:23) +
  labs(
    title = "Revenue by Hour",
    x = "Hour",
    y = "Revenue"
  ) +
  theme_minimal()

# ==========================================================
# 10. Sales Heatmap
# ==========================================================
# Visualize demand patterns across hours and weekdays.

hourly_heatmap <- sales %>%
  filter(hour >= 10) %>%
  group_by(wday, hour) %>%
  summarise(
    total_sales = sum(total_amount),
    .groups = "drop"
  )

ggplot(hourly_heatmap,
       aes(hour, wday, fill = total_sales)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(
    labels = dollar
  ) +
  labs(
    title = "Revenue Heatmap",
    x = "Hour",
    y = "Day",
    fill = "Revenue"
  ) +
  theme_minimal()

# ==========================================================
# 11. Outlier Detection
# ==========================================================
# Remove extreme transactions using IQR methodology.

Q1 <- quantile(
  sales$total_amount,
  0.25
)

Q3 <- quantile(
  sales$total_amount,
  0.75
)

IQR_value <- IQR(
  sales$total_amount
)

upper_limit <- Q3 +
  (1.5 * IQR_value)

sales_clean <- sales %>%
  filter(
    total_amount <= upper_limit
  )

# ==========================================================
# 12. Transaction Distribution
# ==========================================================
# Understand customer spending behavior.

ggplot(sales_clean,
       aes(total_amount)) +
  geom_histogram(
    bins = 40,
    fill = "#3498DB",
    color = "white"
  ) +
  scale_x_continuous(
    labels = dollar
  ) +
  labs(
    title = "Transaction Distribution",
    x = "Transaction Amount",
    y = "Frequency"
  ) +
  theme_minimal()

# ==========================================================
# 13. Average Order Value by Day
# ==========================================================
# Determine customer spending patterns.

avg_order_day <- sales %>%
  group_by(wday) %>%
  summarise(
    avg_order =
      mean(total_amount),
    .groups = "drop"
  )

avg_order_day

# ==========================================================
# 14. Business Recommendations
# ==========================================================

# Findings:
#
# • Identify highest revenue month
# • Identify highest revenue day
# • Identify peak operating hours
# • Measure average customer spend
#
# Recommendations:
#
# 1. Increase staffing during peak hours.
# 2. Launch promotions during low-demand periods.
# 3. Monitor high-value transactions.
# 4. Use monthly trends for inventory planning.
# 5. Develop sales forecasting models.
#
############################################################
# End of Analysis
############################################################
