install.packages("patchwork")
install.packages("scales")

#==============================================
# GOOGLE CAPSTONE PROJECT - PHASE 4: ANALYSIS
#==============================================

library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)  # For combining plots
library(skimr)
library(tidyr)
library(lubridate)

# =====================================================================
# IMPORT CLEANED DATA CSV  - DATA ANALYSIS AND SIMPLE VISUALIZATIONS
# =====================================================================

dimtable_inventory <- read_csv("Data/cleaned/dimtable_inventoryid_cleaned.csv")
purchase_orders <- read_csv("Data/cleaned/purchase_orders_cleaned.csv")
sales_orders <- read_csv("Data/cleaned/sales_orders_cleaned.csv")
po_invoices<- read_csv("Data/cleaned/po_invoices.csv")

cols_chr <- c("InventoryId","Store","Brand", "Classification", "VendorNumber", "VendorName", "PONumber","Size", "Description")

## SALES DATA FRAME
# ==================
sales_orders <- sales_orders %>%
  mutate(across(any_of(cols_chr), as.character))

## PURCHASE ORDERS DATA FRAME
# ============================
dimtable_inventory <- dimtable_inventory %>%
  mutate(across(any_of(cols_chr), as.character))

## PO INVOICES
# =============
po_invoices <- po_invoices %>%
  mutate(across(any_of(cols_chr), as.character))


id_unique <- dimtable_inventory %>%
  select(InventoryId)

purchases_part <- id_unique %>%
  left_join(purchase_orders, by="InventoryId")
skim(purchases_part)
sum(purchases_part$PurchaseQuantity, na.rm = TRUE)


sales_part <- id_unique %>%
  left_join(sales_orders, by="InventoryId")
skim(sales_part)

sum(sales_part$SalesQuantity, na.rm = TRUE)


# Key Definition
by_keys <- c("InventoryId", "month_no", "month_name", "quarter")

# Purchases aggregation
purch_monthly <- purchases_part %>%
  group_by(across(all_of(by_keys))) %>%
  summarise(
    PurchaseQuantity = sum(PurchaseQuantity, na.rm = TRUE),
    PurchasePrice    = suppressWarnings(first(na.omit(PurchasePrice))),
    n_pos            = n_distinct(PONumber, na.rm = TRUE),
    PONumbers        = list(unique(na.omit(PONumber))),
    .groups = "drop"
  )

# Sales Aggregation
sales_monthly <- sales_part %>%
  group_by(across(all_of(by_keys))) %>%
  summarise(
    SalesQuantity = sum(SalesQuantity, na.rm = TRUE),
    SalesPrice    = suppressWarnings(first(na.omit(SalesPrice))),
    .groups = "drop"
  )

# Combine purchases and sales
purchases_and_sales <- purch_monthly %>%
  full_join(sales_monthly, by = by_keys) %>%
  # Fill in NA by 0
  mutate(
    PurchaseQuantity = replace_na(PurchaseQuantity, 0),
    SalesQuantity    = replace_na(SalesQuantity, 0)
  )

purchases_and_sales <- purchases_and_sales %>%
  select(-PurchasePrice,-SalesPrice,-n_pos, -PONumbers)

# See NA and Filter them
purchases_and_sales_na <- purchases_and_sales %>%
  filter(is.na(month_no))
purchases_and_sales<- purchases_and_sales %>%
  filter(!is.na(month_no))

skim(purchases_and_sales)

# Validate Quantities
sum(purchases_and_sales$PurchaseQuantity)
sum(purchases_and_sales$SalesQuantity)

purchases_id <- purchase_orders %>%
  group_by(InventoryId, PONumber) %>%
  summarise(
    PurchaseQuantity = sum(PurchaseQuantity),
    .groups = "drop"
  )

rm(id_unique, purch_monthly, purchases_and_sales_na, purchases_part, sales_monthly, sales_part, by_keys, cols_chr)

# ----------------------------------------------------------------------------------------------
# ****************** INVENTORY LEVELS & STOCK PERFORMANCE **************************************
# ----------------------------------------------------------------------------------------------

sales_summary <- sales_orders %>%
  group_by(InventoryId) %>%
  summarise(
    SalesQuantity_Total = sum(SalesQuantity, na.rm = TRUE),
    SalesValue_Total = sum(SalesQuantity*SalesPrice, na.rm = TRUE),
    .groups = "drop"
  )


inventory_measures  <- dimtable_inventory %>% 
  mutate (
    stock_value_jan = dimtable_inventory$onhand_jan * dimtable_inventory$PurchasePrice,
    stock_value_dec = dimtable_inventory$onhand_dec * dimtable_inventory$PurchasePrice,
    stock_change = (dimtable_inventory$onhand_dec - dimtable_inventory$onhand_jan) / dimtable_inventory$onhand_jan * 100,
    inventory_qty_avg = ((dimtable_inventory$onhand_jan + dimtable_inventory$PurchaseQuantity - dimtable_inventory$SalesQuantity + dimtable_inventory$onhand_dec)/2),
    inventory_value_avg = ((dimtable_inventory$onhand_jan + dimtable_inventory$PurchaseQuantity - dimtable_inventory$SalesQuantity + dimtable_inventory$onhand_dec)/2)*PurchasePrice
    )
inventory_measures
skim(inventory_measures)

inventory_measures <- inventory_measures %>%
  left_join(sales_summary, by = "InventoryId") %>%
  mutate(
    SalesQuantity = coalesce(SalesQuantity, 0),
    SalesValue = coalesce(SalesQuantity*SalesPrice, 0),
    monthly_demand = SalesQuantity/12,
    coverage_months = case_when(
      monthly_demand > 0 ~ pmin(onhand_dec / monthly_demand, 60),
      onhand_dec == 0 ~ 0,
      TRUE ~ 60
    ),
    coverage_category = case_when(
      coverage_months <= 0 ~ "Stockout",
      coverage_months < 1.5 ~ "Low Stock",
      coverage_months <= 3.5 ~ "Optimal",
      coverage_months <= 8 ~ "High Stock",
      TRUE ~ "Overstock",
    ),
    turnover = ifelse(inventory_value_avg > 0,
                      SalesQuantity / inventory_qty_avg,  # usa qty avg, no solo snapshots
                      0),
    turnover_category = case_when(
      SalesQuantity == 0 ~ "Dead/Dormant", # solo si realmente no hubo movimiento
      turnover < 1    ~ "Low Rotation",
      turnover < 5   ~ "Normal",
      turnover < 10 ~ "High Rotation",
      TRUE    ~ "Hot Seller"
    )
  )

skim(inventory_measures)
inventory_measures <- inventory_measures %>% 
  select(-SalesValue_Total, -SalesQuantity_Total)
inventory_measures <- inventory_measures %>%
  filter(!(onhand_jan == 0 &
             onhand_dec == 0 &
             PurchaseQuantity == 0 &
             SalesQuantity == 0))
inventory_measures <- inventory_measures %>%
  mutate(
    stock_change = case_when(
      is.infinite(stock_change) ~ 100,   # reemplaza Inf
      is.nan(stock_change)      ~ 0,     # reemplaza NaN
      TRUE                      ~ stock_change
    )
  )

# =====================================================================
# VISUALIZATION 1: THE BIG PICTURE - INVENTORY HEALTH OVERVIEW
# =====================================================================

# Calculate key metrics for hero numbers
total_skus <- nrow(inventory_measures)
total_stock_value <- sum(inventory_measures$stock_value_dec, na.rm = TRUE) / 1000000
at_risk_skus <- sum(inventory_measures$coverage_category %in% c("Stockout", "Low Stock"))
overstock_skus <- sum(inventory_measures$coverage_category %in% c("High Stock", "Overstock"))
dead_skus <- sum(inventory_measures$turnover_category == "Dead/Dormant")

# Create summary for visualization
risk_summary <- inventory_measures %>%
  count(coverage_category) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    risk_level = case_when(
      coverage_category %in% c("Stockout", "Low Stock") ~ "Critical",
      coverage_category == "Optimal" ~ "Healthy",
      TRUE ~ "Attention Needed"
    )
  ) %>%
  arrange(factor(coverage_category, levels = c("Stockout", "Low Stock", "Optimal", "High Stock", "Overstock")))

# Colors for risk levels
risk_colors <- c(
  "Stockout" = "#ce4257",
  "Low Stock" = "#16425b", 
  "Optimal" = "#38a3a5",
  "High Stock" = "#cccccc",
  "Overstock" = "#eec170"
)

viz1 <- ggplot(risk_summary, aes(x = reorder(coverage_category, n), y = n, fill = coverage_category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(comma(n), "\n(", percentage, "%)")), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = risk_colors) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "🚨 INVENTORY HEALTH CRISIS",
    subtitle = paste0("Out of ", comma(total_skus), " SKUs: ", 
                      comma(at_risk_skus), " at stockout risk, ",
                      comma(overstock_skus), " overstocked"),
    x = "",
    y = "Number of SKUs",
    caption = paste0("Total Stock Value: $", round(total_stock_value, 1), "M")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", color = "#d62728"),
    plot.subtitle = element_text(size = 12, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold")
  )

print(viz1)

# =====================================================================
# VISUALIZATION 2: FINANCIAL IMPACT - WHAT'S AT STAKE
# =====================================================================

# Calculate financial impact by coverage category
financial_impact <- inventory_measures %>%
  group_by(coverage_category) %>%
  summarise(
    sku_count = n(),
    total_value = sum(stock_value_dec, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) %>%
  arrange(desc(total_value))

viz2 <- ggplot(financial_impact, aes(x = reorder(coverage_category, total_value), y = total_value, fill = coverage_category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0("$", round(total_value, 1), "M")), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = risk_colors) +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "💰 CAPITAL AT RISK",
    subtitle = paste0("$", round(sum(financial_impact$total_value[financial_impact$coverage_category %in% 
                                                                    c("High Stock", "Overstock")]), 1), 
                      "M tied up in excess inventory"),
    x = "",
    y = "Stock Value ($ Millions)",
    caption = "Overstock represents immediate working capital optimization opportunity"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", color = "#2ca02c"),
    plot.subtitle = element_text(size = 12, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold")
  )

print(viz2)

# =====================================================================
# VISUALIZATION 3: TURNOVER PERFORMANCE - DEAD STOCK ALERT
# =====================================================================

# Update dead stock calculation
dead_stock_skus <- sum(inventory_measures$turnover_category == "Dead/Dormant")

turnover_summary <- inventory_measures %>%
  count(turnover_category) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(factor(turnover_category, levels = c("Dead/Dormant", "Low Rotation", "Normal", "High Rotation", "Hot Seller")))

turnover_colors <- c(
  "Dead/Dormant" = "#aea4bf",
  "Low Rotation" = "#f49cbb",
  "Normal" = "#fcefb4", 
  "High Rotation" = "#57cc99",
  "Hot Seller" = "#38a3a5"
)

viz3 <- ggplot(turnover_summary, aes(x = reorder(turnover_category, n), y = n, fill = turnover_category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(comma(n), "\n(", percentage, "%)")), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = turnover_colors) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "⚡ INVENTORY VELOCITY ANALYSIS",
    subtitle = paste0(comma(dead_stock_skus), " SKUs with zero turnover - immediate delisting candidates"),
    x = "",
    y = "Number of SKUs",
    caption = "Focus: Convert slow movers to cash, protect fast movers from stockouts"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", color = "#134074"),
    plot.subtitle = element_text(size = 12, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold")
  )

print(viz3)

# =====================================================================
# VISUALIZATION 4: CLASSIFICATION COMPARISON - SPIRITS VS WINES
# =====================================================================

class_performance <- inventory_measures %>%
  group_by(Classification) %>%
  summarise(
    total_skus = n(),
    avg_turnover = round(mean(turnover, na.rm = TRUE), 2),
    total_value = sum(stock_value_dec, na.rm = TRUE) / 1000000,
    at_risk_pct = round(sum(coverage_category %in% c("Stockout", "Low Stock")) / n() * 100, 1),
    overstock_pct = round(sum(coverage_category %in% c("High Stock", "Overstock")) / n() * 100, 1),
    dead_stock_pct = round(sum(turnover_category == "Dead/Dormant") / n() * 100, 1),
    .groups = "drop"
  )

library(tidyr)  # For pivot_longer

viz4 <- class_performance %>%
  select(Classification, at_risk_pct, overstock_pct, dead_stock_pct) %>%
  pivot_longer(cols = c(at_risk_pct, overstock_pct, dead_stock_pct), 
               names_to = "metric", values_to = "percentage") %>%
  mutate(
    metric = case_when(
      metric == "at_risk_pct" ~ "Stockout Risk",
      metric == "overstock_pct" ~ "Overstock",
      metric == "dead_stock_pct" ~ "Dead Stock"
    )
  ) %>%
  ggplot(aes(x = Classification, y = percentage, fill = metric)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_dodge(width = 0.7), vjust = -0.3, 
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Stockout Risk" = "#ce4257", "Overstock" = "#eec170", "Dead Stock" = "#663a7e")) +
  labs(
    title = "🥃🍷 SPIRITS vs WINES PERFORMANCE",
    subtitle = "Risk distribution across product categories",
    x = "Product Classification", 
    y = "Percentage of SKUs",
    fill = "Risk Type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#663a7e"),
    plot.subtitle = element_text(size = 12, color = "#333333"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(viz4)


# =====================================================================
# VISUALIZATION 5: ABC CLASSIFICATION - ACTION PRIORITIZATION MATRIX
# =====================================================================

# Calculate ABC Classification based on sales value
abc_analysis <- inventory_measures %>%
  filter(SalesValue > 0) %>%  # Only products with sales
  arrange(desc(SalesValue)) %>%
  mutate(
    cumulative_value = cumsum(SalesValue),
    cumulative_pct = round(cumulative_value / sum(SalesValue) * 100, 2),
    
    # ABC classification
    abc_class = case_when(
      cumulative_pct <= 80 ~ "A",   # Top 80% of value
      cumulative_pct <= 95 ~ "B",   # Next 15% of value  
      TRUE ~ "C"                    # Bottom 5% of value
    )
  )

# ABC summary with risk analysis
abc_summary <- abc_analysis %>%
  group_by(abc_class) %>%
  summarise(
    sku_count = n(),
    total_sales_value = sum(SalesValue) / 1000000,
    pct_of_total_value = round(SalesValue / sum(abc_analysis$SalesValue) * 1000000 * 100, 1),
    avg_stock_value = mean(stock_value_dec) / 1000,
    at_risk_count = sum(coverage_category %in% c("Stockout", "Low Stock")),
    overstock_count = sum(coverage_category %in% c("High Stock", "Overstock")),
    dead_stock_count = sum(turnover_category == "Dead/Dormant"),
    .groups = "drop"
  ) %>%
  mutate(
    at_risk_pct = round(at_risk_count / sku_count * 100, 1),
    overstock_pct = round(overstock_count / sku_count * 100, 1),
    dead_stock_pct = round(dead_stock_count / sku_count * 100, 1)
  )

print("=== ABC CLASSIFICATION SUMMARY ===")
print(abc_summary)

# ABC Colors
abc_colors <- c("A" = "#d00000", "B" = "#eec170", "C" = "#2a9d8f")

# Viz 5A: ABC Distribution by SKU Count and Value
abc_viz1 <- abc_summary %>%
  select(abc_class, sku_count, total_sales_value) %>%
  pivot_longer(cols = c(sku_count, total_sales_value), names_to = "metric", values_to = "value") %>%
  mutate(
    metric_label = case_when(
      metric == "sku_count" ~ "SKU Count",
      metric == "total_sales_value" ~ "Sales Value ($M)"
    ),
    display_value = case_when(
      metric == "sku_count" ~ paste0(comma(value)),
      metric == "total_sales_value" ~ paste0("$", round(value, 1), "M")
    )
  ) %>%
  ggplot(aes(x = abc_class, y = value, fill = abc_class)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = display_value), vjust = -0.3, size = 3.5, fontface = "bold") +
  facet_wrap(~metric_label, scales = "free_y") +
  scale_fill_manual(values = abc_colors) +
  labs(
    title = "📊 ABC CLASSIFICATION: 80/20 RULE VALIDATION",
    subtitle = "Class A drives 80% of value with 26.8% of SKUs",
    x = "ABC Class",
    y = "Value"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", color = "#d00000"),
    plot.subtitle = element_text(size = 12, color = "#333333"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(abc_viz1)

# Viz 5B: Risk Distribution by ABC Class (Action Priority Matrix)
risk_by_abc <- abc_summary %>%
  select(abc_class, at_risk_pct, overstock_pct, dead_stock_pct) %>%
  pivot_longer(cols = c(at_risk_pct, overstock_pct, dead_stock_pct), 
               names_to = "risk_type", values_to = "percentage") %>%
  mutate(
    risk_label = case_when(
      risk_type == "at_risk_pct" ~ "Stockout Risk",
      risk_type == "overstock_pct" ~ "Overstock", 
      risk_type == "dead_stock_pct" ~ "Dead Stock"
    ),
    priority_level = case_when(
      abc_class == "A" & risk_type == "at_risk_pct" ~ "CRITICAL",
      abc_class == "A" ~ "HIGH",
      abc_class == "B" ~ "MEDIUM",
      abc_class == "C" & risk_type == "overstock_pct" ~ "QUICK WIN",
      abc_class == "C" & risk_type == "dead_stock_pct" ~ "LIQUIDATE",
      TRUE ~ "LOW"
    )
  )

abc_viz2 <- ggplot(risk_by_abc, aes(x = abc_class, y = percentage, fill = risk_label)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_dodge(width = 0.7), vjust = -0.3, 
            size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Stockout Risk" = "#ce4257", "Overstock" = "#eec170", "Dead Stock" = "#663a7e")) +
  labs(
    title = "🎯 ACTION PRIORITY MATRIX",
    subtitle = "Class A stockouts = Revenue Risk | Class C overstock = Quick Wins",
    x = "ABC Classification",
    y = "Percentage of SKUs at Risk",
    fill = "Risk Type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#663a7e"),
    plot.subtitle = element_text(size = 12, color = "#333333"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
print(abc_viz2)

# Add ABC class back to main dataset for export
inventory_measures <- inventory_measures %>%
  left_join(abc_analysis %>% select(InventoryId, abc_class, cumulative_pct), by = "InventoryId") %>%
  mutate(abc_class = coalesce(abc_class, "C"))  # Products with no sales = Class C


rm(abc_analysis, abc_summary,abc_viz1,abc_viz2, class_performance, dimtable_inventory, financial_impact, 
   risk_by_abc, risk_summary, sales_summary,turnover_summary, viz1, viz2, viz3, viz4, abc_colors, at_risk_skus, dead_stock_skus, 
   overstock_skus, risk_colors, total_skus, total_stock_value, turnover_colors, dead_skus, sales_orders, purchase_orders)



# =====================================================================
# EXPORT ANALYSED DATA CSV  - - PHASE 4: ANALYSIS - FINISHED
# =====================================================================
write_csv(purchases_and_sales, "Data/analysed/master_purchase_and_sales.csv")
write_csv(po_invoices, "Data/analysed/po_invoices.csv")
write_csv(inventory_measures, "Data/analysed/master_inventory_analysis.csv")
write_csv(purchases_id, "Data/analysed/purchases_id.csv")


