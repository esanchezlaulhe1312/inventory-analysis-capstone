# =====================================================
# GOOGLE CAPSTONE PROJECT - PHASE - Phase 2: PREPARE
# =====================================================

library(tidyverse)
library(stats)
library(skimr)
library(janitor)


# ===================================================================
# IMPORT RAW DATA DOCS TO START SECOND PHASE - DATA PREPARING
# ===================================================================
# --------------------------------------------------------------
## Product stock levels at the beginning of 2016 and at the end
# ---------------------------------------------------------------
stock_jan <- read_csv("Data/raw/BegInvFINAL12312016.csv")
stock_dec <- read_csv("Data/raw/EndInvFINAL12312016.csv")

# ------------------------------------
## Inventory PO vendors price in 2017
# -------------------------------------
po_prices <- read_csv("Data/raw/2017PurchasePricesDec.csv")

# ---------------------------------------------
## Purchase Orders and Invoices issued in 2016
# ---------------------------------------------
purchase_orders <- read_csv("Data/raw/PurchasesFINAL12312016.csv")
po_invoices <-read_csv("Data/raw/InvoicePurchases12312016.csv")

# -------------------------------------------------------------------------
## Sales in 2016 (1 file divided into quarters due to non affordable size)
# -------------------------------------------------------------------------
sales_Q1 <-read_csv("Data/raw/SalesRaw_Q1.csv")
sales_Q2 <-read_csv("Data/raw/SalesRaw_Q2.csv")
sales_Q3 <-read_csv("Data/raw/SalesRaw_Q3.csv")
sales_Q4 <-read_csv("Data/raw/SalesRaw_Q4.csv")


# ===================================================================
# GROUP DATA FRAMES BY PURCHASE ORDERS, BY STOCK AND BY SALES = LISTS
# ===================================================================

# PURCHASE ORDERS DATA FRAMES
# ============================
# -------------------------------------------------------------------------
## 2017 Vendor Price Lists + 2016 Purchase Orders + 2016 Vendor Invoices
# -------------------------------------------------------------------------
purchasesdf_list <- list(
  po_prices = po_prices,
  purchase_orders = purchase_orders,
  po_invoices = po_invoices
)

# STOCK DATA FRAMES
# ==================
# ------------------------------------------------------------
## STOCK - Inventory in stock in January 2016 + December 2016
# ------------------------------------------------------------
stockdf_list<- list(
  stock_jan = stock_jan,
  stock_dec = stock_dec
)

# SALES DATA FRAMES
# ==================
# -------------------
## Sales by Quarters
# -------------------
salesdf_list<- list(
  sales_Q1 = sales_Q1,
  sales_Q2 = sales_Q2,
  sales_Q3 = sales_Q3,
  sales_Q4 = sales_Q4
)


# =================================
# SHOW EACH DATAFRAME DIMENSIONS
# =================================
# ------------------------------------------
## Create a tibble to show lists dimensions
# ------------------------------------------
map_dfr(purchasesdf_list,~tibble(rows=nrow(.x),cols=ncol(.x)),.id="dataset")
map_dfr(stockdf_list,~tibble(rows=nrow(.x),cols=ncol(.x)),.id="dataset")
map_dfr(salesdf_list,~tibble(rows=nrow(.x),cols=ncol(.x)),.id="dataset")

rm (sales_Q1, sales_Q2,sales_Q3, sales_Q4, stock_dec,stock_jan, purchase_orders, po_invoices, po_prices) # Clean environment


# =======================================
# CHANGE DATA TYPE FROM NUM TO CHR COLS 
# =======================================
# -----------------------------------------------------------------------------------------------------
## Data Type Changes Cols: Store,Brand, Classification, VendorNumber, PONumber- numeric to character
# -----------------------------------------------------------------------------------------------------
chr_columns <- c("InventoryId","Store","Brand", "Classification", "VendorNumber", "VendorName", "PONumber","Approval","Size", "Description")
format_chr_columns <- function(df_list) {
  lapply(df_list, function(df) {
    df <- df %>% 
      mutate(across(any_of(chr_columns), ~ format(as.character(.x), trim = TRUE)))
    return(df)
  })
}

purchasesdf_list <- format_chr_columns(purchasesdf_list)
stockdf_list     <- format_chr_columns(stockdf_list)
salesdf_list     <- format_chr_columns(salesdf_list)


# =================================================================
# FUNCTION TO REMOVE WHITE SPACES AND CHANGE ALL TEXT TO UPPERCASE
# =================================================================
clean_chr_columns <- function(df_list) {
  lapply(df_list, function(df) {
    chr_cols <- names(df)[sapply(df, is.character)]
    df[chr_cols] <- lapply(df[chr_cols], function(col) {
      toupper(stringr::str_trim(col))
    })
    return(df)
  })
}
purchasesdf_list <- clean_chr_columns(purchasesdf_list)
stockdf_list <- clean_chr_columns(stockdf_list)
salesdf_list <- clean_chr_columns(salesdf_list)
purchasesdf_list
stockdf_list
salesdf_list


# ==========================
# SHOW DUPLICATES FUNCTION
# ==========================
# ------------------------------------------------------------------------------------
## No duplicates are found out in any of the df loaded
# ------------------------------------------------------------------------------------
dupl_rows<-function(dflist){
  dataset_names<-names(dflist)
  dup_counts<- vector("integer",length(dflist))
  for (i in seq_along(dflist)){
    dup_counts[i]<-sum(duplicated(dflist[[i]]))
  }
  tibble(dataset=dataset_names,duplicates=dup_counts)
}

dupl_rows(purchasesdf_list)
dupl_rows(stockdf_list)
dupl_rows(salesdf_list)


# =================================
# SHOW DATA FRAMES GENERAL FIGURES
# =================================
# ------------------------------------
## Summary of data franes structure
# ------------------------------------

# PURCHASE ORDERS DATA FRAMES
# ============================
summary(purchasesdf_list$po_prices)
summary(purchasesdf_list$purchase_orders)
summary(purchasesdf_list$po_invoices)

# STOCK DATA FRAMES
# ==================
summary(stockdf_list$stock_jan)
summary(stockdf_list$stock_dec)

# SALES DATA FRAMES
# ==================
summary(salesdf_list$sales_Q1)
summary(salesdf_list$sales_Q2)
summary(salesdf_list$sales_Q3)
summary(salesdf_list$sales_Q4)

# ------------------------------------------------------
## Show in % cols classification, Size, Approval and City
# ------------------------------------------------------
percent_size <- tabyl(purchasesdf_list$po_prices, Size)
adorn_pct_formatting((percent_size),digits =2,affix_sign=TRUE)

percent_class <- tabyl(purchasesdf_list$purchase_orders, Classification)
adorn_pct_formatting((percent_class),digits =2,affix_sign=TRUE)

percent_approval <- tabyl(purchasesdf_list$po_invoices, Approval)
adorn_pct_formatting((percent_approval),digits =2,affix_sign=TRUE)

percent_city <- tabyl(stockdf_list$stock_dec, City)
adorn_pct_formatting((percent_city),digits =2,affix_sign=TRUE)

rm (percent_size, percent_class, percent_approval, percent_city) # Clean environment


# =========================================
# GENERAL DATA VALIDATION AND CONSOLIDATION
# =========================================
# ------------------------------------------------------------------------
## Between Sum Quantity and Invoice Quantity with corresponding PO number
# ------------------------------------------------------------------------
po_qty_sum <- purchasesdf_list$purchase_orders %>%
  group_by(PONumber) %>%
  summarise(PurchaseQty_PO = sum(Quantity, na.rm = TRUE))

invoice_qty_sum <- purchasesdf_list$po_invoices %>%
  group_by(PONumber) %>%
  summarise(PurchaseQty_Invoice = sum(Quantity, na.rm = TRUE))

qty_comparison <- po_qty_sum %>%
  left_join(invoice_qty_sum, by = "PONumber") %>%
  mutate(
    difference = PurchaseQty_PO - PurchaseQty_Invoice,
    match = abs(difference) < 0.01
  )

qty_comparison %>%           # See % that match
  summarise(
    total = n(),
    matching = sum(match, na.rm = TRUE),
    non_matching = sum(!match, na.rm = TRUE),
    pct_matching = round(mean(match, na.rm = TRUE) * 100, 2)
  )
         
non_matching<-qty_comparison %>%         # See % that !match
  filter(!match)
view(non_matching)

rm (invoice_qty_sum, po_qty_sum, qty_comparison, non_matching) # Clean environment

# -----------------------------------------------------------------------
## Between TotalSales_usd and SalesQty * SalesPrice_usd
# -----------------------------------------------------------------------

sales_check <- salesdf_list$sales_Q1 %>% 
  mutate(CostCalc = SalesQuantity * SalesPrice,
         diff = abs(SalesDollars - CostCalc)) %>%
  summarise(
    total_registros = n(),
    registros_iguales = sum(diff < 0.01),
    registros_diferentes = sum(diff >= 0.01),
    porcentaje_iguales = round(mean(diff < 0.01) * 100, 2)
  )

print(sales_check)

# -----------------------------------------------------------------------
## Between Dollars and Quantity * PurchasePrice
# -----------------------------------------------------------------------
purchase_check <- purchasesdf_list$purchase_orders %>%
  mutate(CostCalc = Quantity * PurchasePrice,
         diff = abs(Dollars - CostCalc)) %>%
  summarise(
    total_registros = n(),
    registros_iguales = sum(diff < 0.01),
    registros_diferentes = sum(diff >= 0.01),
    porcentaje_iguales = round(mean(diff < 0.01) * 100, 2)
  )

print(purchase_check)

# ---------------------------------------------------------------------
## Check if total Dolllars are equal in purchase_orders and po_invoices
# ---------------------------------------------------------------------
sum(purchasesdf_list$purchase_orders[[15]])
sum(purchasesdf_list$po_invoices[[8]])

rm (purchase_check,sales_check) # Clean environment


# ==============================================================================
# COLUMNS REMOVAL GIVING THAT THEY WILL BE NO NECESSARY IN THE FOLLOWING PHASES
# ==============================================================================

# PURCHASE ORDERS DATA FRAMES
# ============================
purchasesdf_list$po_invoices <- purchasesdf_list$po_invoices %>%
  select(-Quantity)

purchasesdf_list <- lapply(purchasesdf_list, function(x){
  x %>% 
    select(-any_of(c("startDate","endDate","Approval","Dollars")))
})

# STOCK DATA FRAMES
# ==================
stockdf_list <-  lapply(stockdf_list, function(x){
  x %>% 
    select(-any_of(c("startDate","endDate","Approval")))
})

# SALES DATA FRAMES
# ==================
salesdf_list <-  lapply(salesdf_list, function(x){
  x %>% 
    select(-any_of(c("startDate","endDate","Approval","SalesDollars")))
})


# ===============================
# CHANGE COLS TITLE NAMES
# ===============================
# -------------------------------------------------------------------------------------------------------------------------------------------
## in "onHand" adding _jan/dec and in "Volume" adding _ml - 
## In addition, changes in Prices/Qty cols for differentiation between purchase and sales and to make sure the standardization across df 
# -------------------------------------------------------------------------------------------------------------------------------------------

# STOCK DATA FRAMES
# ==================
names(stockdf_list$stock_jan)[c(7,8)] <- c("onHand_jan","SalesPrice")
names(stockdf_list$stock_dec)[c(7,8)] <- c("onHand_dec","SalesPrice")

# PURCHASE ORDERS DATA FRAMES
# ============================
names(purchasesdf_list$po_prices)[c(3,5)] <- c("SalesPrice","Volume_ml")
names(purchasesdf_list$purchase_orders)[c(14)] <- c("PurchaseQuantity")


# ===========================================
# SORT ALL DATA FRAMES BY MAIN DATES OR BRAND
# ===========================================
sort_by_key <- function(df){
  if("PODate" %in% names(df)){
    df <- df %>%arrange(PODate)
  } else if ("SalesDate" %in% names(df)){
    df <- df %>%arrange(SalesDate)
  } else if ("Brand" %in% names(df)){
    df <- df %>%arrange(Brand)
  }
  return(df)
}
purchasesdf_list <- lapply(purchasesdf_list, sort_by_key)
head(purchasesdf_list)
stockdf_list <- lapply(stockdf_list, sort_by_key)
head(stockdf_list)
salesdf_list <- lapply(salesdf_list, sort_by_key)
head(salesdf_list)


# ===================================================
# EXPORT CSV DOCUMENTS - PHASE 2: PREPARE - FINISHED
# ===================================================

# PURCHASE ORDERS DATA FRAMES
# ============================
write_csv(purchasesdf_list$po_prices,file = "Data/processed/vendorprice2017_prep.csv")
write_csv(purchasesdf_list$purchase_orders,file = "Data/processed/purchaseorders_prep.csv")
write_csv(purchasesdf_list$po_invoices,file = "Data/processed/purchaseinvoices_prep.csv")

# STOCK DATA FRAMES
# ==================
write_csv(stockdf_list$stock_jan,file = "Data/processed/stockjanuary_prep.csv")
write_csv(stockdf_list$stock_dec,file = "Data/processed/stockdecember_prep.csv")

# SALES DATA FRAMES
# ==================
write_csv(salesdf_list$sales_Q1,file = "Data/processed/salesQ1_prep.csv")
write_csv(salesdf_list$sales_Q2,file = "Data/processed/salesQ2_prep.csv")
write_csv(salesdf_list$sales_Q3,file = "Data/processed/salesQ3_prep.csv")
write_csv(salesdf_list$sales_Q4,file = "Data/processed/salesQ4_prep.csv")

getwd()

rm (purchasesdf_list,salesdf_list,stockdf_list,chr_columns,clean_chr_columns, sort_by_key, format_chr_columns,dupl_rows) # Clean environment
