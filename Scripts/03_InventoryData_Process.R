# ==============================================
# GOOGLE CAPSTONE PROJECT - PHASE 3: PROCESS
# ==============================================

library(tidyverse)
library(stats)
library(skimr)
library(janitor)


# ===================================================================
# IMPORT PREPARED DATA DOCS TO START THIRD PHASE - DATA PROCESSING
# ===================================================================

# SALES DATA FRAME
# ==================
sales_Q1 <- read_csv("Data/processed/salesQ1_prep.csv")
sales_Q2 <- read_csv("Data/processed/salesQ2_prep.csv")
sales_Q3 <- read_csv("Data/processed/salesQ3_prep.csv")
sales_Q4 <- read_csv("Data/processed/salesQ4_prep.csv")

# STOCK DATA FRAMES
# ==================
stock_jan <- read_csv("Data/processed/stockjanuary_prep.csv")
stock_dec <- read_csv("Data/processed/stockdecember_prep.csv")

# PURCHASE ORDERS DATA FRAME
# ============================
po_prices <- read_csv("Data/processed/vendorprice2017_prep.csv")
purchase_orders <- read_csv("Data/processed/purchaseorders_prep.csv")
po_invoices <- read_csv("Data/processed/purchaseinvoices_prep.csv")


# ========================================================================================================================================
# CHANGE DATA TYPE FROM NUM TO CHR COLS INVENTORYID, STORE, BRAND, CLASSIFICATION, VENDORNUMBER, VENDORNAME, PONUMBER, SIZE & DESCRIPTION
# ========================================================================================================================================

cols_chr <- c("InventoryId","Store","Brand", "Classification", "VendorNumber", "VendorName", "PONumber","Size", "Description")

## SALES DATA FRAME
# ==================
sales_Q1 <- sales_Q1 %>%
  mutate(across(any_of(cols_chr), as.character))
sales_Q2 <- sales_Q2 %>%
  mutate(across(any_of(cols_chr), as.character))
sales_Q3 <- sales_Q3 %>%
  mutate(across(any_of(cols_chr), as.character))
sales_Q4 <- sales_Q4 %>%
  mutate(across(any_of(cols_chr), as.character))

## STOCK DATA FRAME
# ==================
stock_jan <- stock_jan %>%
  mutate(across(any_of(cols_chr), as.character))
stock_dec <- stock_dec %>%
  mutate(across(any_of(cols_chr), as.character))

## PURCHASE ORDERS DATA FRAME
# ============================
po_prices <- po_prices %>%
  mutate(across(any_of(cols_chr), as.character))

# -------------------------------------------------------------------------------------
## purchase_orders has 'Store', 'Brand', 'VendorNumber', 'PONumber', 'Classification'
# -------------------------------------------------------------------------------------
purchase_orders <- purchase_orders %>%
  mutate(across(any_of(cols_chr), as.character))

# -------------------------------------------
## po_invoices has 'VendorNumber', 'PONumber'
# -------------------------------------------
po_invoices <- po_invoices %>%
  mutate(across(any_of(cols_chr), as.character))

# ================================
# FIXING DETECTED ERRORS IN DATA
# ================================
# -----------------------------
## VendorNumber != VendorName 
# -----------------------------
## Count No. of unique values
# ----------------------------
length(unique(po_invoices$VendorNumber))
length(unique(po_invoices$VendorName))

# --------------------------------------------------------
## Filter VendorNumber with more than one assigned value
# --------------------------------------------------------
vendor_dif_table <- table(po_invoices$VendorNumber,po_invoices$VendorName)
rowSums_no <- rowSums(vendor_dif_table>0)

check_table <- names(rowSums_no[rowSums_no>1])
check_table

vendor_dif_table <- po_invoices[po_invoices$VendorNumber %in% check_table, ]
po_invoices %>% filter(VendorNumber==2000)
po_invoices %>% filter(VendorNumber==1587)
vendor_dif_table

# ---------------------------------------------------------------------------------------------------------------------------------
## Establish all VendorName with VendorNumber == 2000 to SOUTHERN WINE & SPIRITS NE and VendorNumber == 1587 to VINEYARD BRANDS INC
# ---------------------------------------------------------------------------------------------------------------------------------
po_invoices$VendorName[po_invoices$VendorNumber==2000] <- "SOUTHERN WINE & SPIRITS NE"
po_invoices$VendorName[po_invoices$VendorNumber==1587] <- "VINEYARD BRANDS INC"

po_prices$VendorName[po_prices$VendorNumber==2000] <- "SOUTHERN WINE & SPIRITS NE"
po_prices$VendorName[po_prices$VendorNumber==1587] <- "VINEYARD BRANDS INC"

purchase_orders$VendorName[purchase_orders$VendorNumber==2000] <- "SOUTHERN WINE & SPIRITS NE"
purchase_orders$VendorName[purchase_orders$VendorNumber==1587] <- "VINEYARD BRANDS INC"

# =======================================================================================
# CHECK IF INVENTORY ID IS CREATED BY THE COMBINATION OF STORE + _ + CITY + _ + BRAND 
# =======================================================================================
# ---------------------------------------------------------------------------------------------------------------------
## Create the expected InventoryId with mentioned cols from stock_jan as an example that contains Store, City, and Brand
# ---------------------------------------------------------------------------------------------------------------------
stock_jan_checked <- stock_jan %>%
  mutate(
    expected_InventoryId = paste(Store, City, Brand, sep = "_")
  )

# ---------------------------------------------------------------------------------------------------------------
## Compare the actual InventoryId with the expected InventoryId by creating a boolean column to check for a match
# ---------------------------------------------------------------------------------------------------------------
stock_jan_checked <- stock_jan_checked %>%
  mutate(
    InventoryId_matches_derivation = (InventoryId == expected_InventoryId)
  )

stock_jan_checked

# --------------------
## Analyze the results
# ---------------------
matching_rows <- sum(stock_jan_checked$InventoryId_matches_derivation, na.rm = TRUE)
total_rows <- nrow(stock_jan_checked)
matching_rows
total_rows ## Giving the results it is confirmed that InventoryId is composed of Store + _ + City + _ + Brand



# ----------------------------------------------------------------------
## City typing error: TARMSWORTH - TAMWORTH
# ----------------------------------------------------------------------
## Unique values identification and its frequency in stock data frames
# ----------------------------------------------------------------------
city_jan_df <- stock_jan %>% 
  group_by(City) %>% 
  tally()
city_jan_df

city_dec_df <- stock_dec %>% 
  group_by(City) %>% 
  tally()
city_dec_df

# ------------------------------------
## Replacing wrong values in City col
# -------------------------------------
stock_dec$City[stock_dec$City %in% c("TARMSWORTH")] <- "TAMWORTH"
stock_jan$City[stock_jan$City %in% c("TARMSWORTH")] <- "TAMWORTH"

# -------------------------------------------
## Replacing wrong values in InventoryId
# -------------------------------------------
# STOCK DATA FRAME
# ================
stock_jan <- stock_jan %>%
  mutate(
    # Change InventoryId IF City is 'TAMWORTH'
    InventoryId = if_else(
      City == "TAMWORTH", # Check if City is "TAMWORTH"
      paste(as.character(Store), as.character(City), as.character(Brand), sep = "_"), # Make new ID
      InventoryId # Otherwise, keep old ID
    )
  )


stock_dec <- stock_dec %>%
  mutate(
    # Change InventoryId IF City is 'TAMWORTH'
    InventoryId = if_else(
      City == "TAMWORTH", # Check if City is "TAMWORTH"
      paste(as.character(Store), as.character(City), as.character(Brand), sep = "_"), # Make new ID
      InventoryId # Otherwise, keep old ID
    )
  )

# PURCHASE ORDERS DATA FRAME
# ==========================
purchase_orders <- purchase_orders %>%
  mutate(
    InventoryId = if_else(
      Store == "13", #
      paste(as.character(Store), as.character('TAMWORTH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )

# SALES DATA FRAMES
# ==================
sales_Q1 <- sales_Q1 %>%
  mutate(
    InventoryId = if_else(
      Store == "13", #
      paste(as.character(Store), as.character('TAMWORTH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )
sales_Q2 <- sales_Q2 %>%
  mutate(
    InventoryId = if_else(
      Store == "13", #
      paste(as.character(Store), as.character('TAMWORTH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )
sales_Q3 <- sales_Q3 %>%
  mutate(
    InventoryId = if_else(
      Store == "13", #
      paste(as.character(Store), as.character('TAMWORTH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )
sales_Q4 <- sales_Q4 %>%
  mutate(
    InventoryId = if_else(
      Store == "13", #
      paste(as.character(Store), as.character('TAMWORTH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )
# ---------------------------------------------
# Unique values in Store January =! December 
# --------------------------------------------
# STOCK DATA FRAME
# ================
## Create subsets with Store and City columns
store_dif_jan <- select(stock_jan, Store, City)
store_dif_dec <- select(stock_dec, Store, City)

## Check for differences by both Store and City
stores_jan_unique <- anti_join(store_dif_jan, store_dif_dec, by = c("Store", "City"))
stores_dec_unique <- anti_join(store_dif_dec, store_dif_jan, by = c("Store", "City"))

# Between January 2016 and December 2016 a new Store (81) in a new City (PEMBRUKE) must have been opened. That would explain why those values 
# do not appear in January. As Store increases by 1 every time a unique value appears in the df and when reaches 79 passes 
# directly jump to 81, I considered it as an error and changed to be consistent with the rest of the data

sum(stock_dec$City == "PEMBROKE")
stock_dec$Store[stock_dec$City=="PEMBROKE"] <- 80
stock_dec$InventoryId <- paste(stock_dec$Store, stock_dec$City, stock_dec$Brand, sep = "_")

tail(stock_dec,n=25)

# PURCHASE ORDERS DATA FRAME
# ==========================
purchase_orders %>% filter(Store==81)
purchase_orders$Store[purchase_orders$Store==81] <- 80
purchase_orders$InventoryId <- str_replace(purchase_orders$InventoryId, "^81_", "80_")
purchase_orders$InventoryId <- str_replace(purchase_orders$InventoryId, "^80__", "80_PEMBROKE_")

purchase_orders

# SALES DATA FRAMES
# ==================
sales_Q3 %>% filter(Store==81)
sales_Q3$Store[sales_Q3$Store==81] <- 80
sales_Q3$InventoryId <- str_replace(sales_Q3$InventoryId, "^81_", "80_")
sales_Q3$InventoryId <- str_replace(sales_Q3$InventoryId, "^80__", "80_PEMBROKE_")

sales_Q3

sales_Q4 %>% filter(Store==81)
sales_Q4$Store[sales_Q4$Store==81] <- 80
sales_Q4$InventoryId <- str_replace(sales_Q4$InventoryId, "^81_", "80_")
sales_Q4$InventoryId <- str_replace(sales_Q4$InventoryId, "^80__", "80_PEMBROKE_")

sales_Q4


# ===============================================
# MISSING VALUES AND NA DETECTED IN DATA FRAMES
# ===============================================
# -------------------------------------------------------------------------
## Create a unique reference table with Store + Brand + City from stock_jan
# -------------------------------------------------------------------------
city_reference_jan <- stock_jan %>%
  select(Store, Brand, City) %>%
  distinct()
city_reference_jan
# -------------------------------------
## Check na city data frame structure
# -------------------------------------
city_na <- stock_dec %>% filter(is.na(City))
city_na

# ---------------------------------------------------------
## Check if Store 46 in stock jan is assigned to one City
# ---------------------------------------------------------
filter_store <- city_reference_jan %>% filter(Store==46)
length(unique(filter_store$City))

# --------------------------------------------------------------------
## Add City "TYWARDREATH" to the 1284 missing value rows of stock_dec 
# --------------------------------------------------------------------
# STOCK DATA FRAME
# ==================
stock_dec$City[is.na(stock_dec$City)] <- "TYWARDREATH"
skim(stock_dec)
stock_jan <- stock_jan %>%
  mutate(
    InventoryId = if_else(
      Store == "46", #
      paste(as.character(Store), as.character('TYWARDREATH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )


# PURCHASE ORDERS DATA FRAME
# ==========================
purchase_orders <- purchase_orders %>%
  mutate(
    InventoryId = if_else(
      Store == "46", #
      paste(as.character(Store), as.character('TYWARDREATH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )

# SALES DATA FRAMES
# ==================
sales_Q1 <- sales_Q1 %>%
  mutate(
    InventoryId = if_else(
      Store == "46", #
      paste(as.character(Store), as.character('TYWARDREATH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )
sales_Q2 <- sales_Q2 %>%
  mutate(
    InventoryId = if_else(
      Store == "46", #
      paste(as.character(Store), as.character('TYWARDREATH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )
sales_Q3 <- sales_Q3 %>%
  mutate(
    InventoryId = if_else(
      Store == "46", #
      paste(as.character(Store), as.character('TYWARDREATH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )
sales_Q4 <- sales_Q4 %>%
  mutate(
    InventoryId = if_else(
      Store == "46", #
      paste(as.character(Store), as.character('TYWARDREATH'), as.character(Brand), sep = "_"),
      InventoryId
    )
  )

# ----------------------------------
## NA Values in Volume_ml removal
# ----------------------------------
vol_na <- po_prices %>% filter(is.na(Volume_ml))
vol_na
po_prices <- po_prices %>% filter(Brand != 8992,
                                  Brand != 90590, 
                                  Brand != 9908,
                                  Brand != 2993,
                                  Brand != 4202)

## Clean environment 
rm(stock_jan_checked, vendor_dif_table, matching_rows,rowSums_no,total_rows, city_jan_df, store_dif_jan, store_dif_dec,
   city_dec_df, city_reference_jan, city_na, filter_store, vol_na, stores_dec_unique,stores_jan_unique, check_table)


# ===================================================================
# CREATE A DIMENSION DATA FRAME WITH INVENTORY ID UNIQUE VALUES
# ===================================================================
# --------------------------------------------------
## Initialize an empty list to store all InventoryIds
# --------------------------------------------------
all_df_inventoryId <- list()
purchase_orders <- purchase_orders %>%
  filter(format(as.Date(PODate), "%Y") == "2016")
po_invoices <- po_invoices %>%
  filter(format(as.Date(PODate), "%Y") == "2016")

# SALES DATA FRAME
# ==================
# --------------------------------------------------
## Extract InventoryId from sales data frames
# --------------------------------------------------
all_df_inventoryId[[1]] <- sales_Q1 %>% select(InventoryId)
all_df_inventoryId[[2]] <- sales_Q2 %>% select(InventoryId)
all_df_inventoryId[[3]] <- sales_Q3 %>% select(InventoryId)
all_df_inventoryId[[4]] <- sales_Q4 %>% select(InventoryId)

# STOCK DATA FRAME
# ==================
# --------------------------------------------------
## Extract InventoryId from stock data frames
# --------------------------------------------------
all_df_inventoryId[[5]] <- stock_jan %>% select(InventoryId)
all_df_inventoryId[[6]] <- stock_dec %>% select(InventoryId)

# PURCHASE ORDERS DATA FRAME
# ============================
# --------------------------------------------------
## Extract InventoryId from the purchase orders data frame
# --------------------------------------------------
all_df_inventoryId[[7]] <- purchase_orders %>% select(InventoryId)

# --------------------------------------------------
## Combine all InventoryIds into a single data frame and get unique ones
# --------------------------------------------------
dimtable_inventory <- bind_rows(all_df_inventoryId) %>%
  distinct(InventoryId) %>%
  # Sort data
  arrange(InventoryId)
skim(dimtable_inventory)


# ========= SPLIT INVENTORYID UNIQUE VALUES OF DIM TABLE INTO STORE + _ + CITY + _ + BRAND =========

dimtable_inventory <- dimtable_inventory %>%
  separate(
    col = InventoryId,        # The column to split
    into = c("Store", "City", "Brand"), # New column names
    sep = "_",                # The separator character
    remove = FALSE            # Keep the original InventoryId column
  )
dimtable_inventory


# --------------------------------------------------------------------------------------
## Add City "TYWARDREATH" to NA values and fix InventoryId to show 46_TYWARDREATH_(Brand)
# --------------------------------------------------------------------------------------

dimtable_inventory$City[dimtable_inventory$City == "NA"] <- "TYWARDREATH"
dimtable_inventory$InventoryId <- str_replace(dimtable_inventory$InventoryId, "^46_NA", "46_TYWARDREATH")
dimtable_inventory


# =================ADD COLS ONHAND JANUARY AND ONHAND DECEMBER TO INVENTORY ID DIM DF==============

# --------------------------------------------------
## Summarize on-hand quantities by InventoryId for January
# --------------------------------------------------
jan_on_hand <- stock_jan %>%
  group_by(InventoryId) %>%
  summarise(
    onHand_jan = sum(onHand_jan, na.rm = TRUE),
    .groups     = "drop"
  )
dec_on_hand <- stock_dec %>%
  group_by(InventoryId) %>%
  summarise(
    onHand_dec = sum(onHand_dec, na.rm = TRUE),
    .groups     = "drop"
  )
# ----------------------------------------------------------------------------------------------------
## Join the summaries into your inventory dimension table, defaulting to 0 where there was no match
# ----------------------------------------------------------------------------------------------------  
dimtable_inventory <- dimtable_inventory %>%
  left_join(jan_on_hand, by = "InventoryId") %>%
  left_join(dec_on_hand, by = "InventoryId") %>%
  mutate(
    # Replace any NA (no matching record) with 0
    onHand_jan = coalesce(onHand_jan, 0),
    onHand_dec = coalesce(onHand_dec, 0)
  )

# =================ADD COLS SALES & PURCHASE PRICE TO INVENTORY ID DIM DF==============
dimtable_inventory <- dimtable_inventory %>%
  left_join(po_prices, by = "Brand")
skim(dimtable_inventory)

dup_inventory<- dimtable_inventory %>%
  group_by(InventoryId) %>%
  filter(n() > 1) %>%
  ungroup()
print(dup_inventory)

dimtable_inventory <- dimtable_inventory %>%
  distinct(InventoryId, .keep_all = TRUE)
skim(dimtable_inventory)

## FOR SALES PRICES
# --------------------------------------------------
## Avg between SalesPrices Stock Jan & Stock Dec
# --------------------------------------------------
avg_stock_sales_price <- inner_join(stock_dec, stock_jan, by = "InventoryId")

avg_stock_sales_price <-  avg_stock_sales_price  %>%
  mutate(avg_sales_price = (SalesPrice.x + SalesPrice.y) / 2) %>% 
  select(InventoryId, avg_sales_price)

# ----------------------------------------------------------------------------------------------------
# Next, join these new average prices to the main inventory table, matching by InventoryId
# ----------------------------------------------------------------------------------------------------

dimtable_inventory <- dimtable_inventory %>%
  left_join(avg_stock_sales_price, by = "InventoryId") %>%
  mutate(
    # Conditionally update the sales price in the main table: if a valid average price exists, use it; otherwise, keep the original price.
    SalesPrice = if_else(
      !is.na(avg_sales_price) & avg_sales_price > 0,
      avg_sales_price,
      SalesPrice
    )
  ) %>%
  select(-avg_sales_price)

## FOR PURCHASE PRICES
purchase_prices <- purchase_orders %>% 
  group_by(InventoryId) %>% 
  summarise( 
    PurchasePrice = mean(PurchasePrice),
    PurchaseQuantity = sum(PurchaseQuantity),
    .groups = "drop"
    )
dimtable_inventory <- dimtable_inventory %>%
  left_join(purchase_prices, by = "InventoryId") %>%
  mutate(
    PurchasePrice = if_else(
      !is.na(PurchasePrice.y) & PurchasePrice.y > 0,
      mean(PurchasePrice.y),
      PurchasePrice.x
    )
  ) %>%
  select(-PurchasePrice.y, -PurchasePrice)

dimtable_inventory <- dimtable_inventory %>%
  mutate(
    PurchasePrice = PurchasePrice.x)%>%
    select(-PurchasePrice.x)
dimtable_inventory$PurchaseQuantity[is.na(dimtable_inventory$PurchaseQuantity)] <- 0

# Finally, remove any duplicate rows from the resulting table to ensure the data is clean.
dimtable_inventory <- dimtable_inventory %>%
  distinct(InventoryId, .keep_all = TRUE)

dimtable_inventory <- dimtable_inventory %>%
  unite(
    "brand_descr",
    Description,   
    Size,
    sep = " ",
    remove = TRUE
  )

skim(dimtable_inventory)
sum(dimtable_inventory$PurchaseQuantity)
sum(purchase_orders$PurchaseQuantity)

# =================CHECK IF ONE DESCRIPTION CORRESPONDS TO MORE THAN ONE BRAND AND CHANGE DESCRIPTION TO 1:1 RELATION ==============

dup_brands <- dimtable_inventory %>%
  group_by(Brand, brand_descr) %>%
  filter(n() > 1) %>%
  ungroup()
print(dup_brands)

brands_con_multiples_desc <- dimtable_inventory %>%
  group_by(brand_descr) %>%
  summarise(
    NumeroDeDescripcionesUnicas = n_distinct(Brand),
    Brand = Brand,
    VendorNumber = VendorNumber,
  ) %>%
  filter(NumeroDeDescripcionesUnicas > 1)

print(brands_con_multiples_desc)
brands_con_multiples_desc <- brands_con_multiples_desc %>%
  distinct(Brand, .keep_all = TRUE)
brands_con_multiples_desc

# Change description of the 24 Descriptions found
dimtable_inventory$brand_descr[dimtable_inventory$Brand==33333] <- "CAVIT MERLOT TRENTINO 187ML 4PK"
dimtable_inventory <- dimtable_inventory %>%
  mutate(
    brand_descr = if_else(
      VendorNumber == 2000,
      paste0(brand_descr, "V2000"),
      brand_descr
    )
  )
dimtable_inventory <- dimtable_inventory %>%
  mutate(
    brand_descr = if_else(
      VendorNumber == 4425,
      paste0(brand_descr, " V4425"),
      brand_descr
    )
  )

dimtable_inventory <- dimtable_inventory %>%
  mutate(
    brand_descr = if_else(
      VendorNumber == 4692,
      paste0(brand_descr, " V4692"),
      brand_descr
    )
  )
dimtable_inventory$brand_descr[dimtable_inventory$Brand==27095] <- "BERTANI VILLA ARVEDI AMARONE 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand==43223] <- "CH BEAUCHENE COTES DU RHONE 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand==21482] <- "CH LYNCH BAGES PAUILAC 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand==25842] <- "CH BOIS DU FIL BRDX 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand==35049] <- "CH PESQUIE COTES DU VENTOUX 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand==465] <- "CORRALEJO REPOSADO TEQUILA 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand==27262] <- "AVIARY CAB SVGN 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 25842] <- "CH BOIS DU FIL BRDX 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 35049] <- "CH PESQUIE COTES DU VENTOUX 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 37314] <- "DR FRANK JOHANNISBERG RSL 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 44369] <- "GALLO TWIN VLY MOSCATO I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 6579] <- "J LOHR RIVERSTONE CHARD 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 44704] <- "J LOHR RIVERSTONE CHARD 750ML"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 6692] <- "JABOULET COTES DU RHONE PAR 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 25060] <- "JADOT GEVREY CHAMBERTIN 13 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 25056] <- "JADOT GEVREY CHAMBERTIN 13 750ML"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 27778] <- "JADOT SAVIGNY LES BEAUNE 14 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 27782] <- "JADOT SAVIGNY LES BEAUNE 14 750ML"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 26260] <- "LUCE DELLA VITE TOSCANA 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 25137] <- "NICOLE CHANRION COTE DE BROU 750ML"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 25140] <- "NICOLE CHANRION COTE DE BROU 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 1415] <- "OLD GRAND DAD 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 21194] <- "PAUL HOBBS BECKSTOFFER CAB S 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 21737] <- "PAUL HOBBS BECKSTOFFER CAB S 750ML"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 23155] <- "ROCCA DELLE MACIE CHIANTI C 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 26813] <- "SIMONNET-FEBVRE 14 CHABLIS 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 18632] <- "VIBERTI BAROLO BUON PADRE 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 2531] <- "WHISTLEPIG 10 YR OLD RYE 750ML I"
dimtable_inventory$brand_descr[dimtable_inventory$Brand== 1781] <- "WILD TURKEY RUSSELL'S RSV 750ML I"

skim(dimtable_inventory)


rm(inventoryid_con_multiples_desc, all_df_inventoryId, avg_stock_sales_price, brands_con_multiples_desc, dup_brands, po_prices, purchase_prices, stock_dec, stock_jan, up_brands, cols_chr)

#==============CHECK IF THE DIM TABLE HAS ALL UNIQUE VALUES OF THE REST OF DF =================

# ---------------------------------------------------
## Extract unique InventoryId from the dimension table
# --------------------------------------------------
dimtable_inventory_check <- dimtable_inventory %>%
  pull(InventoryId) %>%               # get the vector of all inventoryId
  unique()                      # drop duplicates
# ---------------------------------------------
## Put the target data frames into a named list
# ---------------------------------------------
df_list_inventoryId <- list(
  purchase_orders = purchase_orders,
  sales_Q1        = sales_Q1,
  sales_Q2        = sales_Q2,
  sales_Q3        = sales_Q3,
  sales_Q4        = sales_Q4
)
# --------------------------------------------------
## Helper that computes both directions of “missing”
# --------------------------------------------------
find_inventory_mismatches <- function(df, df_name) {
  df_inventoryId        <- unique(df$InventoryId)
  missing_from_dim <- setdiff(dimtable_inventory_check, df_inventoryId)  # in dimtable but not in df
  missing_from_df  <- setdiff(df_inventoryId, dimtable_inventory_check)  # in df but not in dimtable
  
  tibble(
    data_frame                  = df_name,
    n_missing_from_dimtable     = length(missing_from_dim),
    missing_from_dimtable       = list(missing_from_dim),
    n_missing_from_df           = length(missing_from_df),
    missing_from_df             = list(missing_from_df)
  )
}
# ----------------------
## Build the full report
# ----------------------
inventory_mismatch_report <- imap_dfr(
  df_list_inventoryId,
  find_inventory_mismatches
)
# -------------------
## Inspect the report
# -------------------
print(inventory_mismatch_report)



# =================================
# CREATE A MASTER PURCHASE TABLE
# =================================

# =================ADD FREIGHT TO PURCHASE_ORDERS DF==============

# Sum freight per PO in invoices (handles multiple invoices per PO)
po_freight <- po_invoices  %>%
  group_by(PONumber)  %>%
  summarise(freight_total = sum(Freight, na.rm = TRUE), .groups = "drop")

# Join to purchase orders and allocate freight by line quantity
purchase_orders <- purchase_orders  %>%
  left_join(po_freight, by = "PONumber")  %>%
  group_by(PONumber)  %>%
  mutate(
    qty_po = sum(PurchaseQuantity, na.rm = TRUE),              # total qty in the PO
    freight_per_unit = if_else(qty_po > 0, freight_total / qty_po, NA_real_),
    freight_alloc_line = PurchaseQuantity * freight_per_unit   # line-level allocation
  )  %>%
  ungroup()


# ================= Grab the header fields from purchase_orders =================

purchase_orders <- purchase_orders %>%
  mutate(
    total_purchases_value = PurchasePrice*PurchaseQuantity,
    lead_time = ReceivingDate - PODate
  )


purchase_orders <- purchase_orders  %>%
  mutate(
    month_no   = month(PODate),           # Month number (1–12)
    month_name = month(PODate, label = TRUE, abbr = FALSE), # Full month name
    quarter    = quarter(PODate),         # Quarter (1–4)
    year       = year(PODate)             # Calendar year
  )

purchase_orders <- purchase_orders %>%
  select(InventoryId, PONumber, PODate, month_name, month_no, quarter, PurchasePrice, PurchaseQuantity, total_purchases_value, lead_time)

rm(df_list_inventoryId, inventory_mismatch_report,dimtable_inventory_check, find_inventory_mismatches)
rm(dup_inventory)


# =================================
# CREATE A MASTER SALES TABLE
# =================================

sales_orders  <- rbind(
  sales_Q1,
  sales_Q2,
  sales_Q3,
  sales_Q4
)
sales_orders <- sales_orders  %>%
  mutate(
    week_no    = isoweek(SalesDate),
    month_no   = month(SalesDate),
    month_name = month(SalesDate, label = TRUE, abbr = FALSE),
    quarter    = quarter(SalesDate),
    year       = year(SalesDate),
    total_sales_value = SalesQuantity*SalesPrice
  )
sales_orders <- sales_orders %>% select(-Store,-Brand,-year,-week_no)

rm(sales_Q1,sales_Q2,sales_Q3,sales_Q4)
skim(sales_orders)

# ===========================================
# ADD SALES QUANTITY TO DIMTABLE INVENTORY
# ==========================================
sales_qty <- sales_orders %>%
  select(InventoryId, SalesQuantity)
sales_qty <-sales_qty %>% 
  group_by(InventoryId) %>% 
  summarise(
    SalesQuantity = sum(SalesQuantity),
    .groups = "drop"
  )
sum(sales_qty$SalesQuantity)
sum(purchase_orders$PurchaseQuantity)
sum(dimtable_inventory$PurchaseQuantity)
dimtable_inventory <- dimtable_inventory %>%
  left_join(sales_qty, by = "InventoryId")
skim(dimtable_inventory)
dimtable_inventory$SalesQuantity[is.na(dimtable_inventory$SalesQuantity)] <- 0
sum(dimtable_inventory$SalesQuantity)

# =====================================================================================================================
# CONFIRM IF CLASSIFICATION 1 BELONGS TO BEVERAGES KNOWN AS SPIRITS AND CLASSIFICATION 2 BELONGS TO WINES AND LIQUEURS
# =====================================================================================================================
# -----------------------------------------
# Define keywords for high-alcohol spirits
# -----------------------------------------
spirit_keywords <- c("VODKA", "WHISKEY", "WHISKY", "RUM", "GIN", "TEQUILA", "SCOTCH", "BRANDY", "BOURBON")

# ------------------------------------------------------------------
# Define keywords for lower-alcohol drinks like wine and liqueurs
# ------------------------------------------------------------------
wine_keywords <- c("WINE", "CHARDONNAY", "MERLOT", "PINOT", "CABERNET", "SAUVIGNON", "RIESLING", "LIQUEUR", "SCHNAPPS","CHARD","WINES","WINERY","CAVA")

# ------------------------------------------------------------------
# Create a regex pattern from the keywords
# ------------------------------------------------------------------
spirit_pattern <- paste(spirit_keywords, collapse = "|")
wine_pattern <- paste(wine_keywords, collapse = "|")

# --------------------------------------------------------------------------------------------------------------
# Analysis for Spirits -Filter for descriptions containing spirit keywords and check their classification
# --------------------------------------------------------------------------------------------------------------
spirit_classification <- dimtable_inventory %>%
  filter(str_detect(brand_descr, spirit_pattern) | str_detect(VendorName, spirit_pattern)) %>%
  tabyl(Classification) %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

# ------------------------------------------------------------------------------------------------------------------------------
# Analysis for Wines & Liqueurs -Filter for descriptions containing wine/liqueur keywords and check their classification
# ------------------------------------------------------------------------------------------------------------------------------
wine_classification <- dimtable_inventory %>%
  filter(str_detect(brand_descr, wine_pattern)| str_detect(VendorName, spirit_pattern)) %>%
  tabyl(Classification) %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

# ----------------------------
# Print the resulting tables
# ----------------------------
print("Classification for products identified as Spirits:")
print(spirit_classification)

print("Classification for products identified as Wine/Liqueurs:")
print(wine_classification)

# -----------------------------------------------------------------------------------------------------
# Change Classification Values to Spirits for Classification 1 & Wine & Liqueurs for Classification 2
# -----------------------------------------------------------------------------------------------------
dimtable_inventory <- dimtable_inventory %>%
  mutate(
    Classification = recode(Classification,
                            "1" = "SPIRITS",
                            "2" = "WINES & LIQUEURS")
  )
# -----------------------
# Verify the new levels
# -----------------------
glimpse(dimtable_inventory)
levels(dimtable_inventory$Classification)

dimtable_inventory <- dimtable_inventory %>%
  select(-Volume_ml)


rm(wine_classification, spirit_classification, spirit_keywords,wine_pattern, spirit_pattern,wine_keywords, jan_on_hand,dec_on_hand,po_freight, sales_qty)


# ------------------------------------------------------------------------------------------
# ********************* LAST DF CONFIGURATION BEFORE EXPORT CSV *************************
# -----------------------------------------------------------------------------------------
po_lead <- purchase_orders %>%
  group_by(PONumber) %>%
  summarise(
    lead_time = mean(lead_time),
    .groups = "drop"
  )
#------------------------------------------------
# ----------- DIM INVENTORY -----------------------
#------------------------------------------------

dimtable_inventory <- dimtable_inventory%>%
  mutate(
    InventoryId = InventoryId,
    Store = Store,
    City = City,
    Brand = Brand,
    Description = brand_descr,
    Classification = Classification,
    VendorNumber = VendorNumber,
    VendorName = VendorName,
    PurchasePrice = PurchasePrice,
    SalesPrice = SalesPrice, 
    onhand_jan = onHand_jan,
    PurchaseQuantity = PurchaseQuantity,
    SalesQuantity =SalesQuantity,
    onhand_dec = onHand_jan + PurchaseQuantity - SalesQuantity
  )
dimtable_inventory <- dimtable_inventory %>%
  select(-onHand_dec, -onHand_jan, -brand_descr)
#------------------------------------------------
# ----------- PO INVOICES -----------------------
#------------------------------------------------
po_invoices <- po_invoices %>%
  mutate(
    days_to_invoice = InvoiceDate  - PODate,
    days_to_pay =  PayDate - InvoiceDate
  )
po_invoices  <- po_invoices %>%
  select(-InvoiceDate,-PayDate)
po_invoices <- po_invoices %>%
  left_join(po_lead, by= "PONumber")
rm(po_lead)

#----------------------------------------------------
# ----------- PURCHASE ORDERS -----------------------
#----------------------------------------------------
purchase_orders <- purchase_orders %>%
  select(-PODate, -total_purchases_value)

purchase_orders <- purchase_orders %>%
  group_by(InventoryId, PONumber, month_no, month_name, quarter) %>%
  summarise(
    PurchaseQuantity = sum(PurchaseQuantity),
    PurchasePrice = mean(PurchasePrice),
    .groups = "drop"
  )
sum(purchase_orders$PurchaseQuantity)

#----------------------------------------------------
# ----------- SALES ORDERS --------------------------
#----------------------------------------------------
sales_orders <- sales_orders %>%
  select(-SalesDate, -total_sales_value)

sales_orders <- sales_orders %>%
  group_by(InventoryId, month_no, month_name, quarter) %>%
  summarise(
    SalesQuantity = sum(SalesQuantity),
    SalesPrice = mean(SalesPrice),
    .groups = "drop"
  )
sum(sales_orders$SalesQuantity)


# =======================================
# OVERVIEW OF FINAL CLEANED DATA SETS
# =======================================
# ========= DIMTABLE INVENTORY ==============

skim(dimtable_inventory)

#--------------------------------------------
# ========= PURCHASES MASTER ==============

skim(purchase_orders)

#--------------------------------------------
# ========= SALES MASTER ==============

skim(sales_orders)

#-------------------------------------------
# ========= PO INVOICES ==============

skim(po_invoices)

#-------------------------------------------

# ==========================================================
# EXPORT CSV DOCUMENTS - PHASE 3: PROCESS - FINISHED
# ==========================================================
# Dimension Tables
# ================
write_csv(dimtable_inventory,file = "Data/cleaned/dimtable_inventoryid_cleaned.csv")
write_csv(purchase_orders, file = "Data/cleaned/purchase_orders_cleaned.csv")
write_csv(sales_orders,file = "Data/cleaned/sales_orders_cleaned.csv")
write_csv(po_invoices, file = "Data/cleaned/po_invoices.csv")



ls()
rm(dimtable_inventory, purchase_orders,sales_orders, po_invoices)

