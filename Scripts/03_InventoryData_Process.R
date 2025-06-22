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

# --------------------
## Analyze the results
# ---------------------
matching_rows <- sum(stock_jan_checked$InventoryId_matches_derivation, na.rm = TRUE)
total_rows <- nrow(stock_jan_checked)
matching_rows
total_rows ## Giving the results it is confirmed that InventoryId is composed of Store + _ + City + _ + Brand


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
skim(stock_jan)

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
   city_dec_df, city_reference_jan, city_na, filter_store, vol_na, stores_dec_unique,stores_jan_unique, check_table, cols_chr)


# ===================================================================
# CREATE A DIMENSION DATA FRAME WITH INVENTORY ID UNIQUE VALUES
# ===================================================================
# --------------------------------------------------
## Initialize an empty list to store all InventoryIds
# --------------------------------------------------
all_df_inventoryId <- list()

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
all_df_inventoryId[[5]] <- stock_jan %>% select(InventoryId,Description)
all_df_inventoryId[[6]] <- stock_dec %>% select(InventoryId,Description)

# PURCHASE ORDERS DATA FRAME
# ============================
# --------------------------------------------------
## Extract InventoryId from the purchase orders data frame
# --------------------------------------------------
all_df_inventoryId[[7]] <- purchase_orders %>% select(InventoryId,Description, Classification)

# --------------------------------------------------
## Combine all InventoryIds into a single data frame and get unique ones
# --------------------------------------------------
dimtable_inventory <- bind_rows(all_df_inventoryId) %>%
  distinct(InventoryId) %>%
  # Sort data
  arrange(InventoryId)
skim(dimtable_inventory)


# ====================================================================================
# "SPLIT INVENTORYID UNIQUE VALUES OF DIM TABLE INTO STORE + _ + CITY + _ + BRAND"
# ====================================================================================
dimtable_inventory <- dimtable_inventory %>%
  separate(
    col = InventoryId,        # The column to split
    into = c("Store", "City", "Brand"), # New column names
    sep = "_",                # The separator character
    remove = FALSE            # Keep the original InventoryId column
  )
dimtable_inventory

# ===================================================================
# ADD COLS ONHAND JANUARY AND ONHAND DECEMBER TO INVENTORY ID DIM DF
# ===================================================================
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

# ----------------------------------------------------------
## Display the first few rows of the updated dimension table
# ----------------------------------------------------------
head(dimtable_inventory)

# ---------------------------------------------------------------------------------------------------------------------
## Check the structure to confirm column types (Store and Brand might be character, can convert to numeric if needed)
# ---------------------------------------------------------------------------------------------------------------------
str(dimtable_inventory)


# ===============================================================
# CHECK IF THE DIM TABLE HAS ALL UNIQUE VALUES OF THE REST OF DF
# ===============================================================
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
  sales_Q4        = sales_Q4,
  stock_jan       = stock_jan,
  stock_dec       = stock_dec
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


# =======================================================
# CREATE A DIMENSION DATA FRAME WITH BRAND UNIQUE VALUES
# =======================================================
# ---------------------------------------------------------------------------------------
## Create a dimension table with one row per unique Brand–Description–Size–Classification
# ---------------------------------------------------------------------------------------
dimtable_brand <- po_prices %>%
  # Select and rename columns to snake_case
  select(
    Brand          = Brand,
    Description    = Description,
    Size           = Size,
    Volume_ml      = Volume_ml,
    Classification = Classification,
    VendorNumber   = VendorNumber,
    VendorName     = VendorName
  ) %>%
  distinct() %>%                       # Remove duplicate rows
  arrange(Brand, Description, Size, Volume_ml, Classification)    # Sort for consistent ordering
dimtable_brand
# --------------------------------------
## Inspect the resulting dimension table
# --------------------------------------
glimpse(dimtable_brand)


# ===================================================================
# CHECK IF THE DIM TABLE HAS ALL UNIQUE VALUES OF THE REST OF DF
# ===================================================================
# -----------------------------------------------
# Extract unique brands from the dimension table
# -----------------------------------------------
dimtable_brand_check <- dimtable_brand %>%
  pull(Brand) %>%               # get the vector of all brands
  unique()                      # drop duplicates

# ---------------------------------------------
## Put the target data frames into a named list
# ---------------------------------------------
df_list_brand <- list(
  purchase_orders = purchase_orders,
  sales_Q1        = sales_Q1,
  sales_Q2        = sales_Q2,
  sales_Q3        = sales_Q3,
  sales_Q4        = sales_Q4,
  stock_jan       = stock_jan,
  stock_dec       = stock_dec
)

# ---------------------------------------------------
## Helper that computes both directions of “missing”
# ---------------------------------------------------
find_brand_mismatches <- function(df, df_name) {
  df_brands        <- unique(df$Brand)
  missing_from_dim <- setdiff(dimtable_brand_check, df_brands)  # in dimtable but not in df
  missing_from_df  <- setdiff(df_brands, dimtable_brand_check)  # in df but not in dimtable
  
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
brand_mismatch_report <- imap_dfr(
  df_list_brand,
  find_brand_mismatches
)

# -------------------
## Inspect the report
# -------------------
print(brand_mismatch_report)


# ===========================================================
# CREATE A DIMENSION DATA FRAME WITH PONUMBER UNIQUE VALUES
# ===========================================================
# --------------------------------------------
## Grab the header fields from purchase_orders
# --------------------------------------------
po_header_orders <- purchase_orders %>%
  select(
    PONumber        = PONumber,
    InvoiceDate     = InvoiceDate,
    PayDate         = PayDate,
  ) %>%
  distinct()

# --------------------------------------------------
## Pull in freight from po_invoices (one row per PO)
# --------------------------------------------------
po_freight <- po_invoices %>%
  select(
    PONumber = PONumber,
    Freight
  ) %>%
  group_by(PONumber) %>%
  summarise(
    Freight = sum(Freight, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------------------
## Left-join freight onto the orders header
# ----------------------------------------
dimtable_purchases <- po_header_orders %>%
  left_join(po_freight, by = "PONumber") %>%
  arrange(PONumber)

skim(dimtable_purchases)


# =========================================================================
# REMOVE REDUNDANT COLUMNS THAT APPEAR IN FACT DF AND IN DIMENSION TABLES
# =========================================================================
# --------------------------------------------------------------
## Define which dimension-attributes to drop (keep only the FKs)
# --------------------------------------------------------------
dim_attr_cols <- c(
  # drop everything in the inventory dim except its PK
  setdiff(names(dimtable_inventory), "InventoryId"),
  # drop everything in the brand dim except its PK
  # (note lowercase ‘brand’ if that’s how you named it)
  setdiff(names(dimtable_brand),    "brand"),    
  # drop everything in the PO dim except its PK (fixed typo here)
  setdiff(names(dimtable_purchases), "PONumber")  
) %>%
  unique() %>%              
  # make ABSOLUTELY sure we never drop any Brand column
  setdiff(c("Brand", "Brand"))

# --------------------------
## List all your fact tables
# --------------------------
fact_tables <- list(
  purchase_orders = purchase_orders,
  po_invoices      = po_invoices,
  po_prices        = po_prices,
  sales_Q1         = sales_Q1,
  sales_Q2         = sales_Q2,
  sales_Q3         = sales_Q3,
  sales_Q4         = sales_Q4,
  stock_jan        = stock_jan,
  stock_dec        = stock_dec
)

# ------------------------------------------------------
## Function to drop those dim-attributes from a given df
# ------------------------------------------------------
clean_fact <- function(df) {
  df %>%
    select(-any_of(dim_attr_cols))
}

# -------------------------
## Apply to all fact tables
# -------------------------
cleaned_facts <- fact_tables %>%
  map(clean_fact)

# -------------------------------------------------------
## Overwrite the originals with their cleaned versions
# -------------------------------------------------------
list2env(cleaned_facts, envir = .GlobalEnv)


# ==================================================================
# REMOVE OBSOLETE DATA FRAMES AND COLS FROM THE GLOBAL ENVIRONMENT
# ==================================================================
# ---------------------------------------
## Compute distinct‐price stats per Brand
# ---------------------------------------
price_check <- purchase_orders %>%
  group_by(Brand) %>%
  summarise(
    n_prices  = n_distinct(PurchasePrice),          # how many different PurchasePrices
    min_price = min(PurchasePrice, na.rm = TRUE),   # lowest price seen
    max_price = max(PurchasePrice, na.rm = TRUE),   # highest price seen
    .groups   = "drop"
  )

# --------------------------
## Confirm they are the same
# --------------------------
all_consistent <- all(price_check$n_prices == 1)

if (all_consistent) {
  print("✅ Every Brand has exactly one PurchasePrice in purchase_orders.")
} else {
  print(
    price_check %>%
      filter(n_prices > 1) %>%
      arrange(desc(n_prices))
  )
}

# ----------------------------
## Tidy up your temporary list
# ----------------------------
rm(all_df_inventoryId,brand_mismatch_report,df_list_brand,df_list_inventoryId,inventory_mismatch_report,find_brand_mismatches,find_inventory_mismatches, fact_tables,
   cleaned_facts, dim_attr_cols,dimtable_brand_check,dimtable_inventory_check,clean_fact,dec_on_hand,jan_on_hand, po_freight, po_header_orders,po_invoices, stock_dec, 
   stock_jan, po_prices, price_check, all_consistent)


# ===================================
# DATA CONSOLIDATION AND VALIDATION
# ===================================
# ----------------------------------
## Creating a Function to count zeros
# ----------------------------------

count_zeros <- function(df,df_name){
  df %>% 
    select(where(is.numeric)) %>% 
    summarise(across(everything(),~sum(.x==0))) %>% 
    pivot_longer(cols=everything(), names_to = "column",
                 values_to = "ZeroCount") %>% 
    filter(ZeroCount > 0) %>% 
    mutate(Dataset=df_name) %>% 
    relocate(Dataset)
}

## SALES DATA FRAMES 
# ==================
# -------------------------------
## Identifying 0 in numeric data
# -------------------------------
count_zeros(sales_Q1, "sales_Q1")
count_zeros(sales_Q2, "sales_Q2")
count_zeros(sales_Q3, "sales_Q3")
count_zeros(sales_Q4, "sales_Q4")

### Removal of the obs. = 0 
sales_Q1 <- sales_Q1 %>% filter(SalesPrice != 0)
sales_Q2 <- sales_Q2 %>% filter(SalesPrice != 0)
sales_Q3 <- sales_Q3 %>% filter(SalesPrice != 0)
sales_Q4 <- sales_Q4 %>% filter(SalesPrice != 0)

## PURCHASE ORDERS DATA FRAMES 
# =============================
# -------------------------------
## Identifying 0 in numeric data
# -------------------------------
count_zeros(purchase_orders, "purchase_orders")
purchase_orders <- purchase_orders %>% filter(PurchasePrice != 0)


# ==========================
# COLUMN SIZE NEW STRUCTURE
# ==========================
# ----------------------------
## See weight % per size value
# ----------------------------
percent_size <- tabyl(dimtable_brand$Size)
adorn_pct_formatting((percent_size),digits =2,affix_sign=TRUE)

# -------------------------------
## Split col size by each space
# -------------------------------
dimtable_brand <- dimtable_brand %>%
  separate(Size, into = c("Vol", "UnitQty", "Unit"), sep = " ", fill = "right")

# ----------------------------------------------------------------------------------------------
## Change Unit col values: /x by GIFT INCL., PK by PACK, NA by PC and any other value to OTHER
# ----------------------------------------------------------------------------------------------
dimtable_brand$Unit <- case_when(
  grepl("^\\d+/$", dimtable_brand[[5]]) ~ "GIFT INCL.",
  grepl("^\\.\\./$", dimtable_brand[[5]]) ~ "GIFT INCL.",
  dimtable_brand[[5]] %in% c("PK", "P") ~ "PACK",
  is.na(dimtable_brand[[5]]) ~ "PC",
  TRUE ~ "OTHER"
)

# ---------------------------------
## Change UnitQty col values NA by 1
# ---------------------------------
dimtable_brand$UnitQty[is.na(dimtable_brand$UnitQty)] <- 1

# -------------------------------------------------------------
## Create a new col named UnitNo with values of UnitQty and Unit
# -------------------------------------------------------------
dimtable_brand$UnitNo <- paste(dimtable_brand$UnitQty, toupper(dimtable_brand$Unit))

# ----------------------------------
## Delete cols Vol, UnitQty and Unit
# ----------------------------------
dimtable_brand$Vol <- NULL
dimtable_brand$Unit <- NULL
dimtable_brand$UnitQty <- NULL

percent_UnitNo <- tabyl(dimtable_brand$UnitNo)
adorn_pct_formatting((percent_UnitNo),digits =2,affix_sign=TRUE)

percent_vol <- tabyl(dimtable_brand$Volume_ml)
adorn_pct_formatting((percent_vol),digits =2,affix_sign=TRUE)


# ===================================================
# IDENTIFYING THE STORES ASSIGNED TO ONE UNIQUE CITY
# ===================================================
city_summary <- dimtable_inventory %>%
  group_by(City) %>%
  summarise(
    Stores = paste(sort(unique(Store)), collapse = ", "),
    Num_Stores = n_distinct(Store),
    Num_Brands = n_distinct(Brand),
    .groups = 'drop'
  ) %>%
  arrange(City)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------
## These 8 Cities have more than one store (): "DONCASTER"(2) "EANVERNESS"(3) GOULCREST"(2) "HARDERSFIELD"(2) "HORNSEY"(4) "LARNWICK"(2) "MOUNTMEND"(4) "TAMWORTH"(2)
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------
city_summary

rm(percent_size,percent_UnitNo,percent_vol,count_zeros,city_summary)


# ==================================================================
# SEE SUMMARY AND GENERAL STATISTICS FIGURES OF CLEANED DATA FRAMES
# ==================================================================
# Dimension Tables
# ================
skim(dimtable_brand)
skim(dimtable_inventory)
skim(dimtable_purchases)

# Fact Tables
# ============
skim(purchase_orders)
skim(sales_Q1)
skim(sales_Q2)
skim(sales_Q3)
skim(sales_Q4)


# ==========================================================
# EXPORT CSV DOCUMENTS - PHASE 3: PROCESS - FINISHED
# ==========================================================
# Dimension Tables
# ================
write_csv(dimtable_brand,file = "Data/cleaned/dimtable_brandid_cleaned.csv")
write_csv(dimtable_inventory,file = "Data/cleaned/dimtable_inventoryid_cleaned.csv")
write_csv(dimtable_purchases,file = "Data/cleaned/dimtable_ponumber_cleaned.csv")

## Fact Tables
# ============
# --------------------------------
## Purchase Orders Data Frame
# --------------------------------
write_csv(purchase_orders,file = "Data/cleaned/purchase_orders_cleaned.csv")

# --------------------------------
## Sales by Quarter Data Frames
# --------------------------------
## Sales in 2016 divided by Quarters
write_csv(sales_Q1,file = "Data/cleaned/sales_q1_cleaned.csv")
write_csv(sales_Q2,file = "Data/cleaned/sales_q2_cleaned.csv")
write_csv(sales_Q3,file = "Data/cleaned/sales_q3_cleaned.csv")
write_csv(sales_Q4,file = "Data/cleaned/sales_q4_cleaned.csv")

getwd()


