# 📦 Inventory Analysis for XYZ Manufacturing Company
*Google Data Analytics Capstone Project*  
**Author:** Elena Sánchez-Laulhé

---

## Overview

This project addresses inventory inefficiencies at XYZ Manufacturing Company, a medium-sized producer of electronic components. The goal is to analyze stock and purchasing data to identify stockouts, excess inventory, and inefficiencies in the supply chain. The analysis supports a strategic plan for improved inventory management and procurement practices.

---

## Objectives

- Identify optimal inventory levels for raw materials, WIP, and finished goods.
- Detect stockout patterns and excess inventory.
- Analyze turnover ratios and carrying costs.
- Streamline procurement processes.
- Support sustainable inventory planning.

---

## Project Structure

```
InventoryAnalysis_CapstoneProject/
├── Data/
│   ├── raw/               # Original datasets (ignored from repo)
│   ├── processed/         # Processed datasets during different phases used in the analysis
│   └── cleaned/           # Cleaned and ready-to-use datasets after phases 2 (Prepare) and 3 (Process)
├── Scripts/
│   └── 02_InventoryAnalysis.R     # R script for data preparation Phase 2
├── Docs/
│   ├── Capstone_Report.docx       # Final report
│   ├── Capstone_Journal.docx      # Learning journal
├── .gitignore
├── README.md
├── InventoryAnalysis_CapstoneProject.Rproj
```

---

## Data Sources

- **Origin:** Kaggle  
  Dataset: [Inventory_Purchase_Sales_Analysis and Optimization](https://www.kaggle.com/datasets/bradklassen/stock-inventory-analysis)  
  License: Apache 2.0

- **Datasets included:**
  - 2017PurchasePricesDec.csv
  - PurchasesFINAL12312016.csv
  - InvoicePurchases12312016.csv
  - BegInvFINAL12312016.csv
  - EndInvFINAL12312016.csv
  - SalesFINAL12312016.csv → split into four quarterly files

- **Structure:**
  - *Purchasing:* Purchase orders, invoices, vendor pricing
  - *Stock:* Inventory at January and December 2016
  - *Sales:* Sales data for 2016 (divided into Q1, Q2, Q3, Q4 due to size constraints)

---

## Raw Dataset Dimensions

| Dataset          | Raw Rows   | Raw Columns |
|------------------|------------|-------------|
| `po_prices`      | 12,261     | 9           |
| `purchase_orders`| 45,891     | 15          |
| `po_invoices`    | 5,543      | 10          |
| `stock_jan`      | 206,529    | 9           |
| `stock_dec`      | 224,489    | 9           |
| `sales_orders`   | 12,825,363 | 14          |

---

## Data Preparation (Phase 2)

- Loaded and cleaned data into `purchaselist_df`, `stocklist_df`, and `saleslist_df`.
- Removed duplicates and trimmed white spaces.
- Standardized column types (`character` for IDs).
- Renamed columns for consistency (e.g., `Brand → BrandId`, `Size`, `VendorId`, etc.).
- Removed irrelevant columns (`Approval`, `startDate`, `endDate`, etc.).
- Validated derived columns (`TotalSales_usd`, `TotalCost_usd`) and removed after confirming >99% accuracy.
- Aggregated and compared quantities by `PONumber` between `purchase_orders` and `po_invoices`. 24.1% showed inconsistencies; however, financial totals matched, so `Quantity` was removed from `po_invoices`.
- Sales data was split into quarterly files using Python.

---

## Dataset Dimensions After Preparation

| Dataset         | Prep Rows | Prep Cols |
|-----------------|-----------|-----------|
| `po_prices`     | 12,261    | 9         |
| `purchase_orders`| 45,891   | 12        |
| `po_invoices`   | 5,543     | 8         |
| `stock_jan`     | 206,529   | 8         |
| `stock_dec`     | 224,489   | 8         |
| `sales_Q1`      | 2,758,985 | 5         |
| `sales_Q2`      | 3,058,716 | 5         |
| `sales_Q3`      | 3,470,661 | 5         |
| `sales_Q4`      | 3,537,001 | 5         |

---

## Issues and Observations

- **InventoryId mismatches**: No direct alignment between sales, stock, and purchase datasets.
- **VendorName ≠ VendorNumber**: Present in all purchase-related datasets.
- **Missing values**: Detected in `City`, `Volume`, and `Size` columns.
- **Composite keys suspected**: `InventoryId` may combine `StoreId`, `BrandId`, and `City`.
- **Redundant columns removed**: Including `Quarter`, `Approval`, and derived totals.
- **StoreId meaning ambiguous**: Possibly warehouses, retail outlets, or internal codes.

---

## Data Processing (Phase 3)

- This phase focused on advanced cleaning, error resolution, and transforming data into a star schema for analytical readiness.
  - *Data Type Standardization:* Re-confirmed and applied character type conversion for key identifier columns across all imported _prep.csv datasets.
  - *Inventory ID Validation:* Formally validated that InventoryId is a composite key derived from Store, City, and Brand (e.g., paste(Store, City, Brand, sep = "_")), with 100% matching records in stock_jan. This understanding guided subsequent InventoryId corrections.
  - *Error Correction:*
      - Vendor Alignment: Standardized VendorName for VendorNumber 2000 ("SOUTHERN WINE & SPIRITS NE") and 1587 ("VINEYARD BRANDS INC") across purchase-related datasets.
      - City Spelling: Corrected "TARMSWORTH" to "TAMWORTH" in stock_jan and stock_dec, updating affected InventoryId values accordingly.
      - Store ID Anomaly: Reassigned StoreId 81 to 80 across stock_dec, purchase_orders, sales_Q3, and sales_Q4, updating associated InventoryId values.
      - Missing City Values: Imputed 1,284 missing City entries in stock_dec with "TYWARDREATH" based on consistency with Store 46 in stock_jan, updating corresponding InventoryIds.
      - Missing Volume_ml: Removed records from po_prices with NA values in Volume_ml for specific Brand IDs (8992, 90590, 9908, 2993, 4202).
      - Zero-Value Prices: Removed rows where SalesPrice was zero in sales datasets and PurchasePrice was zero in purchase_orders.
  - *Dimension Table Construction:*
      - dimtable_inventory: Created unique InventoryId entries, splitting into Store, City, and Brand. Enriched with onHand_jan and onHand_dec quantities (defaulting to 0 if not present).
      - dimtable_brand: Consolidated unique combinations of Brand, Description, Size, Volume_ml, Classification, VendorNumber, and VendorName from po_prices.
      - dimtable_purchases: Defined unique PONumber entries with InvoiceDate, PayDate, and aggregated Freight costs.
  - *Referential Integrity & Fact Table Streamlining:*
      - Developed helper functions (find_inventory_mismatches, find_brand_mismatches) to verify that all InventoryId and Brand values in fact tables had corresponding entries in their respective dimension tables. No mismatches were found.
      - Implemented a clean_fact function to remove redundant dimension attributes from fact tables, ensuring adherence to the star schema.
  - *Additional Transformations:*
      - Size Column Restructuring: Parsed Size in dimtable_brand into Vol, UnitQty, and Unit. Standardized Unit values (e.g., "GIFT INCL.", "PACK", "PC", "OTHER") and imputed UnitQty NA values with 1. Created UnitNo combining UnitQty and Unit.
      - Distinct Price Check: Confirmed each Brand in purchase_orders had a single PurchasePrice.
      - Store-City Relationship Analysis: Summarized unique stores per city, noting cities with multiple associated stores.
  - *Summary Statistics and Export:* Used skim() for final validation of all dimension and fact tables, verifying completeness and distributions. Exported all cleaned tables to the Data/cleaned/ directory.

---

## Dataset Dimensions After Processing

| Dataset             | Prep Rows | Prep Cols |
|---------------------|-----------|-----------|
| `dimtable_brand`    | 12,256    | 7         |
| `dimtable_inventory`| 275,882   | 6         |
| `dimtable_ponumber` | 5,543     | 4         |
| `purchase_orders`   | 2,372,321 | 4         |
| `sales_Q1`          | 2,758,980 | 4         |
| `sales_Q2`          | 3,058,707 | 4         |
| `sales_Q3`          | 3,470,628 | 4         |
| `sales_Q4`          | 3,536,993 | 4         |

---

## Tools & Packages

- **Languages:** R, Python
- **R Packages:** `tidyverse`, `janitor`, `skimr`, `tabyl`, `readr`
- **Environment:** RStudio, Jupyter Notebook

---

## Ethics & Licensing

- No PII present.
- Apache 2.0 License acknowledged.
- All transformation steps documented and reproducible.

---

## Phase Progress

✅ Phase 1: Ask
✅ Phase 2: Prepare
✅ Phase 3: Process
⏳ Phase 4: Analyze
❌ Phase 5: Share (Tableau dashboard)
❌ Phase 6: Act (Recommendations)

---
