# 1. Initialize constants
base_url <- "https://data.cityofchicago.org/resource/4ijn-s7e5.csv"
chunk_size <- 50000
offset <- 0
page <- 1
all_chunks <- list()

# 2. Define API call parameters
select_clause <- "inspection_id,dba_name,facility_type,risk,inspection_date,inspection_type,results,zip,latitude,longitude"
where_clause <- "inspection_date >= '2018-07-01' AND inspection_date <= '2025-06-30'"
order_clause <- "inspection_date, inspection_id"

# 3. Call API in chunks, stitch chunks together
repeat {
  url <- paste0(
    base_url,
    "?$select=", URLencode("*", reserved = TRUE),
    "&$where=",  URLencode(where_clause,  reserved = TRUE),
    "&$order=",  URLencode(order_clause,  reserved = TRUE),
    "&$limit=",  format(chunk_size, scientific = FALSE, trim = TRUE),
    "&$offset=", format(offset, scientific = FALSE, trim = TRUE))
  
  cat("Downloading page", page, "with offset", offset, "\n")
  chunk <- read.csv(url, stringsAsFactors = FALSE)
  cat("Rows downloaded:", nrow(chunk), "\n")
  
  if (nrow(chunk) == 0) break
  all_chunks[[page]] <- chunk
  if (nrow(chunk) < chunk_size) break
  offset <- offset + chunk_size
  page <- page + 1}

inspections <- do.call(rbind, all_chunks)

# 4. Reformat date and save file
inspections$inspection_date <- as.Date(inspections$inspection_date)
cat("Total rows:", nrow(inspections), "\n")
saveRDS(inspections,'data/inspections_raw.RDS')

# 5. Import community area shapefiles
if (!require(sf)) install.packages('sf')
require(sf)

areas <- st_read('https://data.cityofchicago.org/resource/igwz-8jzy.geojson',quiet=TRUE)
saveRDS(areas,'data/areas_raw.RDS')

