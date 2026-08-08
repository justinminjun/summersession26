# 1. Read in raw data from import stage, drop unneeded vars
inspections <- readRDS("data/inspections_raw.RDS")

# 2. Standardize a few text fields to lower-case for matching
to_lower_safe <- function(x) {
  x <- ifelse(is.na(x), NA, trimws(x))
  tolower(x)}

inspections$inspection_type <- to_lower_safe(inspections$inspection_type)
inspections$results <- to_lower_safe(inspections$results)
inspections$facility_type <- to_lower_safe(inspections$facility_type)
inspections$dba_name <- to_lower_safe(inspections$dba_name)
inspections$address <- to_lower_safe(inspections$address)

# 3. Add inspection-level features

inspections$fail <- ifelse(inspections$results=='fail',1,0)

inspections$current <- ifelse(inspections$results %in% 
  c('out of business','business not located'),0,1)

inspections$complaint <- ifelse(inspections$inspection_type %in% 
  c('complaint','suspected food poisoning','short form complaint'),1,0)

inspections$canvass <- ifelse(inspections$inspection_type=='canvass',1,0)

# 4. Add restaurant-level features and sort by unique id

inspections$unique_id <- paste(inspections$license_,inspections$address)

inspections$facility_group <- 'other'
inspections$facility_group[grepl('restaurant',inspections$facility_type)] <- 'restaurant'
inspections$facility_group[grepl('school',inspections$facility_type)] <- 'school'
inspections$facility_group[grepl('child',inspections$facility_type)] <- 'school'
inspections$facility_group[grepl('daycare',inspections$facility_type)] <- 'school'
inspections$facility_group[grepl('years',inspections$facility_type)] <- 'school'
inspections$facility_group[grepl('grocery',inspections$facility_type)] <- 'store'
inspections$facility_group[grepl('gas',inspections$facility_type)] <- 'store'
inspections$facility_group[grepl('convenien',inspections$facility_type)] <- 'store'
inspections$facility_group[grepl('store',inspections$facility_type)] <- 'store'
inspections$facility_group[grepl('hospital',inspections$facility_type)] <- 'medical'
inspections$facility_group[grepl('term care',inspections$facility_type)] <- 'medical'
inspections$facility_group[grepl('living',inspections$facility_type)] <- 'medical'
inspections$facility_group[grepl('senior',inspections$facility_type)] <- 'medical'
inspections$facility_group[grepl('bakery',inspections$facility_type)] <- 'bakery'
inspections$facility_group[grepl('banquet',inspections$facility_type)] <- 'caterer'
inspections$facility_group[grepl('cater',inspections$facility_type)] <- 'caterer'
inspections$facility_group[grepl('event',inspections$facility_type)] <- 'caterer'

chrono <- order(inspections$unique_id,inspections$inspection_date)
inspections <- inspections[chrono,]

# 5. Create training set of past canvassing outcomes

last_nonmissing <- function(x) {
  x2 <- x[!is.na(x) & x != ""]
  if (length(x2) == 0) return(NA)
  x2[length(x2)]}

temp <- split(inspections, inspections$unique_id)

temp <- lapply(temp, function(df) {
  data.frame(
    unique_id = last_nonmissing(df$unique_id),
    dba_name = last_nonmissing(df$dba_name),
    address = last_nonmissing(df$address),
    facility_group = last_nonmissing(df$facility_group),
    latitude = last_nonmissing(df$latitude),
    longitude = last_nonmissing(df$longitude),
    days_observed = as.double(df$inspection_date - min(df$inspection_date)) + 1,
    new = 1*(as.double(df$inspection_date - min(df$inspection_date)) < 366),
    inspection_type = df$inspection_type,
    inspections = 1:nrow(df),
    complaints = cumsum(df$complaint),
    fail=df$fail,
    prior_fails = pmax(cumsum(df$fail)-1,0),
    current = df$current,
    stringsAsFactors = FALSE)})

temp <- do.call(rbind, temp)

training_data <- temp[temp$inspection_type=='canvass' & temp$current==1,]

# 6. Create scoring set of current canvassing targets

temp <- split(inspections, inspections$unique_id)

temp <- lapply(temp, function(df) {
  data.frame(
    unique_id = last_nonmissing(df$unique_id),
    dba_name = last_nonmissing(df$dba_name),
    license_ = last_nonmissing(df$license_),
    address = last_nonmissing(df$address),
    facility_group = last_nonmissing(df$facility_group),
    latitude = last_nonmissing(df$latitude),
    longitude = last_nonmissing(df$longitude),
    days_observed = as.double(as.Date('2025-07-01') - min(df$inspection_date)) + 1,
    new = 1*(as.double(as.Date('2025-07-01') - min(df$inspection_date)) < 366),
    inspection_type = 'canvass',
    inspections = nrow(df),
    complaints = sum(df$complaint),
    prior_fails = sum(df$fail),
    current = last_nonmissing(df$current),
    stringsAsFactors = FALSE)})

temp <- do.call(rbind, temp)

scoring_data <- temp[temp$current==1,]

# 7. Add community area to scoring data

if (!require(sf)) install.packages('sf')
require(sf)

restaurant_sf <- st_as_sf(
  scoring_data[complete.cases(scoring_data[,c('latitude','longitude')]),],
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE)

areas <- readRDS('data/areas_raw.RDS')
community_areas <- st_transform(areas, st_crs(restaurant_sf))
scoring_data <- st_join(restaurant_sf, community_areas, join = st_within)

rownames(scoring_data) <- NULL
scoring_data$community <- tolower(scoring_data$community)
scoring_data <- scoring_data[,c('unique_id','dba_name','license_','address',
                                'facility_group','days_observed','new',
                                'inspection_type','inspections','complaints',
                                'prior_fails','community','latitude','longitude')]

# 8. Save training data and scoring data
saveRDS(training_data,'data/training_data.RDS')
saveRDS(scoring_data,'data/scoring_data.RDS')
