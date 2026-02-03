#***********************************************************************
# Compute contextual inequity scores 
# --------------------------------------------------------------
# INPUTS:
# - Variables dataframe, raws are unique cells, columns are contextual inequity variables
# df.cont.inequity.compo.coastal.rds (script 06)
# pop.coastal.ctry.pixel.rds (script 06)
# OUTPUTS
# - dataframe with all variables and the contextual inequity scores : "df.cont.inequity.compo.coastal.with.scores.rds"
# - country level: "country.summary.composite.scores.rds"
#***********************************************************************
# Creation : Stéphanie D'Agata
# Email :stephanie.dagata@ird.fr
# ORCID : https://orcid.org/0000-0001-6941-8489
# Institution : Institut de Recherche pour le Développement
#***********************************************************************

# load script and additional functions
library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)
source(here::here("R","percentile_rank.R"))

# Load your data
df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.rds"))

# load population data
pop.coastal.ctry.pixel <- readRDS(here("data","derived-data","pop.coastal.ctry.pixel.rds"))

# add total population
df.risk.stack.sc.ctry.ind.coastal <- left_join(df.risk.stack.sc.ctry.ind.coastal,pop.coastal.ctry.pixel,by=c("ID","iso_a3"))

# create the same composite ID than below
df.risk.stack.sc.ctry.ind.coastal <- df.risk.stack.sc.ctry.ind.coastal %>%
  filter(!is.na(iso_a3), iso_a3 != "-99", iso_a3 != "") %>%
  # Create unique identifier for each pixel
  mutate(
    pixel_id = row_number(),
    uCode = paste0(as.character(iso_a3), "_pixel_", sprintf("%06d", pixel_id))
  )

# 1. CHECK DATA ----
cat("Original data rows:", nrow(df.risk.stack.sc.ctry.ind.coastal), "\n")
cat("Unique countries:", length(unique(df.risk.stack.sc.ctry.ind.coastal$iso_a3)), "\n")

# 2. PREPARE PIXEL-LEVEL DATA ----
iData <- df.risk.stack.sc.ctry.ind.coastal %>%
  filter(!is.na(iso_a3), iso_a3 != "-99", iso_a3 != "") %>%
  # Create unique identifier for each pixel
  mutate(
    pixel_id = row_number(),
    uCode = paste0(as.character(iso_a3), "_pixel_", sprintf("%06d", pixel_id))
  ) %>%
  dplyr::select(
    uCode,
    iso_a3,  # Keep this for later merging
    # Vulnerability indicators
    mean.count.grav.V2.log.sc,
    povmap.grdi.v1.sc,
    perc.pop.world.coastal.merit.10m.log.sc,
    Nutritional.dependence.sc,
    Economic.dependence.sc,
    # Governance indicators
    Voice_account.sc,
    Gov_effect.sc,
    Reg_quality.sc,
    Political_stab.sc,
    Rule_law.sc,
    control_corr.sc,
    # Inequity indicators
    gender.ineq.sc,
    income.ineq.sc,
    le.ineq.log.sc
  ) %>%
  as.data.frame(stringsAsFactors = FALSE)

# Ensure uCode is character
iData$uCode <- as.character(iData$uCode)

# Verify no duplicates
cat("\nAfter creating pixel IDs:\n")
cat("Total pixels:", nrow(iData), "\n")
cat("Unique uCodes:", length(unique(iData$uCode)), "\n")
cat("Duplicates:", sum(duplicated(iData$uCode)), "\n")

# Store iso_a3 mapping for later
pixel_country_map <- iData %>% dplyr::select(uCode, iso_a3)

# Remove iso_a3 from iData (COINr only needs uCode and indicators)
iData <- iData %>% dplyr::select(-iso_a3)

# 3. CREATE METADATA (iMeta) - WITH ALL REQUIRED COLUMNS ----
iMeta <- data.frame(
  iCode = c(
    # Level 1 indicators
    "mean.count.grav.V2.log.sc", 
    "povmap.grdi.v1.sc", 
    "perc.pop.world.coastal.merit.10m.log.sc",
    "Nutritional.dependence.sc", 
    "Economic.dependence.sc",
    "Voice_account.sc", 
    "Gov_effect.sc", 
    "Reg_quality.sc",
    "Political_stab.sc", 
    "Rule_law.sc", 
    "control_corr.sc",
    "gender.ineq.sc", 
    "income.ineq.sc", 
    "le.ineq.log.sc",
    # Level 2 aggregates
    "Vulnerability", 
    "Governance", 
    "Inequity",
    # Level 3 - final index
    "CompositeInequity"
  ),
  Parent = c(
    # Level 1 indicators point to their Level 2 parent
    rep("Vulnerability", 5),
    rep("Governance", 6),
    rep("Inequity", 3),
    # Level 2 aggregates point to Level 3
    rep("CompositeInequity", 3),
    # Level 3 has no parent
    NA
  ),
  Level = c(
    # Level 1 indicators
    rep(1, 14),
    # Level 2 aggregates
    rep(2, 3),
    # Level 3 - final index
    3
  ),
  Direction = c(
    # Level 1 indicators: 1 = higher is worse, -1 = higher is better
    # For inequity/vulnerability, higher values = worse, so use -1
    rep(1, 14),  # All indicators: higher = more inequity/vulnerability = worse
    # Level 2 and 3 aggregates
    rep(1, 4)  # Higher composite score = more inequity = worse
  ),
  Weight = c(
    # Equal weights within each group (Level 1)
    rep(1, 5),  # Vulnerability
    rep(1, 6),  # Governance
    rep(1, 3),  # Inequity
    # Equal weights for Level 2 aggregation
    rep(1, 3),
    # Level 3
    1
  ),
  Type = c(
    rep("Indicator", 14),
    rep("Aggregate", 3),
    "Aggregate"
  ),
  stringsAsFactors = FALSE
)

# Verify iMeta structure
cat("\niMeta column names:\n")
print(names(iMeta))

# 4. CREATE CUSTOM PERCENTILE RANK FUNCTION ----
my.percentile.rank <- function(x, ties.method = "average") {
  # Convert to percentile rank (0-100 scale)
  # This gives the percentage of values that are less than or equal to x
  n <- length(x[!is.na(x)])
  ranks <- rank(x, ties.method = ties.method, na.last = "keep")
  percentile_rank <- (ranks) / (n )
  return(percentile_rank)
}

# hierarchical percentil score function
my.percentile.rank.V2 <- function(x){
  percentile.rank <-
    rank(x,na.last="keep",ties.method="average")/length(which(!is.na(x)))
  return(percentile.rank)
}

cat("\n=== Testing percentile rank function ===\n")
test_vec <- c(1, 2, 3, 4, 5, NA, 6, 7, 8, 9, 10)
cat("Test vector:", test_vec, "\n")
cat("Percentile ranks:", my.percentile.rank(test_vec), "\n")
cat("Percentile ranks:", my.percentile.rank.V2(test_vec), "\n")

# 5. BUILD THE COIN ----
cat("\n=== Building COIN object ===\n")
coin <- new_coin(
  iData = iData,
  iMeta = iMeta,
  level_names = c("Indicator", "Dimension", "Index")
)

cat("COIN object created successfully!\n")
print(coin)

# 6. STEP 1: NORMALIZE DATA USING PERCENTILE RANKS ----
cat("\n=== STEP 1: Normalizing indicators using percentile ranks ===\n")

# First level: Normalize raw indicators using custom percentile rank function
coin <- Normalise(coin, 
                  dset = "Raw",
                  global = list(f_n = "my.percentile.rank",
                                f_n_para = list(ties.method = "average")))

cat("First level normalization complete!\n")

# Check a sample of normalized data
norm_data <- get_dset(coin, dset = "Normalised")
cat("\nSample of 1st level normalized data (first 5 rows, first 5 columns):\n")
print(head(norm_data[, 1:min(6, ncol(norm_data))], 5))

# 7. STEP 2: AGGREGATE TO GET DOMAIN SCORES (using arithmetic mean) ----
cat("\n=== STEP 2: Aggregating to domain scores using arithmetic mean ===\n")

# Aggregate the percentile-ranked indicators using arithmetic mean
coin <- Aggregate(coin, 
                  dset = "Normalised",
                  f_ag = "weightedMean",
                  f_ag_para = list(na.rm = FALSE),
                  w = "none")

cat("Aggregation to domain scores complete!\n")

# Get the aggregated data (contains domain scores)
agg_data <- get_dset(coin, dset = "Aggregated")
cat("\nColumns in aggregated data:\n")
print(names(agg_data))

cat("\nSample of domain scores (before 2nd level ranking):\n")
print(head(agg_data %>% dplyr::select(uCode, Vulnerability, Governance, Inequity, CompositeInequity), 10))

cat("\nSummary of domain scores (before 2nd level ranking):\n")
print(summary(agg_data %>% dplyr::select(Vulnerability, Governance, Inequity)))

# 8. STEP 3: SECOND LEVEL PERCENTILE RANKING OF DOMAIN SCORES ----
cat("\n=== STEP 3: Second level percentile ranking of domain scores ===\n")

# Extract domain scores for re-ranking
risk.mat.2nd.level.rank <- agg_data %>%
  dplyr::select(uCode, Vulnerability, Governance, Inequity)

# Apply percentile ranking to each domain score
risk.mat.2nd.level.rank.ok <- risk.mat.2nd.level.rank %>%
  mutate(
    vulnerab.score.rank = my.percentile.rank(Vulnerability),
    gov.score.rank = my.percentile.rank(Governance),
    ineq.score.rank = my.percentile.rank(Inequity)
  ) %>%
  dplyr::select(uCode, vulnerab.score.rank, gov.score.rank, ineq.score.rank)

cat("\nSummary of re-ranked domain scores:\n")
print(summary(risk.mat.2nd.level.rank.ok %>% dplyr::select(-uCode)))

cat("\nSample of re-ranked domain scores:\n")
print(head(risk.mat.2nd.level.rank.ok, 10))

# 9. STEP 4: FINAL COMPOSITE SCORE (arithmetic mean of re-ranked domains) ----
cat("\n=== STEP 4: Calculating final composite score ===\n")

# Calculate final hierarchical score as mean of re-ranked domain scores
risk.mat.2nd.level.rank.ok <- risk.mat.2nd.level.rank.ok %>%
  mutate(
    hierachical.score.rank.ineq = rowMeans(dplyr::select(., vulnerab.score.rank, gov.score.rank, ineq.score.rank),na.rm=F)
  )

cat("\nSummary of final hierarchical composite score:\n")
print(summary(risk.mat.2nd.level.rank.ok$hierachical.score.rank.ineq))

# Add back the country codes
domain_scores <- risk.mat.2nd.level.rank.ok %>%
  left_join(pixel_country_map, by = "uCode")

# 10. MERGE BACK TO ORIGINAL DATAFRAME ----
cat("\n=== Merging results back to original dataframe ===\n")

# First, add pixel_id to original dataframe
df.risk.stack.sc.ctry.ind.coastal <- df.risk.stack.sc.ctry.ind.coastal %>%
  filter(!is.na(iso_a3), iso_a3 != "-99", iso_a3 != "") %>%
  mutate(
    pixel_id = row_number(),
    uCode = paste0(as.character(iso_a3), "_pixel_", sprintf("%06d", pixel_id))
  )

# Merge the scores
df.risk.stack.sc.ctry.ind.coastal <- df.risk.stack.sc.ctry.ind.coastal %>%
  left_join(
    domain_scores %>% dplyr::select(uCode, vulnerab.score.rank, gov.score.rank, 
                                    ineq.score.rank, hierachical.score.rank.ineq),
    by = "uCode"
  )

# 11. VIEW RESULTS ----
cat("\n=== SUMMARY OF COMPOSITE SCORES (PIXEL LEVEL) ===\n")
summary(df.risk.stack.sc.ctry.ind.coastal[, c("vulnerab.score.rank", 
                                              "gov.score.rank", 
                                              "ineq.score.rank",
                                              "hierachical.score.rank.ineq")])

cat("\n=== NA COUNTS ===\n")
cat("Vulnerability NAs:", sum(is.na(df.risk.stack.sc.ctry.ind.coastal$vulnerab.score.rank)), "\n")
cat("Governance NAs:", sum(is.na(df.risk.stack.sc.ctry.ind.coastal$gov.score.rank)), "\n")
cat("Inequity NAs:", sum(is.na(df.risk.stack.sc.ctry.ind.coastal$ineq.score.rank)), "\n")
cat("Hierarchical Composite NAs:", sum(is.na(df.risk.stack.sc.ctry.ind.coastal$hierachical.score.rank.ineq)), "\n")

cat("\n=== TOP 10 PIXELS (highest composite inequity - worst) ===\n")
df.risk.stack.sc.ctry.ind.coastal %>%
  arrange(desc(hierachical.score.rank.ineq)) %>%
  dplyr::select(uCode, iso_a3, vulnerab.score.rank, gov.score.rank, 
                ineq.score.rank, hierachical.score.rank.ineq) %>%
  head(10) %>%
  print()

cat("\n=== BOTTOM 10 PIXELS (lowest composite inequity - best) ===\n")
df.risk.stack.sc.ctry.ind.coastal %>%
  arrange(hierachical.score.rank.ineq) %>%
  dplyr::select(uCode, iso_a3, vulnerab.score.rank, gov.score.rank, 
                ineq.score.rank, hierachical.score.rank.ineq) %>%
  head(10) %>%
  print()

# test
df.risk.stack.sc.ctry.ind.coastal %>%
  group_by(iso_a3) %>%
  summarize(vulnerab = mean(vulnerab.score.rank,na.rm=T)) %>%
  summary()

cat("\n=== COUNTRY-LEVEL SUMMARY (average across pixels) ===\n")
country_summary <- df.risk.stack.sc.ctry.ind.coastal %>%
  group_by(iso_a3) %>%
  summarise(
    n_pixels = n(),
    n_pixels_with_composite = sum(!is.na(hierachical.score.rank.ineq)),
    avg_vulnerab = mean(vulnerab.score.rank,na.rm=T),
    avg_gov = mean(gov.score.rank,na.rm=T),
    avg_ineq = mean(ineq.score.rank,na.rm=T),
    avg_composite = mean(hierachical.score.rank.ineq,na.rm=T),
    median_composite = median(hierachical.score.rank.ineq,na.rm=T),
    cont.eq.score.Gini.rank = DescTools::Gini(hierachical.score.rank.ineq,na.rm=T) %>% round(4),
    .groups = "drop"
  )  %>%
  arrange(desc(avg_composite)) %>%
  #tidyr::drop_na() %>%
  as.data.frame()

# add country names
country_summary <- country_summary %>%
  left_join(df.risk.stack.sc.ctry.ind.coastal %>% dplyr::select(iso_a3,name_en) %>% distinct(), by="iso_a3")

# add region
world <- ne_countries(scale = "large", returnclass = "sf")
country_summary <- country_summary %>%
  left_join(world %>% dplyr::select(iso_a3,region_un) %>% st_drop_geometry(), by="iso_a3")

# 12. SAVE RESULTS ----
cat("\n=== Saving results ===\n")
saveRDS(df.risk.stack.sc.ctry.ind.coastal, 
        here("data","derived-data","df.cont.inequity.compo.coastal.with.scores.rds"))

saveRDS(country_summary,
        here("data","derived-data","country.summary.composite.scores.rds"))

cat("\nPixel-level data saved to: df.cont.inequity.compo.coastal.with.scores.rds\n")
cat("Country summary saved to: country.summary.composite.rds\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Total pixels processed:", nrow(df.risk.stack.sc.ctry.ind.coastal), "\n")
cat("Pixels with complete composite scores:", sum(!is.na(df.risk.stack.sc.ctry.ind.coastal$hierachical.score.rank.ineq)), "\n")
cat("Countries covered:", length(unique(df.risk.stack.sc.ctry.ind.coastal$iso_a3)), "\n")

# 13. COMPARISON WITH DIRECT MEAN (for verification) ----
cat("\n=== COMPARISON: Two-level ranking vs Direct mean ===\n")

# Calculate what the score would be with direct mean (no 2nd level ranking)
comparison <- agg_data %>%
  dplyr::select(uCode, Vulnerability, Governance, Inequity, CompositeInequity) %>%
  left_join(
    risk.mat.2nd.level.rank.ok %>% dplyr::select(uCode, hierachical.score.rank.ineq),
    by = "uCode"
  ) %>%
  rename(
    direct_mean_composite = CompositeInequity,
    two_level_rank_composite = hierachical.score.rank.ineq
  )

cat("\nCorrelation between two methods:\n")
cat("Pearson correlation:", 
    cor(comparison$direct_mean_composite, comparison$two_level_rank_composite, use = "complete.obs"), "\n")

cat("\nTop 10 by two-level ranking:\n")
print(head(comparison %>% arrange(desc(two_level_rank_composite)), 10))

