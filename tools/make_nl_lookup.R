################################################################################
## tools/make_nl_lookup.R
## Builds the lookup table Dutch 4-digit postcode -> province, used by
## 05_app_data.R to assign Dutch respondents to provinces. Uses public data of
## Statistics Netherlands (CBS); needs an internet connection. Run once.
##
## Input:  data/lookup/pc4_gemeente_nl.csv  (4-digit postcode -> municipality, 2025;
##         derived from the CBS table "Buurt, wijk en gemeente 2025 voor postcode
##         huisnummer": each 4-digit postcode is assigned to the municipality with
##         the most addresses)
## Output: data/lookup/pc4_province_nl.csv  (columns pc4, province)
## The resulting table is included in the package (data/lookup/pc4_province_nl.csv);
## run this script only to rebuild it, e.g. for a different year.
################################################################################

library(dplyr)
# install.packages("cbsodataR")   # only once

lookup_dir <- file.path("data", "lookup")

# municipality -> province: CBS table 86059NED "Gebieden in Nederland 2025"
gem <- cbsodataR::cbs_get_data("86059NED")

lookup <- read.csv(file.path(lookup_dir, "pc4_gemeente_nl.csv"), colClasses = "character") %>%
  left_join(tibble(gemeente_code = trimws(gem$Code_1),        # municipality code (GM....)
                   province      = trimws(gem$Naam_29)),      # province name
            by = "gemeente_code") %>%
  select(pc4, province)

print(table(lookup$province, useNA = "ifany"))   # should show 12 provinces and no NA
write.csv(lookup, file.path(lookup_dir, "pc4_province_nl.csv"), row.names = FALSE)
