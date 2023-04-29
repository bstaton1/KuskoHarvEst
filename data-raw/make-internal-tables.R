# make a table to store the short and long names for each interview data source
source_names = data.frame(source_long = c("Tundra Villages (ADF&G)", "Bethel Boat Harbor (ONC)", "Bethel Area Fish Camps (ONC)", "Other Villages (BSFA/KRITFC)", "Law Enforcement Officers (USFWS)")
)
rownames(source_names) = c("ADFG", "BBH", "FC", "CBM", "LE")

# make a table to store the names of each stratum
strata_names = data.frame(stratum = c("A", "B", "C", "D1"),
                          stratum_start = c("Tuntutuliak", "Johnson R.", "Napaskiak", "Akiachak"),
                          stratum_end = c("Johnson R.", "Napaskiak", "Akiachak", "Akiak"))

# make a table specifying which species the program should accept
salmon_spp = c("chinook", "chum", "sockeye")
nonsalmon_spp = c("whitefish", "sheefish")
spp = c(salmon_spp, nonsalmon_spp)
is_salmon = spp %in% salmon_spp
in_text = ifelse(spp == "chinook", KuskoHarvUtils::capitalize(spp), spp)
species_names = data.frame(spp = spp, is_salmon = is_salmon, in_text = in_text)

# export them to proper structure and location
usethis::use_data(source_names, strata_names, species_names, internal = TRUE, overwrite = TRUE)
