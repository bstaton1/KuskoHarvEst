# make a table to store the short and long names for each interview data source
source_names = data.frame(source_long = c("Tundra Villages (ADF&G)", "Bethel Boat Harbor (ONC)", "Bethel Area Fish Camps (ONC)", "Other Villages (KRITFC)", "Law Enforcement Officers (USFWS)")
)
rownames(source_names) = c("ADFG", "BBH", "FC", "CBM", "LE")

# make a table to store the names of each stratum
strata_names = data.frame(stratum = c("A", "B", "C", "D1", "D2"),
                          stratum_start = c("Tuntutuliak", "Johnson R.", "Napaskiak", "Akiachak", "Akiak"),
                          stratum_end = c("Johnson R.", "Napaskiak", "Akiachak", "Akiak", "Bogus Cr."))

# make a table specifying which species the program should accept
salmon_species = c("chinook", "chum", "sockeye", "coho")
nonsalmon_species = c("whitefish", "sheefish")
species = c(salmon_species, nonsalmon_species)
is_salmon = species %in% salmon_species
in_text = ifelse(species == "chinook", KuskoHarvUtils::capitalize(species), species)
in_text = ifelse(species == "whitefish", "all whitefishes", in_text)
in_text = ifelse(is_salmon, paste0(in_text, " salmon"), in_text)
species_names = data.frame(species = species, is_salmon = is_salmon, in_text = in_text)

# export them to proper structure and location
usethis::use_data(source_names, strata_names, species_names, internal = TRUE, overwrite = TRUE)
