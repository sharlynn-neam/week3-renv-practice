install.packages("renv")
renv::init()
# 1. Importing data
clinic_raw = read_csv("data/clinic_raw.csv", skip = 2) #'skip = 2' drops the title line and blank line sitting above the real header in the original excel
#OR can also use 'clinic_raw[1:20, ]' if you just want specific rows 

# 2. Check the data import and type
head(clinic_raw)
str(clinic_raw) #important to check, because poor data may make a column type become text instead of numeric

# 3. Check data quality 
sum(is.na(clinic_raw$age)) # TRUE counts as 1, so sum() tallies the NAs
range(clinic_raw$age, na.rm = TRUE) # na.rm drops NAs before computing min/max
summary(clinic_raw)

# 4. Quick plot to check what data says
library(ggplot2)
ggplot(clinic_raw, aes(x = age)) +
  geom_histogram(binwidth = 5, na.rm = TRUE)

##### Reproducible data cleaning #####

# 1. Renaming, creating, selecting columns
clinic_clean = #name a new file as 'cleaned' version
  clinic_raw %>%
  rename(id = patient_id) # patient_id -> id, values untouched

clinic_clean =
  clinic_clean %>%
  dplyr::select(id, site, sex, age, visit_date, blood_pressure, weight_kg, notes)

clinic_clean =
  clinic_clean %>%
  filter(age >= 18 | is.na(age)) # keep adults, but don't drop missing ages

# INSTEAD of doing the above (separate naming of the same thing which just overrides)
# Do a build up so that you only name ONCE
clinic_clean =
  clinic_raw %>%
  rename(id = patient_id) %>%  # change column name:patient_id -> id, values untouched
  dplyr::select(id, site, sex, age, visit_date, blood_pressure, weight_kg, notes)  %>% # keep only columns of interest
  filter(age >= 18 | is.na(age))  # keep adults, but don't drop missing ages

# 2. Sorting rows
clinic_clean %>% arrange(age) %>% head() # youngest patients first
clinic_clean %>% arrange(desc(age)) %>% head() # oldest patients first

# 3. Grouped summaries
clinic_clean %>%
  group_by(site) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE)
    , n = n() # count number of rows in each group
  )

# 4. Cleaning up text
clinic_clean =
  clinic_clean %>%
  mutate(
    site = tolower(trimws(site)) # lowercase + strip whitespace
    , site = gsub("_", " ", site)  # replace underscore with space
  )
table(clinic_clean$site)
substring("PT-0001", 4, 7) # characters 4 through 7: "0001"

# 5. Regular expressions
#e.g., if want to separate the bp 120/80 mmHg as new columns 
library(stringr)
clinic_clean =
  clinic_clean %>%
  mutate(
    bp_upper = as.numeric(str_extract(blood_pressure, "^[0-9]+")) # digits before the "/"
    , bp_lower = as.numeric(str_extract(blood_pressure, "(?<=/)[0-9]+")) # digits after the "/"
  )
head(clinic_clean[, c("blood_pressure", "bp_upper", "bp_lower")])

grep("follow", clinic_clean$notes, ignore.case = TRUE) # ignore.case catches "FOLLOW UP" and "follow up"

clinic_clean %>% filter(grepl("follow", notes, ignore.case = TRUE))

# 6. Converting between data types
clinic_clean =
  clinic_clean %>%
  mutate(
    weight_kg = na_if(weight_kg, "-") # turn placeholder "-" into real NA
    , weight_kg = as.numeric(weight_kg) # now safe to convert to numeric
    , visit_date = as.Date(visit_date, format = "%d/%m/%Y") # dd/mm/yyyy text -> Date
  )
head(clinic_clean)

# 7. Conditional logic
clinic_clean =
  clinic_clean %>%
  mutate(
    sex = ifelse(
      toupper(sex) == "M", "Male"
      , ifelse(toupper(sex) == "F", "Female", NA) # nested ifelse for the third outcome
    )
  )

clinic_clean =
  clinic_clean %>%
  mutate(
    bp_category = case_when(
      is.na(bp_upper)   ~ NA_character_ # keep missing as missing
      , bp_upper < 120    ~ "Normal"
      , bp_upper < 140    ~ "Elevated"
      , TRUE              ~ "High" # catch-all for anything left
    )
  )
table(clinic_clean$bp_category, useNA = "always")

# 8. Writing functions
clean_text = function(x) {
  x = trimws(x)  # strip whitespace
  x = tolower(x) # standardise case
  x
}

clean_text("  FOLLOW UP") #testing if the function works

clinic_clean =
  clinic_clean %>%
  mutate(notes = clean_text(notes)) # reuse the function on the notes column
table(clinic_clean$notes, useNA = "always")

# 9. Reshaping data (adjusting the column into multiple rows and vice versa)
clinic_clean %>%
  select(id, bp_upper, bp_lower) %>%
  head(4) %>%
  tidyr::pivot_longer(
    cols = c(bp_upper, bp_lower) # the two columns to stack
    , names_to = "reading"         # new column naming which reading it was
    , values_to = "value"          # new column holding the actual number
  )

clinic_clean %>%
  group_by(site, sex) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sex, values_from = mean_age)

# 10. Combining datasets
site_lookup = data.frame(
  site = c("site a", "site b", "site c", "site d")
  , region = c("North", "North", "South", "South")
)

clinic_clean =
  clinic_clean %>%
  left_join(site_lookup, by = "site") # attach region, keep every clinic_clean row
head(clinic_clean)

