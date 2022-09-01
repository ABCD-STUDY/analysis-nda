# this file is used to create choices_coding_nda.4.0 version file
# used in categorical_extension4.0.R
library("dplyr")
rm(list = ls())
gc()
script.dir <- getwd()
setwd(script.dir)
print(paste0("script.dir: ", script.dir))

# ================================================================
# read in ABCD_data_dictionary.csv
# ================================================================
abcd_data_dictionary <- read.csv("./ABCD_data_dictionary.csv")
print(paste0(
  "dimension of abcd data dictionary: ",
  dim(abcd_data_dictionary)[1], " x ", dim(abcd_data_dictionary)[2]
))
# filter choices out & dropna
abcd_data_dictionary <- abcd_data_dictionary %>%
  filter(type == "Integer") %>%
  filter(!is.na(valueRange))
print(paste0(
  "dimension of abcd data dictionary: ",
  dim(abcd_data_dictionary)[1], " x ", dim(abcd_data_dictionary)[2]
))

# ================================================================
# read in choices_coding_nda.3.0.csv
# ================================================================
# choices_coding_nda_3 = read.csv("./choices_coding_nda.3.0.csv")
# print(paste0('unique types of selections: ',
#              length(unique(choices_coding_nda_3$type)),
#              ': ',
#              unique(choices_coding_nda_3$type)))
# # there is one row that doesnt have a type (type == '') and it looks like it should have one
# na_type_choices = choices_coding_nda_3 %>% filter(type == '')

# ================================================================
# create choices coding 4.0
# ================================================================
choices_coding_nda_4 <- data.frame(
  name = abcd_data_dictionary$Element.Name,
  choices = abcd_data_dictionary$notes,
  type = ""
)
for (i in 1:length(abcd_data_dictionary$Element.Name)) {
  # we want to change the choice into a radio with "0, not endorsed | 1, whatever"
  idx <- NULL
  nn <- NULL
  nam <- as.character(abcd_data_dictionary$Element.Name[[i]])

  parts <- strsplit(nam, "___")
  if (length(parts[[1]]) > 1) {
    nam <- parts[[1]][[1]]
  }

  if (length(parts[[1]]) > 1) {
    options <- strsplit(as.character(abcd_data_dictionary$Element.Description[[i]]), "\\|")
    for (j in 1:length(options[[1]])) {
      c <- trimws(options[[1]][j])
      aa <- gregexpr("([0-9]+), (.*$)", c, perl = TRUE)
      s <- attr(aa[[1]], "capture.start")
      l <- attr(aa[[1]], "capture.length")
      v1 <- substring(c, s[[1]], s[[1]] + l[[1]] - 1)
      v2 <- substring(c, s[[2]], s[[2]] + l[[2]] - 1)
      if (v1 == parts[[1]][[2]]) {
        choices_coding_nda_4$choices[[i]] <- paste0("0, not endorsed | ", "1, ", v2)
        choices_coding_nda_4$type[[i]] <- "radio/dropdown"
        break
      }
    }
  }

  # decide which ones are dropdown and ones are radio
  # :: in valueRange -> dropdown
  # ; in valueRange -> radio / checkbox
  if (grepl(";", abcd_data_dictionary$valueRange[[i]], fixed = TRUE)) {
    choices_coding_nda_4$type[[i]] <- "radio/checkbox"
  }
  if (grepl("::", abcd_data_dictionary$valueRange[[i]], fixed = TRUE)) {
    choices_coding_nda_4$type[[i]] <- "dropdown"
  }
}


choices_coding_nda_4$choices <- gsub("  ", " ", choices_coding_nda_4$choices)
choices_coding_nda_4$choices <- gsub("=", ", ", choices_coding_nda_4$choices)
choices_coding_nda_4$choices <- gsub("= ", ", ", choices_coding_nda_4$choices)
choices_coding_nda_4$choices <- gsub(";", " |", choices_coding_nda_4$choices)
choices_coding_nda_4$choices <- gsub("; ", " |", choices_coding_nda_4$choices)
choices_coding_nda_4$choices <- gsub("  ", " ", choices_coding_nda_4$choices)
choices_coding_nda_4$choices <- gsub(" ,", ",", choices_coding_nda_4$choices)

# we have to strip html from the choices (keep english language only)
choices_coding_nda_4$choices <- gsub("<span lang=''en''>", "", choices_coding_nda_4$choices)

# remove everything after '['
# e.g.  [blm_cv] , '1' in the end of many choices_coding_nda_4$choices
choices_coding_nda_4$choices <- gsub("\\[(.*)", "", as.character(choices_coding_nda_4$choices))
# remove everything after '//'
choices_coding_nda_4$choices <- gsub("\\//(.*)", "", as.character(choices_coding_nda_4$choices))

# remove ) or ( incomplete parentheses in some fields
for (i in 1:length(choices_coding_nda_4$choices)) {
  if (!grepl("(", choices_coding_nda_4$choices[[i]], fixed = TRUE)) {
    if (grepl(")", choices_coding_nda_4$choices[[i]], fixed = TRUE)) {
      choices_coding_nda_4$choices[[i]] <- gsub("\\)", "", choices_coding_nda_4$choices[[i]])
    }
  }

  if (!grepl(")", choices_coding_nda_4$choices[[i]], fixed = TRUE)) {
    if (grepl("(", choices_coding_nda_4$choices[[i]], fixed = TRUE)) {
      choices_coding_nda_4$choices[[i]] <- gsub("\\(", "", choices_coding_nda_4$choices[[i]])
    }
  }
}

# remove head or tail spaces
choices_coding_nda_4$choices <- trimws(choices_coding_nda_4$choices)

# merge yes no questions into one by switching the sequence
choices_coding_nda_4$choices <- sub("1, Yes \\| 0, No", "0, No \\| 1, Yes", as.character(choices_coding_nda_4$choices))

# 0, No | 1, Yes -> type yesno
for (i in 1:length(choices_coding_nda_4$choices)) {
  if (grepl("0, No | 1, Yes", choices_coding_nda_4$choices[[i]], fixed = TRUE)) {
    choices_coding_nda_4$type[[i]] <- "yesno"
  }
}

# 0, No | 1, Yes -> type yesno
for (i in 1:length(choices_coding_nda_4$choices)) {
  if (choices_coding_nda_4$type[[i]] == "") {
    choices_coding_nda_4$type[[i]] <- "radio"
  }
}

# remove text that starts with / and ends with a digit number
choices_coding_nda_4$choices <- gsub("\\/.+\\d$", "", choices_coding_nda_4$choices)

# remove text that starts with / and ends with a digit number
choices_coding_nda_4$choices <- gsub("\\/.+\\'$", "", choices_coding_nda_4$choices)

# remove head or tail spaces
choices_coding_nda_4$choices <- trimws(choices_coding_nda_4$choices)

# remove trailing |
choices_coding_nda_4$choices <- gsub("\\|$", "", choices_coding_nda_4$choices)
choices_coding_nda_4$choices <- trimws(choices_coding_nda_4$choices)


# ================================================================
# write file
# ================================================================
write.csv(choices_coding_nda_4, file = "./choices_coding_nda.4.0.csv", row.names = FALSE)


# ==============================================================================
# testing script below
# ==============================================================================
choices_coding_nda_4 <- read.csv("./choices_coding_nda.4.0.csv")

tmp <- choices_coding_nda_4[!duplicated(choices_coding_nda_4$choices), ]
# count(choices_coding_nda_4, choices)
