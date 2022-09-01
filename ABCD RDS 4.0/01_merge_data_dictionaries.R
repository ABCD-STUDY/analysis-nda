# title: "merge_data_dictionaries"
# author: "w3zheng"
# date: "1/20/2022"
# output: html_document

rm(list = ls())
library("curl")
library("jsonlite")
getwd()

# Combine individual data dictionaries into a single spreadsheet
# NDA provides a web-API that can be used to pull information about the data dictionary without the need to login.
# The following web-calls should therefore work for any user.
# We would like to use curl to pull the JSON formatted list of ABCD data dictionaries first.
# Two libraries have to be installed in R to make the following merge operation work. This install operation needs to be performed only once.
# We start by pulling the list of all ABCD instruments from NDA:

download_abcd_instruments <- F
if (download_abcd_instruments) {
  # use ABCD Release 4.0 or the latest release number
  abcd_instruments <- fromJSON("https://ndar.nih.gov/api/datadictionary/v2/datastructure?source=ABCD%20Release%204.0")

  # save local file
  abcd_instruments_saveFile <- apply(abcd_instruments, 2, as.character)
  write.csv(abcd_instruments_saveFile, file = "./abcd_instruments.csv", row.names = FALSE)
} else {
  abcd_instruments <- read.csv("./abcd_instruments.csv")
}

# The next step downloads the NDA versions of each ABCD data dictionary and merges them together into a single data frame.
download_abcd_data_dict <- F
if (download_abcd_data_dict) {
  dd <- data.frame()
  for (i in 1:length(abcd_instruments$shortName)) {
    inst_name <- abcd_instruments$shortName[i]
    print(paste("Try to get: ", inst_name))
    inst <- fromJSON(paste("https://ndar.nih.gov/api/datadictionary/v2/datastructure/", inst_name, sep = ""))
    inst <- inst$dataElements
    # The alias names for each element name are in a list, concatenate that list into a single string
    aliases <- lapply(inst$aliases, function(x) {
      str <- ""
      if (length(x) > 0) {
        for (i in 1:length(x)) {
          str <- paste(str, x[[i]], sep = " ")
        }
      }
      trimws(str)
    })
    # create a new data frame
    nd <- data.frame(
      "Element Name" = inst$name,
      "Element Description" = inst$description,
      "type" = inst$type,
      "valueRange" = inst$valueRange,
      "notes" = inst$notes,
      "aliases" = unlist(aliases),
      "NDA Instrument" = inst_name
    )
    # merge
    if (dim(dd)[1] == 0) {
      dd <- nd
    } else {
      dd <- merge(dd, nd, all = TRUE)
    }
    print(paste("Merged", inst_name, i, "/", length(abcd_instruments$shortName), sep = " "))
  }
  # The resulting data dictionary will have close to 66,000 entries. Only about 38,000 are actually provided by ABCD and contain values.
  # * new dd now has 87,647 entries
  # Save the merged data dictionary as a spreadsheet.
  write.csv(dd, "./ABCD_data_dictionary.csv", sep = "/", row.names = FALSE)
} else {
  dd <- read.csv("./ABCD_data_dictionary.csv")
}

# Since release 2.0 the NDA dictionary contains all the alias names used on the DEAP system.
# In order to generate the NDA_DEAP_names_2.0.csv we can use the following code:
numAliases <- length(which(!is.na(dd$aliases)))
aliasNames <- data.frame(
  "nda" = character(numAliases),
  "abcd" = character(numAliases),
  "instrument" = character(numAliases)
)

for (i in 1:dim(dd)[1]) {
  if (dd[i, ]$aliases != "") {
    # There can be a list of alias names. We want to use only a single alias and we want to prefer to use the alias that is longest.
    alias <- strsplit(as.character(dd[i, ]$aliases), " ")[[1]][1]
    aliasNames[i, ] <- data.frame(
      "nda" = dd[i, ]$Element.Name,
      "abcd" = alias,
      "instrument" = as.character(dd[i, ]$NDA.Instrument)
    )
  }
}

# WRITE TO LOCAL FILE
write.csv(aliasNames, "./NDA_DEAP_names_4.0.csv", row.names = FALSE)
