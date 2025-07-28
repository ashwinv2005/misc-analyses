##################################### full data

require(lubridate)
require(tidyverse)
require(sp)
require(sf)

preimp = c("COMMON.NAME","COUNTY", "CATEGORY")

nms = read.delim("ebd_IN-TS_relMar-2025.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim("ebd_IN-TS_relMar-2025.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))

data1 = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(!is.na(COUNTY)) %>%
  distinct(COMMON.NAME,COUNTY)

# making comma separated lists
delim.district.list = data1 %>%
  group_by(COMMON.NAME) %>%
  summarise(district.list = toString(COUNTY)) %>%
  ungroup()

write.csv(delim.district.list, "Telangana_district_list.csv", row.names = F)
