

pacman::p_load(tidyverse, magrittr)

# Locate surveillance data
files <- list.files("data/survstat", pattern = "*.csv", full.names = T)
dts <- lapply(files, read.csv, sep="\t", header=FALSE, fileEncoding="UTF-16")

# Data cleaning
# The information on the variables is present in the file names, so I will extract
# that information from the file names

dtest <- NULL
for(i in 1:length(dts)){
  dtest[[i]] <- dts[[i]] %>% 
    set_colnames(c("nuts3", 
                             # Change column names to match the week number
                             paste0("W", ifelse(dts[[i]][2,-1] < 10 & 
                                                  !grepl("^0", dts[[i]][2,-1]),
                                                paste0("0",dts[[i]][2,-1]),
                                                dts[[i]][2,-1])))) %>%
    slice(-c(1:2)) %>% 
    filter(nuts3!="Unknown",
           nuts3!="") %>% 
    mutate(across(2:length(names(dts[[i]])), ~as.numeric(.x)),
           year=as.numeric(str_extract(files[[i]], pattern="20..")),
           age=str_extract(files[[i]], pattern="A.."),
           age=as.numeric(str_replace(age, "A", "")),
           age=ifelse(is.na(age), -9, age),
           sex=str_extract(files[[i]], pattern="..male"),
           sex=ifelse(sex=="female", "female", "male")) %>% 
    pivot_longer(2:length(names(dts[[i]])), 
                 names_to="week", 
                 values_to="counts") %>% 
    mutate(date=ISOweek::ISOweek2date(paste(year, week, "1", sep = "-")),
           month=lubridate::month(date))
  
  cat(i, "\r")
}

# Survstat divides Berlin in subregions. To match the population data I will regroup
# Berlin into one district
out2 <- bind_rows(dtest) %>% 
  filter(counts!=0) %>% 
  mutate(nuts3=ifelse(grepl("Berlin", nuts3), "City of Berlin", nuts3)) %>% 
  group_by(nuts3, month, age, sex) %>% 
  summarise(counts=sum(counts, na.rm=T), .groups="drop")


# Add population data
pop <- read.csv("data/pop_2021_destatis.csv")


t2 <-  out2 %>% 
  mutate(age=case_when(age < 3 ~ "[00-03)",
                       age < 6 ~ "[03-06)",
                       age < 10 ~ "[06-10)",
                       age < 15 ~ "[10-15)",
                       age < 18 ~ "[15-18)",
                       age < 20 ~ "[18-20)",
                       age < 25 ~ "[20-25)",
                       age < 30 ~ "[25-30)",
                       age < 35 ~ "[30-35)",
                       age < 40 ~ "[35-40)",
                       age < 45 ~ "[40-45)",
                       age < 50 ~ "[45-50)",
                       age < 55 ~ "[50-55)",
                       age < 60 ~ "[55-60)",
                       age < 65 ~ "[60-65)",
                       age < 75 ~ "[65-75)",
                       age < 100 ~ "[75+)",
                       TRUE ~ NA)) %>% 
  group_by(nuts3, month, age, sex) %>% 
  summarise(counts=sum(counts, na.rm=TRUE), .groups="drop") %>%
  mutate(month=factor(month, levels=c(1:12))) %>% 
  group_by(nuts3, age, sex) %>% 
  complete(month) %>% 
  ungroup() %>% 
  mutate(counts=ifelse(is.na(counts), 0, counts)) %>% 
  left_join(pop)

  



# write.csv(out, "data_in/lyme/lyme_cases_full.csv", row.names=FALSE)
# out <- read.csv("data_in/lyme/lyme_cases_full.csv")

# Arrange dataset
n3 <- out3 %>% 
  group_by(nuts1, nuts2, NUTS2_ID, nuts3, NUTS3_ID, year, month) %>% 
  summarise(counts=sum(counts), .groups="drop")

write.csv(n3, "data_out/lyme_cases_nuts3.csv", row.names=FALSE)

## END