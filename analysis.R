library(DBI)
library(tidyverse)

con <- dbConnect(RSQLite::SQLite(), "C:/Users/Spencer/OneDrive - Schools That Can Milwaukee/Data & Impact/R Files/schools_db/school_db.sqlite")

rc <- dbReadTable(con, "report_cards")

dbDisconnect(con)
my_cols <- c("#003349", "#E57200", "#69DBC8", "#C7DBF4", "#00A9E0")

low_performers <- c("Alternate Rating - Needs Improvement",
                    "Fails to Meet Expectations",
                    "Fails to Meed Expectations^",
                    "Meets Few Expectations",
                    "Meets Few Expectations^")

rc_18 <- rc %>%
  filter(school_year == "2017-18" & !(has_2_rc == 1 & report_card_type == "Private - Choice Students")) %>%
  mutate(mke_indicator = ifelse((city == "Milwaukee" & (locale_description == "City" | locale_description == "NA")), "Milwaukee", "Out State"),
         low_perf = ifelse(overall_rating %in% low_performers, 1, 0))


performance <- rc_18 %>%
  group_by(mke_indicator) %>%
  summarise(total = n(),
            low = sum(low_perf)) 

model <- lm(data = rc_18, formula = low_perf ~ mke_indicator)

summary1 <- summary(model)

performance %>%
  ggplot(aes(mke_indicator, low)) +
  geom_bar(stat = "identity") +
  theme_minimal()

p_mke_low <- 55/(55+42)
p_low <- (55+42)/(2124+271)
p_mke <- 271/(2124 +271)
p_not <- 2124/(2124+271)
p_not_low <- 42/(55+42)
p_low_mke <- 55/271

prob <- p_low_mke * p_mke / p_low
prob_not <- p_not_low * p_low / p_not
 

library(broom)

nested <- rc_18 %>%
  nest(-grade_band)

mod_fun_clust <- function(df) {
  lm(sch_ach ~ per_ed + per_swd + per_b_aa, data = df)
}

nested_ach <- nested %>%
  mutate(model = map(data, mod_fun_clust),
         tidied = map(model, tidy),
         augmented = map2(model, data, augment_columns)) %>%
  arrange(grade_band)

modeled_ach <- nested_ach  %>%
  unnest(augmented) %>%
  mutate(bench_low_ind = ifelse(.resid >= mean(.resid, na.rm = TRUE) - sd(.resid, na.rm = TRUE), 0, 1))


bench_perf <- modeled_ach %>%
  group_by(mke_indicator) %>%
  summarise(total = n(),
            low = sum(bench_low_ind)) 

model2 <- lm(data = modeled_ach, formula = bench_low_ind ~ mke_indicator)
summary2 <- summary(model2)
