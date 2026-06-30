# ───────────────────────────────────────────────────
## PURPOSE
# ───────────────────────────────────────────────────
#   The first rendition of our meristic analysis was as follows: run a general
# liner mixed model with a poisson distribution, test for overdispersion, and
# if overdispersed, test with a negative binomial distribution and unequal 
# dispersion by species. The results showed severe underdispersion (all 
# dispersion ratios <0.05). So the final analysis switched to a Conway-Maxwell
# poisson distribution to control for underdispersion. 

# ───────────────────────────────────────────────────
## INSTALL NECESSARY PACAKGES
# ───────────────────────────────────────────────────

#install.packages("curl")             #v5.2.1
library(curl)

#install.packages("dplyr")            #v1.1.4
library(dplyr)

#install.packages("gt")               #v0.11.1
library(gt)

#install.packages("car")              #v3.1-2
library(car)

#install.packages("glmmTMB")          #v1.1.14
library(glmmTMB)

#install.packages("DHARMa")           #v0.4.7
library(DHARMa)

# ───────────────────────────────────────────────────
## LOAD DATA
# ───────────────────────────────────────────────────


# OPTION 1: download 'Final_morph-data_2026.csv' into working directory 
#raw <- read.csv("Final_morph-data_2026.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
#head(raw)


# OPTION 2: load from Github repository
raw <- curl("https://raw.githubusercontent.com/allisondavis/morphology_analysis/refs/heads/master/Final_morph-data_2026.csv")

raw <- read.csv(raw, header = TRUE, sep = ",", stringsAsFactors = TRUE)


# Removing info we don't need
## Note: Right & left pelvic (P2.L/R) identical for both species in all samples
## Note: Total length (TL) is missing data; will use standard length (SL)
## Note: Sample size too sparse in watersheds (hydro.lv8); geographic analysis will use basin (hydro.lv6; hydroBASIN level 6)
raw1 <- select(raw, -c(INSTITUTE, ID.NUM, LOCATION, LAT, LONG, SAMPLING.DATE, MEASUREMENT.DATE, PHOTO.DATE, TAG.., P2.L, P2.R, TL, HYDRO.LV8 ))


# Remove samples with missing values
raw1 <- raw1[complete.cases(raw1[, 4:23]),] 



# Make data more reader friendly
## Species
raw1$SPP <- factor(raw1$SPP,
                   levels = c("p.latipinna", "p.formosa", "p.mexicana"),
                   labels = c("Sailfin molly", "Amazon molly", "Atlantic molly"))

## Zones
raw1$ZONE <- factor(raw1$ZONE,
                    levels = c("Z1", "Z2", "Z3", "Z4"),
                    labels = c("Zone 1", "Zone 2", "Zone 3", "Zone 4"))

## Basins
raw1$HYDRO.LV6 <- factor(raw1$HYDRO.LV6, 
                         levels = c("7060048310", "7060048490", "7060048620", "7060048630", "7060048870", "7060049130", "7060049270", "7060049500", "7060049830", "7060820630", "7060824100"),
                         labels = c("Brazos-Colorado Coastal", "Lavaca-Guadalupe Coastal", "San Antonio-Guadalupe", "San Antonio-Nueces Coastal", "Upper Southwest Texas Coastal", "Lower Southwest Texas Coastal", "Lower R\u00edo Grande", "R\u00edo San Fernando", "Lagunas de San Andr\u00e9s-Morales", "R\u00edo Tames\u00ed", "R\u00edo Tamu\u00edn"))


raw1 <- raw1 %>%
  rename(Species = SPP,
         Zone = ZONE,
         Basin = HYDRO.LV6)



## Trait names
trait.names <- c(
  D = "Dorsal rays",
  P1 = "Left pectoral rays",
  P1.R = "Right pectoral rays",
  A = "Anal rays", 
  LLSC = "Lateral line scale count",
  SALL = "Scales above lateral line",
  SBLL = "Scales below lateral line", 
  SBDF = "Scales before dorsal fin",
  SL = "Standard length", 
  BD = "Body depth", 
  CPD = "Caudal peduncle depth",
  CPL = "Caudal peduncle length",
  PreDL = "Predorsal length",
  DbL = "Dorsal base length",
  HL = "Head length",
  HD = "Head depth",
  HW = "Head width",
  SnL = "Snout length", 
  OL = "Orbit length",
  FLA = "Fluctuating asymmetries")


# Final data frames

## main analysis
raw2 <- raw1[raw1$Species !="Atlantic molly", ]  



#create separate df for meristic (count) data
count <- raw2 %>%
  select(-c(14:23)) #still includes SL for analysis

cnt.vars <- c("D", "P1", "P1.R", "A", "LLSC", "SALL", "SBLL", "SBDF", "FLA")



# ───────────────────────────────────────────────────
## MERISTIC (COUNT) ANALYSIS 
# ───────────────────────────────────────────────────


# Ensure baseline is set
count$Species <- as.factor(count$Species)
count$Species <- relevel(count$Species, ref = "Amazon molly")

poisson_results <- lapply(cnt.vars, function(trait) {
  
  # Build formula: Linear SL is best for count link functions
  form <- as.formula(paste0(trait, " ~ SL * Species"))
  
  # 1. Fit Poisson GLM
  mod <- glmmTMB(form, data = count, family = poisson)
  
  # 2. Run Type III Wald Anova
  anova_res <- tryCatch({
    Anova(mod, type = "III")
  }, error = function(e) { NULL })
  
  # Extract p-values for main effects and interaction safely
  p_sl          <- if(!is.null(anova_res)) anova_res["SL", "Pr(>Chisq)"] else NA
  p_species     <- if(!is.null(anova_res)) anova_res["Species", "Pr(>Chisq)"] else NA
  p_interaction <- if(!is.null(anova_res)) anova_res["SL:Species", "Pr(>Chisq)"] else NA
  
  # 3. DHARMa Overdispersion Diagnostic
  sim_res   <- simulateResiduals(mod, plot = FALSE)
  disp_test <- testDispersion(sim_res, plot = FALSE)
  
  #DHARMa residual checks
  message(paste0("--- DHARMa Diagnostics for Meristic Trait: ", trait, " ---"))
  
  plot(sim_res)
  title(main = paste(trait, "- Poisson Residuals"), line = 2.5)
  
  # Extract the raw dispersion ratio (> 1 means overdispersion) 
  # and the significance p-value
  disp_ratio  <- disp_test$statistic
  disp_pval   <- disp_test$p.value
  is_overdisp <- disp_pval < 0.05 & disp_ratio > 1
  
  data.frame(
    Trait = trait,
    AIC = AIC(mod),
    P_SL = p_sl,
    P_Species = p_species,
    P_Interaction = p_interaction,
    DHARMa_DispRatio = disp_ratio,
    DHARMa_p = disp_pval,
    Overdispersed = is_overdisp,
    row.names = NULL
  )
})

# Combine into a clean data frame
poisson_df <- do.call(rbind, poisson_results)

poisson_corrected <- poisson_df %>%
  mutate(
    # 1. Correct for the species main effect across all traits
    P_Species_adj = p.adjust(P_Species, method = "holm"),
    
    # 2. Correct for the size interaction across all traits
    P_Interaction_adj = p.adjust(P_Interaction, method = "holm"),
    
    # 3. Correct for the standard length effect across all traits
    P_SL_adj = p.adjust(P_SL, method = "holm")
  )

cnt.results <- poisson_corrected %>%
  mutate(
    across(
      .cols = c(P_SL_adj, P_Species_adj, P_Interaction_adj),
      .names = "{.col}_display", 
      .fns = ~ case_when(
        .x < 0.001 ~ "*p* < 0.001*",
        .x < 0.05  ~ paste0(sprintf("%.3f", .x), "*"),
        TRUE       ~ sprintf("%.3f", .x)))) %>%
  mutate(
    Trait_Name = factor(
      trait.names[Trait],
      levels = unname(trait.names))) %>%
  arrange(Trait_Name) %>%
  relocate(Trait_Name, .before = everything())

cnt.tbl <- cnt.results %>%
  gt() %>%
  cols_hide(c(Trait, P_SL, P_Species, P_Interaction, P_SL_adj, P_Species_adj, P_Interaction_adj, DHARMa_p)) %>%
  cols_label(
    Trait_Name = "Trait",
    AIC= "Model AIC",
    DHARMa_DispRatio = "Dispersion Ratio",
    Overdispersed = "Is overdispersed \n(True/False)",
    P_SL_adj_display = md("*p*-value for SL"),
    P_Species_adj_display = md("*p*-value for Species"),
    P_Interaction_adj_display = md("*p*-value for SL:Species")) %>%
  fmt_number(
    columns = c(DHARMa_DispRatio, AIC),
    decimals = 2) %>%
  fmt_markdown(columns = starts_with("P_")) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "lightgray", weight = px(1.5)),
    locations = cells_body(columns = c(Trait_Name, AIC, DHARMa_DispRatio, Overdispersed, P_SL_adj_display, P_Species_adj_display))) %>%
  cols_align("center", everything()) %>%
  opt_horizontal_padding(scale = 2)



cnt.tbl
