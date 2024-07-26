ovarianData <- readRDS(
  "/Users/grantcarr/Documents/Michigan/BDSI2024/ovarianData.RDS"
)
lungData <- readRDS(
  "/Users/grantcarr/Documents/Michigan/BDSI2024/lungData.RDS"
)

# merge cell level data with image level metadata
lungCombined <- lungData$lung_cells %>% left_join(
  lungData$lung_metadata , by = "image_id"
)

# lung datasets subsetted by survival status
lungCombined_alive <- lungCombined %>% filter(survival_status == 0)
lungCombined_dead <- lungCombined %>% filter(survival_status == 1)

# plotting random images from alive/dead to look for general patterns
set.seed(1)
ggplot(
  lungCombined_alive %>% filter(image_id == sample(image_id , 1)), 
  aes(x = x, y = y, col = pheno)
) +
  geom_point()
ggplot(
  lungCombined_dead %>% filter(image_id == sample(image_id , 1)), 
  aes(x = x, y = y, col = pheno)
) +
  geom_point()

ggplot(
  lungCombined_alive %>% filter(image_id == sample(image_id , 1)), 
  aes(x = x, y = y, col = pheno)
) +
  geom_point()
ggplot(
  lungCombined_dead %>% filter(image_id == sample(image_id , 1)), 
  aes(x = x, y = y, col = pheno)
) +
  geom_point()

ggplot(
  lungCombined_alive %>% filter(image_id == sample(image_id , 1)), 
  aes(x = x, y = y, col = pheno)
) +
  geom_point()
ggplot(
  lungCombined_dead %>% filter(image_id == sample(image_id , 1)), 
  aes(x = x, y = y, col = pheno)
) +
  geom_point()

# what about cancer stage instead of survival status?

lungData$lung_cells %>% group_by(
  slide_id, image_id
) %>% 
  dplyr::summarize(Proportion_Tumor = sum(pheno == "CK", na.rm = T)/n())

ggplot(
  lungData$lung_cells %>% group_by(
    slide_id, image_id
  ) %>% 
    dplyr::summarize(Proportion = sum(pheno == "CK", na.rm = T)/n()) ,
  aes(x = fct_reorder(slide_id, Proportion, .desc = T), y = Proportion)
) + 
  geom_boxplot() +
  geom_boxplot(inherit.aes = F, aes(y = Proportion), col = "red") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  labs(x = "Slide ID", title = "Proportion of Tumor Cells by Patient")



ggplot(
  lungCombined %>% group_by(patient_id) %>% dplyr::summarize(
    Proportion_Tumor = sum(pheno == "CK", na.rm = T)/n(),
    meanSurv = mean(survival_days)
  ), aes(x = Proportion_Tumor, y = meanSurv)
) +
  geom_point() +
  geom_smooth()
