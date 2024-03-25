library(ComplexHeatmap)
library(tidyverse)
Bdef1_med_mat <- data_fix_stat %>% 
  select(Time,media) %>% 
  ungroup() %>% 
  mutate(rows = paste(Trat,Time,sep = "_")) %>% 
  select(-Trat,-Time) %>% 
  pivot_wider(names_from = Citosina,values_from = media) %>% 
  select(starts_with("Bdef"),rows) %>% 
  column_to_rownames("rows") %>% 
  drop_na() %>% 
  as.matrix()

Bdef1_med_scaled <- scale(Bdef1_med_mat,center = T,scale = T)

prep_bdef <- tibble(groups =rownames(Bdef1_med_scaled)) %>% 
  mutate(treat = str_split_i(groups,pattern = "_",i = 1),
         time = str_split_i(groups,pattern = "_",i = 2))

ha_bdef = rowAnnotation(
  Treatment = prep_bdef$treat, 
  Time = factor(prep_bdef$time,levels = c("18h","24h")),
  col = list(Time = c("18h" = "coral2",
                      "24h" = "dodgerblue")),
  gp = gpar(col = "black")
)


Heatmap(Bdef1_med_scaled,
        name = "Median",
        col = rev(colores),
        rect_gp = gpar(col = "white", lwd = 1),
        right_annotation = ha_bdef)

decorate_heatmap_body("Median", {
  
  grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
  
})


# Citosinas ---------------------------------------------------------------

Citosinas_med_mat <- data_fix_stat %>% 
  select(Time,media) %>% 
  ungroup() %>% 
  mutate(rows = paste(Trat,Time,sep = "_")) %>% 
  select(-Trat,-Time) %>% 
  pivot_wider(names_from = Citosina,values_from = media) %>% 
  select(!starts_with("Bdef"),rows) %>% 
  column_to_rownames("rows") %>% 
  drop_na() %>%
  as.matrix()


Citosinas_med_mat_scaled <- scale(Citosinas_med_mat,center = T,scale = T)


# Annotations -------------------------------------------------------------
prep_cit <- tibble(groups =rownames(Citosinas_med_mat_scaled)) %>% 
  mutate(treat = str_split_i(groups,pattern = "_",i = 1),
         time = str_split_i(groups,pattern = "_",i = 2))


ha_cit = rowAnnotation(
  Treatment = prep_cit$treat, 
  Time = factor(prep_cit$time,levels = c("0h","6h","12h","18h","24h")),
  gp = gpar(col = "black")
)



tiff(filename = "hm_mediana_cit_annot.tiff",width = 3000,height = 7000,
     compression = 'lzw',res = 600)
Heatmap(Citosinas_med_mat_scaled,
        name = "Median",
        col = rev(colores),
        rect_gp = gpar(col = "white", lwd = 1),
        right_annotation = ha_cit)

decorate_heatmap_body("Median", {
  
  grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
  
})
dev.off()
