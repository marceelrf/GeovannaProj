bs_data <- readxl::read_xlsx(path = "data/mrf_BS.xlsx")

bs_mat <- bs_data %>% 
  pivot_longer(cols = ends_with("h")) %>% 
  mutate(Group = paste0(Group,name)) %>% 
  select(-name) %>% 
  pivot_wider(names_from = Citosina,values_from = value) %>% 
  mutate(Group = str_remove(Group,pattern = " ")) %>% 
  column_to_rownames(var = "Group") %>% 
  as.matrix()

bs_mat_filt <- bs_data %>% 
  pivot_longer(cols = ends_with("h")) %>% 
  mutate(Group = paste0(Group,"_",name)) %>% 
  select(-name) %>% 
  pivot_wider(names_from = Citosina,values_from = value) %>% 
  mutate(Group = str_remove(Group,pattern = " ")) %>% 
  column_to_rownames(var = "Group") %>% 
  select(-IL1B,-IL10) %>% 
  as.matrix()


# Style -------------------------------------------------------------------

RColorBrewer::display.brewer.all(n = 7,select = "RdBu")
colores <- RColorBrewer::brewer.pal(n = 7,name = "RdBu")

colores <- circlize::colorRamp2(breaks = c(-1.5,
                                           0,
                                           8),
                                colors = colores[c(1,4,7)],
                                transparency = 0)



bs_mat_scaled <- scale(bs_mat,center = T,scale = T)

bs_mat_scaled[is.nan(bs_mat_scaled)] <- 0

Heatmap(bs_mat_scaled,col = (colores),
        rect_gp = gpar(col = "grey40", lwd = 1))

bs_mat_filt_scaled <- scale(bs_mat_filt,center = T,scale = T)

prep_cit <- tibble(groups =rownames(bs_mat_filt_scaled)) %>% 
  mutate(treat = str_split_i(groups,pattern = "_",i = 1),
         time = str_split_i(groups,pattern = "_",i = 2))

col_time <-  RColorBrewer::brewer.pal(n = 5,name = "RdPu")
col_treat <- rainbow(13)
ha_cit = rowAnnotation(
  Treatment = prep_cit$treat, 
  Time = factor(prep_cit$time,levels = c("0h","6h","12h","18h","24h")),
  col = list(Time = c("0h" = col_time[1],
                      "6h" = col_time[2],
                      "12h" = col_time[3],
                      "18h" = col_time[4],
                      "24h" = col_time[5])),
  gp = gpar(col = "black")
)


tiff(filename = "hm_bs_cit_annot.tiff",width = 3000,height = 7000,
     compression = 'lzw',res = 600)
Heatmap(bs_mat_filt_scaled,col = colores,
        name = "Scaled",border = "black",
        border_gp =gpar(lwd = 2), 
        rect_gp = gpar(col = "grey40", lwd = 1),
        right_annotation = ha_cit,
        column_title = "Boostrap data")
dev.off()
