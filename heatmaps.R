(mat_mediana<-data_fix_stat %>% 
  select(Time,mediana) %>% 
  ungroup() %>% 
  mutate(rows = paste(Trat,Time,sep = "_")) %>% 
  select(-Trat,-Time) %>% 
  pivot_wider(names_from = Citosina,values_from = mediana) %>% 
  column_to_rownames("rows") %>% 
  as.matrix()
)
(mat_media<-data_fix_stat %>% 
    select(Time,media) %>% 
    ungroup() %>% 
    mutate(rows = paste(Trat,Time,sep = "_")) %>% 
    select(-Trat,-Time) %>% 
    pivot_wider(names_from = Citosina,values_from = media) %>% 
    column_to_rownames("rows") %>% 
    as.matrix()
)

library(ComplexHeatmap)
colores <- RColorBrewer::brewer.pal(n = 7,name = "RdBu")
Heatmap(mat_mediana,name = "Medianas")
Heatmap(log10(mat_media+1),
        name = "Medias",
        col = rev(colores),
        rect_gp = gpar(col = "white", lwd = 1))


tiff(filename = "hm_medias.tiff",width = 3000,height = 7000,
     compression = 'lzw',res = 600)
Heatmap(log10(mat_media+1),
        name = "Medias",
        col = rev(colores),
        rect_gp = gpar(col = "white", lwd = 1),
        column_title = "Escala log", 
        column_title_gp = gpar(fontsize = 20, fontface = "bold",
                               fontfamily = "serif"))
decorate_heatmap_body("Medias", {
  
  grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
  
})
dev.off()

