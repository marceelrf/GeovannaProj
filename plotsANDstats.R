(data_fix_stat <- data_fix %>% 
  drop_na() %>% 
  group_by(Citosina,Trat,Time) %>% 
  summarise(across(Values,
                   list(media = mean, 
                        desvpad = sd, 
                        mediana = median,
                        max = max,
                        min = min),
                   .names = "{.fn}")) %>% 
  mutate(cv = desvpad/media,
         disp = media/mediana))





# teste -------------------------------------------------------------------


data_fix_stat %>% 
  filter(Citosina == "IL6") %>% 
  select(-cv,-disp) %>%
  mutate(sd_min = media-desvpad,
         sd_max = media+desvpad) %>% 
  mutate(Time = as.numeric(str_remove(Time,"h"))) %>% 
  ggplot(aes(x=Time, fill = Trat)) +
  geom_point(aes(y = media, col = Trat),
             alpha = .5,size = 3) +
  geom_linerange(aes(ymin = sd_min, ymax = sd_max, col = Trat),
                 linewidth = 1.2,
                 alpha = .5) +
  geom_line(aes(y = media, col = Trat),
            linewidth = 1.2) + 
  # geom_ribbon(aes(ymin=sd_min, ymax=sd_max), alpha = .15) +
  geom_smooth(aes(y = media, col = Trat),method = "glm") +
  scale_x_continuous(breaks = c(0,6,12,18,24)) +
  labs(title = "IL6",
       x = "Time (Hours)",
       y = "Concentration (?)",
       fill = "Treatment") +
  #theme_bw() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(face = "bold",size = 20),
        axis.title = element_text(face = "italic",size = 15),
        legend.position = "bottom",
        panel.background = element_rect(fill = "grey97",
                                        colour = "black",
                                        linewidth = 1),
        panel.grid = element_line(colour = "black",
                                  linewidth = .01),
        panel.grid.minor.x = element_blank()) +
  guides(color = "none", shape = "none")


# multiple plots ----------------------------------------------------------
p_fun <- function(x){
  
  x %>% 
    select(-cv,-disp) %>%
    mutate(sd_min = media-desvpad,
           sd_max = media+desvpad) %>% 
    mutate(Time = as.numeric(str_remove(Time,"h"))) %>% 
    ggplot(aes(x=Time, fill = Trat)) +
    geom_point(aes(y = media, col = Trat),
               alpha = .5,size = 3) +
    geom_linerange(aes(ymin = sd_min, ymax = sd_max, col = Trat),
                   linewidth = 1.2,
                   alpha = .5) +
    geom_line(aes(y = media, col = Trat),
              linewidth = 1.2) + 
    geom_ribbon(aes(ymin=sd_min, ymax=sd_max), alpha = .15) +
    scale_x_continuous(breaks = c(0,6,12,18,24)) +
    labs(title = glue::glue("{Citosina}"),
         x = "Time (Hours)",
         y = "Concentration (?)",
         fill = "Treatment") +
    #theme_bw() +
    theme(text = element_text(family = 'serif'),
          plot.title = element_text(face = "bold",size = 20),
          axis.title = element_text(face = "italic",size = 15),
          legend.position = "bottom",
          panel.background = element_rect(fill = "grey97",
                                          colour = "black",
                                          linewidth = 1),
          panel.grid = element_line(colour = "black",
                                    linewidth = .01),
          panel.grid.minor.x = element_blank()) +
    guides(color = "none", shape = "none")
} # Erro com Glue

plot_list <- data_fix_stat %>% 
  group_by(Citosina) %>% 
  nest() %>% 
  mutate(plots = map(data, \(x) x %>% 
                       select(-cv,-disp) %>%
                       mutate(sd_min = media-desvpad,
                              sd_max = media+desvpad) %>% 
                       mutate(Time = as.numeric(str_remove(Time,"h"))) %>% 
                       ggplot(aes(x=Time, fill = Trat)) +
                       geom_point(aes(y = media, col = Trat),
                                  alpha = .5,size = 3) +
                       geom_linerange(aes(ymin = sd_min, ymax = sd_max, col = Trat),
                                      linewidth = 1.2,
                                      alpha = .5) +
                       geom_line(aes(y = media, col = Trat),
                                 linewidth = 1.2) + 
                       geom_ribbon(aes(ymin=sd_min, ymax=sd_max), alpha = .15) +
                       scale_x_continuous(breaks = c(0,6,12,18,24)) +
                       labs(title = glue::glue("{Citosina}"),
                            x = "Time (Hours)",
                            y = "Concentration (?)",
                            fill = "Treatment") +
                       #theme_bw() +
                       theme(text = element_text(family = 'serif'),
                             plot.title = element_text(face = "bold",size = 20),
                             axis.title = element_text(face = "italic",size = 15),
                             legend.position = "bottom",
                             panel.background = element_rect(fill = "grey97",
                                                             colour = "black",
                                                             linewidth = 1),
                             panel.grid = element_line(colour = "black",
                                                       linewidth = .01),
                             panel.grid.minor.x = element_blank()) +
                       guides(color = "none", shape = "none")))

fig_names <- paste0("Output/",plot_list$Citosina,".png")

walk2(.x = plot_list$plots,.y = fig_names,
      .f = ~ggsave(filename = .y,plot = .x,
                   device = "png", dpi = 600,scale = 2.5),
      .progress = TRUE)
