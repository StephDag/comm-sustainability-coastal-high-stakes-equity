#***********************************************************************
# Figure 3, panel A,B,C
# --------------------------------------------------------------
# INPUTS:
# - 08_PREP_Figure3_PanelsABC.R
# 
# 
# OUTPUTS
# - Figure 3,ABC
#***********************************************************************
# Creation : Stéphanie D'Agata
# Email :stephanie.dagata@ird.fr & David Gill
# ORCID : https://orcid.org/0000-0001-6941-8489
# Institution : Institut de Recherche pour le Développement
#***********************************************************************

library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)
source(here("analyses","08_PREP_Figure3_PanelsABC.R"))
       
### Figure related
# theme of the figures
       my_theme <- theme_minimal(base_size = 16, base_family = "sans") +
         theme(
           panel.grid.minor = element_blank(),
           axis.title = element_text(face = "bold"),
           plot.title = element_text(face = "bold", hjust = 0.5),
           legend.position = "bottom"
         )

#### Figure 1A ###########
### chloropeth map mean cont inequity and commited investment
       
       # create classes
       N <- 2
       pal = "PinkGrn"
       
       ### Jenks
       data.eq <- bi_class(projected.eez.world.cont.eq.sf.NA, x = usd.comm.year.CADC.total, y = cont.eq.score.mean.rank, style = "quantile", dim = N,dig_lab = 5) %>%
         distinct()
       dim(data.eq)
       break_vals.eq <- bi_class_breaks(projected.eez.world.cont.eq.sf.NA, style = "quantile",
                                        x = usd.comm.year.CADC.total, y = cont.eq.score.mean.rank, dim = N, dig_lab = c(x = 4, y = 5),
                                        split = TRUE)
       rm(data.eq.main) ## add the bi class values for the 84 countries countries - sf file
       data.eq.main <- cont.ineq.ctry.sf.nonNA %>%
         st_drop_geometry() %>%
         left_join(data.eq %>%
                     dplyr::select(sov_a3,bi_class,-geometry,-name_en) %>%  
                     st_drop_geometry(),by=c("iso_a3" = "sov_a3")) %>%
         distinct() %>%
         dplyr::select(-geometry)
       
       ## add sf to data.eq.main
       data.eq.main.sf <- projected.world %>%
         left_join(data.eq.main %>% dplyr::select(-name_en), by="iso_a3") %>%
         distinct() %>%
         filter(!is.na(cont.eq.score.mean.rank))
       data.eq.main.sf %>% filter(name_en == "South Africa")  # main country
       data.eq %>% filter(name == "South Africa") # eez
       dim(data.eq.main.sf) # 84 210

       # get aspect ratior:
       asp.ratio <- get_asp_ratio( projected.world)
       
       # map 4 quadrants
       map <- ggplot(data =  projected.world) +
         geom_sf(color="black",fill = NA) + # empty country
         geom_sf(data=projected.eez.world.sf,color="black",fill = "white") + # empyy eez
         geom_sf(data=projected.eez.world.cont.eq.sf.NA,fill="lightgrey",color = "black")  + # EEZ 84 countries
         geom_sf(data=df.NA.sf %>% filter(legend.ctry == "TRUE_CE_NA_USD"),fill="ivory1",color = "black") + # mainland
         geom_sf(data=  projected.df.NA.sf.eez %>% filter(legend.ctry == "TRUE_CE_NA_USD"),fill="ivory1",color = "black") + # eez
         geom_sf(data = data.eq, mapping = aes(fill = bi_class), color = "black", size = 0.1, show.legend = FALSE,alpha = 0.4) + # eez
         geom_sf(data = data.eq.main.sf, mapping = aes(fill = bi_class), color = "black", size = 0.1, show.legend = FALSE) + # mainland
         bi_scale_fill(pal = pal, dim = N) +
         theme(plot.margin = margin(-2, 0, -2, 0, "cm"),
               panel.border = element_blank(),
               panel.spacing = unit(0, "lines"),
               plot.background = element_blank(),
               panel.background = element_blank())+
         bi_theme()  +
         coord_sf(expand = FALSE)   # This ensures the plot extends to the edge of the panel
       
       # legend
       legend <- bi_legend(pal = pal,
                           dim = N,
                           xlab = "Total invest. (million USD) ",
                           ylab = "Avg. contextual inequity ",
                           size = 8,
                           breaks = break_vals.eq) +
         theme(
           plot.margin = margin(0, 0, 0, 0),
           legend.box.margin = margin(0, 0, 0, 0),
           legend.margin = margin(0, 0, 0, 0)
         )+ my_theme
       
       height=7
       finalPlot.cont.ineq <- ggdraw() +
         draw_plot(map, 0, 0, 1, 1) +
         draw_plot(legend, x = -0.075, y = -0.015, width = 0.35, height = 0.35)
       ggsave(here("figures","Figure3A_chloropeth_quartile_test.pdf"),finalPlot.cont.ineq,width=height*asp.ratio,height=height,dpi=300, bg = "transparent")

### END FIGURE 1A

### FIGURE 1B
       # retrieve color names
       g <- ggplot_build(map)

       colors.legend <- cbind(g$data[[6]]["fill"],g$data[[6]]["group"],data.eq$ISO_SOV1,data.eq$iso_n3) %>%
         distinct() %>%
         arrange(group) %>%
         as.data.frame()
       names(colors.legend)[3] <- "ISO_SOV1"
       names(colors.legend)[4] <- "iso_n3"

       # CADC invest - contextual ineq
       equ.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[4] # 0.79
       committed.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[4] # 322.0225
       equ.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[3] # 0.66
       committed.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[3] # 179.38

       # top left
       TOP_LEFT <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total <med.dollars & cont.eq.score.mean.rank >=med.CE)
       TOP_LEFT$name_en

       # top right
       TOP_RIGHT <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total >med.dollars & cont.eq.score.mean.rank >=med.CE)
       TOP_RIGHT$name_en

       # CI 90%
       # plot quartiles for equity
       equ.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[4]
       committed.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[4]

       equ.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[3]
       committed.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[3]

       # countries top 25%
       top25_perc = projected.eez.world.cont.eq.sf.NA %>% filter(usd.comm.year.CADC.total >= committed.Q.top25 & cont.eq.score.mean.rank >= equ.Q.top25)

       # Figure 1B
       sc.risk.CADC.dollars.90 <- ggplot(cont.ineq.ctry.sf.nonNA %>% distinct(),aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean.rank,label=name_en,color=color.vect.90.V2,size=abs(trend))) +
         annotate("rect", xmin = 0, xmax = committed.Q.top50, ymin = equ.Q.top50, ymax = 1, fill= "#bc177d", alpha = 0.3) + # top left
         annotate("rect", xmin = 0, xmax = committed.Q.top50, ymin = 0, ymax = equ.Q.top50, fill= "#d3d3d3", alpha = 0.3) + # bottom left
         annotate("rect", xmin = committed.Q.top50, xmax = 2250, ymin = 0, ymax = equ.Q.top50, fill= "#459b22", alpha = 0.3) + # bottom right
         annotate("rect", xmin = committed.Q.top50, xmax = 2250, ymin = equ.Q.top50, ymax =1, fill= "#3e1114", alpha = 0.3) + # top right
         geom_point()+
         geom_text_repel( size=4,force=4,max.overlaps=10,show_legend=F,force_pull=2) +
         geom_point(data=top25_perc,aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean.rank,label=name_en),color="white",size=1)+
         xlab("Total investment (Million USD)") +
         ylab("Average contextual inequity") +
         ylim(0,1) + #   xlim(0,1)  +
         scale_color_manual(values = c("firebrick4","grey10","blue4","lightgrey")) +
         labs(color = "Investment direction",size="Investment level (Million USD/year)") +
         theme(legend.position = c(0.45, 0.15),legend.direction="horizontal",
               panel.background = element_rect(fill = "white",
                                               colour = "grey",
                                               linewidth = 0.5, linetype = "solid"),
               panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                               colour = "lightgrey"),
               panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                               colour = "lightgrey"),
               legend.box.background = element_rect(colour = "black"))+ my_theme +
             guides(color = guide_legend(nrow = 2),size = guide_legend(nrow = 2),legend.text=element_text(size = 14),
                    plot.margin = unit(c(1, 2, 1, 2), "cm"))

       sc.risk.CADC.dollars.90

       ### save Figure 1B
       height_2=7
       ggsave(here("figures","Figure3B_trend_90CI.simple_RANK_updates.pdf"),sc.risk.CADC.dollars.90,width=10,height=10,dpi=300,
              device = cairo_pdf)
       ggsave(here("figures","Figure3B_trend_90CI.simple_RANK_updates.png"),sc.risk.CADC.dollars.90,width=10,height=10,dpi=300,
              device = cairo_pdf)

#### END FIGURE 3B
       
       ### FIGURE 1B
       # retrieve color names
       g <- ggplot_build(map)

       colors.legend <- cbind(g$data[[6]]["fill"],g$data[[6]]["group"],data.eq$ISO_SOV1,data.eq$iso_n3) %>%
         distinct() %>%
         arrange(group) %>%
         as.data.frame()
       names(colors.legend)[3] <- "ISO_SOV1"
       names(colors.legend)[4] <- "iso_n3"

       # CADC invest - contextual ineq
       equ.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[4] # 0.79
       committed.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[4] # 322.0225
       equ.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[3] # 0.66
       committed.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[3] # 179.38

       # top left
       TOP_LEFT <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total <med.dollars & cont.eq.score.mean.rank >=med.CE)
       TOP_LEFT$name_en

       # top right
       TOP_RIGHT <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total >med.dollars & cont.eq.score.mean.rank >=med.CE)
       TOP_RIGHT$name_en

       # CI 90%
       # plot quartiles for equity
       equ.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[4]
       committed.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[4]

       equ.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[3]
       committed.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[3]

       # countries top 25%
       top25_perc = projected.eez.world.cont.eq.sf.NA %>% filter(usd.comm.year.CADC.total >= committed.Q.top25 & cont.eq.score.mean.rank >= equ.Q.top25)

       # Figure 1B
       sc.risk.CADC.dollars.90 <- ggplot(cont.ineq.ctry.sf.nonNA %>% distinct(),aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean.rank,label=name_en,color=color.vect.90.V2,size=abs(trend))) +
         annotate("rect", xmin = 0, xmax = committed.Q.top50, ymin = equ.Q.top50, ymax = 1, fill= "#bc177d", alpha = 0.3) + # top left
         annotate("rect", xmin = 0, xmax = committed.Q.top50, ymin = 0, ymax = equ.Q.top50, fill= "#d3d3d3", alpha = 0.3) + # bottom left
         annotate("rect", xmin = committed.Q.top50, xmax = 2250, ymin = 0, ymax = equ.Q.top50, fill= "#459b22", alpha = 0.3) + # bottom right
         annotate("rect", xmin = committed.Q.top50, xmax = 2250, ymin = equ.Q.top50, ymax =1, fill= "#3e1114", alpha = 0.3) + # top right
         geom_point()+
         geom_text_repel( size=4,force=4,max.overlaps=10,show_legend=F,force_pull=2) +
         geom_point(data=top25_perc,aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean.rank,label=name_en),color="white",size=1)+
         xlab("Total investment (Million USD)") +
         ylab("Average contextual inequity") +
         ylim(0,1) + #   xlim(0,1)  +
         scale_color_manual(values = c("firebrick4","grey10","blue4","lightgrey")) +
         labs(color = "Investment direction",size="Investment level (Million USD/year)") +
         theme(legend.position = c(0.45, 0.15),legend.direction="horizontal",
               panel.background = element_rect(fill = "white",
                                               colour = "grey",
                                               size = 0.5, linetype = "solid"),
               panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                               colour = "lightgrey"),
               panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                               colour = "lightgrey"),
               legend.box.background = element_rect(colour = "black"))+ my_theme +
             guides(color = guide_legend(nrow = 2),size = guide_legend(nrow = 2),legend.text=element_text(size = 14),
                    plot.margin = unit(c(1, 2, 1, 2), "cm"))

       sc.risk.CADC.dollars.90

       ### save Figure 1B
       height_2=7
       ggsave(here("figures","Figure1B_trend_90CI.simple_RANK_updates.pdf"),sc.risk.CADC.dollars.90,width=10,height=10,dpi=300,
              device = cairo_pdf)
       ggsave(here("figures","Figure1B_trend_90CI.simple_RANK_updates.png"),sc.risk.CADC.dollars.90,width=10,height=10,dpi=300,
              device = cairo_pdf)

       #### Statistics of Figure 1A and 1B
       # number of countries above both 25%
       test0 <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total >= committed.Q.top25 & cont.eq.score.mean.rank >= equ.Q.top25)
       # number of countries above both 50%
       cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total >= committed.Q.top50 & cont.eq.score.mean.rank >= equ.Q.top50) %>% dim()
       test_2 <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total >= committed.Q.top50 & cont.eq.score.mean.rank >= equ.Q.top50)
       sum(test_2$usd.comm.year.CADC.total)

       # number of countries top left
       cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total < committed.Q.top50 & cont.eq.score.mean.rank >= equ.Q.top50) %>% dim()
       test_3 <-cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total < committed.Q.top50 & cont.eq.score.mean.rank >= equ.Q.top50)
       sum(test_3$usd.comm.year.CADC.total)

       # number of countries bottom left
       cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total < committed.Q.top50 & cont.eq.score.mean.rank < equ.Q.top50) %>% dim()
       test_4 <-cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total < committed.Q.top50 & cont.eq.score.mean.rank < equ.Q.top50)
       sum(test_4$usd.comm.year.CADC.total)

       # number of countries bottom right
       cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total >= committed.Q.top50 & cont.eq.score.mean.rank < equ.Q.top50) %>% dim()
       test_5 <-cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total >= committed.Q.top50 & cont.eq.score.mean.rank < equ.Q.top50)
       sum(test_5$usd.comm.year.CADC.total)

       # total
       tot <- sum(test_2$usd.comm.year.CADC.total)+sum(test_3$usd.comm.year.CADC.total)+
         sum(test_4$usd.comm.year.CADC.total)+sum(test_5$usd.comm.year.CADC.total)
       round(sum(test_2$usd.comm.year.CADC.total)/tot,2)*100
       round(sum(test_3$usd.comm.year.CADC.total)/tot,2)*100
       round(sum(test_4$usd.comm.year.CADC.total)/tot,2)*100
       round(sum(test_5$usd.comm.year.CADC.total)/tot,2)*100
       
       # PREP FIGURE 3C  equity
         # join contextual vulnerability
       oecd.dat.sf.ctry.equity.commit.vulnerab <- oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA %>%
         left_join(data.eq %>% st_drop_geometry() %>% dplyr::select(ISO_SOV1,cont.eq.score.Gini.rank,bi_class),by=c("iso_a3"="ISO_SOV1"),relationship = "many-to-many")

       oecd.dat.sf.ctry.equity.commit.vulnerab <- oecd.dat.sf.ctry.equity.commit.vulnerab %>%
         left_join(colors.legend,by=c("iso_a3" = "ISO_SOV1")) %>%
         filter(!is.na(cont.eq.score.mean.rank)) %>%
         filter(cadc.type=="equity CADC") %>%
         droplevels() %>%
         distinct()
       oecd.dat.sf.ctry.equity.commit.vulnerab$fill

       oecd.dat.sf.ctry.equity.commit.vulnerab$colorname <-  l_colorName(as.character(oecd.dat.sf.ctry.equity.commit.vulnerab$fill))

       data.eq.main
       
       # create color scale
       color.biclass <- oecd.dat.sf.ctry.equity.commit.vulnerab %>%
         dplyr::select(bi_class,fill) %>%
         distinct() %>%
         mutate(bi_class = as.factor(bi_class))

       eq.df <- oecd.dat.sf.ctry.equity.commit.vulnerab %>% filter(cadc.type=="equity CADC") %>% distinct()
       summary(eq.df$perc.equi)
      
# END PREP FIGURE 3C

#### START Figure 1C - V2

CADC.risk.eq.dollars.2 <- ggplot(data=oecd.dat.sf.ctry.equity.commit.vulnerab,aes(x=perc.equi,y=cont.eq.score.mean.rank,label=name_en)) +
  geom_point(aes(colour = fill),size=5) +
  scale_color_identity() +
  xlab("Equity (% total of CADC projects)") +
  geom_text_repel(force_pull=2) +
  ylab("") +
  xlim(0,55) +  ylim(0,1) +
  geom_vline(xintercept=50,linetype = 2)+
  geom_hline(yintercept=0.65,linetype = 2) +
  #scale_x_break(c(50,101),space=0.05, ticklabels = c(50,100)) +
  theme_bw()+
  theme(axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.line.x.top = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1, 2, 1, 2), "cm"))+
  my_theme
CADC.risk.eq.dollars.2

height_2=7
ggsave(here("figures","Figure3C_equity_erc_projects.pdf"),CADC.risk.eq.dollars.2,width=10,height=10,dpi=300,
       device = cairo_pdf)
ggsave(here("figures","Figure3C_equity_erc_projects.png"),CADC.risk.eq.dollars.2,width=10,height=10,dpi=300,
       device = cairo_pdf)

### END FIGURE 3C

#### START COMBINE PLOTS
combined_plot <- finalPlot.cont.ineq /
  (sc.risk.CADC.dollars.90 + CADC.risk.eq.dollars.2) +
  plot_layout(nrow = 2, heights = c(1, 1)) +
  patchwork::plot_annotation(tag_levels = 'A')

ggsave(here("figures","Figure3_CADC.risk.equity.ABC_2.quartiles.pdf"),combined_plot,width=18.4,height=18.4,dpi = 300)
ggsave(here("figures","Figure3_CADC.risk.equity.ABC_2.quartiles.tiff"),combined_plot,width=18.4,height=18.4,dpi = 300)

### END COMBINED PLOT