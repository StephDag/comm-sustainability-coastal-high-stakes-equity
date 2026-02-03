
# # for packages not on CRAN :
# install.packages("remotes")
#  remotes::install_github("frbcesab/rutils")
#  remotes::install_github("wmgeolab/rgeoboundaries")
# remotes::install_github("ropensci/rnaturalearthhires")

x <- c("here","dplyr","janitor","tidytext","strex","readxl","ggpubr","scico","ggrepel",
       "rnaturalearth","rnaturalearthdata","leaflet","sf","ggmap","rio","terra","maps","sp","wdpar",
       "wordcloud","RColorBrewer","wordcloud2","tm","data.table","rutils","htmltools",
        "ncdf4","RANN","mapview","stars",#"rgeoboundaries"
       "pdftools","tidyverse",
       "factoextra","vegan","GWmodel","ggrepel","elevatr",
       "wesanderson","colorspace",
       "EnvCpt",
       "biscale","cowplot","factoextra","vegan","GWmodel","rcartocolor",
       "R.utils","ggpmisc",
       "tmaptools","kableExtra","tinytex","loon",
       "ggh4x","ggbreak",
       "multcompView","ggdist","patchwork",
       "gridExtra","stringr",
        "bigmemory","openxlsx","DescTools","loon",
       "COINr","matrixStats")

# Load the packages using lapply and require
lapply(x, require, character.only = TRUE)

