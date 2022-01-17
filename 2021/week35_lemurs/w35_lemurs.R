library(tidyverse)
library(extrafont)
library(networkD3)
library(htmlwidgets)

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

#Keep only unique dlc_id, dam_taxon and sire_taxon
lemur_clean <- lemurs %>% distinct(dlc_id, .keep_all = TRUE) %>% 
  select(dlc_id, dam_taxon, sire_taxon) %>% na.omit()

#Keep only instances of cross-breeding
lemur_clean <- lemur_clean[which(lemur_clean$dam_taxon != lemur_clean$sire_taxon), ]

lemur_unique <- data.table::as.data.table(lemur_clean)
lemur_unique <- lemur_unique[, .N, by = c('dam_taxon','sire_taxon')]

#Assign source and target
lemur_unique$source <- lemur_unique$dam_taxon
lemur_unique$target <- lemur_unique$sire_taxon

#Replace with common names
lemur_unique$source <- str_replace(lemur_unique$source, "EUL", "Eulemur hybrid")
lemur_unique$source <- str_replace(lemur_unique$source, "ERUF", "Red-fronted brown lemur")
lemur_unique$source <- str_replace(lemur_unique$source, "EALB", "White-fronted brown lemur")
lemur_unique$source <- str_replace(lemur_unique$source, "EMAC", "Black lemur")
lemur_unique$source <- str_replace(lemur_unique$source, "VAR", "Varecia hybrid")
lemur_unique$source <- str_replace(lemur_unique$source, "ESAN", "Sanford’s brown lemur")
lemur_unique$source <- str_replace(lemur_unique$source, "EFUL", "Common brown lemur")
lemur_unique$source <- str_replace(lemur_unique$source, "ERUB", "Red-bellied lemur")

lemur_unique$target <- str_replace(lemur_unique$target, "EMAC", "Black lemur")
lemur_unique$target <- str_replace(lemur_unique$target, "ECOL", "Collared brown lemur")
lemur_unique$target <- str_replace(lemur_unique$target, "EUL", "Eulemur hybrid")
lemur_unique$target <- str_replace(lemur_unique$target, "EFUL", "Common brown lemur")
lemur_unique$target <- str_replace(lemur_unique$target, "VRUB", "Red ruffed lemur")
lemur_unique$target <- str_replace(lemur_unique$target, "EALB", "White-fronted brown lemur")
lemur_unique$target <- str_replace(lemur_unique$target, "ERUF", "Red-fronted brown lemur")
lemur_unique$target <- str_replace(lemur_unique$target, "ECOR", "Crowned lemur")
lemur_unique$target <- str_replace(lemur_unique$target, "ESAN", "Sanford’s brown lemur")

#Add a blank space to make nodes unique
lemur_unique$source <- paste0(lemur_unique$source, " ")

nodes <- data.frame(name=c(as.character(lemur_unique$source), as.character(lemur_unique$target)) %>% unique())

lemur_unique$IDsource=match(lemur_unique$source, nodes$name)-1 
lemur_unique$IDtarget=match(lemur_unique$target, nodes$name)-1

ColourScal ='d3.scaleOrdinal() .range(["#420C14","#D96C06","#BAD9B5","#EFF7CF","#606C38","#F5C396","#FFBC0A","#9BC53D","#A51C30","#A53F2B"])'

sn <- sankeyNetwork(
    Links = lemur_unique, Nodes = nodes,
    Source = "IDsource", Target = "IDtarget",
    Value = "N", NodeID = "name", 
    sinksRight=FALSE, 
    colourScale=ColourScal, 
    nodeWidth=40, nodePadding=20,
    fontSize=13, fontFamily="Roboto")

sn

sn <- onRender(
  sn,
  '
  function(el,x){
  // select all our node text
  d3.select(el)
  .selectAll(".node text")
  .filter(function(d) { return d.name.endsWith(" "); })
  .attr("x", x.options.nodeWidth - 45)
  .attr("text-anchor", "end");
  }
  '
)

sn <- onRender(sn, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Female", "Male"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .attr("font-family", "Roboto")
        .text(labels[i]);
    })
  }
')

sn
