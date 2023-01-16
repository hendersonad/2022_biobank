library(VennDiagram) 

make_venn_diagram <- function(title, x) {
  venn.diagram(
    main=title, main.cex = 2, main.fontfamily = "sans",
    main.fontface = 2, main.pos = c(0.5,0.85),
    x, col = c("#0D5257", "#00BF6F"),
    alpha = c(1,1), lwd =4, cex=1,
    fontfamily="sans",
    cat.cex=1, cat.fontfamily="sans",
    filename = NULL,
    #filename=paste0("venn_diagram_", title, ".tiff"), 
    disable.logging = TRUE,
    #width = 3000, height = 3000,
    margin=0.15,
    print.mode = c("raw", "percent"), sigdigs = 2
    )
}

pdf(here::here("out/venn_diagrams.pdf"), 8,8)
cowplot::plot_grid(
make_venn_diagram("Eczema", list(UKB = 1:5605, GP = 4070:15079)),
make_venn_diagram("Psoriasis", list(UKB = 1:2557, GP = 700:7886)),
make_venn_diagram("Anxiety", list(UKB = 1:3241, GP = 1401:27229)),
make_venn_diagram("Depression", list(UKB = 1:13320, GP = 3733:44187))
)
dev.off()

