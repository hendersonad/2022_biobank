library(VennDiagram) 

make_venn_diagram <- function(title, x, mar) {
  venn.diagram(
    main=title, main.cex = 2, main.fontfamily = "sans",
    main.fontface = 2, main.pos = c(0.5,0.85),
    x, col = c("#0D5257", "#00BF6F"),
    alpha = c(1,1), lwd =4, cex=1, pos=100,
    fontfamily="sans",
    cat.cex=1, cat.fontfamily="sans",
    filename = NULL,
    #filename=paste0("venn_diagram_", title, ".tiff"), 
    disable.logging = TRUE,
    #width = 3000, height = 3000,
    margin=mar,
    print.mode = c("raw", "percent"), sigdigs = 2
    )
}

### Assign totals 
tot1 <- 15079
tot2 <- 7886
tot3 <- 27229
tot4 <- 44187
totall <- sum(tot1, tot2, tot3, tot4)

### Assign ratios
rat1 <- tot1/totall
rat2 <- tot2/totall
rat3 <- tot3/totall
rat4 <- tot4/totall
rats <- c(rat1, rat2, rat3, rat4)
margs <- log(1/rats, base=exp(exp(1)))


pdf(here::here("out/venn_diagrams.pdf"), 8,8)
cowplot::plot_grid(
make_venn_diagram("Eczema", list(UKB = 1:5605, GP = 4070:15079), margs[[1]]),
make_venn_diagram("Psoriasis", list(UKB = 1:2557, GP = 700:7886), margs[[2]]),
make_venn_diagram("Anxiety", list(UKB = 1:3241, GP = 1401:27229), margs[[3]]),
make_venn_diagram("Depression", list(UKB = 1:13320, GP = 3733:44187), margs[[4]])
)
dev.off()

svg(here::here("out/venn_diagrams.svg"), 8,8)
cowplot::plot_grid(
  make_venn_diagram("Eczema", list(UKB = 1:5605, GP = 4070:15079), margs[[1]]),
  make_venn_diagram("Psoriasis", list(UKB = 1:2557, GP = 700:7886), margs[[2]]),
  make_venn_diagram("Anxiety", list(UKB = 1:3241, GP = 1401:27229), margs[[3]]),
  make_venn_diagram("Depression", list(UKB = 1:13320, GP = 3733:44187), margs[[4]]),
  scale=exprats
)
dev.off()
