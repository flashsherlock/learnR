View(mixed.2_1b1w)
MANOVA(data=mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
       between="A", within="B", sph.correction="GG") %>%
  EMMEANS("A", by="B") %>%
  EMMEANS("B", by="A")

MANOVA(data=data, dvs="B1:B2", dvs.pattern="B(.)",
       between="A", within="B", sph.correction="GG") %>%
  EMMEANS("A", by="B") %>%
  EMMEANS("B", by="A")
View(within.2)

MANOVA(data=within.2, dvs="A1B1:A2B3", dvs.pattern="A(.)B(.)",
       within=c("A", "B")) %>%
  EMMEANS("A", by="B") %>%
  EMMEANS("B", by="A") %>%  # with some errors
  EMMEANS("B", by="A", repair=TRUE)