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

shapiro.test(matlab_data$Pre)

## 输入数据
变量<-c(0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.20,0.21,0.23)
GDP<-c(42,43.5,45,45.5,45,47.5,49,53,50,55,55,60)
## 回归分析
lm.sol<-lm(GDP ~  变量)
summary(lm.sol)
## 提取参数
c <- round(coef(lm.sol),3)
#  提取公式变量名
fm <- strsplit(deparse(formula(lm.sol))," " )
## 绘图
plot(lm.sol$fitted.values,lm.sol$model[,2])
#  在图上标注回归公式
text(51 ,0.13,as.expression(paste(fm[[1]][1], "=", c[1],"+",c[2],"*",fm[[1]][3])))


library(tex2exp)
plot(TeX("A $\\LaTeX$ formula: $\\frac{2hc^2}{\\lambda^5} \\,
      \\frac{1}{e^{\\frac{hc}{\\lambda k_B T}} - 1}$"), cex=2)

a <- TeX("A $\\LaTeX$ formula: $\\frac{2hc^2}{\\lambda^5} \\,
      \\frac{1}{e^{\\frac{hc}{\\lambda k_B T}} - 1}$")

library(ymlthis)
blogdown_template("reading")
yml() %>%
  yml_blogdown_opts(
    draft = TRUE,
    slug = "blog-post"
  )

