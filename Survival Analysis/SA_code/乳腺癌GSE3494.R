library(survminer)
library(survival)
library(GEOquery)
gse <- getGEO('GSE3494', GSEMatrix = TRUE, destdir = './')
show(gse)

#“[2]”是因为这个数据集中有两个平台的数据（GPL96 是“[1]”、GPL97 是“[2]”）
gse <- gse[[2]]  

library(dplyr)
pdata = pData(gse) ## print the sample information
fdata = fData(gse) ## print the gene annotation 基因注释信息
exprs = exprs(gse) ## print the expression data


#发现pdata中没有需要的临床信息
clin_info1 <- read.table(file = "C:\\Users\\Lenovo\\Desktop\\医院实习\\clin_info1.txt", sep = '\t')
View(clin_info1)
colnames(clin_info1) <- clin_info1[1,]
clin_info1 <- clin_info1[-1,]

clin_info2 <- read.table(file = "C:\\Users\\Lenovo\\Desktop\\医院实习\\clin_info2.txt", sep = '\t')
View(clin_info2)
colnames(clin_info2) <- c('geo_accession', 'Patient_ID', 'Affy_platform')
clin_info2 <- clin_info2[clin_info2$Affy_platform == 'HG-U133B',]

clin_info <- merge(clin_info1, clin_info2)
View(clin_info)

#处理clin_info数据
sample_data <- clin_info %>%
  select(grade, ER, PR, size, Lymph_node,time, event, geo_accession)

class(sample_data[,'event'])
class(sample_data[,'time'])

sample_data <- sample_data %>% 
  mutate(time = as.numeric(time),
         event = as.numeric(event))
View(sample_data) 


View(fdata)
#我需要的是第1列（ID）和第11列（Gene symbol）
fdata <- fdata[,c(1,11)]
#芯片ID会有两个Gene symbol与之对应并用“///”分隔开，通过只取第一个Gene symbol来达到去重效果
fdata$`Gene Symbol` <- data.frame(sapply(fdata$`Gene Symbol`,function(x)unlist(strsplit(x,"///"))[1]),stringsAsFactors=F)[,1]

View(exprs)
class(exprs)
#将表达矩阵exprs转为dataframe格式
exprs <- as.data.frame(exprs)
#exprs的探针ID号（最左侧），但这个ID号并不是真正在列中，所以我们还需要将探针ID放到数据框里面
exprs$ID <- rownames(exprs)

#将fdata合并到exprs中，从而在exprs中将芯片ID号转化为Gene symbol
exprs <- merge(exprs, fdata, by="ID")
#Gene symbol有一些未能匹配上的（NA），需要去除这些匹配不上的
exprs <- na.omit(exprs)

#还有一些Gene symbol是重复的，需要我们去重,同时将矩阵转换成标准的表达矩阵形式
table(duplicated(exprs$`Gene Symbol`))
library(limma)
library(affy)
#avereps函数对重复ID的基因表达量求均值
exprs <- avereps(exprs[,-c(1,ncol(exprs))],ID=exprs$`Gene Symbol`)
exprs['XIST',]

#基因XIST的表达量，添加到sample—data中
exprs_XIST <- as.data.frame(exprs['XIST',],optional = TRUE) 
colnames(exprs_XIST) <- c('exprs')
exprs_XIST$geo_accession <- rownames(exprs_XIST)
sample_data = merge(sample_data, exprs_XIST, by='geo_accession')
View(sample_data)


#所有数据，计算最佳截断点
cut_point = surv_cutpoint(sample_data, 
                          time = "time", 
                          event = 'event',
                          variables = 'exprs')
plot(cut_point, palette = 'npg')
#根据截点分类数据
sample_data.cat <- surv_categorize(cut_point)
head(sample_data.cat)

#拟合生存分析
fit <- survfit(Surv(time, event) ~ exprs , data = sample_data.cat)
#绘制生存曲线并显示P值
ggsurvplot(fit,
           data = sample_data.cat,
           palette = c("#E7B800", "#2E9FDF"),
           conf.int = TRUE,
           pval = T,
           xlab = "Time in years",
           title = "Breast cancer, GSE3494-GPL97",
           ggtheme = theme_light())

ggsave(
  filename = "生存曲线.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)



#Lymph_node转移 的数据，计算最佳截断点  
Lymph_node_positive <- sample_data[sample_data$Lymph_node == 'LN+',]
View(Lymph_node_positive)
cut_point1 = surv_cutpoint(Lymph_node_positive, 
                          time = "time", 
                          event = 'event',
                          variables = 'exprs')
plot(cut_point1, palette = 'npg')
#根据截点分类数据
Lymph_node_positive.cat <- surv_categorize(cut_point1)
head(Lymph_node_positive.cat)

#拟合生存分析
fit <- survfit(Surv(time, event) ~ exprs , data = Lymph_node_positive.cat)
#绘制生存曲线并显示P值
ggsurvplot(fit,
           data = Lymph_node_positive.cat,
           palette = c("#E7B800", "#2E9FDF"),
           conf.int = TRUE,
           pval = T,
           xlab = "Time in years",
           title = "Breast cancer with LN+, GSE3494-GPL97",
           ggtheme = theme_light())

ggsave(
  filename = "生存曲线(LN+).png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)
    


#ER negative 的数据，计算最佳截断点  
Lymph_node__negative <- sample_data[sample_data$Lymph_node == 'LN-',]
View(Lymph_node__negative)
cut_point2 = surv_cutpoint(Lymph_node__negative, 
                           time = "time", 
                           event = 'event',
                           variables = 'exprs')
plot(cut_point2, palette = 'npg')
#根据截点分类数据
Lymph_node__negative.cat <- surv_categorize(cut_point2)
head(Lymph_node__negative.cat)

#拟合生存分析
fit <- survfit(Surv(time, event) ~ exprs , data = Lymph_node__negative.cat)
#绘制生存曲线并显示P值
ggsurvplot(fit,
           data = Lymph_node__negative.cat,
           palette = c("#E7B800", "#2E9FDF"),
           conf.int = TRUE,
           pval = T,
           xlab = "Time in years",
           title = "Breast cancer with LN-, GSE3494-GPL97",
           ggtheme = theme_light())

ggsave(
  filename = "生存曲线(LN-).png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)

