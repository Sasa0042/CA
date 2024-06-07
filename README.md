# CA

### 2024/05  Random intercept logistic regression in Bayesian setting:

了解到随机截距模型可以对(相关的)分类数据进行回归，using 'brms' package to sample the posterior draws and check for the prior's distribution.


### 2024/05   Time series analysis for Olist e-commerce:

对Olist公司运营情况可视化进行回顾，作为时间序列数据对其seasonality进行可视化，simple as it is, but finding many interesting Brazilian consumption habits.

运用时间序列分析课程上的内容，拟合SARIMA模型对时序数据进行预测，不过感觉预测效果并不是很好，猜测是由于节假日的因素导致有很多异常的残差值。后面了解到structural time series, which includs trend, seasonality, impacts and noise. 它可以处理节假日带来的impacts影响，使用bsts(Bayesian Structural Time Series) package 对仅考虑Black Friday购物季带来的影响进行了尝试，更精细化的模型可以加入所有巴西的节日。


### 2024/05   Clustering analysis for 2024 QS rankings dataset:

Inspired by the paper named 'Cluster Analysis of Top 200 Universities in Mathematics' stored in 'getPDF2', 在论文提出的方法的基础上，我对数据集中各大学根据其与各项指标都满分间的欧式距离以及加权的高尔距离进行排名，高尔距离可以包含连续变量和离散变量，QS排名事实上对发表论文难的艺术类院校其实不公平，分学科对大学进行排名是一种选择， 那么也可以选择采用高尔距离包含一些离散型指标如学校公立私立，大小等等。

In the paper named 'How Do The Scores of World University Rankings Distrubute' stored in 'getPDF', 论文发现各指标得分中高分的学校很多，这显然不甚合理。我也绘制了各指标得分的直方图，不过貌似QS排名在这篇文章发表后就修改了计算大学指标得分的方法，现在的分布就非常正常。

在课上学习了聚类分析的理论，但我一直不甚清楚聚类分析有什么用，不清楚为什么要分类感觉就是多此一举，而网上却到处都在说聚类分析有助于更好的展示数据、挖掘出数据间的联系。这次project后，我才深刻体会到deodrogram树图内隐藏的大量信息。同时对仅仅利用数学上定义的距离远近来分类，非常simple的idea，却能解决大问题，大受震撼。


### 2024/03-04   Factor analysis for MoCA questionnaires:

Given the ordinal nature of the variables，I computed its polychoric correlation matrix, and conduct an EFA based on the papar named 'Exploratory Factor Analysis: A Guide to Best Practice'. 对得到的因子结构执行CFA进行验证，the result is not an acceptable model based on the criterias listed in the paper.

Studying the item response theory，the unobserved factor is viewed as ability in IRT，it assumes conditional independence. 我选用了两个高维的模型：'The graded response model' and 'The generalized partial credit model'，对模型拟合结果检验拟合优度和模型假设，最终得到一个两因子模型。

回顾了很多之前对MoCA认知问卷做因子分析的论文，类似于MMSE问卷（另一个认知问卷）因子分析的结果，提出模型因子数1,2,...,6(the theoretical factor structure)的都有。且我的数据集是MoCA-Basic是一个基础版，且还对得到的问卷数据进行了处理，最后得到的数据集只有10个变量，跟各论文中得到的因子分析结果也是大相径庭。

在这次课题研究中，我感知到了整个进行因子分析的大体系，体系中各个小的问题都有大帮学者去研究。In a paper named 'Testing the Unidimensionality of Items', it listed three cases of item construction where the items are supposed to be unidimensional but are multidimensional. 


### 2023/12   Linear regression and generalized linear regression for followers:

粉丝数作为response variable, 右偏进行log变换，直接将其看作连续变量进行传统线性回归，走完线性回归的完整流程，最后对模型假设进行检验发现了异方差，分别尝试了Generalized Least Squares(GLS) and Weighted Least Squares(WLS) model to deal with the heteroskedasticity. 

In addition，we can view it as count variable, conducting the poisson regression, which assumes the mean equals to the variance. No surprise to find the existence of overdispersion. So we refit a negative binomial regression model, which variance equals its mean plus an extra term. 

For model comparsion, at this time, I have no idea for the method to test the goodness of fit for the glm model.


### 2023/07-09   Survival Analysis for breast cancer:

Without the knowledge of R code, I need to learn it by myself. 从GEO和TCGA上面提取乳腺癌患者的数据并进行预处理非常磨人。Similarly, after learning the basic idea of survival analysis by myself, 该次实习主要通过绘制生存分析曲线来进行探索。

最终并没有得到好的结果，一来对生存分析相关知识背后机理不甚熟悉，二来对乳腺癌相关的研究背景专业名词也不清晰，画出的图并没有直接显示出适配的结果，我也不知道接下来该往哪里前进，就放弃了。



