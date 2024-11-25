data = read.csv("SDR2024-data-full.csv")
data = data[0:208,] # NA行が入っているので無視する

colnames(data)

# 各ゴールの達成状況とNAの数の関係を調べる
scores = data[C(1:193),c(640:656)]
nrow(scores)
na_count_row = rowSums(is.na(data[C(1:193),]))

scores = cbind(data$Country[c(1:193)], scores, na_count_row)
View(scores)

summary(scores$na_count_row)
sd(scores$na_count_row)

cor(data$X2024.SDG.Index.Score[c(1:193)], scores$na_count_row, use = "complete.obs")
plot(data$X2024.SDG.Index.Score[c(1:193)], scores$na_count_row, xlab = "2024 SDG Index Score", ylab = "欠損値の個数", main = "欠損値の個数とSDG Index Scoreの関係")

filtered_data <- data[!is.na(data$X2024.SDG.Index.Score), ]
filtered_na_count_row <- na_count_row[!is.na(data$X2024.SDG.Index.Score)] 

unique_regions <- unique(filtered_data$Regions.used.for.the.SDR)[c(1:7)]
colors <- rainbow(length(unique_regions))
region_colors <- setNames(colors, unique_regions)

par(mar = c(5, 4, 4, 10))

# 描画
plot(
  filtered_na_count_row[1:193], 
  filtered_data$X2024.SDG.Index.Score[1:193], 
  xlab = "欠損値の個数", 
  ylab = "2024 SDG Index Score", 
  main = "欠損値の個数とSDG Index Scoreの関係", 
  col = "black", # 初期プロットを白に設定
  pch = 19 # 点を丸に設定
)

for (i in 1:193) {
  points(
    filtered_na_count_row[i], 
    filtered_data$X2024.SDG.Index.Score[i], 
    col = region_colors[data$Regions.used.for.the.SDR[i]], 
    pch = 19
  )
}

legend(
  "topright", 
  inset = c(-0.5, 0),
  legend = unique_regions, 
  col = colors, 
  pch = 19, 
  title = "Region",
  xpd = TRUE
)

# 1:17と18の相関係数17個を求める
cor_list = list()
for(i in 1:17){
  cor_list[[i]] = cor(scores[,i+1], scores$na_count_row, use = "complete.obs")
}

cor_list = cbind(colnames(scores)[2:18], cor_list)
cor_list = data.frame(cor_list)
View(cor_list)
