# 医学統計学演習：資料3

芳賀昭弘  
Electronic address: haga@tokushima-u.ac.jp

## 復習

Rを起動し、まずは作業ディレクトリをデスクトップのMSEフォルダ（これまで作業してきたディレクトリ）に設定してください。

manabaもしくはgithubからOhtani.csvのファイルをダウンロードしてMSEフォルダに入れてください。次のようにデータを読み込んでみよう。

```r
> data = read.csv("Ohtani.csv")
> NLB = subset(data, NLBorMLB == "N")
> MLB = subset(data, NLBorMLB == "M")
> data
> NLB
> MLB
```

まずOhtani.csvをRで読み込んでdataという名前で格納する。そのうちNLBorMLBの項目でNとなっている方をNLB、Mとなっている方をMLBとして分けて格納する。これは野球の大谷翔平選手の日本のプロ野球（NLB）とアメリカ・メジャー（MLB）での年度別の主要成績をまとめたデータです。これを使って簡単な統計量の演算を本日は行います。

では、その中の列項目Yearを横軸に、Averageを縦軸にデータをプロットしてみましょう。

```r
> plot(NLB$Year,NLB$Average,"l")
> plot(MLB$Year,MLB$Average,"l")
```

Average（打率）の年次推移がプロットされたでしょうか。AverageはHits（ヒット数）/AtBats（打数）で計算できます。実際にそのようになっているかRで計算させて確かめてみよ。（ちなみにxを有効数字３桁で表示させるには format(x,digits=3)だったのを思い出そう。）

例題：NLBとMLBでの年次毎のホームラン数の推移を図示せよ。

## データの代表値の計算

### 平均 mean

大谷の日本とアメリカでの年間のホームラン数の平均値を算出してみよう。

```r
> mean(MLB$HomeRuns)
> mean(NLB$HomeRuns)
```

例題：１ホームラン（HomeRuns）あたりに要した打数（AtBats）の年度別のデータとMLB, NLBごとの平均値を求めよ。

上で述べた平均は、算術平均と言われるもの。平均と言えば、普通は算術平均をいう。しかし、平均にはこの他にも幾何平均、重み付け平均などがある。

算術平均：

$$
\bar{x} = \frac{1}{n}\sum_{i=1}^{n}{x_i}
$$

幾何平均 (geometric mean)：

$$
m_g = \left(\prod_{i=1}^{n}{x_i}\right)^{1/n}
$$

重み付け平均：

$$
E_w[x] = \frac{\sum_{i=1}^{n}{w_i x_i}}{\sum_{i=1}^{n}{w_i}}
$$

$w_i$は$x_i$の重み。

### 中央値median及び最頻値

中央値はデータを小さい順（もしくは大きい順）に並べた時に中央に位置する値（データ数が偶数の場合は、中央２つの平均値）のこと。

```r
> median(MLB$HomeRuns)
> median(NLB$HomeRuns)
```

ヒストグラムのデータで最大度数（最大頻度）を持つ階級値のことを、最頻値という。

### 最大値maxと最小値min

```r
> max(MLB$HomeRuns)
> max(NLB$HomeRuns)
> min(MLB$HomeRuns)
> min(NLB$HomeRuns)
```

### 分散・不偏分散と標準偏差

ホームラン数の不偏分散と標準偏差はどうなっているでしょう？

```r
> var(MLB$HomeRuns)
> sd(MLB$HomeRuns)
```

Rの関数varとsdは、不偏分散と不偏標準偏差を与える。標本分散と標本標準偏差を与えるには、前に作成したvarp関数を利用する。

```r
> source("varp.R")
> varp(MLB$HomeRuns)
> sqrt(varp(MLB$HomeRuns))
```

Rでは、summary関数によるデータの代表値の計算が用意されている。summary関数では、最小値、第1四分位数、中央値、平均値、第3四分位数、最大値が計算される。

```r
> summary(NLB)
> summary(MLB)
```

### 共分散 cov

共分散(covariance)は２つのデータ間の相関（関係性）を見る時に有効な指標となる。不偏共分散は、

$$
\sigma_{xy} = \frac{1}{n-1}\sum_{i=1}^{n}{(x_i-\bar{x})(y_i-\bar{y})}
$$

で与えられ、Rでは不偏共分散を計算するためのcov関数が用意されている。

```r
> cov(MLB$HomeRuns, MLB$Average)
```

標本共分散(covariance)：

$$
s_{xy} = \frac{1}{n}\sum_{i=1}^{n}{(x_i-\bar{x})(y_i-\bar{y})}
$$

は、Rでは特定の関数を用意していないが、次のようにcov関数の値から簡単に変換できる。

```r
> cov(MLB$HomeRuns, MLB$Average) * (length(MLB$HomeRuns)-1)/(length(MLB$HomeRuns))
```

例題：標本共分散を与える新しい関数を作成せよ。

共分散が正のとき正の相関（一方が大きくなると一方も大きくなる）、負の時は負の相関（一方が大きくなると一方は小さくなる）があると言える。しかし、その値の大きさは、データで扱っている値の大きさに依存してしまうため、相関を見る場合には次の相関係数を用いる。

### 相関係数 cor

相関係数（correlation coefficient）は２つの変量の関係を表す統計量で、

$$
r = \frac{\sum_{i=1}^{n}{(x_i-\bar{x})(y_i-\bar{y})}}
{\sqrt{\sum_{i=1}^{n}(x_i-\bar{x})^2}\sqrt{\sum_{i=1}^{n}(y_i-\bar{y})^2}}
$$

と定義される。Rではcor関数が用意されている。

```r
> cor(MLB$Hits, MLB$Average)
> cor(MLB$Hits, MLB$HomeRuns)
```

安打数と打率は相関がありそう。ホームラン数とも相関がある？

## 演習

1. 前回、前々回に使用したEOM2.csvをRで読み込み、投与前beforeと投与後just after、投与後１時間one hour afterそれぞれの算術平均、中央値、最大値、最小値、不偏分散と標本分散、不偏標準偏差と標本標準偏差を求めよ。

2. EOM2.csvの投与前（Before）と投与後（Just.after）の相関係数を求めよ。

3. 上記1と2を出力するスクリプトを作成せよ。

   ヒント: for ループを使うと便利。例えばdataの２列目からn列目までそれぞれの平均を表示させるには、

   ```r
   for ( j in 2:n) {
       Data_mean <- mean(data[,j])
       cat("Mean:",Data_mean,"\n")
   }
   ```

   とする。この出力(print(Data_mean))前に、何の平均なのかを出力させよう（colnames(data)[j]で項目名を取り出すことができる）。このループの中で平均だけでなく計算したい統計量を出力させるように書く。

   作成したスクリプトは、manabaの小テストに貼り付け、**解説してください**。
