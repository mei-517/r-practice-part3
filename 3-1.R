## Author: Mei Nagai
## Date:2026/08/03 - 2026/08/10
## 7章 プログラム

## シンボルを抽出
get_symbols <- function(){
  wheel <- c("DD","7","BBB","BB","B","C","0")
  sample(wheel, size = 3, replace = TRUE, prob = c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}

get_symbols()

## score関数
score <- function(symbols){
  
  ## 場合の確定
  same <- length(unique(symbols)) == 1
  bars <- symbols %in% c("B","BB","BBB")
  
  ## 賞金の計算
  if(same){
    payouts <- c("DD" = 100,"7" = 80,"BBB" = 40,"BB" = 25,"B" = 10,"C" = 10,"0" = 0)
    prize <- unname(payouts[symbols[1]])
  }else if(all(bars)){
    prize <- 5
  }else{
    cherries <- sum(symbols == "C")
    prize <- c(0,2,5)[cherries+1]
  }
  
  ## ダイヤによる賞金の加算
  diamonds <- sum(symbols == "DD")
  prize*2^diamonds
  
}

## play関数
play <- function(){
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}

play()



## 以下score下書き #################################################################

## if(#条件１：すべて同じシンボル){
## prize <- 賞金をルックアップ
## }else if(＃条件２：すべてバー){
## prize <- 5ドル割り当て
## }else{
## チェリーの数を計算
## prize <- 賞金を計算
## }

## ダイヤを数える
## 必要に応じて賞金を２倍にする



## 選択
## 第１の条件
same <- length(unique(symbols)) == 1

## sameほかの例
## symbols[1] ==symbols[2] & symbols[2] == symbols[3]
## symbols == symbols[1]
## length(unique(symbols)) == 1

#テスト
symbols <- c("7","7","7")
same <- length(unique(symbols)) == 1
same
symbols <- c("7","B","7")
same <- length(unique(symbols)) == 1
same

## 20260808 p141~143

## 第２の条件
bars <- symbols %in% c("B","BB","BBB")

## テスト
symbols <- c("B","BBB","BB")
bars <- symbols %in% c("B","BB","BBB")
all(bars)
symbols <- c("0","BB","7")
bars <- symbols %in% c("B","BB","BBB")
all(bars)

## score関数中身下書き

##　賞金計算
## ルックアップテーブル
payouts <- c("DD" = 100,"7" = 80,"BBB" = 40,"BB" = 25,"B" = 10,"C" = 10,"0" = 0)

## テスト
payouts
payouts["DD"]
unname(payouts["DD"])


## 点数入力　テスト
symbols <- c("0","7","DD")
unname(payouts[symbols[1]])
unname(payouts[symbols])

## チェリー数え上げ　テスト
symbols <- c("C","DD","C")
symbols == "C"
sum(symbols == "C")

## チェリーの数に合わせて賞金計算

## 下記を添字操作で置き換える
## if(cherries == 2){
##  prize <- 5
## }else if(cherries = 1){
##  prize <- 2
## }else{
##  prize <- 0
## }
prize <- c(0,2,5)[cherries+1]

##ダイヤモンドの数に合わせて賞金計算
prize*2^didamonds

## テスト
symbols <- c("C","7","DD")

cherries <- sum(symbols == "C")
prize <- c(0,2,5)[cherries+1]
prize

diamonds <- sum(symbols == "DD")
prize*2^diamonds

## 以上score下書き ##################################################################



























## Author: Mei Nagai
## Date:2026/08/14
## 8章 S3


## play()でsymbolsも保存できるようにする

## 元のplay関数
play <- function(){
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}
one_play <- play()
one_play


## 属性を追加してplay関数を書き換え
play <- function(){
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize,"symbols") <- symbols
  prize
}
two_play <- play()
two_play


## structure関数を使用して、play関数を置き換え
play <- function(){
  symbols <- get_symbols()
  structure(score(symbols),symbols = symbols)
}
three_play <- play()
three_play

## 出力をきれいな書式に変換する
slot_display <- function(prize){
  
  ## シンボルの抽出
  symbols <- attr(prize,"symbols")
  
  ## symbolsを１つの文字列に変換
  symbols <- paste(symbols,collapse = " ")
  
  ## シンボルと賞金額を正規表現として結合
  string <- paste(symbols, prize, sep = "\n$")
  
  ## クォートなしでコンソールに正規表現を表示
  cat(string)
}
four_play <- play()
four_play
slot_display(four_play)


## ジェネリック関数













## Author: Mei Nagai
## Date:2026/08/12-2026/08/14
## 9章 ループ


## 以下練習 ウェイトのかかったさいころの期待値を計算する#################################

## さいころを２回振る
die <- c(1,2,3,4,5,6)
rolls <- expand.grid(die,die)

## 二つの出目の和の行を追加する
rolls$value <- rolls$Var1 + rolls$Var2
head(rolls,3)

## 確率
prob <- c("1"=1/8, "2"=1/8, "3"=1/8, "4"=1/8, "5"=1/8,"6"=3/8)
prob

## さいころ１の確率
startprob[rolls$Var1]

## 結果をrollsに追加
rolls$prob1 <- prob[rolls$Var1]
head(rolls,3)

## さいころ２の確率も追加
rolls$prob2 <- prob[rolls$Var2]
head(rolls,3)

## かけ合わせて個々の組の出る確率を追加
rolls$prob <- rolls$prob1 *rolls$prob2
head(rolls,3)

## 値と確率の積を足し合わせて期待値を計算
sum(rolls$value * rolls$prob)

## 以上練習 さいころの期待値#########################################################


## スロットマシンの期待値の計算

## スロットの中身を用意する
wheel <- c("DD","7","BBB","BB","B","C","0")
## ３つの組み合わせのリストを作る
combos <- expand.grid(wheel,wheel,wheel,stringsAsFactors = FALSE)
head(combos,3)

## 確率のルックアップテーブルを用意する
prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

## Var1,Var2,Var3それぞれの確率を追加する
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos,3)

## 組み合わせごとの確率を追加する
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
head(combos,3)

## 確率の合計が１になるか確かめる
sum(combos$prob)


## 下記343回分をfor文で繰り返す
## symbols <- c(combos[1,1],combos[1,2],combos[1,3])
## score(symbols)

## 賞金を入れる空の列を追加する
combos$prize <- NA

## score関数のコピー
score <- function(symbols){
  same <- length(unique(symbols)) == 1
  bars <- symbols %in% c("B","BB","BBB")
  
  if(same){
    payouts <- c("DD" = 100,"7" = 80,"BBB" = 40,"BB" = 25,"B" = 10,"C" = 10,"0" = 0)
    prize <- unname(payouts[symbols[1]])
  }else if(all(bars)){
    prize <- 5
  }else{
    cherries <- sum(symbols == "C")
    prize <- c(0,2,5)[cherries+1]
  }
  
  diamonds <- sum(symbols == "DD")
  prize*2^diamonds
}

## 各行に賞金を代入する
for(i in 1:nrow(combos)){
  symbols <- c(combos[i,1],combos[i,2],combos[i,3])
  combos$prize[i] <- score(symbols)
}
head(combos,3)

## 期待値を出す
sum(combos$prize * combos$prob)




## score関数の訂正(ダイヤをワイルドカードとして扱う)

score <- function(symbols){
  
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  ## ケースの確定
  ## ダイヤモンドはワイルドカードなので、同じシンボルが３つそろっているか、
  ## ３つともバーになっているかはダイヤ以外で考える
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B","BB","BBB")
  
  ## 賞金の計算
  if(diamonds == 3){
    prize <- 100
  }else if(same){
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  }else if(all(bars)){
    prize <- 5
  }else if(cherries > 0){
    ## 本物チェリーがあるときに限り
    ## ダイヤをチェリーとしてカウント
    prize <- c(0,2,5)[cherries + diamonds + 1]
  }else{
    prize <- 0
  }
  
  ## ダイヤによる賞金の加算(１個ごとに２倍)
  prize*2^diamonds
  
}

## 賞金を再計算する
for (i in 1:nrow(combos)){
  symbols <- c(combos[i,1],combos[i,2],combos[i,3])
  combos$prize[i] <- score(symbols)
}

sum(combos$prize * combos$prob)



##持ち金がある限りゲームを続ける(while文)
plays_till_broke <- function(start_with){
  cash <- start_with
  n <- 0
  while(cash >0){
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}
plays_till_broke(100)


##持ち金がある限りゲームを続ける(repeat文)
plays_till_broke <- function(start_with){
  cash <- start_with
  n <- 0
  repeat{
    cash <- cash - 1 + play()
    n <- n + 1
    if (cash <= 0){
      break
    }
  }
  n
}
plays_till_broke(100)
