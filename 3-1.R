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
## Date:2026/08/10
## 8章 S3


## test
num <- 1000000000
print(num)











## Author: Mei Nagai
## Date:2026/08/10
## 9章 ループ













