## 20260803 p127~140

###シンボルを抽出
get_symbols <- function(){
  wheel <- c("DD","7","BBB","BB","B","C","0")
  sample(wheel, size = 3, replace = TRUE, prob = c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}

get_symbols()

## play関数
play <- function(){
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}


## score下書き

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

## ルックアップテーブル
payouts <- c("DD" = 100,"7" = 80,"BBB" = 40,"BB" = 25,"B" = 10,"C" = 10,"0" = 0)

## テスト
payouts
payouts["DD"]
unname(payouts["DD"])
















## 計算
score <- function(symbols){
  
  ## 賞金計算
  
  prize
}

