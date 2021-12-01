# SDTM-Central-Monitoring
## 概要
それぞれ以下の処理を実行するプログラム群です。  
* VISITの導出（1-get-visit.R）
* 必要なオブザベーションの抽出（2-exctract-grade-observation.R）
* VISITおよび有害事象のGradeに関する頻度集計（3-summarize-by-grade.R）
* GRADE毎の集計結果についてのレポート出力（4-output-docx.Rmd）   

実行プログラムはRディレクトリ配下に格納されています。   
各プログラムは独立しているので、必要なプログラムだけ実行可能です。詳細は[wiki](https://github.com/nnh/SDTM-Central-Monitoring/wiki)に記載しました。  
## 入出力ファイル（例）
入力ファイルの例として、TEST/temp/ディレクトリに'dummyFA.csv', 'dummyVISIT.csv'を格納しています。   
出力ファイルの例として、TEST/temp/ディレクトリに'compare_summarize-by-grade.csv'を格納しています。  
出力ファイルは上記のテスト用入力ファイルを入力とし、1-get-visit.R, 2-exctract-grade-observation.R, 3-summarize-by-grade.Rを順に実行した場合のイメージです。  
## リポジトリのディレクトリ構造
```
.
├── DESCRIPTION
├── LICENSE
├── NAMESPACE
├── R
│   ├── 1-get-visit.R
│   ├── 2-extract-grade-observation.R
│   ├── 3-summarize-by-grade.R
│   └── 4-output-docx.Rmd
├── README.md
├── SDTM-Central-Monitoring.Rproj
└── TEST
    ├── all_exec.R
    ├── run_test.R
    ├── temp
    │   ├── compare_summarize-by-grade.csv
    │   ├── dummyFA.csv
    │   ├── dummyVISIT.csv
    │   └── encode_bom.csv
    ├── test.1-get-visit.R
    ├── test.2-extract-grade-observation.R
    ├── test.3-summarize-by-grade.R
    ├── test.common.R
    └── test.inputfile-edit.R
```
