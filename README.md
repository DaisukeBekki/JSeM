# JSeM: Japanese semantic test suite (FraCaS counterpart and extensions)

## 内容

このリポジトリには、以下のファイルが含まれています。

jsem_beta.xml
　　日本語意味論テストセットβ版のデータです。
jsem.dtd
　　xmlの仕様を定めたdtdファイル
jsem.xsl
　　MultiFraCaSに似た形式でデータを表示するためのxslファイル


## データの概要

FraCaS test suite（Cooper et al.1996）で扱われている以下の各現象について、対応する日本語のテストを作成し、集めたものを中心に構成したものです。

・一般量化子
・複数性
・照応
・省略
・形容詞
・比較
・テンス
・動詞
・命題的態度

FraCaSの対訳となっているテストが中心ですが、自然な対訳を作れない項目の一部に関しては、同様の現象を含む日本語例を独自に作成しています。また、日本語独自の関連現象も一部扱っています。
FraCaSの項目の対訳ではあるが本質的に異なる現象も存在するため、ここではlink要素のtranslation属性とsame_phenomena属性により、「（リンク先の項目と）対訳レベルで同一視できるか」と「現象レベルで同一視できるか」とを明示的に区別しています。
また、FraCaSのテスト項目の中でも、日本語に対応する現象がないもの、またどのような日本語現象を対応させるかについて議論を要するものは含めていません。

## データ構造

テストセット内のXML要素および属性の説明は以下の通りです。（@が付いたものは属性、その下にあるのは属性値の説明です）

```
jsem_problems：ルート要素。comment, problemを子要素に取る。
comment：全般的なコメント。
problem ： テスト。link, p, h, noteを子要素に取る。
　　@jsem_id：固有の ID
　　@answer：含意関係の有無（ yes, no, unknown, undef ）。
　　　　yes：前提が仮説を含意する
　　　　no: 前提が仮説の否定を含意する
　　　　unknown：前提が仮説を含意せず、その否定も含意しない
　　　　undef：与えられた情報のみからは判断ができない
　　@phenomena： 現象の種類（複数指定可）
　　@inference_type：推論のタイプ。以下のいずれかの値をとる（*）。
　　　　entailment：含意
　　　　presupposition：前提
　　　　CI：conventional implicature
　　　　GCI：generalized conversational implicature
　　　　PCI：particularized conversational implicature
　　　　（*注：現時点では、デフォルトで"entailment"が値に入っています）
　　@jsem_nonsatandard：文の特定の解釈を指定する場合にtrueを取る
link ：他言語リソースとのリンク（FraCaS対応部分のみ）
　　@resource 属性： リンク先リソース名
　　@link id ： リンク先の対応項目 ID
　　@translation ： リンク先の項目と対訳レベルで一致するか（ yes,no,unknown ）
　　@same phenomena ： リンク先の項目と現象レベルで一致するか（ yes,no,unknown ）
p ： 前提。script, englishを子要素に取る。
     @idx：前提につけられた通し番号
h ： 前提（群）から推論可能かどうかが問われる文（仮説）。script, englishを子要素に取る。
script：日本語文
english：対応するFraCaS原文（FraCaSの対訳でない文に関しては指定なし）
note ： コメント
```
## 謝辞

本研究の一部は、JST CREST「ビッグデータ統合利活用のための次世代基盤技術の創出・体系化」領域「知識に基づく構造的言語処理の確立と知識インフラの構築」プロジェクト(JPMJCR1301)，およびJSPS科学研究費補助金基盤研究(B)「日本語CCG統語解析器lightblueの開発」プロジェクト(JP18H03284)の助成を受けている。

## 参考文献

```
機能語の意味を表現する推論テストセット－JSeMとりたて助詞テストの構築－
川添愛, 田中リベカ, 峯島宏次, 戸次大介, (2016).
言語処理学会第22回年次大会発表論文集, B5-3, 東北大学, 2016/3/7-11.
```
```
An Inference Problem Set for Evaluating Semantic Theories and Semantic Processing Systems for Japanese
Kawazoe,Ai; Tanaka,Ribeka; Mineshima,Koji; Bekki,Daisuke; (2015).
In Proceedings of the Twelfth International Workshop on Logic and Engineering of Natural Language Semantics (LENLS12), JSAI International Symposia on AI 2015, pp.67-73, Tokyo-Yokohama, Japan.
```
```
A Framework for Constructing Multilingual Inference Problem
Kawazoe,Ai; Tanaka,Ribeka; Mineshima,Koji; Bekki,Daisuke; (2015).
In Proceedings of 1st International Workshop on the Use of Multilingual Language Resources in Knowledge Representation Systems (MLKRep2015), 08-10 July 2015, Vienna, Austria.
```
