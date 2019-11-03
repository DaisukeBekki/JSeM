# JSeM: Japanese semantic test suite (Japanese FraCaS and extensions)

## 概要
叙述文間の含意関係は、言語学においては意味論の中心的な説明対象の一つであるとともに、理論を検証するためのベンチマークとして用いられています。また近年の自然言語処理においては、含意関係認識(Recognizing Textual Entailment: RTE)が意味処理タスクの中核となっています。当研究グループでは、日本語の意味理論および自然言語理解技術の評価に資することを目的として「日本語意味論テストセット」(Japansese Semantics test suite）を構築しています。これは、日本語の意味的な現象に基づく含意関係のデータセットで、主に以下の三つの部分からなるテストを集めたものです。

1. 前提(premise)：一つ以上の文
1. 仮説(hypothesis)：一つの文
1. 判定(answer)：1.と2.の間に含意関係があるかどうかについての母語話者の判断（yes, no, unknownあるいはundef）

このテストセットでは、FraCaS test suite（Cooper et al.(1996)で公開されたオリジナルのテストセット, およびBill MacCartney氏による同セットのXML版）の方針にならい、言語現象ごとに含意関係のテストをまとめています。FraCaSで扱われている現象については、FraCaSのテスト項目へのリンクの付いたデータを作成しています（下記のβ版を参照）。また、FraCaSで扱われていない現象や、日本語独自の現象の関わるテストも多く構築しており、今後も追加する予定です。

## 日本語意味論テストセットv1.0の内容
このリポジトリのdata/v1.0フォルダには、以下のファイルが含まれています。

### FraCaS test suite日本語版
- GeneralizedQuantifier.xml（一般量化子）
- Plurals.xml（複数性）
- NominalAnaphora.xml（照応）
- Ellipsis.xml（省略）
- Adjectives.xml（形容詞）
- Comparatives.xml（比較）
- TemporalReference.xml（テンス）
- Verbs.xml（動詞）
- Attitudes.xml（命題的態度）

扱われている各現象に対応する日本語のテストFraCaSの対訳となっているテストが中心ですが、自然な対訳を作れない項目の一部に関しては、同様の現象を含む日本語例を独自に作成しています。FraCaSの項目の対訳であっても本質的に異なる現象も存在するため、ここではlink要素のtranslation属性とsame_phenomena属性により、「（リンク先の項目と）対訳レベルで同一視できるか」と「現象レベルで同一視できるか」とを明示的に区別しています。また、FraCaSのテスト項目の中でも、日本語に対応する現象がないもの、またどのような日本語現象を対応させるかについて議論を要するものは含めていません。

### JSeM拡張データセット
- Adverb.xml（副詞）
- Coordination.xml（等位接続）
- Modality.xml（モダリティ）
- NP.xml（名詞句）
- NewAdjective.xml（形容詞）
- Question.xml（疑問文）
- Toritate.xml（取り立て詞）

FraCaSには対応項目のない言語現象についての、JSeM独自の含意関係データセットです。日本語独自の現象も含まれます。

### その他
- jsem.dtd：xmlの仕様を定めたdtdファイル
- jsem.xsl：MultiFraCaSに即した形式でxmlデータを表示するためのxslファイル

## xmlデータの仕様
テストセット内のXML要素および属性の説明は以下の通りです。@が付いたものは属性、その下にあるのは属性値の説明です。

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

## 関連リンク
- FraCaS textual infefence test suite
  http://www-nlp.stanford.edu/~wcmac/downloads/
　（利用には、Bill MacCartney氏へのクレジット必要）

- MultiFraCaS HP
  http://www.ling.gu.se/~cooper/multifracas/

## 参考文献
- 川添愛，田中リベカ，峯島宏次，戸次大介 (2016) 
「機能語の意味を表現する推論テストセット－JSeMとりたて助詞テストの構築－」
言語処理学会第22回年次大会, B5-3, 東北大学, 2016/3/7-11.
- Ai Kawazoe, Ribeka Tanaka, Koji Mineshima, Daisuke Bekki (2015) 
"An Inference Problem Set for Evaluating Semantic Theories and Semantic Processing Systems for Japanese" 
In Proceedings of the Twelfth International Workshop on Logic and Engineering of Natural Language Semantics (LENLS12), pp.67-73, Tokyo-Yokohama, Japan.
- Ai Kawazoe, Ribeka Tanaka, Koji Mineshima, Daisuke Bekki (2015) 
"A Framework for Constructing Multilingual Inference Problem"
In Proceedings of 1st International Workshop on the Use of Multilingual Language Resources in Knowledge Representation Systems (MLKRep2015), 08-10 July 2015, Vienna, Austria. 
- 川添愛，田中リベカ，峯島宏次，戸次大介（2015)
「形式意味論に基づく含意関係テストセット構築の方法論」
第29回人工知能学会全国大会論文集, はこだて未来大学, 2015/5/30-6/2.
- 川添愛，田中リベカ，峯島宏次，戸次大介 (2015) 
「日本語意味論テストセットの構築」言語処理学会第21会年次大会, E4-1, 京都大学.
- R. Cooper, D. Crouch, J. van Eijck, C. Fox, J. van Genabith, J. Jan, H. Kamp, D. Milward, M. Pinkal, M. Poesio, S. Pulman,T. Briscoe, H. Maier, and K. Konrad. (1996) 
"Using the framework." 
Technical report,FraCaS: A Framework for Computational Semantics. FraCaS deliverable D16.
- B. MacCartney and C. D. Manning. (2008) 
"Modeling semantic containment and exclusion in natural language inference." 
The 22nd International Conference on Computational Linguistics (Coling-08), Manchester, UK, August.

## 謝辞
本研究の一部は、JST CREST「ビッグデータ統合利活用のための次世代基盤技術の創出・体系化」領域「知識に基づく構造的言語処理の確立と知識インフラの構築」プロジェクト(JPMJCR1301)，およびJSPS科学研究費補助金基盤研究(B)「日本語CCG統語解析器lightblueの開発」プロジェクト(JP18H03284)の助成によるものです。

