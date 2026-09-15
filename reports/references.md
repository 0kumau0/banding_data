---
title: "参照文献 — SGD / Adam / 尤度 / 捕獲再捕獲"
date: 2026-09-15
tags:
  - ADCR
  - reference
---

# 参照文献

この track で私（Claude）が説明してきた内容について、**論文に引ける出典**をまとめる。
会話の中の説明は引用できないので、主張ごとに足場になる文献を対応づけた。

**この文書は随時更新する。** 新しい主張が出たら §2 の表に行を足す。

## 確度の表示

書誌情報は記憶から提示しているため、**引用前に必ず原典で確認すること**。

| 印 | 意味 |
|---|---|
| ★★★ | 著者・年・掲載誌まで確実 |
| ★★ | 内容と関連は確実。巻号・ページの細部は要確認 |
| ★ | 存在と関連は確かだが、書誌情報を確認していない |

---

## 1. まず押さえるべき注意 — 機械学習の文献をそのまま引けない

**ML の「収束した」と、この解析の「収束した」は意味が違う。**

| | 機械学習 | この解析 |
|---|---|---|
| 目的 | **汎化**（未知データでの性能） | **最尤解そのもの**を得る |
| データ量 | 巨大（n が百万〜） | **小さい**（個体 100〜1000） |
| 「収束」の意味 | 損失が実用的に下がった | **停留点に標準誤差の 1% まで到達** |
| 最適点に届かないこと | しばしば**望ましい**（過学習の回避） | **そのまま推定の誤り** |

ML の文献に「Adam で十分」と書いてあっても、それは**厳密な最適点を要求していない**から。
`beta2 = 0.999` のような既定値は「ML でうまくいく値」であって、
**M推定で最適点まで詰めたい場合の推奨値ではない**。

**この差は論文に書く価値のある論点**で、§2 の「失速」の行がその根拠になる。

### もうひとつ: これは厳密には SGD ではない

私たちの勾配は**有限差分**で作っている（`grad_cachewise`）。
つまりアルゴリズムの分類としては

- **Robbins–Monro 型**（真の勾配の不偏推定を使う）ではなく
- **Kiefer–Wolfowitz 型**（有限差分による確率的近似、FDSA）

にあたる。収束速度の理論も別物（FDSA は次元の呪いを受けやすい）なので、
**引く文献を間違えないこと。**

---

## 2. 主張と出典の対応表 ← この文書の核心

| 我々の主張 | どこに書いたか | 足場になる文献 |
|---|---|---|
| Adam の歩幅は `m/√v` により ±1 に正規化され、実質 `alpha` になる | walkthrough §8 | Kingma & Ba (2015) |
| `beta2` の記憶は約 `1/(1-beta2)` 反復 | walkthrough §8 | Kingma & Ba (2015)（指数移動平均の性質。この近似自体は一般的） |
| バイアス補正により1歩目がちょうど `alpha` になる | walkthrough §8 | Kingma & Ba (2015) §2 |
| **序盤の大きな勾配が `v` に残り、後半の歩幅を潰す（失速）** | 20260914/15 報告 | **Reddi, Kale & Kumar (2018)** ← 最重要 |
| `v` を指数移動平均にしたことが収束の失敗を生む | 同上 | Reddi et al. (2018)。AMSGrad が対策 |
| 残りの距離 ≒ `H⁻¹g`、残りの対数尤度 ≒ `½gᵀH⁻¹g` | `sgd_simulation.R` §7 | **Boyd & Vandenberghe (2004) §9.5.1**（Newton decrement） |
| 曲率＝情報量、分散＝情報量の逆数 | walkthrough §6 | Pawitan (2001)。標準的な尤度理論 |
| 対数尤度が 0.5 下がる＝±1標準誤差、1.92 下がる＝95%区間 | walkthrough §6 | Pawitan (2001)。1.92 = χ²(1) の 95% 点 3.841 の半分 |
| **単回捕獲を `1/rate` 倍して全体を推定する（不偏）** | `sgd_loglf()` | **Horvitz & Thompson (1952)**。抽出率の逆数による重み付けそのもの |
| ミニバッチの勾配は不偏だが分散を持つ | Step 1 の議論 | Bottou, Curtis & Nocedal (2018) |
| 差分評価は同じ部分集合で行う（共通乱数） | `sgd_utils.R` の注意書き | Spall (2003)。FDSA と共通乱数 |
| Polyak 平均で漸近有効性が得られる | Step 1 で試した | **Polyak & Juditsky (1992)** |
| 確率的勾配法を推定量として評価する枠組み | track 全体の位置づけ | **Toulis & Airoldi (2017)** |
| BFGS は曲率を積み上げて曲がった谷を進む | walkthrough §6 | Nocedal & Wright (2006) 6章 |
| CFL 条件 `dt < 1/(4c)` | walkthrough §4 | Courant, Friedrichs & Lewy (1928)。数値解析の標準的教科書でも可 |
| 再捕獲個体が連結性の情報を担う | 09-15 の議論 | Royle et al. (2013); Borchers & Efford (2008) |

---

## 3. 分野別の一覧

### 3.1 Adam と確率的最適化の原典

| 文献 | 確度 | 何のために |
|---|---|---|
| **Kingma, D.P. & Ba, J. (2015)** "Adam: A Method for Stochastic Optimization." *ICLR 2015*. arXiv:1412.6980 | ★★★ | Adam の定義。既定値 `β1=0.9, β2=0.999, ε=1e-8`、バイアス補正 |
| **Reddi, S.J., Kale, S. & Kumar, S. (2018)** "On the Convergence of Adam and Beyond." *ICLR 2018*（Best Paper） | ★★★ | **Adam が収束しない場合があることの証明。** 原典の収束証明の誤りを指摘。原因は `v` の指数移動平均。**今回の失速の説明はここに立つ** |
| **Robbins, H. & Monro, S. (1951)** "A Stochastic Approximation Method." *Ann. Math. Statist.* 22(3):400–407 | ★★ | 確率的近似の原典。ステップ幅の条件 Σa=∞, Σa²<∞ |
| **Kiefer, J. & Wolfowitz, J. (1952)** "Stochastic Estimation of the Maximum of a Regression Function." *Ann. Math. Statist.* 23(3):462–466 | ★★ | **有限差分版の原典。我々の手法はこちらの系列** |

### 3.2 統計の立場から書かれたもの ← この track の本命

| 文献 | 確度 | 何のために |
|---|---|---|
| **Toulis, P. & Airoldi, E.M. (2017)** "Asymptotic and finite-sample properties of estimators based on stochastic gradients." *Ann. Statist.* 45(4):1694–1727 | ★★ | **確率的勾配法を「推定量」として扱う。** 漸近分散・有効性・implicit SGD。R パッケージ `sgd` の背景 |
| **Polyak, B.T. & Juditsky, A.B. (1992)** "Acceleration of Stochastic Approximation by Averaging." *SIAM J. Control Optim.* 30(4):838–855 | ★★ | Polyak 平均の原典。Step 1 で試した手法 |
| **Bottou, L., Curtis, F.E. & Nocedal, J. (2018)** "Optimization Methods for Large-Scale Machine Learning." *SIAM Review* 60(2):223–311 | ★★★ | **統計・数理最適化の言葉による総説。まずこれを読むとよい** |
| **Spall, J.C. (2003)** *Introduction to Stochastic Search and Optimization.* Wiley | ★★ | FDSA、SPSA、共通乱数。有限差分で確率的最適化をやる立場の教科書 |
| **Kushner, H.J. & Yin, G.G. (2003)** *Stochastic Approximation and Recursive Algorithms and Applications.* Springer | ★ | 厳密な理論が要るとき |

### 3.3 尤度・収束判定・ヘッセ行列

| 文献 | 確度 | 何のために |
|---|---|---|
| **Boyd, S. & Vandenberghe, L. (2004)** *Convex Optimization.* Cambridge. §9.5.1 | ★★★ | **Newton decrement** の定義、`f(x) − p* ≈ λ²/2`、停止条件。**Stanford で全文無料公開** |
| **Pawitan, Y. (2001)** *In All Likelihood: Statistical Modelling and Inference Using Likelihood.* Oxford | ★★ | 曲率と情報量、尤度区間、0.5 / 1.92 の目盛。**生態学の統計をやる読者に合う** |
| **Nocedal, J. & Wright, S.J. (2006)** *Numerical Optimization.* 2nd ed. Springer | ★★★ | BFGS・準ニュートン法（6章）、停止条件の設計 |
| **Efron, B. & Hastie, T. (2016)** *Computer Age Statistical Inference.* Cambridge | ★★ | 古典的推論と計算の橋渡し。**無料 PDF あり** |
| **Horvitz, D.G. & Thompson, D.J. (1952)** "A generalization of sampling without replacement from a finite universe." *JASA* 47(260):663–685 | ★★ | **抽出率の逆数で重みを戻す**推定量の原典。`/sampling_rate` の根拠 |

### 3.4 捕獲再捕獲・空間モデル

| 文献 | 確度 | 何のために |
|---|---|---|
| **Fukasawa, K. & Higashide, D. (2025)** *Ecology* 106(2):e70046 | ★★★ | **ADCR の原典。この track の出発点** |
| **Efford, M.G. (2004)** "Density estimation in live-trapping studies." *Oikos* 106:598–610 | ★★ | SECR の出発点 |
| **Borchers, D.L. & Efford, M.G. (2008)** "Spatially explicit maximum likelihood methods for capture-recapture studies." *Biometrics* 64:377–385 | ★★ | SECR の尤度。実装の基礎 |
| **Royle, J.A., Chandler, R.B., Sollmann, R. & Gardner, B. (2013)** *Spatial Capture-Recapture.* Academic Press | ★★★ | 標準的な教科書 |
| **McRae, B.H. et al. (2008)** "Using circuit theory to model connectivity in ecology, evolution, and conservation." *Ecology* 89:2712–2724 | ★★ | 連結性のもう一つの定式化。ADCR との対比に |

### 3.5 足環データ・分散（Step 4 に向けて）

| 文献 | 確度 | 何のために |
|---|---|---|
| **Paradis, E., Baillie, S.R., Sutherland, W.J. & Gregory, R.D. (1998)** "Patterns of natal and breeding dispersal in birds." *J. Anim. Ecol.* 67:518–536 | ★★ | **英国の標識データから出生地分散・繁殖地分散を推定した古典。** 齢による層別の根拠、回収データのバイアスの議論 |
| **Nathan, R. et al. (2012)** "Dispersal kernels: review." in *Dispersal Ecology and Evolution*, Oxford | ★ | 分散カーネルの総説。**裾の重さ**の議論 |
| **Clark, J.S. et al. (1999)** "Seed dispersal near and far." *Ecology* 80:1475–1494 | ★ | 裾の重いカーネルの古典。2成分混合の発想の背景 |

**注**: このカテゴリは私の確度が最も低い。鳥類分散の文献は
ユーザのほうが詳しいはずなので、**上書きして構わない**。

---

## 4. ウェブ資料

| | 確度 | |
|---|---|---|
| **Sebastian Ruder** "An overview of gradient descent optimization algorithms" — ruder.io / arXiv:1609.04747 | ★★★ | Adam とその周辺を一望。査読論文ではないが広く引用される |
| **Distill.pub** "Why Momentum Really Works"（Goh 2017） | ★★ | モーメンタムの**触れる解説**。図の作り方の参考にもなる |
| **Goodfellow, Bengio & Courville** *Deep Learning* 8章 — deeplearningbook.org | ★★★ | Adam の実務的扱い。**全文無料** |
| **Boyd & Vandenberghe** *Convex Optimization* — Stanford で全文無料 | ★★★ | §9.5 が Newton decrement |

**引用の可否**: Ruder と Distill は査読を経ていないので、
**論文に引くなら原典（Kingma & Ba、Reddi et al.）に当たること。**
理解のための入口としては優秀。

---

## 5. まだ出典を持っていない主張

以下は**この track で我々が実測した内容**で、対応する文献を私は知らない。
論文に書くなら、我々の結果そのものが根拠になる。

| 主張 | 根拠 |
|---|---|
| **`|step|/alpha` が小さいこと自体は失敗の指標にならない** | 2026-09-14 の実測（合成のほうが18倍深く「凍って」いながら収束） |
| 勾配の符号の一貫性と正味移動の組み合わせが失速を検出する | 同上。`reports/20260914_report.md` |
| `pcap_poisson` の `ind_cov.minCoeff()` がループ不変で `O(nind²)` を生む | `examples/pcap_bench.R` の実測。**本家 adcrtest2 のコード** |
| `loglf` のコストは `nind` の約1.8乗 | `examples/nind_scaling.R` |
| 再捕獲41個体では `conn_agri` の標準誤差が約 0.2 | `examples/compare_runs.R`（独立2データで 0.63 と 0.99） |

---

## 6. 読む順序の提案

**1本だけ読むなら** Bottou, Curtis & Nocedal (2018)。
統計の言葉で書かれていて、SGD の全体像が最短で入る。

**今回の失速を論文に書くなら** Reddi et al. (2018) を必ず読む。
主張の骨格がそのままある。

**尤度と誤差の書き方に迷ったら** Pawitan (2001)。
曲率と情報量の関係を、この分野の読者に通じる言葉で説明している。
