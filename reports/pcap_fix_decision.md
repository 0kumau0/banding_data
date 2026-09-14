---
title: "pcap_poisson の性能バグ — 修正するか否かの判断材料"
date: 2026-09-14
tags:
  - ADCR
  - performance
  - decision
---

# pcap_poisson の性能バグ — 修正するか否かの判断材料

**結論から: 2026-09-14 時点の決定は「修正しない」。** 論文化のときに記述が煩雑になるため。
この文書は、その判断の根拠と、**後から見直すときに必要な材料**をまとめたもの。

数値はすべて実測（`examples/profile_loglf.R`、`examples/pcap_bench.R`、
`examples/nind_scaling.R`。生データは `results/*.csv`）。編集機（20論理プロセッサ）で取得。

---

## 1. 何が問題なのか

### 発見の経緯

足環データでは BFGS が重すぎて落ちるため SGD を選んだ、というのがこの track の前提。
その SGD が実際のボトルネックに効くのかを確かめるため、`ncell` を固定して
`nind`（個体数）だけを振り、`loglf` 1回の所要時間を測った。

| `nind` | 300 | 1,200 | 4,800 | 19,200 | 76,800 |
|---|---|---|---|---|---|
| `loglf` 1回 | 0.87秒 | 1.63秒 | 7.20秒 | 89.4秒 | **1,258.5秒** |
| R ヒープ峰 | 176MB | 194MB | 332MB | 802MB | 2,504MB |

**個体数256倍で1447倍。局所的な指数は大きい側で約1.9。**
個体ごとの計算は本来 `nind` に線形のはずで、この形はモデルの構造からは出てこない。

### プロファイル

`ncell = 400` 固定で `Rprof`（`examples/profile_loglf.R`）:

| 関数 | `nind`=4,800 | `nind`=19,200 | 倍率 |
|---|---|---|---|
| **`pcap_poisson`** | 4.18秒（84.6%） | **54.32秒（96.0%）** | **13倍** |
| `log1m_exp_mat` | 0.28秒 | 1.24秒 | 4.4倍（線形） |
| `<GC>` | 0.30秒 | 0.56秒 | 1.9倍 |
| `advdiff_core` | 0.10秒 | 0.10秒 | **1倍（横ばい）** |

個体数4倍に対して `pcap_poisson` だけが13倍。
**ガベージコレクション説は棄却**（総時間の1.0%）。
`advdiff_core` が完全に横ばいなのも確認できた（`ncell` 固定なので当然だが、
「advdiff が重い」という先入観を排除できた）。

### 原因は2つ

`adcrsgd/secrad.r:313-323`（`pcap_poisson`）:

```cpp
#pragma omp parallel for private(i,j,k,colnum,temp)
for(i=0;i<nmu;i++){
    for(j=0;j<nind;j++){
        for(k=0;k<neffort;k++){
            colnum = k+neffort*(ind_cov(j)-ind_cov.minCoeff());   // ← ①
            temp = dpoisson_c(detect(k,j),
                              loglambda_mat(i,colnum)+log(srv(j,effort_occ(k)-1)),
                              logprob);
            #pragma omp atomic                                     // ← ②
            res(i,j) += temp;
        }
    }
}
```

#### ① `ind_cov.minCoeff()` が最内ループの中にある ← **2乗の正体**

`ind_cov` は `const` で一度も変わらないので、`minCoeff()` の値は**ループ不変**。
しかし最内ループに置かれているため、長さ `nind` の縮約が
`nmu × nind × neffort` 回評価される。

```
アルゴリズム本体 : O(nmu × nind × neffort)        ← 線形
実際のコスト     : O(nmu × nind × neffort × nind) ← 2乗
```

**この1行だけで全体が `nind` の2乗になっている。**

#### ② `#pragma omp atomic` が不要

並列化しているのは `i` のループで、**各スレッドは自分の `i` の行にしか書き込まない**。
`res(i,j)` を2つのスレッドが触ることはないので排他は要らない。

- `nind = 19,200` では **1億9,200万回**の atomic が走る
- `MatrixXd` は列優先なので、異なる `i`・同じ `j` を担当するスレッドが
  同じキャッシュラインに書き込む（**false sharing**）

**2026-09-10 に `advdiff_core` で見つけた `#pragma omp critical` と同じアンチパターン**
（「排他が不要。`i` ごとに書き込む行が違うので競合しない」）。同じファイルの別の箇所。

### 影響範囲

| 関数 | 定義 | ① `minCoeff` | ② 排他 | 尤度計算で使われるか |
|---|---|---|---|---|
| `pcap_bin` | `secrad.r:270-297` | 288行 | `atomic` 290行 | **使われる**（`pcap_bin` モデル時） |
| `pcap_poisson` | `secrad.r:300-326` | 317行 | `atomic` 319行 | **使われる**（`pcap_poisson` モデル時） |
| `pcap_poisson_debug` | `secrad.r:329-` | 346行 | `critical` 349行 | デバッグ用。通常経路では呼ばれない |

**これは本家 kfukasawa37/adcrtest2 のコード。** こちらの改変ではない。
論文（Fukasawa & Higashide 2025）の解析の実行時間にも効いていたはず。
**ただし結果には影響しない。**

---

## 2. 修正した場合の効果（実測）

`examples/pcap_bench.R` は **`secrad.r` を一切触らず**、同じ関数の3版を
別々にコンパイルして比較する。

- `orig` … 現行そのまま
- `fix1` … ①だけ修正
- `fix2` … ①+②修正

`nmu = 400 / neffort = 25`:

| `nind` | orig | fix1 | fix2 | 速度比(fix2) |
|---|---|---|---|---|
| 2,400 | 2.06秒 | 0.72秒 | 0.72秒 | 2.9倍 |
| 4,800 | 5.74秒 | 1.87秒 | 1.83秒 | 3.1倍 |
| 9,600 | 21.22秒 | 3.83秒 | 3.55秒 | 6.0倍 |
| 19,200 | **83.09秒** | 7.68秒 | **7.02秒** | **11.8倍** |
| **スケーリング指数** | **1.78** | 1.14 | **1.10** | |

**全条件で結果は完全一致（最大絶対差 0）。変わるのは速度だけ。**

- **①だけで指数が 1.78 → 1.14 に落ちる。** 2乗が消え、ほぼ線形に戻る
- ②の上乗せは `nind`=19,200 で約9%。`nind` が増えるほど効く
- **速度比は `nind` とともに伸びる**（2.9倍 → 11.8倍）。2乗と線形の差なので当然

`nind = 76,800` への外挿では `loglf` 1回が **1,258秒 → 約85秒（約15倍）**。

---

## 3. 修正する / しないで何がどう変わるか

| 観点 | **修正しない**（現行の決定） | 修正する |
|---|---|---|
| **論文の記述** | 「adcrtest2 を使用」で済む | 改変の内容と理由を Methods に書く必要がある |
| **再現性** | 公開パッケージでそのまま再現できる | 改変版を配布・archive する必要がある |
| **`secrad.r` の管理** | 本家+4行（sgd 対応のみ）を維持 | +4行 + 3箇所の性能修正 |
| **本家への追随** | `git checkout` で素の状態に戻せる | 本家更新時にマージの手間が生じうる |
| **推定結果** | — | **変わらない**（最大絶対差 0 を実証済み） |
| **`loglf` 1回**（`nind`=19,200 相当） | 83秒 | 7秒 |
| **速度を稼ぐ主な手段** | `sampling_rate`（超線形に効く） | 素の速度 |
| **原著者との関係** | 伝えるかは任意 | 伝えたうえで取り込みを待つのが筋 |

### 見落としやすい相互作用: 修正すると `sampling_rate` の価値が下がる

コストの指数が違うので、**間引きの効果も変わる**。

| | 指数 | rate 1.0 | rate 0.1（全部単回の理想ケース） |
|---|---|---|---|
| 修正しない | 1.78 | 83秒 | 1.4秒 |
| 修正する | 1.10 | 7.0秒 | 0.56秒 |

**素の速度差は11.8倍だが、両方とも低い `sampling_rate` で走らせると差は約2.5倍まで縮む。**
2乗であることが、間引きの効果を増幅しているため。

**つまり「修正しない」ことの実質的なコストは、11.8倍ではなく2.5倍程度。**
これは「修正しない」という判断を支持する材料になる。

### ただし `sampling_rate` の効き方には上限がある

複数回捕獲個体は毎回全部使う設計なので、

```
実効 nind = n_multi + sampling_rate × n_single
```

**`nind` は `sampling_rate` に比例しない。** 速度比 = (実効 nind の比)^1.78:

| 単回捕獲の割合 | rate 0.5 | rate 0.2 | rate 0.1 |
|---|---|---|---|
| 100% | 3.4倍 | 17倍 | 60倍 |
| 90% | 2.8倍 | 8.6倍 | 18倍 |
| 50%（クマ: 複数回59 / 単回50） | 1.8倍 | 2.5倍 | **2.9倍** |
| 30% | 1.4倍 | 1.7倍 | 1.8倍 |

**足環データの複数回/単回の内訳が、`sampling_rate` で買える速度の上限を決める。**
複数回捕獲の割合が高ければ間引きは効かず、そのとき修正の価値は 2.5倍ではなく
11.8倍に近づく。**Step 4 で最初に確認すべき数値。**

---

## 4. 修正するとしたら、どういう変更になるか

### 案A: ①だけ（最小。効果の大半を得る）

各関数で **1行追加・1行変更**。`pcap_poisson` の例:

```diff
 	int nind=ind_cov.size();
+	const int cov_min = ind_cov.minCoeff();
 	MatrixXd res= MatrixXd::Zero(nmu,nind);
 	Eigen::setNbThreads(1);
 	#pragma omp parallel for private(i,j,k,colnum,temp)
 	for(i=0;i<nmu;i++){
 		for(j=0;j<nind;j++){
 			for(k=0;k<neffort;k++){
-				colnum = k+neffort*(ind_cov(j)-ind_cov.minCoeff());
+				colnum = k+neffort*(ind_cov(j)-cov_min);
```

**変更量: 3箇所 × 2行 = 追加3行 / 変更3行（計6行）。**

- 制御構造もアルゴリズムも変えない。**ループ不変式をループの外に出すだけ**
- 効果: 指数 1.78 → 1.14、`nind`=19,200 で **10.8倍速**
- レビューが容易で、差分だけ見れば正しさが自明

### 案B: ①+②（完全版）

①に加えて内側ループを組み替える。

```diff
 	for(i=0;i<nmu;i++){
 		for(j=0;j<nind;j++){
-			for(k=0;k<neffort;k++){
-				colnum = k+neffort*(ind_cov(j)-cov_min);
-				temp = dpoisson_c(detect(k,j),
-				                  loglambda_mat(i,colnum)+log(srv(j,effort_occ(k)-1)),
-				                  logprob);
-				#pragma omp atomic
-				res(i,j) += temp;
-			}
+			double acc = 0.0;
+			const int base = neffort*(ind_cov(j)-cov_min);
+			for(k=0;k<neffort;k++){
+				acc += dpoisson_c(detect(k,j),
+				                  loglambda_mat(i,base+k)+log(srv(j,effort_occ(k)-1)),
+				                  logprob);
+			}
+			res(i,j) = acc;
 		}
 	}
```

**変更量: 3箇所 × 約10行 = 計約30行。**

- `res` への書き込みが `neffort` 回 → 1回になり、atomic が消える
- 効果: 案Aに対してさらに約9%（`nind` が増えるほど大きい）
- 案Aより差分が大きく、「排他が不要である」根拠の説明が要る

### 対象を絞る選択肢

`pcap_poisson_debug` は通常の尤度計算では呼ばれない。
**実行時間に効くのは `pcap_bin` と `pcap_poisson` の2つだけ**なので、
そこに限れば変更量はさらに 2/3 になる（案Aなら計4行）。

### 検証の手順（修正する場合）

1. `Rscript examples/pcap_bench.R` — 修正版と現行版で**結果が完全一致**することを確認済み
2. `Rscript tests/smoke_test.R` — 既存経路（22件のチェック）を通す
3. クマ実データで `RUN_CHECK` の検算（`loglf = -685.520222`）が通ることを確認
   — **これが最も強い保証**。8497×8497 の計算全体が当時と一致することを示す

---

## 5. 判断を見直すべき条件

「修正しない」は現時点の条件のもとでの判断。次のいずれかが起きたら再検討に値する。

1. **本家 adcrtest2 に取り込まれたとき。**
   「バージョン X を使用」で済むようになり、論文記述の問題が消える。
   **ローカル改変ではなくなるので、デメリットがほぼ無くなる**
2. **足環データの複数回捕獲個体の割合が高いと分かったとき。**
   `sampling_rate` で速度を買えないので、修正の価値が 2.5倍 → 11.8倍 に近づく
3. **`nind` が想定より桁違いに大きいとき。**
   2乗なので、`nind` が10倍になれば差は100倍に開く
4. **計算時間が研究の律速になったとき。** 現状は Adam の収束のほうが問題

---

## 6. 再現方法

```
Rscript examples/nind_scaling.R    # nind と loglf 1回のコスト
Rscript examples/profile_loglf.R   # どの関数に時間が集中しているか
Rscript examples/pcap_bench.R      # 現行版 vs 修正版（secrad.r は触らない）
```

いずれも**実データ不要**、編集機で完結する。出力は `results/` へ。

原著者に伝える場合、`examples/pcap_bench.R` がそのまま再現用の最小コードになる
（`secrad.r` に依存せず、問題の関数の3版を自己完結で比較する）。
