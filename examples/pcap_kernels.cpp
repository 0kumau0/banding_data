// examples/pcap_kernels.cpp ---------------------------------------------------
//
// `pcap_poisson` の3版。**adcrsgd/secrad.r は触らない。**
// 同じ関数の別実装をここに置いて、測定スクリプトから比べるだけ。
//
//   orig … 本家 kfukasawa37/adcrtest2 の現行実装そのまま（secrad.r:300-326 の写し）
//   fix1 … (1) minCoeff() をループの外へ
//   fix2 … (1) + (2) atomic を外し、ローカルに足し込んで1回だけ書く
//
// 使う側:
//   Rcpp::sourceCpp("examples/pcap_kernels.cpp")
//     examples/pcap_bench.R   … nind を振って「どれだけ速いか」
//     examples/pcap_threads.R … スレッド数を振って「どれだけ伸びるか」
//
// **2026-09-29 に pcap_bench.R のインライン文字列から切り出した。**
// 2つの測定でカーネルが食い違うと、結果同士が比較できなくなるため。
//
// ---------------------------------------------------------------------------
// 現行実装の2つの問題（2026-09-14 に特定。どちらも結果を変えず速度だけ変わる）
//
// (1) `ind_cov.minCoeff()` が最内ループの中にある。ループ不変（`ind_cov` は const）
//     なのに、長さ nind の縮約が nmu × nind × neffort 回評価され、
//     全体が **O(nind²)** になる。
//
// (2) `#pragma omp atomic` が不要。並列化しているのは i のループで、
//     各スレッドは自分の i の行にしか書かない。nind=19200 で1.92億回の atomic。
//     さらに MatrixXd は列優先なので、異なる i・同じ j のスレッドが
//     同じキャッシュラインに書く（false sharing）。
//     **これはスレッド数が増えるほど悪化する** → pcap_threads.R で測る。
// ---------------------------------------------------------------------------

#include <RcppEigen.h>
#include <omp.h>

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;
using namespace Eigen;

// secrad.r:206-221 と同一
inline double dpoisson_c(const int k, const double loglambda, const bool logprob=true){
    double res;
    if((k==0)&(exp(loglambda)==0)){
        res = 0;
    }else if((k!=0)&(exp(loglambda)==0)){
        res = R_NegInf;
    }else{
        res = loglambda*k-exp(loglambda)-lgamma(k+1);
    }
    if(logprob==false){ res = exp(res); }
    return(res);
}

// --- 現行（secrad.r:300-326 の写し）------------------------------------------
// [[Rcpp::export]]
MatrixXd pcap_orig(const MatrixXi detect, const VectorXi effort_occ,
                   const MatrixXd loglambda_mat, const MatrixXi srv,
                   const VectorXi ind_cov, const bool logprob=true){
    int i,j,k,colnum;
    double temp;
    int nmu=loglambda_mat.rows();
    int neffort=effort_occ.size();
    int nind=ind_cov.size();
    MatrixXd res= MatrixXd::Zero(nmu,nind);
    Eigen::setNbThreads(1);
    #pragma omp parallel for private(i,j,k,colnum,temp)
    for(i=0;i<nmu;i++){
        for(j=0;j<nind;j++){
            for(k=0;k<neffort;k++){
                colnum = k+neffort*(ind_cov(j)-ind_cov.minCoeff());
                temp = dpoisson_c(detect(k,j),loglambda_mat(i,colnum)+log(srv(j,effort_occ(k)-1)),logprob);
                #pragma omp atomic
                res(i,j) += temp;
            }
        }
    }
    Eigen::setNbThreads(0);
    return(res);
}

// --- fix1: minCoeff をループの外へ -------------------------------------------
// [[Rcpp::export]]
MatrixXd pcap_fix1(const MatrixXi detect, const VectorXi effort_occ,
                   const MatrixXd loglambda_mat, const MatrixXi srv,
                   const VectorXi ind_cov, const bool logprob=true){
    int i,j,k,colnum;
    double temp;
    int nmu=loglambda_mat.rows();
    int neffort=effort_occ.size();
    int nind=ind_cov.size();
    const int cov_min = ind_cov.minCoeff();          // ← ループ不変なので外へ
    MatrixXd res= MatrixXd::Zero(nmu,nind);
    Eigen::setNbThreads(1);
    #pragma omp parallel for private(i,j,k,colnum,temp)
    for(i=0;i<nmu;i++){
        for(j=0;j<nind;j++){
            for(k=0;k<neffort;k++){
                colnum = k+neffort*(ind_cov(j)-cov_min);
                temp = dpoisson_c(detect(k,j),loglambda_mat(i,colnum)+log(srv(j,effort_occ(k)-1)),logprob);
                #pragma omp atomic
                res(i,j) += temp;
            }
        }
    }
    Eigen::setNbThreads(0);
    return(res);
}

// --- fix2: fix1 + atomic を外し、ローカルに足し込んで1回だけ書く --------------
// [[Rcpp::export]]
MatrixXd pcap_fix2(const MatrixXi detect, const VectorXi effort_occ,
                   const MatrixXd loglambda_mat, const MatrixXi srv,
                   const VectorXi ind_cov, const bool logprob=true){
    int nmu=loglambda_mat.rows();
    int neffort=effort_occ.size();
    int nind=ind_cov.size();
    const int cov_min = ind_cov.minCoeff();
    MatrixXd res= MatrixXd::Zero(nmu,nind);
    Eigen::setNbThreads(1);
    #pragma omp parallel for
    for(int i=0;i<nmu;i++){
        for(int j=0;j<nind;j++){
            double acc = 0.0;                        // ← スレッド固有。競合しない
            const int base = neffort*(ind_cov(j)-cov_min);
            for(int k=0;k<neffort;k++){
                acc += dpoisson_c(detect(k,j),
                                  loglambda_mat(i,base+k)+log(srv(j,effort_occ(k)-1)),
                                  logprob);
            }
            res(i,j) = acc;                          // ← 書き込みは1回だけ
        }
    }
    Eigen::setNbThreads(0);
    return(res);
}

// --- スレッド数の制御 ---------------------------------------------------------
//
// **R 側の Sys.setenv("OMP_NUM_THREADS") は当てにならない。** OpenMP ランタイムは
// 最初の parallel 領域で環境を読むので、読み込み後に変えても効かないことがある。
// 実行時に omp_set_num_threads() を呼ぶのが確実。

// [[Rcpp::export]]
int pcap_set_threads(int n){
    omp_set_num_threads(n);
    return omp_get_max_threads();
}

// [[Rcpp::export]]
int pcap_threads_used(){
    int t = 1;
    #pragma omp parallel
    {
        #pragma omp single
        t = omp_get_num_threads();
    }
    return t;                                        // 実際に使われた数を確認する
}

// [[Rcpp::export]]
int pcap_procs(){ return omp_get_num_procs(); }
