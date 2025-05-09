open Ctypes
open Foreign

(* CBLAS_ORDER enum *)
type order = 
  | RowMajor
  | ColMajor

let int_of_order = function
  | RowMajor -> 101
  | ColMajor -> 102

let order_of_int = function
  | 101 -> RowMajor
  | 102 -> ColMajor
  | _ -> failwith "Invalid order value"

(* CBLAS_TRANSPOSE enum *)
type transpose = 
  | NoTrans
  | Trans
  | ConjTrans

let int_of_transpose = function
  | NoTrans -> 111
  | Trans -> 112
  | ConjTrans -> 113

let transpose_of_int = function
  | 111 -> NoTrans
  | 112 -> Trans
  | 113 -> ConjTrans
  | _ -> failwith "Invalid transpose value"

(* CBLAS_UPLO enum *)
type uplo = 
  | Upper
  | Lower

let int_of_uplo = function
  | Upper -> 121
  | Lower -> 122

let uplo_of_int = function
  | 121 -> Upper
  | 122 -> Lower
  | _ -> failwith "Invalid uplo value"

(* CBLAS_DIAG enum *)
type diag = 
  | NonUnit
  | Unit

let int_of_diag = function
  | NonUnit -> 131
  | Unit -> 132

let diag_of_int = function
  | 131 -> NonUnit
  | 132 -> Unit
  | _ -> failwith "Invalid diag value"

(* CBLAS_SIDE enum *)
type side = 
  | Left
  | Right

let int_of_side = function
  | Left -> 141
  | Right -> 142

let side_of_int = function
  | 141 -> Left
  | 142 -> Right
  | _ -> failwith "Invalid side value"

(* BLAS functions *)

let sdsdot = foreign "cblas_sdsdot" (int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> returning float)

let dsdot = foreign "cblas_dsdot" (int @-> ptr float @-> int @-> ptr float @-> int @-> returning double)

let sdot = foreign "cblas_sdot" (int @-> ptr float @-> int @-> ptr float @-> int @-> returning float)

let ddot = foreign "cblas_ddot" (int @-> ptr double @-> int @-> ptr double @-> int @-> returning double)

let cdotu_sub = foreign "cblas_cdotu_sub" (int @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> returning void)

let cdotc_sub = foreign "cblas_cdotc_sub" (int @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> returning void)

let zdotu_sub = foreign "cblas_zdotu_sub" (int @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> returning void)

let zdotc_sub = foreign "cblas_zdotc_sub" (int @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> returning void)

let snrm2 = foreign "cblas_snrm2" (int @-> ptr float @-> int @-> returning float)

let sasum = foreign "cblas_sasum" (int @-> ptr float @-> int @-> returning float)

let dnrm2 = foreign "cblas_dnrm2" (int @-> ptr double @-> int @-> returning double)

let dasum = foreign "cblas_dasum" (int @-> ptr double @-> int @-> returning double)

let scnrm2 = foreign "cblas_scnrm2" (int @-> ptr void @-> int @-> returning float)

let scasum = foreign "cblas_scasum" (int @-> ptr void @-> int @-> returning float)

let dznrm2 = foreign "cblas_dznrm2" (int @-> ptr void @-> int @-> returning double)

let dzasum = foreign "cblas_dzasum" (int @-> ptr void @-> int @-> returning double)

let isamax = foreign "cblas_isamax" (int @-> ptr float @-> int @-> returning size_t)

let idamax = foreign "cblas_idamax" (int @-> ptr double @-> int @-> returning size_t)

let icamax = foreign "cblas_icamax" (int @-> ptr void @-> int @-> returning size_t)

let izamax = foreign "cblas_izamax" (int @-> ptr void @-> int @-> returning size_t)

let sswap = foreign "cblas_sswap" (int @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let scopy = foreign "cblas_scopy" (int @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let saxpy = foreign "cblas_saxpy" (int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let dswap = foreign "cblas_dswap" (int @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let dcopy = foreign "cblas_dcopy" (int @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let daxpy = foreign "cblas_daxpy" (int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let cswap = foreign "cblas_cswap" (int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ccopy = foreign "cblas_ccopy" (int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let caxpy = foreign "cblas_caxpy" (int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let zswap = foreign "cblas_zswap" (int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let zcopy = foreign "cblas_zcopy" (int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let zaxpy = foreign "cblas_zaxpy" (int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let srotg = foreign "cblas_srotg" (ptr float @-> ptr float @-> ptr float @-> ptr float @-> returning void)

let srotmg = foreign "cblas_srotmg" (ptr float @-> ptr float @-> ptr float @-> float @-> ptr float @-> returning void)

let srot = foreign "cblas_srot" (int @-> ptr float @-> int @-> ptr float @-> int @-> float @-> float @-> returning void)

let srotm = foreign "cblas_srotm" (int @-> ptr float @-> int @-> ptr float @-> int @-> ptr float @-> returning void)

let drotg = foreign "cblas_drotg" (ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)

let drotmg = foreign "cblas_drotmg" (ptr double @-> ptr double @-> ptr double @-> double @-> ptr double @-> returning void)

let drot = foreign "cblas_drot" (int @-> ptr double @-> int @-> ptr double @-> int @-> double @-> double @-> returning void)

let drotm = foreign "cblas_drotm" (int @-> ptr double @-> int @-> ptr double @-> int @-> ptr double @-> returning void)

let sscal = foreign "cblas_sscal" (int @-> float @-> ptr float @-> int @-> returning void)

let dscal = foreign "cblas_dscal" (int @-> double @-> ptr double @-> int @-> returning void)

let cscal = foreign "cblas_cscal" (int @-> ptr void @-> ptr void @-> int @-> returning void)

let zscal = foreign "cblas_zscal" (int @-> ptr void @-> ptr void @-> int @-> returning void)

let csscal = foreign "cblas_csscal" (int @-> float @-> ptr void @-> int @-> returning void)

let zdscal = foreign "cblas_zdscal" (int @-> double @-> ptr void @-> int @-> returning void)

let sgemv = foreign "cblas_sgemv" (int @-> int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> float @-> ptr float @-> int @-> returning void)

let sgbmv = foreign "cblas_sgbmv" (int @-> int @-> int @-> int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> float @-> ptr float @-> int @-> returning void)

let strmv = foreign "cblas_strmv" (int @-> int @-> int @-> int @-> int @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let stbmv = foreign "cblas_stbmv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let stpmv = foreign "cblas_stpmv" (int @-> int @-> int @-> int @-> int @-> ptr float @-> ptr float @-> int @-> returning void)

let strsv = foreign "cblas_strsv" (int @-> int @-> int @-> int @-> int @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let stbsv = foreign "cblas_stbsv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let stpsv = foreign "cblas_stpsv" (int @-> int @-> int @-> int @-> int @-> ptr float @-> ptr float @-> int @-> returning void)

let dgemv = foreign "cblas_dgemv" (int @-> int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> double @-> ptr double @-> int @-> returning void)

let dgbmv = foreign "cblas_dgbmv" (int @-> int @-> int @-> int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> double @-> ptr double @-> int @-> returning void)

let dtrmv = foreign "cblas_dtrmv" (int @-> int @-> int @-> int @-> int @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let dtbmv = foreign "cblas_dtbmv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let dtpmv = foreign "cblas_dtpmv" (int @-> int @-> int @-> int @-> int @-> ptr double @-> ptr double @-> int @-> returning void)

let dtrsv = foreign "cblas_dtrsv" (int @-> int @-> int @-> int @-> int @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let dtbsv = foreign "cblas_dtbsv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let dtpsv = foreign "cblas_dtpsv" (int @-> int @-> int @-> int @-> int @-> ptr double @-> ptr double @-> int @-> returning void)

let cgemv = foreign "cblas_cgemv" (int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let cgbmv = foreign "cblas_cgbmv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let ctrmv = foreign "cblas_ctrmv" (int @-> int @-> int @-> int @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ctbmv = foreign "cblas_ctbmv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ctpmv = foreign "cblas_ctpmv" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let ctrsv = foreign "cblas_ctrsv" (int @-> int @-> int @-> int @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ctbsv = foreign "cblas_ctbsv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ctpsv = foreign "cblas_ctpsv" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let zgemv = foreign "cblas_zgemv" (int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let zgbmv = foreign "cblas_zgbmv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let ztrmv = foreign "cblas_ztrmv" (int @-> int @-> int @-> int @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ztbmv = foreign "cblas_ztbmv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ztpmv = foreign "cblas_ztpmv" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let ztrsv = foreign "cblas_ztrsv" (int @-> int @-> int @-> int @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ztbsv = foreign "cblas_ztbsv" (int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ztpsv = foreign "cblas_ztpsv" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let ssymv = foreign "cblas_ssymv" (int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> float @-> ptr float @-> int @-> returning void)

let ssbmv = foreign "cblas_ssbmv" (int @-> int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> float @-> ptr float @-> int @-> returning void)

let sspmv = foreign "cblas_sspmv" (int @-> int @-> int @-> float @-> ptr float @-> ptr float @-> int @-> float @-> ptr float @-> int @-> returning void)

let sger = foreign "cblas_sger" (int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let ssyr = foreign "cblas_ssyr" (int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let sspr = foreign "cblas_sspr" (int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> returning void)

let ssyr2 = foreign "cblas_ssyr2" (int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let sspr2 = foreign "cblas_sspr2" (int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> ptr float @-> returning void)

let dsymv = foreign "cblas_dsymv" (int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> double @-> ptr double @-> int @-> returning void)

let dsbmv = foreign "cblas_dsbmv" (int @-> int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> double @-> ptr double @-> int @-> returning void)

let dspmv = foreign "cblas_dspmv" (int @-> int @-> int @-> double @-> ptr double @-> ptr double @-> int @-> double @-> ptr double @-> int @-> returning void)

let dger = foreign "cblas_dger" (int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let dsyr = foreign "cblas_dsyr" (int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let dspr = foreign "cblas_dspr" (int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> returning void)

let dsyr2 = foreign "cblas_dsyr2" (int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let dspr2 = foreign "cblas_dspr2" (int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> ptr double @-> returning void)

let chemv = foreign "cblas_chemv" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let chbmv = foreign "cblas_chbmv" (int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let chpmv = foreign "cblas_chpmv" (int @-> int @-> int @-> ptr void @-> ptr void @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let cgeru = foreign "cblas_cgeru" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let cgerc = foreign "cblas_cgerc" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let cher = foreign "cblas_cher" (int @-> int @-> int @-> float @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let chpr = foreign "cblas_chpr" (int @-> int @-> int @-> float @-> ptr void @-> int @-> ptr void @-> returning void)

let cher2 = foreign "cblas_cher2" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let chpr2 = foreign "cblas_chpr2" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> returning void)

let zhemv = foreign "cblas_zhemv" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let zhbmv = foreign "cblas_zhbmv" (int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let zhpmv = foreign "cblas_zhpmv" (int @-> int @-> int @-> ptr void @-> ptr void @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let zgeru = foreign "cblas_zgeru" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let zgerc = foreign "cblas_zgerc" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let zher = foreign "cblas_zher" (int @-> int @-> int @-> double @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let zhpr = foreign "cblas_zhpr" (int @-> int @-> int @-> double @-> ptr void @-> int @-> ptr void @-> returning void)

let zher2 = foreign "cblas_zher2" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let zhpr2 = foreign "cblas_zhpr2" (int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> returning void)

let sgemm = foreign "cblas_sgemm" (int @-> int @-> int @-> int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> float @-> ptr float @-> int @-> returning void)

let ssymm = foreign "cblas_ssymm" (int @-> int @-> int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> float @-> ptr float @-> int @-> returning void)

let ssyrk = foreign "cblas_ssyrk" (int @-> int @-> int @-> int @-> int @-> float @-> ptr float @-> int @-> float @-> ptr float @-> int @-> returning void)

let ssyr2k = foreign "cblas_ssyr2k" (int @-> int @-> int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> float @-> ptr float @-> int @-> returning void)

let strmm = foreign "cblas_strmm" (int @-> int @-> int @-> int @-> int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let strsm = foreign "cblas_strsm" (int @-> int @-> int @-> int @-> int @-> int @-> int @-> float @-> ptr float @-> int @-> ptr float @-> int @-> returning void)

let dgemm = foreign "cblas_dgemm" (int @-> int @-> int @-> int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> double @-> ptr double @-> int @-> returning void)

let dsymm = foreign "cblas_dsymm" (int @-> int @-> int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> double @-> ptr double @-> int @-> returning void)

let dsyrk = foreign "cblas_dsyrk" (int @-> int @-> int @-> int @-> int @-> double @-> ptr double @-> int @-> double @-> ptr double @-> int @-> returning void)

let dsyr2k = foreign "cblas_dsyr2k" (int @-> int @-> int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> double @-> ptr double @-> int @-> returning void)

let dtrmm = foreign "cblas_dtrmm" (int @-> int @-> int @-> int @-> int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let dtrsm = foreign "cblas_dtrsm" (int @-> int @-> int @-> int @-> int @-> int @-> int @-> double @-> ptr double @-> int @-> ptr double @-> int @-> returning void)

let cgemm = foreign "cblas_cgemm" (int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let csymm = foreign "cblas_csymm" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let csyrk = foreign "cblas_csyrk" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let csyr2k = foreign "cblas_csyr2k" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let ctrmm = foreign "cblas_ctrmm" (int @-> int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ctrsm = foreign "cblas_ctrsm" (int @-> int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let zgemm = foreign "cblas_zgemm" (int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let zsymm = foreign "cblas_zsymm" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let zsyrk = foreign "cblas_zsyrk" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let zsyr2k = foreign "cblas_zsyr2k" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let ztrmm = foreign "cblas_ztrmm" (int @-> int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let ztrsm = foreign "cblas_ztrsm" (int @-> int @-> int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> returning void)

let chemm = foreign "cblas_chemm" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let cherk = foreign "cblas_cherk" (int @-> int @-> int @-> int @-> int @-> float @-> ptr void @-> int @-> float @-> ptr void @-> int @-> returning void)

let cher2k = foreign "cblas_cher2k" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> float @-> ptr void @-> int @-> returning void)

let zhemm = foreign "cblas_zhemm" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> ptr void @-> ptr void @-> int @-> returning void)

let zherk = foreign "cblas_zherk" (int @-> int @-> int @-> int @-> int @-> double @-> ptr void @-> int @-> double @-> ptr void @-> int @-> returning void)

let zher2k = foreign "cblas_zher2k" (int @-> int @-> int @-> int @-> int @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @-> double @-> ptr void @-> int @-> returning void)

(* Unit tests *)
(* The tests are here because this module is private and cannot be tested outside *)
(* Following tests only check the functions are linked correctly or not. They don't make sure the correctness of the linked library. *)
open Bigarray

let%test "sdsdot" = 
  let x = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float32 c_layout [|0.3; 0.4|] in
  Alcotest.(check (float 0.0001)) "result" 1.11 (sdsdot 2 1.0 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1)

let%test "dsdot" = 
  let x = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float32 c_layout [|0.3; 0.4|] in
  Alcotest.(check (float 0.0001)) "result" 0.11 (dsdot 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1)

let%test "sdot" = 
  let x = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float32 c_layout [|0.3; 0.4|] in
  Alcotest.(check (float 0.0001)) "result" 0.11 (sdot 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1)

let%test "ddot" = 
  let x = Array1.of_array float64 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float64 c_layout [|0.3; 0.4|] in
  Alcotest.(check (float 0.0001)) "result" 0.11 (ddot 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1)

let%test "cdotu_sub" = 
  let x = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let y = Array1.of_array float32 c_layout [|5.0; 6.0; 7.0; 8.0|] in
  let result = Array1.of_array float32 c_layout [|0.0; 0.0|] in
  let _ = cdotu_sub 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 y)) 1 
    (to_voidp (bigarray_start array1 result)) in
  Alcotest.(check (float 0.0001)) "result[0]" (-18.0) (Array1.unsafe_get result 0);
  Alcotest.(check (float 0.0001)) "result[1]" 68.0 (Array1.unsafe_get result 1)

let%test "cdotc_sub" = 
  let x = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let y = Array1.of_array float32 c_layout [|5.0; 6.0; 7.0; 8.0|] in
  let result = Array1.of_array float32 c_layout [|0.0; 0.0|] in
  let _ = cdotc_sub 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 y)) 1 
    (to_voidp (bigarray_start array1 result)) in
  Alcotest.(check (float 0.0001)) "result[0]" 70.0 (Array1.unsafe_get result 0);
  Alcotest.(check (float 0.0001)) "result[1]" (-8.0) (Array1.unsafe_get result 1)

let%test "zdotu_sub" = 
  let x = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let y = Array1.of_array float64 c_layout [|5.0; 6.0; 7.0; 8.0|] in
  let result = Array1.of_array float64 c_layout [|0.0; 0.0|] in
  let _ = zdotu_sub 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 y)) 1 
    (to_voidp (bigarray_start array1 result)) in
  Alcotest.(check (float 0.0001)) "result[0]" (-18.0) (Array1.unsafe_get result 0);
  Alcotest.(check (float 0.0001)) "result[1]" 68.0 (Array1.unsafe_get result 1)

let%test "zdotc_sub" = 
  let x = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let y = Array1.of_array float64 c_layout [|5.0; 6.0; 7.0; 8.0|] in
  let result = Array1.of_array float64 c_layout [|0.0; 0.0|] in
  let _ = zdotc_sub 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 y)) 1 
    (to_voidp (bigarray_start array1 result)) in
  Alcotest.(check (float 0.0001)) "result[0]" 70.0 (Array1.unsafe_get result 0);
  Alcotest.(check (float 0.0001)) "result[1]" (-8.0) (Array1.unsafe_get result 1)
  
let%test "snrm2" = 
  let x = Array1.of_array float32 c_layout [|0.3; 0.4|] in
  Alcotest.(check (float 0.0001)) "result" 0.5 (snrm2 2 (bigarray_start array1 x) 1)

let%test "sasum" = 
  let x = Array1.of_array float32 c_layout [|0.3; 0.4|] in
  Alcotest.(check (float 0.0001)) "result" 0.7 (sasum 2 (bigarray_start array1 x) 1)

let%test "dnrm2" = 
  let x = Array1.of_array float64 c_layout [|0.3; 0.4|] in
  Alcotest.(check (float 0.0001)) "result" 0.5 (dnrm2 2 (bigarray_start array1 x) 1)

let%test "dasum" = 
  let x = Array1.of_array float64 c_layout [|0.3; 0.4|] in
  Alcotest.(check (float 0.0001)) "result" 0.7 (dasum 2 (bigarray_start array1 x) 1)

let%test "scnrm2" = 
  let x = Array1.of_array float32 c_layout [|0.1; 0.1; 0.1; 0.1|] in
  Alcotest.(check (float 0.0001)) "result" 0.2 (scnrm2 2 (to_voidp (bigarray_start array1 x)) 1)

let%test "scasum" = 
  let x = Array1.of_array float32 c_layout [|0.1; 0.1; 0.1; 0.1|] in
  Alcotest.(check (float 0.0001)) "result" 0.4 (scasum 2 (to_voidp (bigarray_start array1 x)) 1)

let%test "dznrm2" = 
  let x = Array1.of_array float64 c_layout [|0.1; 0.1; 0.1; 0.1|] in
  Alcotest.(check (float 0.0001)) "result" 0.2 (dznrm2 2 (to_voidp (bigarray_start array1 x)) 1)

let%test "dzasum" = 
  let x = Array1.of_array float64 c_layout [|0.1; 0.1; 0.1; 0.1|] in
  Alcotest.(check (float 0.0001)) "result" 0.4 (dzasum 2 (to_voidp (bigarray_start array1 x)) 1)

let%test "isamax" =
  let x = Array1.of_array float32 c_layout [|0.1; 0.2; 0.3; 0.4|] in
  Alcotest.(check int) "result" 3 (Unsigned.Size_t.to_int (isamax 4 (bigarray_start array1 x) 1))

let%test "idamax" =
  let x = Array1.of_array float64 c_layout [|0.1; 0.2; 0.3; 0.4|] in
  Alcotest.(check int) "result" 3 (Unsigned.Size_t.to_int (idamax 4 (bigarray_start array1 x) 1))
  
let%test "icamax" =
  let x = Array1.of_array float32 c_layout [|0.1; 0.1; 0.1; 0.2|] in
  Alcotest.(check int) "result" 1 (Unsigned.Size_t.to_int (icamax 2 (to_voidp (bigarray_start array1 x)) 1))
  
let%test "izamax" =
  let x = Array1.of_array float64 c_layout [|0.1; 0.1; 0.1; 0.2|] in
  Alcotest.(check int) "result" 1 (Unsigned.Size_t.to_int (izamax 2 (to_voidp (bigarray_start array1 x)) 1))

let%test "sswap" =
  let x = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float32 c_layout [|0.3; 0.4|] in
  let _ = sswap 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.4 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "y[0]" 0.1 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.2 (Array1.unsafe_get y 1)
  
let%test "scopy" =
  let x = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float32 c_layout [|0.0; 0.0|] in
  let _ = scopy 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.1 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.2 (Array1.unsafe_get y 1)
  
let%test "saxpy" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let _ = saxpy 2 0.5 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 1.5 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 3.0 (Array1.unsafe_get y 1)

let%test "dswap" =
  let x = Array1.of_array float64 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float64 c_layout [|0.3; 0.4|] in
  let _ = dswap 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[0]" 0.4 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "y[0]" 0.1 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.2 (Array1.unsafe_get y 1)

let%test "dcopy" =
  let x = Array1.of_array float64 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float64 c_layout [|0.0; 0.0|] in
  let _ = dcopy 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.1 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.2 (Array1.unsafe_get y 1)
  
let%test "daxpy" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let _ = daxpy 2 0.5 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 1.5 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 3.0 (Array1.unsafe_get y 1)

let%test "cswap" =
  let x = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float32 c_layout [|0.3; 0.4|] in
  let _ = cswap 1 (to_voidp (bigarray_start array1 x)) 1 (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.4 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "y[0]" 0.1 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.2 (Array1.unsafe_get y 1)
  
let%test "ccopy" =
  let x = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float32 c_layout [|0.0; 0.0|] in
  let _ = ccopy 1 (to_voidp (bigarray_start array1 x)) 1 (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.1 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.2 (Array1.unsafe_get y 1)
  
let%test "caxpy" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.5|] in
  let _ = caxpy 1 (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 x)) 1 (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.5 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 3.5 (Array1.unsafe_get y 1)

let%test "zswap" =
  let x = Array1.of_array float64 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float64 c_layout [|0.3; 0.4|] in
  let _ = zswap 1 (to_voidp (bigarray_start array1 x)) 1 (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[0]" 0.4 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "y[0]" 0.1 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.2 (Array1.unsafe_get y 1)
  
let%test "zcopy" =
  let x = Array1.of_array float64 c_layout [|0.1; 0.2|] in
  let y = Array1.of_array float64 c_layout [|0.0; 0.0|] in
  let _ = zcopy 1 (to_voidp (bigarray_start array1 x)) 1 (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.1 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.2 (Array1.unsafe_get y 1)
  
let%test "zaxpy" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.5|] in
  let _ = zaxpy 1 (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 x)) 1 (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.5 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 3.5 (Array1.unsafe_get y 1)

let%test "srotg" =
  let a = allocate float 3.0 in
  let b = allocate float 4.0 in
  let c = allocate float 0.0 in
  let s = allocate float 0.0 in
  let _ = srotg a b c s in
  Alcotest.(check (float 0.0001)) "a" 5.0 !@a;
  Alcotest.(check (float 0.0001)) "cos" 0.6 !@c;
  Alcotest.(check (float 0.0001)) "sin" 0.8 !@s

let%test "srotmg" =
  let d1 = allocate float 1.0 in
  let d2 = allocate float 1.0 in
  let b1 = allocate float 3.0 in
  let b2 = 4.0 in
  let p = Array1.of_array float32 c_layout [|0.0; 0.0; 0.0; 0.0; 0.0|] in
  let _ = srotmg d1 d2 b1 b2 (bigarray_start array1 p) in
  Alcotest.(check (float 0.0001)) "d1" 0.64 !@d1;
  Alcotest.(check (float 0.0001)) "d2" 0.64 !@d2;
  Alcotest.(check (float 0.0001)) "b1" 6.25 !@b1
  
let%test "srot" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float32 c_layout [|3.0; 4.0|] in
  let c = 0.6 in
  let s = 0.8 in
  let _ = srot 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 c s in
  Alcotest.(check (float 0.0001)) "x[0]" 3.0 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 4.4 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "y[0]" 1.0 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.8 (Array1.unsafe_get y 1)
  
let%test "srotm" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float32 c_layout [|3.0; 4.0|] in
  let p = Array1.of_array float32 c_layout [|1.0; 0.75; 0.0; 0.0; 0.75|] in
  let _ = srotm 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 (bigarray_start array1 p) in
  Alcotest.(check (float 0.0001)) "x[0]" 3.75 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 5.5 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "y[0]" 1.25 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 1.0 (Array1.unsafe_get y 1)

let%test "drotg" =
  let a = allocate double 3.0 in
  let b = allocate double 4.0 in
  let c = allocate double 0.0 in
  let s = allocate double 0.0 in
  let _ = drotg a b c s in
  Alcotest.(check (float 0.0001)) "a" 5.0 !@a;
  Alcotest.(check (float 0.0001)) "cos" 0.6 !@c;
  Alcotest.(check (float 0.0001)) "sin" 0.8 !@s

let%test "drotmg" =
  let d1 = allocate double 1.0 in
  let d2 = allocate double 1.0 in
  let b1 = allocate double 3.0 in
  let b2 = 4.0 in
  let p = Array1.of_array float64 c_layout [|0.0; 0.0; 0.0; 0.0; 0.0|] in
  let _ = drotmg d1 d2 b1 b2 (bigarray_start array1 p) in
  Alcotest.(check (float 0.0001)) "d1" 0.64 !@d1;
  Alcotest.(check (float 0.0001)) "d2" 0.64 !@d2;
  Alcotest.(check (float 0.0001)) "b1" 6.25 !@b1
  
let%test "drot" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float64 c_layout [|3.0; 4.0|] in
  let _ = drot 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 0.6 0.8 in
  Alcotest.(check (float 0.0001)) "x[0]" 3.0 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 4.4 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "y[0]" 1.0 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.8 (Array1.unsafe_get y 1)
  
let%test "drotm" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float64 c_layout [|3.0; 4.0|] in
  let p = Array1.of_array float64 c_layout [|1.0; 0.75; 0.0; 0.0; 0.75|] in
  let _ = drotm 2 (bigarray_start array1 x) 1 (bigarray_start array1 y) 1 (bigarray_start array1 p) in
  Alcotest.(check (float 0.0001)) "x[0]" 3.75 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 5.5 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "y[0]" 1.25 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 1.0 (Array1.unsafe_get y 1)

let%test "sscal" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let _ = sscal 2 0.5 (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.5 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 1.0 (Array1.unsafe_get x 1)

let%test "dscal" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let _ = dscal 2 0.5 (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.5 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 1.0 (Array1.unsafe_get x 1)

let%test "cscal" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let _ = cscal 2 (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" (-0.3) (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.4 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" (-0.5) (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 1.0 (Array1.unsafe_get x 3)


let%test "zscal" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.1; 0.2|] in
  let _ = zscal 2 (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" (-0.3) (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.4 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" (-0.5) (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 1.0 (Array1.unsafe_get x 3)

let%test "csscal" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let _ = csscal 2 0.5 (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.5 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 1.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 1.5 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 2.0 (Array1.unsafe_get x 3)

let%test "zdscal" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let _ = zdscal 2 0.5 (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.5 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 1.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 1.5 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 2.0 (Array1.unsafe_get x 3)

let%test "sgemv" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.1|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.5|] in
  let _ = sgemv (int_of_order RowMajor) (int_of_transpose NoTrans) 
    2 2 0.5 
    (bigarray_start array1 a) 2 
    (bigarray_start array1 x) 1 
    1.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.65 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.85 (Array1.unsafe_get y 1)

let%test "sgbmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 4.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.1|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.5|] in
  let _ = sgbmv (int_of_order RowMajor) (int_of_transpose NoTrans) 
    2 2 0 0 0.5 
    (bigarray_start array1 a) 1 
    (bigarray_start array1 x) 1 
    1.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.55 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.7 (Array1.unsafe_get y 1)

let%test "strmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 0.0; 3.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.1|] in
  let _ = strmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (bigarray_start array1 a) 2 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.3 (Array1.unsafe_get x 1)

let%test "stbmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 3.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.1|] in
  let _ = stbmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 0 (bigarray_start array1 a) 1 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.3 (Array1.unsafe_get x 1)

let%test "stpmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.1|] in
  let _ = stpmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (bigarray_start array1 a) 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.3 (Array1.unsafe_get x 1)

let%test "strsv" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 0.0; 3.0|] in
  let x = Array1.of_array float32 c_layout [|0.3; 0.3|] in
  let _ = strsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (bigarray_start array1 a) 2 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.1 (Array1.unsafe_get x 1)

let%test "stbsv" =
  let a = Array1.of_array float32 c_layout [|1.0; 3.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.3|] in
  let _ = stbsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 0 (bigarray_start array1 a) 1 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.1 (Array1.unsafe_get x 1)

let%test "stpsv" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0|] in
  let x = Array1.of_array float32 c_layout [|0.3; 0.3|] in
  let _ = stpsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (bigarray_start array1 a) 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.1 (Array1.unsafe_get x 1)

let%test "dgemv" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.1|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.5|] in
  let _ = dgemv (int_of_order RowMajor) (int_of_transpose NoTrans) 
    2 2 0.5 
    (bigarray_start array1 a) 2 
    (bigarray_start array1 x) 1 
    1.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.65 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.85 (Array1.unsafe_get y 1)

let%test "dgbmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 4.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.1|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.5|] in
  let _ = dgbmv (int_of_order RowMajor) (int_of_transpose NoTrans) 
    2 2 0 0 0.5 
    (bigarray_start array1 a) 1 
    (bigarray_start array1 x) 1 
    1.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.55 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.7 (Array1.unsafe_get y 1)

let%test "dtrmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 0.0; 3.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.1|] in
  let _ = dtrmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (bigarray_start array1 a) 2 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.3 (Array1.unsafe_get x 1)

let%test "dtbmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 3.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.1|] in
  let _ = dtbmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 0 (bigarray_start array1 a) 1 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.3 (Array1.unsafe_get x 1)

let%test "dtpmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.1|] in
  let _ = dtpmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (bigarray_start array1 a) 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.3 (Array1.unsafe_get x 1)

let%test "dtrsv" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 0.0; 3.0|] in
  let x = Array1.of_array float64 c_layout [|0.3; 0.3|] in
  let _ = dtrsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (bigarray_start array1 a) 2 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.1 (Array1.unsafe_get x 1)

let%test "dtbsv" =
  let a = Array1.of_array float64 c_layout [|1.0; 3.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.3|] in
  let _ = dtbsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 0 (bigarray_start array1 a) 1 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.1 (Array1.unsafe_get x 1)

let%test "dtpsv" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0|] in
  let x = Array1.of_array float64 c_layout [|0.3; 0.3|] in
  let _ = dtpsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (bigarray_start array1 a) 
    (bigarray_start array1 x) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.1 (Array1.unsafe_get x 1)

let%test "cgemv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|1.0; 0.0|] in
  let _ = cgemv (int_of_order RowMajor) (int_of_transpose NoTrans) 
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta)) 
    (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.65 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.0 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.85 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" 0.0 (Array1.unsafe_get y 3)

let%test "cgbmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 4.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|1.0; 0.0|] in
  let _ = cgbmv (int_of_order RowMajor) (int_of_transpose NoTrans) 
    2 2 0 0 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 1 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta))
    (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.55 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.0 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.7 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" 0.0 (Array1.unsafe_get y 3)

let%test "ctrmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let _ = ctrmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.3 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ctbmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let _ = ctbmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 0 (to_voidp (bigarray_start array1 a)) 1 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.3 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ctpmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let _ = ctpmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (to_voidp (bigarray_start array1 a)) 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.3 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ctrsv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.3; 0.0; 0.3; 0.0|] in
  let _ = ctrsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.1 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ctbsv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.3; 0.0|] in
  let _ = ctbsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 0 (to_voidp (bigarray_start array1 a)) 1 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.1 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ctpsv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.3; 0.0; 0.3; 0.0|] in
  let _ = ctpsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (to_voidp (bigarray_start array1 a)) 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.1 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "zgemv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|1.0; 0.0|] in
  let _ = zgemv (int_of_order RowMajor) (int_of_transpose NoTrans) 
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta)) 
    (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.65 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.0 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.85 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" 0.0 (Array1.unsafe_get y 3)

let%test "zgbmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 4.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|1.0; 0.0|] in
  let _ = zgbmv (int_of_order RowMajor) (int_of_transpose NoTrans) 
    2 2 0 0 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 1 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta))
    (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.55 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.0 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.7 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" 0.0 (Array1.unsafe_get y 3)

let%test "ztrmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let _ = ztrmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.3 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ztbmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let _ = ztbmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 0 (to_voidp (bigarray_start array1 a)) 1 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.3 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ztpmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let _ = ztpmv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (to_voidp (bigarray_start array1 a)) 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.3 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.3 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ztrsv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.3; 0.0; 0.3; 0.0|] in
  let _ = ztrsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.1 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ztbsv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.3; 0.0|] in
  let _ = ztbsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 0 (to_voidp (bigarray_start array1 a)) 1 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.1 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ztpsv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.3; 0.0; 0.3; 0.0|] in
  let _ = ztpsv (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 (to_voidp (bigarray_start array1 a)) 
    (to_voidp (bigarray_start array1 x)) 1 in
  Alcotest.(check (float 0.0001)) "x[0]" 0.1 (Array1.unsafe_get x 0);
  Alcotest.(check (float 0.0001)) "x[1]" 0.0 (Array1.unsafe_get x 1);
  Alcotest.(check (float 0.0001)) "x[2]" 0.1 (Array1.unsafe_get x 2);
  Alcotest.(check (float 0.0001)) "x[3]" 0.0 (Array1.unsafe_get x 3)

let%test "ssymv" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 0.0; 3.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.1|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.5|] in
  let _ = ssymv (int_of_order RowMajor) (int_of_uplo Upper)
    2 0.5 (bigarray_start array1 a) 2
    (bigarray_start array1 x) 1 
    1.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.65 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.75 (Array1.unsafe_get y 1)

let%test "ssbmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0; 4.0; 5.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.1; 0.1|] in
  let y = Array1.of_array float32 c_layout [|0.0; 0.0; 0.0|] in
  let _ = ssbmv (int_of_order RowMajor) (int_of_uplo Upper)
    3 1 0.5 (bigarray_start array1 a) 2
    (bigarray_start array1 x) 1 
    0.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.15 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.45 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.45 (Array1.unsafe_get y 2)

let%test "sspmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.1|] in
  let y = Array1.of_array float32 c_layout [|0.0; 0.0|] in
  let _ = sspmv (int_of_order RowMajor) (int_of_uplo Upper)
    2 0.5 (bigarray_start array1 a)
    (bigarray_start array1 x) 1 
    0.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.15 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.25 (Array1.unsafe_get y 1)

let%test "sger" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float32 c_layout [|3.0; 4.0|] in
  let a = Array1.of_array float32 c_layout [|0.1; 0.2; 0.3; 0.4|] in
  let _ = sger (int_of_order RowMajor) 2 2 0.5
    (bigarray_start array1 x) 1 
    (bigarray_start array1 y) 1 
    (bigarray_start array1 a) 2 in
  Alcotest.(check (float 0.0001)) "y[0]" 1.6 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 2.2 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[2]" 3.3 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "y[3]" 4.4 (Array1.unsafe_get a 3)

let%test "ssyr" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let a = Array1.of_array float32 c_layout [|0.1; 0.2; 0.0; 0.3|] in
  let _ = ssyr (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (bigarray_start array1 x) 1 
    (bigarray_start array1 a) 2 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 1.2 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[3]" 2.3 (Array1.unsafe_get a 3)

let%test "sspr" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let a = Array1.of_array float32 c_layout [|0.1; 0.2; 0.3|] in
  let _ = sspr (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (bigarray_start array1 x) 1 
    (bigarray_start array1 a) in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 1.2 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[2]" 2.3 (Array1.unsafe_get a 2)

let%test "ssyr2" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let a = Array1.of_array float32 c_layout [|0.1; 0.2; 0.0; 0.3|] in
  let _ = ssyr2 (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (bigarray_start array1 x) 1
    (bigarray_start array1 y) 1 
    (bigarray_start array1 a) 2 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.2 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.4 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[3]" 0.7 (Array1.unsafe_get a 3)

let%test "sspr2" =
  let x = Array1.of_array float32 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float32 c_layout [|0.1; 0.2|] in
  let a = Array1.of_array float32 c_layout [|0.1; 0.2; 0.3|] in
  let _ = sspr2 (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (bigarray_start array1 x) 1 
    (bigarray_start array1 y) 1 
    (bigarray_start array1 a) in
  Alcotest.(check (float 0.0001)) "y[0]" 0.2 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.4 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.7 (Array1.unsafe_get a 2)

let%test "dsymv" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 0.0; 3.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.1|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.5|] in
  let _ = dsymv (int_of_order RowMajor) (int_of_uplo Upper)
    2 0.5 (bigarray_start array1 a) 2
    (bigarray_start array1 x) 1 
    1.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.65 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.75 (Array1.unsafe_get y 1)

let%test "dsbmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0; 4.0; 5.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.1; 0.1|] in
  let y = Array1.of_array float64 c_layout [|0.0; 0.0; 0.0|] in
  let _ = dsbmv (int_of_order RowMajor) (int_of_uplo Upper)
    3 1 0.5 (bigarray_start array1 a) 2
    (bigarray_start array1 x) 1 
    0.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.15 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.45 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.45 (Array1.unsafe_get y 2)

let%test "dspmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.1|] in
  let y = Array1.of_array float64 c_layout [|0.0; 0.0|] in
  let _ = dspmv (int_of_order RowMajor) (int_of_uplo Upper)
    2 0.5 (bigarray_start array1 a)
    (bigarray_start array1 x) 1 
    0.0 (bigarray_start array1 y) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.15 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.25 (Array1.unsafe_get y 1)

let%test "dger" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float64 c_layout [|3.0; 4.0|] in
  let a = Array1.of_array float64 c_layout [|0.1; 0.2; 0.3; 0.4|] in
  let _ = dger (int_of_order RowMajor) 2 2 0.5
    (bigarray_start array1 x) 1 
    (bigarray_start array1 y) 1 
    (bigarray_start array1 a) 2 in
  Alcotest.(check (float 0.0001)) "y[0]" 1.6 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 2.2 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[2]" 3.3 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "y[3]" 4.4 (Array1.unsafe_get a 3)

let%test "dsyr" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let a = Array1.of_array float64 c_layout [|0.1; 0.2; 0.0; 0.3|] in
  let _ = dsyr (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (bigarray_start array1 x) 1 
    (bigarray_start array1 a) 2 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 1.2 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[3]" 2.3 (Array1.unsafe_get a 3)

let%test "dspr" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let a = Array1.of_array float64 c_layout [|0.1; 0.2; 0.3|] in
  let _ = dspr (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (bigarray_start array1 x) 1 
    (bigarray_start array1 a) in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 1.2 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[2]" 2.3 (Array1.unsafe_get a 2)

let%test "dsyr2" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float64 c_layout [|0.1; 0.2|] in
  let a = Array1.of_array float64 c_layout [|0.1; 0.2; 0.0; 0.3|] in
  let _ = dsyr2 (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (bigarray_start array1 x) 1
    (bigarray_start array1 y) 1 
    (bigarray_start array1 a) 2 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.2 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.4 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[3]" 0.7 (Array1.unsafe_get a 3)

let%test "dspr2" =
  let x = Array1.of_array float64 c_layout [|1.0; 2.0|] in
  let y = Array1.of_array float64 c_layout [|0.1; 0.2|] in
  let a = Array1.of_array float64 c_layout [|0.1; 0.2; 0.3|] in
  let _ = dspr2 (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (bigarray_start array1 x) 1 
    (bigarray_start array1 y) 1 
    (bigarray_start array1 a) in
  Alcotest.(check (float 0.0001)) "y[0]" 0.2 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.4 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.7 (Array1.unsafe_get a 2)

let%test "chemv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 1.0; 2.0; 0.0; 0.0; 2.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|1.0; 0.0|] in
  let _ = chemv (int_of_order RowMajor) (int_of_uplo Upper) 2
    (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.1 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.65 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" (-0.1) (Array1.unsafe_get y 3)

let%test "chbmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 1.0; 1.0; 2.0; 0.0; 1.0; 2.0; 3.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|1.0; 0.0|] in
  let _ = chbmv (int_of_order RowMajor) (int_of_uplo Upper) 3 1
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta)) 
    (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.05 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.7 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" 0.05 (Array1.unsafe_get y 3);
  Alcotest.(check (float 0.0001)) "y[4]" 0.7 (Array1.unsafe_get y 4);
  Alcotest.(check (float 0.0001)) "y[5]" (-0.1) (Array1.unsafe_get y 5)

let%test "chpmv" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 1.0; 2.0; 2.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|1.0; 0.0|] in
  let _ = chpmv (int_of_order RowMajor) (int_of_uplo Upper) 2
    (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 a))
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.1 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.65 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" (-0.1) (Array1.unsafe_get y 3)

let%test "cgeru" =
  let a = Array1.of_array float32 c_layout [|0.25; 0.0; 0.25; 0.0; 0.25; 0.0; 0.25; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float32 c_layout [|5.0; 0.0; 5.0; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = cgeru (int_of_order RowMajor) 2 2
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 y)) 1
    (to_voidp (bigarray_start array1 a)) 2  in
  Alcotest.(check (float 0.0001)) "a[0]" 0.5 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 0.5 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 0.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[4]" 0.5 (Array1.unsafe_get a 4);
  Alcotest.(check (float 0.0001)) "a[5]" 0.0 (Array1.unsafe_get a 5);
  Alcotest.(check (float 0.0001)) "a[6]" 0.5 (Array1.unsafe_get a 6);
  Alcotest.(check (float 0.0001)) "a[7]" 0.0 (Array1.unsafe_get a 7)

let%test "cgerc" =
  let a = Array1.of_array float32 c_layout [|0.25; 0.0; 0.25; 0.0; 0.25; 0.0; 0.25; 0.0|] in
  let x = Array1.of_array float32 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float32 c_layout [|5.0; 0.0; 5.0; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = cgerc (int_of_order RowMajor) 2 2
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 y)) 1
    (to_voidp (bigarray_start array1 a)) 2  in
  Alcotest.(check (float 0.0001)) "a[0]" 0.5 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 0.5 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 0.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[4]" 0.5 (Array1.unsafe_get a 4);
  Alcotest.(check (float 0.0001)) "a[5]" 0.0 (Array1.unsafe_get a 5);
  Alcotest.(check (float 0.0001)) "a[6]" 0.5 (Array1.unsafe_get a 6);
  Alcotest.(check (float 0.0001)) "a[7]" 0.0 (Array1.unsafe_get a 7)

let%test "cher" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 1.0; 1.0; 0.0; 0.0; 1.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|2.0; 0.0; 2.0; 0.0|] in
  let _ = cher (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (to_voidp (bigarray_start array1 x)) 1
    (to_voidp (bigarray_start array1 a)) 2  in
  Alcotest.(check (float 0.0001)) "a[0]" 3.0 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 3.0 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 1.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[6]" 3.0 (Array1.unsafe_get a 6);
  Alcotest.(check (float 0.0001)) "a[7]" 0.0 (Array1.unsafe_get a 7)

let%test "chpr" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 1.0; 1.0; 1.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|2.0; 0.0; 2.0; 0.0|] in
  let _ = chpr (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (to_voidp (bigarray_start array1 x)) 1
    (to_voidp (bigarray_start array1 a))  in
  Alcotest.(check (float 0.0001)) "a[0]" 3.0 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 3.0 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 1.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[4]" 3.0 (Array1.unsafe_get a 4);
  Alcotest.(check (float 0.0001)) "a[5]" 0.0 (Array1.unsafe_get a 5)

let%test "cher2" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 1.0; 1.0; 0.0; 0.0; 1.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|2.0; 0.0; 2.0; 0.0|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = cher2 (int_of_order RowMajor) (int_of_uplo Upper) 2
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 x)) 1
    (to_voidp (bigarray_start array1 y)) 1
    (to_voidp (bigarray_start array1 a)) 2  in
  Alcotest.(check (float 0.0001)) "a[0]" 2.0 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 2.0 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 1.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[6]" 2.0 (Array1.unsafe_get a 6);
  Alcotest.(check (float 0.0001)) "a[7]" 0.0 (Array1.unsafe_get a 7)

let%test "chpr2" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 1.0; 1.0; 1.0; 0.0|] in
  let x = Array1.of_array float32 c_layout [|2.0; 0.0; 2.0; 0.0|] in
  let y = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = chpr2 (int_of_order RowMajor) (int_of_uplo Upper) 2
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 x)) 1
    (to_voidp (bigarray_start array1 y)) 1
    (to_voidp (bigarray_start array1 a))  in
  Alcotest.(check (float 0.0001)) "a[0]" 2.0 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 2.0 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 1.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[4]" 2.0 (Array1.unsafe_get a 4);
  Alcotest.(check (float 0.0001)) "a[5]" 0.0 (Array1.unsafe_get a 5)

let%test "zhemv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 1.0; 2.0; 0.0; 0.0; 2.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|1.0; 0.0|] in
  let _ = zhemv (int_of_order RowMajor) (int_of_uplo Upper) 2
    (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.1 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.65 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" (-0.1) (Array1.unsafe_get y 3)

let%test "zhbmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 1.0; 1.0; 2.0; 0.0; 1.0; 2.0; 3.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|1.0; 0.0|] in
  let _ = zhbmv (int_of_order RowMajor) (int_of_uplo Upper) 3 1
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta)) 
    (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.05 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.7 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" 0.05 (Array1.unsafe_get y 3);
  Alcotest.(check (float 0.0001)) "y[4]" 0.7 (Array1.unsafe_get y 4);
  Alcotest.(check (float 0.0001)) "y[5]" (-0.1) (Array1.unsafe_get y 5)

let%test "zhpmv" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 1.0; 2.0; 2.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|1.0; 0.0|] in
  let _ = zhpmv (int_of_order RowMajor) (int_of_uplo Upper) 2
    (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 a))
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 y)) 1 in
  Alcotest.(check (float 0.0001)) "y[0]" 0.6 (Array1.unsafe_get y 0);
  Alcotest.(check (float 0.0001)) "y[1]" 0.1 (Array1.unsafe_get y 1);
  Alcotest.(check (float 0.0001)) "y[2]" 0.65 (Array1.unsafe_get y 2);
  Alcotest.(check (float 0.0001)) "y[3]" (-0.1) (Array1.unsafe_get y 3)

let%test "zgeru" =
  let a = Array1.of_array float64 c_layout [|0.25; 0.0; 0.25; 0.0; 0.25; 0.0; 0.25; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float64 c_layout [|5.0; 0.0; 5.0; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zgeru (int_of_order RowMajor) 2 2
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 y)) 1
    (to_voidp (bigarray_start array1 a)) 2  in
  Alcotest.(check (float 0.0001)) "a[0]" 0.5 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 0.5 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 0.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[4]" 0.5 (Array1.unsafe_get a 4);
  Alcotest.(check (float 0.0001)) "a[5]" 0.0 (Array1.unsafe_get a 5);
  Alcotest.(check (float 0.0001)) "a[6]" 0.5 (Array1.unsafe_get a 6);
  Alcotest.(check (float 0.0001)) "a[7]" 0.0 (Array1.unsafe_get a 7)

let%test "zgerc" =
  let a = Array1.of_array float64 c_layout [|0.25; 0.0; 0.25; 0.0; 0.25; 0.0; 0.25; 0.0|] in
  let x = Array1.of_array float64 c_layout [|0.1; 0.0; 0.1; 0.0|] in
  let y = Array1.of_array float64 c_layout [|5.0; 0.0; 5.0; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zgerc (int_of_order RowMajor) 2 2
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 x)) 1 
    (to_voidp (bigarray_start array1 y)) 1
    (to_voidp (bigarray_start array1 a)) 2  in
  Alcotest.(check (float 0.0001)) "a[0]" 0.5 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 0.5 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 0.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[4]" 0.5 (Array1.unsafe_get a 4);
  Alcotest.(check (float 0.0001)) "a[5]" 0.0 (Array1.unsafe_get a 5);
  Alcotest.(check (float 0.0001)) "a[6]" 0.5 (Array1.unsafe_get a 6);
  Alcotest.(check (float 0.0001)) "a[7]" 0.0 (Array1.unsafe_get a 7)

let%test "zher" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 1.0; 1.0; 0.0; 0.0; 1.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|2.0; 0.0; 2.0; 0.0|] in
  let _ = zher (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (to_voidp (bigarray_start array1 x)) 1
    (to_voidp (bigarray_start array1 a)) 2  in
  Alcotest.(check (float 0.0001)) "a[0]" 3.0 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 3.0 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 1.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[6]" 3.0 (Array1.unsafe_get a 6);
  Alcotest.(check (float 0.0001)) "a[7]" 0.0 (Array1.unsafe_get a 7)

let%test "zhpr" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 1.0; 1.0; 1.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|2.0; 0.0; 2.0; 0.0|] in
  let _ = zhpr (int_of_order RowMajor) (int_of_uplo Upper) 2 0.5
    (to_voidp (bigarray_start array1 x)) 1
    (to_voidp (bigarray_start array1 a))  in
  Alcotest.(check (float 0.0001)) "a[0]" 3.0 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 3.0 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 1.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[4]" 3.0 (Array1.unsafe_get a 4);
  Alcotest.(check (float 0.0001)) "a[5]" 0.0 (Array1.unsafe_get a 5)

let%test "zher2" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 1.0; 1.0; 0.0; 0.0; 1.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|2.0; 0.0; 2.0; 0.0|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zher2 (int_of_order RowMajor) (int_of_uplo Upper) 2
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 x)) 1
    (to_voidp (bigarray_start array1 y)) 1
    (to_voidp (bigarray_start array1 a)) 2  in
  Alcotest.(check (float 0.0001)) "a[0]" 2.0 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 2.0 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 1.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[6]" 2.0 (Array1.unsafe_get a 6);
  Alcotest.(check (float 0.0001)) "a[7]" 0.0 (Array1.unsafe_get a 7)

let%test "zhpr2" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 1.0; 1.0; 1.0; 0.0|] in
  let x = Array1.of_array float64 c_layout [|2.0; 0.0; 2.0; 0.0|] in
  let y = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zhpr2 (int_of_order RowMajor) (int_of_uplo Upper) 2
    (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 x)) 1
    (to_voidp (bigarray_start array1 y)) 1
    (to_voidp (bigarray_start array1 a))  in
  Alcotest.(check (float 0.0001)) "a[0]" 2.0 (Array1.unsafe_get a 0);
  Alcotest.(check (float 0.0001)) "a[1]" 0.0 (Array1.unsafe_get a 1);
  Alcotest.(check (float 0.0001)) "a[2]" 2.0 (Array1.unsafe_get a 2);
  Alcotest.(check (float 0.0001)) "a[3]" 1.0 (Array1.unsafe_get a 3);
  Alcotest.(check (float 0.0001)) "a[4]" 2.0 (Array1.unsafe_get a 4);
  Alcotest.(check (float 0.0001)) "a[5]" 0.0 (Array1.unsafe_get a 5)

let%test "sgemm" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.5; 0.5; 0.5|] in
  let c = Array1.of_array float32 c_layout [|2.0; 4.0; 6.0; 8.0|] in
  let _ = sgemm (int_of_order RowMajor) (int_of_transpose NoTrans) (int_of_transpose NoTrans)
    2 2 2 0.5
    (bigarray_start array1 a) 2 
    (bigarray_start array1 b) 2 
    0.5 (bigarray_start array1 c) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 2.75 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[2]" 4.75 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 5.75 (Array1.unsafe_get c 3)

let%test "ssymm" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 0.0; 4.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.5; 0.5; 0.5|] in
  let c = Array1.of_array float32 c_layout [|2.0; 4.0; 6.0; 8.0|] in
  let _ = ssymm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper)
    2 2 0.5
    (bigarray_start array1 a) 2 
    (bigarray_start array1 b) 2 
    0.5 (bigarray_start array1 c) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 2.75 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[2]" 4.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 5.5 (Array1.unsafe_get c 3)

let%test "ssyrk" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let c = Array1.of_array float32 c_layout [|2.0; 4.0; 0.0; 8.0|] in
  let _ = ssyrk (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 0.5
    (bigarray_start array1 a) 2 
    0.5 (bigarray_start array1 c) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 3.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 7.5 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[3]" 16.5 (Array1.unsafe_get c 3)

let%test "ssyr2k" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.5; 0.5; 0.5|] in
  let c = Array1.of_array float32 c_layout [|2.0; 4.0; 0.0; 8.0|] in
  let _ = ssyr2k (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 0.5
    (bigarray_start array1 a) 2
    (bigarray_start array1 b) 2 
    0.5 (bigarray_start array1 c) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 2.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 4.5 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[3]" 7.5 (Array1.unsafe_get c 3)

let%test "strmm" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 0.0; 4.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.5; 0.5; 0.5|] in
  let _ = strmm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 2 2.0
    (bigarray_start array1 a) 2 
    (bigarray_start array1 b) 2 in
  Alcotest.(check (float 0.0001)) "b[0]" 3.0 (Array1.unsafe_get b 0);
  Alcotest.(check (float 0.0001)) "b[1]" 3.0 (Array1.unsafe_get b 1);
  Alcotest.(check (float 0.0001)) "b[2]" 4.0 (Array1.unsafe_get b 2);
  Alcotest.(check (float 0.0001)) "b[3]" 4.0 (Array1.unsafe_get b 3)

let%test "strsm" =
  let a = Array1.of_array float32 c_layout [|1.0; 2.0; 0.0; 2.0|] in
  let b = Array1.of_array float32 c_layout [|3.0; 1.0|] in
  let _ = strsm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 1 0.5
    (bigarray_start array1 a) 2 
    (bigarray_start array1 b) 1 in
  Alcotest.(check (float 0.0001)) "b[0]" 1.0 (Array1.unsafe_get b 0);
  Alcotest.(check (float 0.0001)) "b[1]" 0.25 (Array1.unsafe_get b 1)

let%test "dgemm" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.5; 0.5; 0.5|] in
  let c = Array1.of_array float64 c_layout [|2.0; 4.0; 6.0; 8.0|] in
  let _ = dgemm (int_of_order RowMajor) (int_of_transpose NoTrans) (int_of_transpose NoTrans)
    2 2 2 0.5
    (bigarray_start array1 a) 2 
    (bigarray_start array1 b) 2 
    0.5 (bigarray_start array1 c) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 2.75 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[2]" 4.75 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 5.75 (Array1.unsafe_get c 3)

let%test "dsymm" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 0.0; 4.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.5; 0.5; 0.5|] in
  let c = Array1.of_array float64 c_layout [|2.0; 4.0; 6.0; 8.0|] in
  let _ = dsymm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper)
    2 2 0.5
    (bigarray_start array1 a) 2 
    (bigarray_start array1 b) 2 
    0.5 (bigarray_start array1 c) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 2.75 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[2]" 4.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 5.5 (Array1.unsafe_get c 3)

let%test "dsyrk" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let c = Array1.of_array float64 c_layout [|2.0; 4.0; 0.0; 8.0|] in
  let _ = dsyrk (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 0.5
    (bigarray_start array1 a) 2 
    0.5 (bigarray_start array1 c) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 3.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 7.5 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[3]" 16.5 (Array1.unsafe_get c 3)

let%test "dsyr2k" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 3.0; 4.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.5; 0.5; 0.5|] in
  let c = Array1.of_array float64 c_layout [|2.0; 4.0; 0.0; 8.0|] in
  let _ = dsyr2k (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 0.5
    (bigarray_start array1 a) 2
    (bigarray_start array1 b) 2 
    0.5 (bigarray_start array1 c) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 2.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 4.5 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[3]" 7.5 (Array1.unsafe_get c 3)

let%test "dtrmm" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 0.0; 4.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.5; 0.5; 0.5|] in
  let _ = dtrmm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 2 2.0
    (bigarray_start array1 a) 2 
    (bigarray_start array1 b) 2 in
  Alcotest.(check (float 0.0001)) "b[0]" 3.0 (Array1.unsafe_get b 0);
  Alcotest.(check (float 0.0001)) "b[1]" 3.0 (Array1.unsafe_get b 1);
  Alcotest.(check (float 0.0001)) "b[2]" 4.0 (Array1.unsafe_get b 2);
  Alcotest.(check (float 0.0001)) "b[3]" 4.0 (Array1.unsafe_get b 3)

let%test "dtrsm" =
  let a = Array1.of_array float64 c_layout [|1.0; 2.0; 0.0; 2.0|] in
  let b = Array1.of_array float64 c_layout [|3.0; 1.0|] in
  let _ = dtrsm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 1 0.5
    (bigarray_start array1 a) 2 
    (bigarray_start array1 b) 1 in
  Alcotest.(check (float 0.0001)) "b[0]" 1.0 (Array1.unsafe_get b 0);
  Alcotest.(check (float 0.0001)) "b[1]" 0.25 (Array1.unsafe_get b 1)

let%test "cgemm" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float32 c_layout [|2.0; 0.0; 4.0; 0.0; 6.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = cgemm (int_of_order RowMajor) (int_of_transpose NoTrans) (int_of_transpose NoTrans)
    2 2 2
    (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 0.0 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[2]" 2.75 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 0.0 (Array1.unsafe_get c 3);
  Alcotest.(check (float 0.0001)) "c[4]" 4.75 (Array1.unsafe_get c 4);
  Alcotest.(check (float 0.0001)) "c[5]" 0.0 (Array1.unsafe_get c 5);
  Alcotest.(check (float 0.0001)) "c[6]" 5.75 (Array1.unsafe_get c 6);
  Alcotest.(check (float 0.0001)) "c[7]" 0.0 (Array1.unsafe_get c 7)

let%test "csymm" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float32 c_layout [|2.0; 0.0; 4.0; 0.0; 6.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = csymm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 2.75 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[4]" 4.5 (Array1.unsafe_get c 4);
  Alcotest.(check (float 0.0001)) "c[6]" 5.5 (Array1.unsafe_get c 6)

let%test "csyrk" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let c = Array1.of_array float32 c_layout [|2.0; 0.0; 4.0; 0.0; 0.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = csyrk (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 3.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 7.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[6]" 16.5 (Array1.unsafe_get c 6)

let%test "csyr2k" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float32 c_layout [|2.0; 0.0; 4.0; 0.0; 0.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = csyr2k (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2
    (to_voidp (bigarray_start array1 b)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 2.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 4.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[6]" 7.5 (Array1.unsafe_get c 6)

let%test "ctrmm" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|2.0; 0.0|] in
  let _ = ctrmm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 2 in
  Alcotest.(check (float 0.0001)) "b[0]" 3.0 (Array1.unsafe_get b 0);
  Alcotest.(check (float 0.0001)) "b[2]" 3.0 (Array1.unsafe_get b 2);
  Alcotest.(check (float 0.0001)) "b[4]" 4.0 (Array1.unsafe_get b 4);
  Alcotest.(check (float 0.0001)) "b[6]" 4.0 (Array1.unsafe_get b 6)

let%test "ctrsm" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 2.0; 0.0|] in
  let b = Array1.of_array float32 c_layout [|3.0; 0.0; 1.0; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = ctrsm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 1 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 1 in
  Alcotest.(check (float 0.0001)) "b[0]" 1.0 (Array1.unsafe_get b 0);
  Alcotest.(check (float 0.0001)) "b[2]" 0.25 (Array1.unsafe_get b 2)

let%test "zgemm" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float64 c_layout [|2.0; 0.0; 4.0; 0.0; 6.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zgemm (int_of_order RowMajor) (int_of_transpose NoTrans) (int_of_transpose NoTrans)
    2 2 2
    (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 0.0 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[2]" 2.75 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 0.0 (Array1.unsafe_get c 3);
  Alcotest.(check (float 0.0001)) "c[4]" 4.75 (Array1.unsafe_get c 4);
  Alcotest.(check (float 0.0001)) "c[5]" 0.0 (Array1.unsafe_get c 5);
  Alcotest.(check (float 0.0001)) "c[6]" 5.75 (Array1.unsafe_get c 6);
  Alcotest.(check (float 0.0001)) "c[7]" 0.0 (Array1.unsafe_get c 7)

let%test "zsymm" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float64 c_layout [|2.0; 0.0; 4.0; 0.0; 6.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zsymm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 2.75 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[4]" 4.5 (Array1.unsafe_get c 4);
  Alcotest.(check (float 0.0001)) "c[6]" 5.5 (Array1.unsafe_get c 6)

let%test "zsyrk" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let c = Array1.of_array float64 c_layout [|2.0; 0.0; 4.0; 0.0; 0.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zsyrk (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 3.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 7.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[6]" 16.5 (Array1.unsafe_get c 6)

let%test "zsyr2k" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float64 c_layout [|2.0; 0.0; 4.0; 0.0; 0.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zsyr2k (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2
    (to_voidp (bigarray_start array1 b)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 2.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 4.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[6]" 7.5 (Array1.unsafe_get c 6)

let%test "ztrmm" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|2.0; 0.0|] in
  let _ = ztrmm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 2 in
  Alcotest.(check (float 0.0001)) "b[0]" 3.0 (Array1.unsafe_get b 0);
  Alcotest.(check (float 0.0001)) "b[2]" 3.0 (Array1.unsafe_get b 2);
  Alcotest.(check (float 0.0001)) "b[4]" 4.0 (Array1.unsafe_get b 4);
  Alcotest.(check (float 0.0001)) "b[6]" 4.0 (Array1.unsafe_get b 6)

let%test "ztrsm" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 0.0; 0.0; 2.0; 0.0|] in
  let b = Array1.of_array float64 c_layout [|3.0; 0.0; 1.0; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = ztrsm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper) (int_of_transpose NoTrans) (int_of_diag NonUnit)
    2 1 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 1 in
  Alcotest.(check (float 0.0001)) "b[0]" 1.0 (Array1.unsafe_get b 0);
  Alcotest.(check (float 0.0001)) "b[2]" 0.25 (Array1.unsafe_get b 2)

let%test "chemm" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 1.0; 0.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float32 c_layout [|2.0; 0.0; 4.0; 0.0; 6.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = chemm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper)
    2 2
    (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 0.25 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[2]" 2.75 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 0.25 (Array1.unsafe_get c 3);
  Alcotest.(check (float 0.0001)) "c[4]" 4.5 (Array1.unsafe_get c 4);
  Alcotest.(check (float 0.0001)) "c[5]" (-0.25) (Array1.unsafe_get c 5);
  Alcotest.(check (float 0.0001)) "c[6]" 5.5 (Array1.unsafe_get c 6);
  Alcotest.(check (float 0.0001)) "c[7]" (-0.25) (Array1.unsafe_get c 7)

let%test "cherk" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let c = Array1.of_array float32 c_layout [|2.0; 0.0; 4.0; 2.0; 0.0; 0.0; 8.0; 0.0|] in
  let _ = cherk (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 0.5
    (to_voidp (bigarray_start array1 a)) 2 
    0.5 (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 3.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 7.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 1.0 (Array1.unsafe_get c 3);
  Alcotest.(check (float 0.0001)) "c[6]" 16.5 (Array1.unsafe_get c 6)

let%test "cher2k" =
  let a = Array1.of_array float32 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float32 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float32 c_layout [|2.0; 0.0; 4.0; 2.0; 0.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float32 c_layout [|0.5; 0.0|] in
  let _ = cher2k (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2
    (to_voidp (bigarray_start array1 b)) 2 
    0.5 (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 2.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 4.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 1.0 (Array1.unsafe_get c 3);
  Alcotest.(check (float 0.0001)) "c[6]" 7.5 (Array1.unsafe_get c 6)

let%test "zhemm" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 1.0; 0.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float64 c_layout [|2.0; 0.0; 4.0; 0.0; 6.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let beta = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zhemm (int_of_order RowMajor) (int_of_side Left) (int_of_uplo Upper)
    2 2
    (to_voidp (bigarray_start array1 alpha)) (to_voidp (bigarray_start array1 a)) 2 
    (to_voidp (bigarray_start array1 b)) 2 
    (to_voidp (bigarray_start array1 beta)) (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 1.75 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[1]" 0.25 (Array1.unsafe_get c 1);
  Alcotest.(check (float 0.0001)) "c[2]" 2.75 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 0.25 (Array1.unsafe_get c 3);
  Alcotest.(check (float 0.0001)) "c[4]" 4.5 (Array1.unsafe_get c 4);
  Alcotest.(check (float 0.0001)) "c[5]" (-0.25) (Array1.unsafe_get c 5);
  Alcotest.(check (float 0.0001)) "c[6]" 5.5 (Array1.unsafe_get c 6);
  Alcotest.(check (float 0.0001)) "c[7]" (-0.25) (Array1.unsafe_get c 7)

let%test "zherk" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let c = Array1.of_array float64 c_layout [|2.0; 0.0; 4.0; 2.0; 0.0; 0.0; 8.0; 0.0|] in
  let _ = zherk (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 0.5
    (to_voidp (bigarray_start array1 a)) 2 
    0.5 (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 3.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 7.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 1.0 (Array1.unsafe_get c 3);
  Alcotest.(check (float 0.0001)) "c[6]" 16.5 (Array1.unsafe_get c 6)

let%test "zher2k" =
  let a = Array1.of_array float64 c_layout [|1.0; 0.0; 2.0; 0.0; 3.0; 0.0; 4.0; 0.0|] in
  let b = Array1.of_array float64 c_layout [|0.5; 0.0; 0.5; 0.0; 0.5; 0.0; 0.5; 0.0|] in
  let c = Array1.of_array float64 c_layout [|2.0; 0.0; 4.0; 2.0; 0.0; 0.0; 8.0; 0.0|] in
  let alpha = Array1.of_array float64 c_layout [|0.5; 0.0|] in
  let _ = zher2k (int_of_order RowMajor) (int_of_uplo Upper) (int_of_transpose NoTrans)
    2 2 (to_voidp (bigarray_start array1 alpha))
    (to_voidp (bigarray_start array1 a)) 2
    (to_voidp (bigarray_start array1 b)) 2 
    0.5 (to_voidp (bigarray_start array1 c)) 2 in
  Alcotest.(check (float 0.0001)) "c[0]" 2.5 (Array1.unsafe_get c 0);
  Alcotest.(check (float 0.0001)) "c[2]" 4.5 (Array1.unsafe_get c 2);
  Alcotest.(check (float 0.0001)) "c[3]" 1.0 (Array1.unsafe_get c 3);
  Alcotest.(check (float 0.0001)) "c[6]" 7.5 (Array1.unsafe_get c 6)