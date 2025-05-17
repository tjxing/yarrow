open Ctypes

(* cublasStatus_t enum *)
type status =
  | Success
  | NotInitialized
  | AllocFailed
  | InvalidValue
  | ArchMismatch
  | MappingError
  | ExecutionFailed
  | InternalError
  | NotSupported
  | LicenseError

let int_of_status = function
  | Success -> 0
  | NotInitialized -> 1
  | AllocFailed -> 3
  | InvalidValue -> 7
  | ArchMismatch -> 8
  | MappingError -> 11
  | ExecutionFailed -> 13
  | InternalError -> 14
  | NotSupported -> 15
  | LicenseError -> 16

let status_of_int = function
  | 0 -> Success
  | 1 -> NotInitialized
  | 3 -> AllocFailed
  | 7 -> InvalidValue
  | 8 -> ArchMismatch
  | 11 -> MappingError
  | 13 -> ExecutionFailed
  | 14 -> InternalError
  | 15 -> NotSupported
  | 16 -> LicenseError
  | _ -> failwith "Invalid status value"

(* cublasFillMode_t enum *)
type fill_mode =
  | Lower
  | Upper
  | Full

let int_of_fill_mode = function
  | Lower -> 0
  | Upper -> 1
  | Full -> 2

let fill_mode_of_int = function
  | 0 -> Lower
  | 1 -> Upper
  | 2 -> Full
  | _ -> failwith "Invalid fill mode value"

(* cublasDiagType_t enum *)
type diag =
  | NotUnit
  | Unit

let int_of_diag = function
  | NotUnit -> 0
  | Unit -> 1

let diag_of_int = function
  | 0 -> NotUnit
  | 1 -> Unit
  | _ -> failwith "Invalid diag value"

(* cublasSideMode_t enum *)
type side_mode =
  | Left
  | Right

let int_of_side_mode = function
  | Left -> 0
  | Right -> 1

let side_mode_of_int = function
  | 0 -> Left
  | 1 -> Right
  | _ -> failwith "Invalid side mode value"

(* cublasOperation_t enum *)
type operation =
  | N
  | T
  | C
  | Hermitan
  | Conjg

let int_of_operation = function
  | N -> 0
  | T -> 1
  | C -> 2
  | Hermitan -> 2
  | Conjg -> 3

let operation_of_int = function
  | 0 ->N
  | 1 -> T
  | 2 -> C
  | 3 -> Conjg
  | _ -> failwith "Invalid operation value"

(* cublasPointerMode_t enum *)
type pointer_mode =
  | Host
  | Device

let int_of_pointer_mode = function
  | Host -> 0
  | Device -> 1

let pointer_mode_of_int = function
  | 0 -> Host
  | 1 -> Device
  | _ -> failwith "Invalid pointer mode value"

(* cublasAtomicsMode_t enum *)
type atomics_mode =
  | NotAllowed
  | Allowed

let int_of_atomics_mode = function
  | NotAllowed -> 0
  | Allowed -> 1

let atomics_mode_of_int = function
  | 0 -> NotAllowed
  | 1 -> Allowed
  | _ -> failwith "Invalid atomics mode value"

(* cublasGemmAlgo_t enum *)
type gemm_algo =
  | DFALT
  | DEFAULT
  | ALGO0
  | ALGO1
  | ALGO2
  | ALGO3
  | ALGO4
  | ALGO5
  | ALGO6
  | ALGO7
  | ALGO8
  | ALGO9
  | ALGO10
  | ALGO11
  | ALGO12
  | ALGO13
  | ALGO14
  | ALGO15
  | ALGO16
  | ALGO17
  | ALGO18
  | ALGO19
  | ALGO20
  | ALGO21
  | ALGO22
  | ALGO23
  | DEFAULT_TENSOR_OP
  | DFALT_TENSOR_OP
  | ALGO0_TENSOR_OP
  | ALGO1_TENSOR_OP
  | ALGO2_TENSOR_OP
  | ALGO3_TENSOR_OP
  | ALGO4_TENSOR_OP
  | ALGO5_TENSOR_OP
  | ALGO6_TENSOR_OP
  | ALGO7_TENSOR_OP
  | ALGO8_TENSOR_OP
  | ALGO9_TENSOR_OP
  | ALGO10_TENSOR_OP
  | ALGO11_TENSOR_OP
  | ALGO12_TENSOR_OP
  | ALGO13_TENSOR_OP
  | ALGO14_TENSOR_OP
  | ALGO15_TENSOR_OP

let int_of_gemm_algo = function
  | DFALT -> -1
  | DEFAULT -> -1
  | ALGO0 -> 0
  | ALGO1 -> 1
  | ALGO2 -> 2
  | ALGO3 -> 3
  | ALGO4 -> 4
  | ALGO5 -> 5
  | ALGO6 -> 6
  | ALGO7 -> 7
  | ALGO8 -> 8
  | ALGO9 -> 9
  | ALGO10 -> 10
  | ALGO11 -> 11
  | ALGO12 -> 12
  | ALGO13 -> 13
  | ALGO14 -> 14
  | ALGO15 -> 15
  | ALGO16 -> 16
  | ALGO17 -> 17
  | ALGO18 -> 18
  | ALGO19 -> 19
  | ALGO20 -> 20
  | ALGO21 -> 21
  | ALGO22 -> 22
  | ALGO23 -> 23
  | DEFAULT_TENSOR_OP -> 99
  | DFALT_TENSOR_OP -> 99
  | ALGO0_TENSOR_OP -> 100
  | ALGO1_TENSOR_OP -> 101
  | ALGO2_TENSOR_OP -> 102
  | ALGO3_TENSOR_OP -> 103
  | ALGO4_TENSOR_OP -> 104
  | ALGO5_TENSOR_OP -> 105
  | ALGO6_TENSOR_OP -> 106
  | ALGO7_TENSOR_OP -> 107
  | ALGO8_TENSOR_OP -> 108
  | ALGO9_TENSOR_OP -> 109
  | ALGO10_TENSOR_OP -> 110
  | ALGO11_TENSOR_OP -> 111
  | ALGO12_TENSOR_OP -> 112
  | ALGO13_TENSOR_OP -> 113
  | ALGO14_TENSOR_OP -> 114
  | ALGO15_TENSOR_OP -> 115

let gemm_algo_of_int = function
  | -1-> DEFAULT
  | 0 -> ALGO0
  | 1 -> ALGO1
  | 2 -> ALGO2
  | 3 -> ALGO3
  | 4 -> ALGO4
  | 5 -> ALGO5
  | 6 -> ALGO6
  | 7 -> ALGO7
  | 8 -> ALGO8
  | 9 -> ALGO9
  | 10 -> ALGO10
  | 11 -> ALGO11
  | 12 -> ALGO12
  | 13 -> ALGO13
  | 14 -> ALGO14
  | 15 -> ALGO15
  | 16 -> ALGO16
  | 17 -> ALGO17
  | 18 -> ALGO18
  | 19 -> ALGO19
  | 20 -> ALGO20
  | 21 -> ALGO21
  | 22 -> ALGO22
  | 23 -> ALGO23
  | 99 -> DEFAULT_TENSOR_OP
  | 100 -> ALGO0_TENSOR_OP
  | 101 -> ALGO1_TENSOR_OP
  | 102 -> ALGO2_TENSOR_OP
  | 103 -> ALGO3_TENSOR_OP
  | 104 -> ALGO4_TENSOR_OP
  | 105 -> ALGO5_TENSOR_OP
  | 106 -> ALGO6_TENSOR_OP
  | 107 -> ALGO7_TENSOR_OP
  | 108 -> ALGO8_TENSOR_OP
  | 109 -> ALGO9_TENSOR_OP
  | 110 -> ALGO10_TENSOR_OP
  | 111 -> ALGO11_TENSOR_OP
  | 112 -> ALGO12_TENSOR_OP
  | 113 -> ALGO13_TENSOR_OP
  | 114 -> ALGO14_TENSOR_OP
  | 115 -> ALGO15_TENSOR_OP
  | _ -> failwith "Invalid gemm algorithm value"

(* cublasMath_t enum *)
type math =
  | DEFAULT
  | TENSOR_OP
  | PEDANTIC
  | TF32_TENSOR_OP
  | FP32_EMULATED_BF16X9
  | DISALLOW_REDUCED_PRECISION_REDUCTION

let int_of_math = function
  | DEFAULT -> 0
  | TENSOR_OP -> 1
  | PEDANTIC -> 2
  | TF32_TENSOR_OP -> 3
  | FP32_EMULATED_BF16X9 -> 4
  | DISALLOW_REDUCED_PRECISION_REDUCTION -> 16

let math_of_int = function
  | 0 -> DEFAULT
  | 1 -> TENSOR_OP
  | 2 -> PEDANTIC
  | 3 -> TF32_TENSOR_OP
  | 4 -> FP32_EMULATED_BF16X9
  | 16 -> DISALLOW_REDUCED_PRECISION_REDUCTION
  | _ -> failwith "Invalid math value"

(* cublasDataType_t enum *)
type data_type =
  | R_16F
  | C_16F
  | R_16BF
  | C_16BF
  | R_32F
  | C_32F
  | R_64F
  | C_64F
  | R_4I
  | C_4I
  | R_4U
  | C_4U
  | R_8I
  | C_8I
  | R_8U
  | C_8U
  | R_16I
  | C_16I
  | R_16U
  | C_16U
  | R_32I
  | C_32I
  | R_32U
  | C_32U
  | R_64I
  | C_64I
  | R_64U
  | C_64U
  | R_8F_E4M3
  | R_8F_UE4M3
  | R_8F_E5M2
  | R_8F_UE8M0
  | R_6F_E2M3
  | R_6F_E3M2
  | R_4F_E2M1

let int_of_data_type = function
  | R_16F -> 2
  | C_16F -> 6
  | R_16BF -> 14
  | C_16BF -> 15
  | R_32F -> 0
  | C_32F -> 4
  | R_64F -> 1
  | C_64F -> 5
  | R_4I -> 16
  | C_4I -> 17
  | R_4U -> 18
  | C_4U -> 19
  | R_8I -> 3
  | C_8I -> 7
  | R_8U -> 8
  | C_8U -> 9
  | R_16I -> 20
  | C_16I -> 21
  | R_16U -> 22
  | C_16U -> 23
  | R_32I -> 10
  | C_32I -> 11
  | R_32U -> 12
  | C_32U -> 13
  | R_64I -> 24
  | C_64I -> 25
  | R_64U -> 26
  | C_64U -> 27
  | R_8F_E4M3 -> 28
  | R_8F_UE4M3 -> 28
  | R_8F_E5M2 -> 29
  | R_8F_UE8M0 -> 30
  | R_6F_E2M3 -> 31
  | R_6F_E3M2 -> 32
  | R_4F_E2M1 -> 33

let data_type_of_int = function
  | 2 -> R_16F
  | 6 -> C_16F
  | 14 -> R_16BF
  | 15 -> C_16BF
  | 0 -> R_32F
  | 4 -> C_32F
  | 1 -> R_64F
  | 5 -> C_64F
  | 16 -> R_4I
  | 17 -> C_4I
  | 18 -> R_4U
  | 19 -> C_4U
  | 3 -> R_8I
  | 7 -> C_8I
  | 8 -> R_8U
  | 9 -> C_8U
  | 20 -> R_16I
  | 21 -> C_16I
  | 22 -> R_16U
  | 23 -> C_16U
  | 10 -> R_32I
  | 11 -> C_32I
  | 12 -> R_32U
  | 13 -> C_32U
  | 24 -> R_64I
  | 25 -> C_64I
  | 26 -> R_64U
  | 27 -> C_64U
  | 28 -> R_8F_E4M3
  | 29 -> R_8F_E5M2
  | 30 -> R_8F_UE8M0
  | 31 -> R_6F_E2M3
  | 32 -> R_6F_E3M2
  | 33 -> R_4F_E2M1
  | _ -> failwith "Invalid data type value"

(* cublasComputeType_t enum *)
type compute_type =
  | COMPUTE_16F
  | COMPUTE_16F_PEDANTIC
  | COMPUTE_32F
  | COMPUTE_32F_PEDANTIC
  | COMPUTE_32F_FAST_16F
  | COMPUTE_32F_FAST_16BF
  | COMPUTE_32F_FAST_TF32
  | COMPUTE_32F_EMULATED_16BFX9
  | COMPUTE_64F
  | COMPUTE_64F_PEDANTIC
  | COMPUTE_32I
  | COMPUTE_32I_PEDANTIC

let int_of_compute_type = function
  | COMPUTE_16F -> 64
  | COMPUTE_16F_PEDANTIC -> 65
  | COMPUTE_32F -> 68
  | COMPUTE_32F_PEDANTIC -> 69
  | COMPUTE_32F_FAST_16F -> 74
  | COMPUTE_32F_FAST_16BF -> 75
  | COMPUTE_32F_FAST_TF32 -> 77
  | COMPUTE_32F_EMULATED_16BFX9 -> 78
  | COMPUTE_64F -> 70
  | COMPUTE_64F_PEDANTIC -> 71
  | COMPUTE_32I -> 72
  | COMPUTE_32I_PEDANTIC -> 73

let compute_type_of_int = function
  | 64 -> COMPUTE_16F
  | 65 -> COMPUTE_16F_PEDANTIC
  | 68 -> COMPUTE_32F
  | 69 -> COMPUTE_32F_PEDANTIC
  | 74 -> COMPUTE_32F_FAST_16F
  | 75 -> COMPUTE_32F_FAST_16BF
  | 77 -> COMPUTE_32F_FAST_TF32
  | 78 -> COMPUTE_32F_EMULATED_16BFX9
  | 70 -> COMPUTE_64F
  | 71 -> COMPUTE_64F_PEDANTIC
  | 72 -> COMPUTE_32I
  | 73 -> COMPUTE_32I_PEDANTIC
  | _ -> failwith "Invalid compute type value"

(* cublasEmulationStrategy_t enum *)
type emulation_strategy =
  | DEFAULT
  | PERFORMANT
  | EAGER

let int_of_emulation_strategy = function
  | DEFAULT -> 0
  | PERFORMANT -> 1
  | EAGER -> 2

let emulation_strategy_of_int = function
  | 0 -> DEFAULT
  | 1 -> PERFORMANT
  | 2 -> EAGER
  | _ -> failwith "Invalid emulation strategy value"

(* cuComplex and cuDoubleComplex struct *)
module Complex (C : sig 
  val name: string
  val t: float typ
end) = struct
  type t
  let t : t structure typ = structure C.name
  let x = field t "x" C.t
  let y = field t "y" C.t
  let () = seal t

  let create x_val y_val = 
    let complex = make t in
    setf complex x x_val;
    setf complex y y_val;
    complex
end

module FloatComplex = Complex(struct let name = "cuComplex" let t = float end)
module DoubleComplex = Complex(struct let name = "cuDoubleComplex" let t = double end)