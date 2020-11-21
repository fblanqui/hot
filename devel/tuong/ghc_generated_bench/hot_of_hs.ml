open Xtc
open Type

type ('a, 'b) haskell = 
  | Fundef of 'a
  | Typedef of 'b

(** Conversion *)

let fresh =
  let r = ref 0 in
  fun () -> 
    let _ = incr r in
    Printf.sprintf "%d" !r

let convert_rule = function
    | ParseOk (Module (_, ModuleName "Main", [  ], Nothing, Just [ EVar (UnQual (Ident "main")) ], _, l)) -> 
    List.fold_left (fun acc -> function
      | TypeSig (_, [Ident s], t) -> 
        let rec aux = function
          | TyFun (t1, t2) -> TypeArrow (aux t1, aux t2)
          | TyApp (t1, t2) -> let _ = ignore (aux t1, aux t2) in TypeBasic "2do"
          | TyCon (UnQual (Ident s)) -> TypeBasic s
          | TyParen t -> let _ = ignore (aux t) in TypeBasic "2do" 
          | TyVar (Ident s) -> TypeBasic s in
        Typedef (s, aux t) :: acc
      | FunBind l -> 
        List.fold_left (fun acc -> function
          | Match (_, Ident s, l, Nothing, UnGuardedRhs e, BDecls [  ]) -> 
            Fundef (TermFunapp (s, List.map (let rec _pat = function 
                                     | PParen e -> _pat e
                                     | PApp (UnQual (Ident p), l) -> TermFunapp (p, List.map _pat l)
                                     | PVar (Ident i) -> TermVar i
                                     | PWildCard -> TermVar (fresh ()) in
                                     _pat) l), 
            (let of_name = function
              | Con (UnQual (Ident name)) 
              | Var (UnQual (Ident name)) -> name in

             let rec app_left l = function
               | App (e1, e2) -> app_left (e2 :: l) e1
               | x -> x, l in

             let rec _exp = function
               | Paren e -> _exp e
               | e ->
                 match app_left [] e with
                   | e, [] -> TermVar (of_name e)
                   | e, l -> TermFunapp (of_name e, List.map _exp l) in
             _exp e
            )) :: acc
        ) acc l
      | _ -> acc) [] l

(** Test *)

let l = 
  let open Value in
[ _abs_1
; _abs_2
; _abs_3
; _all_1
; _AMPAMP_1
; _and_1
; _any_1
; _asTypeOf_1
; _BANGBANG_1
; _BARBAR_1
; _basicIORun_1
; _blockIO_1
; _break_1
; _CARET_1
; _CARET_2
; _catchHugsException_1
; _ceiling_1
; _chr_1
; _compare_1
; _compare_2
; _compare_3
; _compare_4
; _compare_5
; _compare_6
; _compare_7
; _concat_1
; _concatMap_1
; _const_1
; _curry_1
; _cycle_1
; _denominator_1
; _digitToInt_1
; _div_1
; _divMod_1
; _DOLLAR_1
; _DOLLARBANG_1
; _DOT_1
; _drop_1
; _dropWhile_1
; _either_1
; _elem_1
; _elem_2
; _elem_3
; _elem_4
; _elem_5
; _elem_6
; _elem_7
; _encodeFloat_1
; _enumFrom_1
; _enumFrom_2
; _enumFrom_3
; _enumFrom_4
; _enumFrom_5
; _enumFrom_6
; _enumFromThen_1
; _enumFromThen_2
; _enumFromThen_3
; _enumFromThen_4
; _enumFromThen_5
; _enumFromThen_6
; _enumFromThen_7
; _enumFromThenTo_1
; _enumFromThenTo_2
; _enumFromThenTo_3
; _enumFromThenTo_4
; _enumFromThenTo_5
; _enumFromThenTo_6
; _enumFromTo_1
; _enumFromTo_2
; _enumFromTo_3
; _enumFromTo_4
; _enumFromTo_5
; _enumFromTo_6
; _enumFromTo_7
; _EQEQ_1
; _EQEQ_2
; _EQEQ_3
; _EQEQ_4
; _EQEQ_5
; _EQEQ_6
; _EQEQ_7
; _EQLTLT_1
; _error_1
; _even_1
; _fail_1
; _filter_1
; _flip_1
; _floatDigits_1
; _floatRadix_1
; _floatRange_1
; _floor_1
; _fmap_1
; _fmap_2
; _foldl1_1
; _foldl_1
; _foldr1_1
; _foldr_1
; _fromDouble_1
; _fromDouble_2
; _fromEnum_1
; _fromEnum_2
; _fromEnum_3
; _fromEnum_4
; _fromEnum_5
; _fromEnum_6
; _fromEnum_7
; _fromInt_1
; _fromInt_2
; _fromInt_3
; _fromInteger_1
; _fromInteger_2
; _fromInteger_3
; _fromIntegral_1
; _fromIntegral_2
; _fromIntegral_3
; _fromRational_1
; _fromRational_2
; _fst_1
; _gcd_1
; _GT_1
; _GT_2
; _GT_3
; _GT_4
; _GT_5
; _GT_6
; _GT_7
; _GTEQ_1
; _GTEQ_2
; _GTEQ_3
; _GTEQ_4
; _GTEQ_5
; _GTEQ_6
; _GTEQ_7
; _GTGT_1
; _GTGTEQ_1
; _head_1
; _id_1
; _index_1
; _index_2
; _index_3
; _index_4
; _init_1
; _inRange_1
; _inRange_2
; _inRange_3
; _inRange_4
; _intToDigit_1
; _isAlpha_1
; _isAlphaNum_1
; _isAscii_1
; _isControl_1
; _isDenormalized_1
; _isDigit_1
; _isHexDigit_1
; _isIEEE_1
; _isInfinite_1
; _isLatin1_1
; _isLower_1
; _isNaN_1
; _isNegativeZero_1
; _isOctDigit_1
; _isPrint_1
; _isSpace_1
; _isUpper_1
; _iterate_1
; _last_1
; _lcm_1
; _length_1
; _lex_1
; _lexDigits_1
; _lexLitChar_1
; _lines_1
; _lookup_1
; _lookup_2
; _lookup_3
; _lookup_4
; _lookup_5
; _lookup_6
; _LT_1
; _LT_2
; _LT_3
; _LT_4
; _LT_5
; _LT_6
; _LTEQ_1
; _LTEQ_2
; _LTEQ_3
; _LTEQ_4
; _LTEQ_5
; _LTEQ_6
; _LTEQ_7
; _map_1
; _mapM_1
; _mapM__1
; _max_1
; _max_2
; _max_3
; _max_4
; _max_5
; _max_6
; _maxBound_1
; _maxBound_2
; _maxBound_3
; _maxBound_4
; _maxBound_5
; _maximum_1
; _maximum_2
; _maximum_3
; _maximum_4
; _maximum_5
; _maximum_6
; _maybe_1
; _min_1
; _min_2
; _min_3
; _min_4
; _min_5
; _min_6
; _minBound_1
; _minBound_2
; _minBound_3
; _minBound_4
; _minBound_5
; _minimum_1
; _minimum_2
; _minimum_3
; _minimum_4
; _minimum_5
; _minimum_6
; _MINUS_1
; _MINUS_2
; _mod_1
; _negate_1
; _negate_2
; _not_1
; _notElem_1
; _notElem_2
; _notElem_3
; _notElem_4
; _notElem_5
; _notElem_6
; _null_1
; _numerator_1
; _odd_1
; _or_1
; _ord_1
; _otherwise_1
; _PLUS_1
; _PLUS_2
; _PLUSPLUS_1
; _pred_1
; _pred_2
; _pred_3
; _pred_4
; _pred_5
; _pred_6
; _pred_7
; _primThrowException_1
; _product_1
; _product_2
; _properFraction_1
; _properFraction_2
; _quot_1
; _quotRem_1
; _range_1
; _range_2
; _range_3
; _range_4
; _rangeSize_1
; _rangeSize_2
; _rangeSize_3
; _rangeSize_4
; _read_1
; _read_2
; _readDec_1
; _readHex_1
; _readInt_1
; _readList_1
; _readLitChar_1
; _readOct_1
; _readParen_1
; _readParen_2
; _readParen_3
; _readParen_4
; _readParen_5
; _readParen_6
; _reads_1
; _reads_2
; _reads_3
; _readSigned_1
; _readsPrec_1
; _readsPrec_2
; _realToFrac_1
; _realToFrac_2
; _recip_1
; _recip_2
; _rem_1
; _repeat_1
; _replicate_1
; _return_1
; _return_2
; _reverse_1
; _scanl1_1
; _scanl_1
; _scanr1_1
; _scanr_1
; _seq_1
; _sequence_1
; _sequence_2
; _sequence__1
; _sequence__2
; _show_1
; _show_2
; _show_3
; _show_4
; _show_5
; _showChar_1
; _showInt_1
; _showParen_1
; _shows_1
; _shows_2
; _shows_3
; _shows_4
; _shows_5
; _showSigned_1
; _showSigned_2
; _showSigned_3
; _showsPrec_1
; _showsPrec_2
; _showsPrec_3
; _showsPrec_4
; _showsPrec_5
; _showString_1
; _signum_1
; _signum_2
; _signum_3
; _SLASH_1
; _SLASHEQ_1
; _SLASHEQ_2
; _SLASHEQ_3
; _SLASHEQ_4
; _SLASHEQ_5
; _SLASHEQ_6
; _SLASHEQ_7
; _snd_1
; _span_1
; _splitAt_1
; _STAR_1
; _STAR_2
; _subtract_1
; _subtract_2
; _succ_1
; _succ_2
; _succ_3
; _succ_4
; _succ_5
; _succ_6
; _succ_7
; _sum_1
; _sum_2
; _tail_1
; _take_1
; _takeWhile_1
; _toEnum_1
; _toEnum_2
; _toEnum_3
; _toEnum_4
; _toEnum_5
; _toEnum_6
; _toEnum_7
; _toInt_1
; _toInteger_1
; _toLower_1
; _toRational_1
; _toUpper_1
; _truncate_1
; _truncate_2
; _uncurry_1
; _undefined_1
; _unlines_1
; _until_1
; _unwords_1
; _unzip3_1
; _unzip_1
; _userError_1
; _words_1
; _zip3_1
; _zip_1
; _zipWith3_1
; _zipWith_1 ]

let _ = 
  let v = List.fold_left (fun (nb, l) v -> 
    let _ = Printf.printf "zzzzzzzzz %d\n%!" nb in
    succ nb, convert_rule v :: l) (1, []) l in
  let _ = ignore v in
  ()
