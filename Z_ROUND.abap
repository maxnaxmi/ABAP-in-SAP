*&---------------------------------------------------------------------*
*& Report  ROUND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_ROUND.
PARAMETERS:
      p_value TYPE p DECIMALS 5,      "入力値
      p_deci  TYPE i,                 "丸め位置
      p_sign  TYPE c.                 "丸め区分

DATA : l_result TYPE p DECIMALS 5.    "TYPE　Pを利用

*整数10の位で切り捨て
CALL FUNCTION 'ROUND'
  EXPORTING
    DECIMALS = p_deci                 "丸めが実行される小数点以下桁数、マイナス可
    INPUT    = p_value                "丸められる値
    SIGN     = p_sign                 "+:切り上げ、-:切り捨て、X:四捨五入
  IMPORTING
    OUTPUT   = l_result               "丸め後の値
 EXCEPTIONS
   INPUT_INVALID = 1
   OVERFLOW      = 2
   TYPE_INVALID  = 3
   OTHERS        = 4 .

WRITE : / '整数' , p_deci , 'の位で切り捨て:' , p_value , '->' , l_result .