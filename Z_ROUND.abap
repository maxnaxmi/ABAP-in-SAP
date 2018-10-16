*&---------------------------------------------------------------------*
*& Report  Z_ROUND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_ROUND.
PARAMETERS:
      P_VALUE TYPE P DECIMALS 5,      "入力値
      P_DECI  TYPE I,                 "丸め位置
      P_SIGN  TYPE C.                 "丸め区分

DATA :
       L_RESULT  TYPE P DECIMALS 5.    "TYPE　Pを利用

*整数10の位で切り捨て
CALL FUNCTION 'ROUND'
  EXPORTING
    DECIMALS      = P_DECI                 "丸めが実行される小数点以下桁数、マイナス可
    INPUT         = P_VALUE                "丸められる値
    SIGN          = P_SIGN                 "+:切り上げ、-:切り捨て、X:四捨五入
  IMPORTING
    OUTPUT        = L_RESULT               "丸め後の値
  EXCEPTIONS
    INPUT_INVALID = 1
    OVERFLOW      = 2
    TYPE_INVALID  = 3
    OTHERS        = 4.


WRITE : / P_VALUE , '->' , L_RESULT .