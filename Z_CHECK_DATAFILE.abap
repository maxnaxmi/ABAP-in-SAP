*&---------------------------------------------------------------------*
*& Report  Z_CHECK_DATAFILE
*&
*&---------------------------------------------------------------------*
*&  データファイルの存在チェック
*&
*&---------------------------------------------------------------------*

REPORT Z_CHECK_DATAFILE.

PARAMETERS :
  P_PARA3 TYPE STRING,                                      "パラメータ3
  P_FNAME TYPE STRING.                          "ファイル名

*----- データ定義
CONSTANTS:
  LC_PATH_IN TYPE PATHINTERN VALUE 'ZP_IFIN',   "論理パスIN
  LC_FLG_ON  TYPE FLAG VALUE 'X'.               "フラグON

DATA:
  LW_SYSID    TYPE STRING,                      "システムID
  LW_IO       TYPE STRING,                      "IO区分
  LW_EFPATH   TYPE STRING,                      "ファイルパス
  O_FLG_EXIST TYPE FLAG.                        "エラーフラグ

*----- パスに必要な引数の設定
* IFIDの上2桁がZMの場合、EMをSYSIDとする
IF P_PARA3(2) = 'ZM'.
  LW_SYSID = 'EM'.
* IFIDの上2桁がZMではない場合、IFIDの3～4桁をSYSIDとする
ELSE.
  LW_SYSID = P_PARA3+2(2).
ENDIF.

* IFIDの上2桁がZMの場合、ZをIOとする
IF P_PARA3(2) = 'ZM'.
  LW_IO = 'Z'.
* IFIDの上2桁がZMではない場合、IFIDの1桁をIOとする
ELSE.
  LW_IO = P_PARA3(1).
ENDIF.

*----- 完全ファイル名の生成
*(論理パスおよびファイル名・パラメータはFILEで定義したパスに準じる)
CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
  EXPORTING
    LOGICAL_PATH        = LC_PATH_IN  "論理パス
    PARAMETER_1         = LW_SYSID    "SYS_ID
    PARAMETER_2         = LW_IO       "IO区分
    PARAMETER_3         = P_PARA3      "IFID
    FILE_NAME           = P_FNAME
  IMPORTING
    FILE_NAME_WITH_PATH = LW_EFPATH.
*
*----- 異常ファイルの読込処理
OPEN DATASET LW_EFPATH FOR INPUT IN TEXT MODE ENCODING DEFAULT.

*----- 存在している場合は当処理もエラーとする
IF SY-SUBRC = 0.
  O_FLG_EXIST = LC_FLG_ON.
  CLOSE DATASET LW_EFPATH.
ENDIF.


WRITE :/01 '完全ファイル名', 20 '：', 23 LW_EFPATH,
       /01 'パラメータ2'   , 20 '：', 23 LW_IO,
       /01 'パラメータ3'   , 20 '：', 23 P_PARA3,
       /01 '存在フラグ'    , 20 '：', 23 O_FLG_EXIST.

*----------MEMO----------
*  ※論理パスおよび物理パスへのマッピングの定義
*    ->  テーブルID：FILEPATH（論理ファイルパス定義）
*    ->  テーブルID：PATH（物理ファイルパス定義）
*  ※論理パス定義トランザクション ->  FILE