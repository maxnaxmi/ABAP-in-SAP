*----------------------------------------------------------------------*
* PROGRAM ID  : ZOTM0004
* NAME        : テーブルデータ退避(削除&更新)
* AUTHOR      : RS 鈴木
* DATE        : 2018/08/01
* DESCRIPTION : 実テーブル ⇔ バックアップテーブル/テストテーブル間のデータ退避
*               更新テーブルは、抽出条件に合致するレコードを全削除してから登録
*----------------------------------------------------------------------*
*& 変更履歴
*& LOG#  DATE        AUTHOR     DESCRIPTION
*& 0001  2018/08/08  RS鈴木    SAP商品コードを選択画面に追加
*&                             部門コードの必須を解除
*----------------------------------------------------------------------*
REPORT  ZOTM0004.

*----------------------------------------------------------------------*
* CONSTANTS定義
*----------------------------------------------------------------------*
CONSTANTS:
  C_FLG_ON(1) TYPE C VALUE 'X'.               "フラグON

*----------------------------------------------------------------------*
* DATA定義（構造）
*----------------------------------------------------------------------*
DATA:
  D_SPART  TYPE ZPESPART,                     "部門コード
  D_WERKS  TYPE ZPEWERKS,                     "店舗コード
*--> ADD 0001 START 2018/08/08 RS鈴木
  D_MATNR  TYPE ZPEMATNR.                     "SAP商品コード
*--> ADD 0001 END   2018/08/08 RS鈴木

*----------------------------------------------------------------------*
* DATA定義（ワーク）
*----------------------------------------------------------------------*
DATA:
  V_TABNAME       TYPE DD02L-TABNAME,         "テーブル名
  V_FIELDNAME     TYPE DD03L-FIELDNAME,       "項目名
  W_MESSAGE       TYPE STRING,                "メッセージ出力用
  W_CNT           TYPE STRING,                "メッセージ出力用データ件数

  W_REF_TD        TYPE REF TO DATA,           "動的参照用データ（テーブル）
  FLG_ERR(1)      TYPE C,                     "エラーフラグ
  W_CONDITION(50) TYPE C,                     "SELECT文検索条件用（一時保持）
  TD_SQL          LIKE TABLE OF W_CONDITION.  "SELECT文検索条件用（溜め込み）

*----------------------------------------------------------------------*
* FIELD-SYMBOLS定義
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <FS_TAB> TYPE STANDARD TABLE.               "動的作業用内部テーブル

*----------------------------------------------------------------------*
* PARAMETERS・SELECT-OPTIONS定義
*----------------------------------------------------------------------*

"テーブル選択
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  P_TABNM  TYPE RSRD1-TBMA_VAL OBLIGATORY,      "テーブルID
  P_TABNMC TYPE RSRD1-TBMA_VAL OBLIGATORY.      "コピー元テーブルID
SELECTION-SCREEN END   OF BLOCK B0.

"抽出条件
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
*--> MOD 0001 START 2018/08/08 RS鈴木
*  S_SPART FOR D_SPART OBLIGATORY DEFAULT 30,    "部門コード
  S_SPART FOR D_SPART DEFAULT 30,               "部門コード
*--> MOD 0001 END   2018/08/08 RS鈴木
  S_WERKS FOR D_WERKS,                          "店舗コード
*--> ADD 0001 START 2018/08/08 RS鈴木
  S_MATNR FOR D_MATNR.                          "SAP商品コード
*--> ADD 0001 END   2018/08/08 RS鈴木
SELECTION-SCREEN END   OF BLOCK B1.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

*--- 選択画面入力チェック
  PERFORM INDATA_CHECK.

  IF FLG_ERR <> C_FLG_ON.

*--- 選択条件のSQL作成
    PERFORM CREATE_SQL.
*--- テーブルデータ取得
    PERFORM GET_DATA.

  ENDIF.

  IF FLG_ERR <> C_FLG_ON.
*--- テーブルデータ更新
    PERFORM MOD_DATA.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  INDATA_CHECK
*&---------------------------------------------------------------------*
*       選択画面入力チェック
*----------------------------------------------------------------------*
FORM INDATA_CHECK .

  CLEAR: FLG_ERR.

*----- テーブル名チェック（テーブルIDがアドオンテーブル（Z始まり）であること）
  IF P_TABNM(1) <> 'Z'.
    "アドオンテーブルのみ更新可能です
    MESSAGE TEXT-M01 TYPE 'S'.
    FLG_ERR = C_FLG_ON.
    EXIT.
  ENDIF.

*----- 整合性チェック（テーブルID頭8桁が一致していること）
  IF P_TABNM(8) <> P_TABNMC(8).
    "テーブルIDが一致しません
    MESSAGE TEXT-M02 TYPE 'S'.
    FLG_ERR = C_FLG_ON.
    EXIT.
  ENDIF.


*----- 存在チェック
  SELECT SINGLE TABNAME
           INTO V_TABNAME
           FROM DD02L
          WHERE TABNAME  =  P_TABNM
          AND   AS4LOCAL =  'A'
          AND   TABCLASS <> 'INTTAB'
          AND   TABCLASS <> 'VIEW'
          AND   TABCLASS <> 'APPEND'.

  IF SY-SUBRC <> 0.
    "テーブルIDが存在しません
    MESSAGE TEXT-M03 TYPE 'S'.
    FLG_ERR = C_FLG_ON.
    EXIT.
  ENDIF.

  SELECT SINGLE TABNAME
           INTO V_TABNAME
           FROM DD02L
          WHERE TABNAME  =  P_TABNMC
          AND   AS4LOCAL =  'A'
          AND   TABCLASS <> 'INTTAB'
          AND   TABCLASS <> 'VIEW'
          AND   TABCLASS <> 'APPEND'.

  IF SY-SUBRC <> 0.
    "コピー元テーブルIDが存在しません
    MESSAGE TEXT-M04 TYPE 'S'.
    FLG_ERR = C_FLG_ON.
    EXIT.
  ENDIF.


ENDFORM.                    " INDATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  CREATE_SQL
*&---------------------------------------------------------------------*
*       選択条件のSQL作成
*----------------------------------------------------------------------*
FORM CREATE_SQL .
*処理対象データのSQL文作成
REFRESH: TD_SQL.
CLEAR:   W_CONDITION, V_FIELDNAME.

APPEND 'MANDT = SY-MANDT' TO TD_SQL.

*部門コードが項目にある場合
SELECT SINGLE FIELDNAME
  INTO  V_FIELDNAME
  FROM DD03L
  WHERE TABNAME  =  P_TABNMC
  AND FIELDNAME  = 'SPART'
  .

IF SY-SUBRC = 0.
    APPEND 'AND' TO TD_SQL.
    APPEND ' SPART IN S_SPART ' TO TD_SQL.
ENDIF.


*サイトコードが項目にある場合
SELECT SINGLE FIELDNAME
  INTO  V_FIELDNAME
  FROM DD03L
  WHERE TABNAME  =  P_TABNMC
  AND FIELDNAME  = 'WERKS'
  .

IF SY-SUBRC = 0.
    APPEND 'AND' TO TD_SQL.
    APPEND ' WERKS IN S_WERKS ' TO TD_SQL.
ENDIF.

*--> ADD 0001 START 2018/08/08 RS鈴木
*SAP商品コードが項目にある場合
SELECT SINGLE FIELDNAME
  INTO  V_FIELDNAME
  FROM DD03L
  WHERE TABNAME  =  P_TABNMC
  AND FIELDNAME  = 'MATNR'
  .

IF SY-SUBRC = 0.
    APPEND 'AND' TO TD_SQL.
    APPEND ' MATNR IN S_MATNR ' TO TD_SQL.
ENDIF.
*--> ADD 0001 END   2018/08/08 RS鈴木



ENDFORM.                    " CREATE_SQL
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       テーブルデータ取得
*----------------------------------------------------------------------*
FORM GET_DATA .

*-----動的データの作成
  CREATE DATA W_REF_TD TYPE STANDARD TABLE OF (P_TABNMC).
  "動的テーブル名用
  ASSIGN W_REF_TD->* TO <FS_TAB>.

*-----変数の初期化
  REFRESH:
    <FS_TAB>.

  CLEAR:
    W_REF_TD.

*----- 変更レコード抽出
  SELECT *
    INTO TABLE <FS_TAB>
    FROM (P_TABNMC)
    WHERE (TD_SQL)
    .

  IF SY-SUBRC <> 0.
*     処理対象レコードが存在しません
    MESSAGE TEXT-M07 TYPE 'S'.
    FLG_ERR = C_FLG_ON.
    EXIT.
  ENDIF.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  MOD_DATA
*&---------------------------------------------------------------------*
*       テーブルデータ更新
*----------------------------------------------------------------------*
FORM MOD_DATA .

  CLEAR: W_CNT.

*--- テーブルデータ削除
  DELETE FROM (P_TABNM) CLIENT SPECIFIED
    WHERE (TD_SQL).

* リターンコード判別
  "正常終了 OR 対象なし 以外
  IF NOT ( SY-SUBRC = 0 OR SY-SUBRC = 4 ).
      ROLLBACK WORK.
*     削除時エラー
      MESSAGE TEXT-M08 TYPE 'E'.
      EXIT.
  ENDIF.


*--- テーブルデータ登録
  MODIFY (P_TABNM) FROM TABLE <FS_TAB>.

* リターンコード判別
  CASE SY-SUBRC.
*   正常
    WHEN 0.
      COMMIT WORK.
      W_CNT = SY-DBCNT.
*     正常終了しました
      CONCATENATE TEXT-M05 W_CNT TEXT-M06 INTO W_MESSAGE.
      MESSAGE W_MESSAGE TYPE 'S'.
*   異常→処理停止
    WHEN OTHERS.
*     登録時エラー
      MESSAGE TEXT-M09 TYPE 'E'.
      EXIT.
  ENDCASE.

ENDFORM.                    " MOD_DATA