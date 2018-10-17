*&---------------------------------------------------------------------*
*& Report  Z_TABLE_LOCK
*&
*&---------------------------------------------------------------------*
*&  テーブルロック/解除
*&
*&---------------------------------------------------------------------*

REPORT  Z_TABLE_LOCK.

PARAMETERS:
      P_ITEM TYPE CHAR12.                       "入力値(テーブル項目)

*----- データ定義
CONSTANTS:
  C_FLG_ON  TYPE FLAG VALUE 'X',                "フラグ等ON
  C_FLG_OFF TYPE FLAG VALUE SPACE,              "フラグ等OFF
  C_ENQUEUE_MODE_WRITE TYPE ENQMODE VALUE 'E',  "ロックモード(書込)
  C_SCOPE_NOUPD(1) TYPE C VALUE '1'.            "更新プログラム非依存

DATA:
  W_MSGV1    TYPE SY-MSGV1,
  W_ITEM(30) TYPE C,
  O_MSG      TYPE STRING.                       "エラーメッセージ


W_ITEM = P_ITEM.

*----- ロック処理
CALL FUNCTION 'ENQUEUE_E_TABLE'
  EXPORTING
    MODE_RSTABLE   = C_ENQUEUE_MODE_WRITE
    TABNAME        = W_ITEM
    _SCOPE         = C_SCOPE_NOUPD
  EXCEPTIONS
    FOREIGN_LOCK   = 1
    SYSTEM_FAILURE = 2
    OTHERS         = 3.

*----- エラー処理
IF SY-SUBRC = 1.
  W_MSGV1 = SY-MSGV1.
*   メッセージ生成
*   ITEM &1 は ユーザ &2 にロックされています
  MESSAGE S302(ZC01)
    WITH P_ITEM W_MSGV1.
ELSEIF SY-SUBRC <> 0.
*   メッセージ生成
*   I/FID &1 のロックに失敗しました
  MESSAGE S301(ZC01)
    WITH P_ITEM.
ELSE.
*   メッセージ生成
*   ユーザロック/ロック解除 処理が終了しました
  MESSAGE S012(ZC01).
ENDIF.

WRITE :/ O_MSG .

*----- ロック解除処理
CALL FUNCTION 'DEQUEUE_E_TABLE'
  EXPORTING
    MODE_RSTABLE = C_ENQUEUE_MODE_WRITE
    TABNAME      = W_ITEM
  EXCEPTIONS
    OTHERS       = 1.


* ※ロックエントリの確認 -> SM12