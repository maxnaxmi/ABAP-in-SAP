*&---------------------------------------------------------------------*
*& Report  Z_TABLE_ROCK
*&
*&---------------------------------------------------------------------*
*&  テーブルロック/解除
*&
*&---------------------------------------------------------------------*

REPORT  Z_TABLE_ROCK.

  DATA:  C_LOCK_MODE  TYPE C    VALUE 'E',        "ロックモード書込
         C_TABNAME  TYPE CHAR30 VALUE 'ZPTT0081'. "エラーワーニングテーブル

*----- テーブルロック
  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      MODE_RSTABLE   = C_LOCK_MODE
      TABNAME        = C_TABNAME
      _SCOPE         = '2'
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.
*----- 排他に失敗した場合 メッセージを出力し、処理を終了する
  IF SY-SUBRC <> 0.
    MESSAGE S349(ZC01) WITH TEXT-M01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*----- テーブルロック解除
  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      MODE_RSTABLE = C_LOCK_MODE
      TABNAME      = C_TABNAME.