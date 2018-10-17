*----------------------------------------------------------------------*
* PROGRAM ID  : ZPIM3301
* NAME        : 年齢管理テーブル登録機能
* AUTHOR      : RS 鈴木
* DATE        : 2018/10/01
* DESCRIPTION : 中間テーブル ZPMT0067_SUBから標準T094_年齢管理テーブル
*               (ZPMT0067)へデータを更新する機能
*----------------------------------------------------------------------*
REPORT  ZPIM3301 NO STANDARD PAGE HEADING
                             LINE-SIZE  170
                             LINE-COUNT  58.

*----------------------------------------------------------------------*
* CONSTANTS定義
*----------------------------------------------------------------------*
CONSTANTS:
  C_FLG_ON  TYPE FLAG VALUE 'X',           "フラグ等ON
  C_FLG_OFF TYPE FLAG VALUE SPACE,         "フラグ等OFF

  C_INMSG_JOBSTART(4) TYPE N VALUE 10,     "JOBLOG_MESSAGE : ジョブ開始
  C_INMSG_ENQUEUE(4)  TYPE N VALUE 20,     "JOBLOG_MESSAGE : ロック
  C_INMSG_GETFILE(4)  TYPE N VALUE 30,     "JOBLOG_MESSAGE : ファイル取得
  C_INMSG_GETREC(4)   TYPE N VALUE 40,     "JOBLOG_MESSAGE : レコード登録
  C_INMSG_UPD(4)      TYPE N VALUE 50,     "JOBLOG_MESSAGE : 更新
  C_INMSG_UPDCNT(4)   TYPE N VALUE 55,     "JOBLOG_MESSAGE : 更新件数
  C_INMSG_UPDSUB(4)   TYPE N VALUE 60,     "JOBLOG_MESSAGE : 更新(中間TBL)
  C_INMSG_DELFILE(4)  TYPE N VALUE 70,     "JOBLOG_MESSAGE : ファイル削除
  C_INMSG_REPORT(4)   TYPE N VALUE 80,     "JOBLOG_MESSAGE : レポート
  C_INMSG_REPCNT(4)   TYPE N VALUE 85,     "JOBLOG_MESSAGE : レポート(件数)
  C_INMSG_JOBEND(4)   TYPE N VALUE 90,     "JOBLOG_MESSAGE : ジョブ終了
  C_INMSG_VAR_DUMMY   TYPE I VALUE 0,      "メッセージ用ダミー引数
  C_MSGCOUNT          TYPE I VALUE 1000,   "更新件数出力タイミング
  C_STATUS_YET        TYPE ZCESTATUS VALUE SPACE,
                                           "処理ステータス : 未処理
  C_STATUS_SUC        TYPE ZCESTATUS VALUE 'S',   "処理ステータス : 正常
  C_STATUS_ERR        TYPE ZCESTATUS VALUE 'E',   "処理ステータス : エラー
  C_INFVAL_ZIFID      TYPE STRING VALUE 'I/FID',
                                          "項目名称 : I/FID
  C_INFVAL_ACD01      TYPE STRING VALUE '属性内容コード１',
                                          "項目名称 : 属性内容コード１
  C_INFVAL_ACD02      TYPE STRING VALUE '属性内容コード２',
                                          "項目名称 : 属性内容コード２
  C_INFVAL_MDKIKNCD   TYPE STRING VALUE 'MD期間コード'.
"項目名称 : MD期間コード

*----------------------------------------------------------------------*
* DATA定義（内部テーブル/内部テーブルヘッダ）
*----------------------------------------------------------------------*
DATA:
*----- I/FID管理TBL
  TD_IFID TYPE STANDARD TABLE OF ZCOTIFID, "I/FID管理
  TH_IFID LIKE LINE OF TD_IFID,            "I/FID管理

*----- 中間テーブル
  TD_ZPMT0067_SUB TYPE STANDARD TABLE OF ZPMT0067_SUB,
  TH_ZPMT0067_SUB LIKE LINE OF TD_ZPMT0067_SUB,

*----- 中間テーブル(比較用)
  TD_ZPMT0067_SUB_TMP TYPE STANDARD TABLE OF ZPMT0067_SUB,
  TH_ZPMT0067_SUB_TMP LIKE LINE OF TD_ZPMT0067_SUB,

*----- エラーリスト用(エラーメッセージ複数行対応)
  TD_LIST TYPE STANDARD TABLE OF ZPMT0067_SUB,
  TH_LIST LIKE LINE OF TD_ZPMT0067_SUB,

*----- ERPテーブル
  TD_ZPMT0067 TYPE STANDARD TABLE OF ZPMT0067,
  TH_ZPMT0067 LIKE LINE OF TD_ZPMT0067.

*----------------------------------------------------------------------*
* DATA定義（ワーク）
*----------------------------------------------------------------------*
DATA:
  FLG_STOP      TYPE FLAG,                 "処理終了フラグ
  FLG_ERR       TYPE FLAG VALUE C_FLG_OFF, "エラーフラグ
  W_SUBRC       TYPE C,                    "SUBRCの値
  W_ERRMSG      TYPE STRING,               "エラーメッセージ
  W_IFID        TYPE ZCEIFID,              "インターフェースｲD
  W_MAXREC      TYPE I,                    "itab最大行
  W_CURRENT_IDX TYPE I,                    "現行INDEX
  W_NEXT_IDX    TYPE I,                    "次行INDEX

*-----counter
  CTR_MOD TYPE I,                          "メッセージ出力タイミング
  CTR_ALL TYPE I,                          "全件
  CTR_SUC TYPE I,                          "正常
  CTR_ERR TYPE I.                          "異常

*----------------------------------------------------------------------*
* PARAMETERS・SELECT-OPTIONS定義
*----------------------------------------------------------------------*
PARAMETERS:
  P_IFID TYPE ZCEIFID OBLIGATORY.       "I/FID

*----------------------------------------------------------------------*
* INCLUDE定義
*----------------------------------------------------------------------*
INCLUDE:
  ZCCO0101.                             " 標準共通機能

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
*----- ジョブログ出力 ジョブ開始
  PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                         USING C_INMSG_JOBSTART
                               W_IFID
                               C_INMSG_VAR_DUMMY
                               C_INMSG_VAR_DUMMY
                               C_INMSG_VAR_DUMMY
                               FLG_ERR.
*----- I/FIDの取得
  W_IFID = P_IFID.


*----- I/FIDのロック処理
  PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                         USING C_INMSG_ENQUEUE
                               W_IFID
                               C_INMSG_VAR_DUMMY
                               C_INMSG_VAR_DUMMY
                               C_INMSG_VAR_DUMMY
                               FLG_ERR.
  PERFORM ZCIO_IFID_ENQUEUE IN PROGRAM ZCIO0101
                      USING W_IFID
                   CHANGING FLG_ERR
                            FLG_STOP
                            W_ERRMSG.

*----- 正常ファイル終了ファイル名取得
  IF FLG_STOP = C_FLG_OFF.
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_GETFILE
                                 W_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.
    PERFORM ZCIO_GET_SFILE_NAME IN PROGRAM ZCIO0101
                         TABLES TD_IFID
                          USING W_IFID
                       CHANGING FLG_ERR
                                FLG_STOP.
  ENDIF.

*----- 処理対象レコード取得
  IF FLG_STOP = C_FLG_OFF.
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_GETREC
                                 W_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.

    PERFORM GET_INDATA.
  ENDIF.

*----- 年齢管理テーブル登録
  IF FLG_STOP = C_FLG_OFF.
*----- 実マスタ更新
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                          USING C_INMSG_UPD
                                W_IFID
                                C_INMSG_VAR_DUMMY
                                C_INMSG_VAR_DUMMY
                                C_INMSG_VAR_DUMMY
                                FLG_ERR.
    PERFORM UPDATE_MASTER.
  ENDIF.

*----- 中間テーブル更新
  IF FLG_STOP = C_FLG_OFF.
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_UPDSUB
                                 W_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.
    PERFORM UPDATE_SUB_TABLE.
  ENDIF.

*----- 正常ファイル削除処理
  IF FLG_STOP = C_FLG_OFF.
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_DELFILE
                                 W_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.
    PERFORM DELETE_FILES.
  ENDIF.

*----- 処理結果リスト出力
  IF FLG_STOP = C_FLG_OFF.
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_REPORT
                                 W_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.

    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_REPCNT
                                 W_IFID
                                 CTR_ALL
                                 CTR_SUC
                                 CTR_ERR
                                 FLG_ERR.

    PERFORM OUTPUT_REPORT.
  ENDIF.

*----- I/FIDのロック解除処理
  PERFORM ZCIO_IFID_DEQUEUE IN PROGRAM ZCIO0101
                      USING W_IFID.

*----- ジョブログ出力
  PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                         USING C_INMSG_JOBEND
                               W_IFID
                               C_INMSG_VAR_DUMMY
                               C_INMSG_VAR_DUMMY
                               C_INMSG_VAR_DUMMY
                               FLG_ERR.

*----------------------------------------------------------------------*
* TOP-OF-PAGE
*----------------------------------------------------------------------*
TOP-OF-PAGE.
*----- 帳票ヘッダ出力
  PERFORM OUTPUT_REPORT_HEADER.

*&---------------------------------------------------------------------*
*&      Form  GET_INDATA
*&---------------------------------------------------------------------*
*       処理対象レコード取得
*----------------------------------------------------------------------*
FORM GET_INDATA .

*----- 中間テーブル取得
  SELECT *
    FROM ZPMT0067_SUB
    INTO TABLE TD_ZPMT0067_SUB
     FOR ALL ENTRIES IN TD_IFID
   WHERE ZIFID    = TD_IFID-ZIFID       "I/FID
    AND ZIFCOUNT  = TD_IFID-ZIFCOUNT    "I/FID連番
    AND ZIFSTATUS = C_STATUS_YET.       "ステータス未処理

*----- エラー処理
  IF SY-SUBRC <> 0.
*----- 処理中止フラグ
    FLG_STOP = C_FLG_ON.
*----- メッセージ出力
    MESSAGE S001(ZC01) WITH TEXT-M01
                            TEXT-M02.
  ENDIF.

ENDFORM.                    " GET_INDATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MASTER
*&---------------------------------------------------------------------*
*       マスタ更新処理
*----------------------------------------------------------------------*
FORM UPDATE_MASTER .
*----- データ定義
  DATA:
    LFLG_ERR_TMP TYPE FLAG,   "ローカルエラーフラグ
    LW_ERR_MSG   TYPE STRING. "エラーメッセージ

*----- ITAB行数取得
  W_MAXREC = LINES( TD_ZPMT0067_SUB ).

*----- SORT処理
  SORT TD_ZPMT0067_SUB BY ZIFID ACD01 ACD02  MDYM MDKIKNKB MDKIKNCD ZIFCOUNT.

*----- 比較用tabコピー
  TD_ZPMT0067_SUB_TMP = TD_ZPMT0067_SUB.

*----- ERPテーブルにある既存データを全件削除する
  DELETE FROM ZPMT0067.

*----- 全件エラーが発生したとき
  IF SY-SUBRC <> 0 AND SY-SUBRC <> 4.
*----- 処理中止フラグon
    FLG_STOP = C_FLG_ON.
*----- エラーフラグon
    FLG_ERR = C_FLG_ON.
    MESSAGE S328(ZC01) WITH TEXT-M03 SY-SUBRC.
  ENDIF.

  IF FLG_STOP = C_FLG_OFF.
*----- 処理対象レコード全件処理
    LOOP AT TD_ZPMT0067_SUB INTO TH_ZPMT0067_SUB.
      W_CURRENT_IDX = SY-TABIX.

*----- 処理中の件数出力
      CTR_MOD = CTR_ALL MOD C_MSGCOUNT.
      IF CTR_MOD = 0 AND CTR_ALL <> 0.
        PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                               USING C_INMSG_UPDCNT
                                     W_IFID
                                     CTR_ALL
                                     CTR_SUC
                                     CTR_ERR
                                     FLG_ERR.
      ENDIF.

*----- count全件
      CTR_ALL = CTR_ALL + 1.

*----- 重複レコードチェック
*----- キー比較のため次行読込み
      W_NEXT_IDX = SY-TABIX + 1.
      IF W_NEXT_IDX > W_MAXREC.
        CLEAR TH_ZPMT0067_SUB_TMP.
      ELSE.
        READ TABLE TD_ZPMT0067_SUB_TMP INTO TH_ZPMT0067_SUB_TMP
          INDEX W_NEXT_IDX.
      ENDIF.

*----- 次行とのキー比較
      IF  TH_ZPMT0067_SUB-ZIFID    = TH_ZPMT0067_SUB_TMP-ZIFID
      AND TH_ZPMT0067_SUB-ACD01    = TH_ZPMT0067_SUB_TMP-ACD01
      AND TH_ZPMT0067_SUB-ACD02    = TH_ZPMT0067_SUB_TMP-ACD02
      AND TH_ZPMT0067_SUB-MDYM     = TH_ZPMT0067_SUB_TMP-MDYM
      AND TH_ZPMT0067_SUB-MDKIKNKB = TH_ZPMT0067_SUB_TMP-MDKIKNKB
      AND TH_ZPMT0067_SUB-MDKIKNCD = TH_ZPMT0067_SUB_TMP-MDKIKNCD.

*----- 次行と同一キーの場合、上書きされた扱いにする
        TH_ZPMT0067_SUB-ZIFSTATUS = C_STATUS_SUC.
        MESSAGE S002(ZC01) WITH SY-DATUM SY-UZEIT
          INTO TH_ZPMT0067_SUB-ZIFMESSAGE.
        MODIFY TD_ZPMT0067_SUB FROM TH_ZPMT0067_SUB
         INDEX W_CURRENT_IDX
         TRANSPORTING ZIFSTATUS ZIFMESSAGE.

*----- count正常
        CTR_SUC = CTR_SUC + 1.
*----- 次レコード
        CONTINUE.
      ENDIF.

*----- 必須項目チェック
*----- インターフェースID（ZIFID）必須チェック
      PERFORM ZCIO_CHECK_FIELD_NOTNULL IN PROGRAM ZCIO0101
                                 USING TH_ZPMT0067_SUB-ZIFID
                                       C_INFVAL_ZIFID
                              CHANGING LFLG_ERR_TMP
                                       LW_ERR_MSG.
      IF LFLG_ERR_TMP = C_FLG_ON.
*----- エラーフラグ
        FLG_ERR = C_FLG_ON.

        TH_ZPMT0067_SUB-ZIFSTATUS = C_STATUS_ERR.
        TH_ZPMT0067_SUB-ZIFMESSAGE = LW_ERR_MSG.
        MODIFY TD_ZPMT0067_SUB FROM TH_ZPMT0067_SUB
               INDEX W_CURRENT_IDX
               TRANSPORTING ZIFSTATUS ZIFMESSAGE.
*----- 結果リストITAB作成
        TH_LIST            = TH_ZPMT0067_SUB.
        TH_LIST-ZIFSTATUS  = C_STATUS_ERR.
        TH_LIST-ZIFMESSAGE = LW_ERR_MSG.
        APPEND TH_LIST TO TD_LIST.
*----- countエラー
        CTR_ERR = CTR_ERR + 1.
        CONTINUE.
      ENDIF.

*----- 数値項目チェック
*----- ①属性内容コード１(ZPEACD01)数値項目チェック
      PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                                USING TH_ZPMT0067_SUB-ACD01
                                      C_INFVAL_ACD01
                             CHANGING LFLG_ERR_TMP
                                      LW_ERR_MSG.
*----- 数値型エラーフラグがONの場合
      IF LFLG_ERR_TMP = C_FLG_ON.
*----- エラーフラグon
        FLG_ERR = C_FLG_ON.

        TH_ZPMT0067_SUB-ZIFSTATUS  = C_STATUS_ERR.
        TH_ZPMT0067_SUB-ZIFMESSAGE = LW_ERR_MSG.
        MODIFY TD_ZPMT0067_SUB FROM TH_ZPMT0067_SUB
               INDEX W_CURRENT_IDX
               TRANSPORTING ZIFSTATUS ZIFMESSAGE.

*----- 結果リストITAB作成
        TH_LIST            = TH_ZPMT0067_SUB.
        TH_LIST-ZIFSTATUS  = C_STATUS_ERR.
        TH_LIST-ZIFMESSAGE = LW_ERR_MSG.
        APPEND TH_LIST TO TD_LIST.
*----- count異常
        CTR_ERR = CTR_ERR + 1.
*----- 次レコード
        CONTINUE.
      ENDIF.

*----- ②属性内容コード２(ZPEACD02)数値項目チェック
      PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                                USING TH_ZPMT0067_SUB-ACD02
                                      C_INFVAL_ACD02
                             CHANGING LFLG_ERR_TMP
                                      LW_ERR_MSG.
*----- 数値型エラーフラグがONの場合
      IF LFLG_ERR_TMP = C_FLG_ON.
*----- エラーフラグon
        FLG_ERR = C_FLG_ON.

        TH_ZPMT0067_SUB-ZIFSTATUS  = C_STATUS_ERR.
        TH_ZPMT0067_SUB-ZIFMESSAGE = LW_ERR_MSG.
        MODIFY TD_ZPMT0067_SUB FROM TH_ZPMT0067_SUB
               INDEX W_CURRENT_IDX
               TRANSPORTING ZIFSTATUS ZIFMESSAGE.

*----- 結果リストITAB作成
        TH_LIST            = TH_ZPMT0067_SUB.
        TH_LIST-ZIFSTATUS  = C_STATUS_ERR.
        TH_LIST-ZIFMESSAGE = LW_ERR_MSG.
        APPEND TH_LIST TO TD_LIST.
*----- count異常
        CTR_ERR = CTR_ERR + 1.
*----- 次レコード
        CONTINUE.
      ENDIF.

*----- ③MD期間コード(MDKIKNCD)数値項目チェック
      PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                                USING TH_ZPMT0067_SUB-MDKIKNCD
                                      C_INFVAL_MDKIKNCD
                             CHANGING LFLG_ERR_TMP
                                      LW_ERR_MSG.
*----- 数値型エラーフラグがONの場合
      IF LFLG_ERR_TMP = C_FLG_ON.
*----- エラーフラグon
        FLG_ERR = C_FLG_ON.

        TH_ZPMT0067_SUB-ZIFSTATUS  = C_STATUS_ERR.
        TH_ZPMT0067_SUB-ZIFMESSAGE = LW_ERR_MSG.
        MODIFY TD_ZPMT0067_SUB FROM TH_ZPMT0067_SUB
               INDEX W_CURRENT_IDX
               TRANSPORTING ZIFSTATUS ZIFMESSAGE.

*----- 結果リストITAB作成
        TH_LIST            = TH_ZPMT0067_SUB.
        TH_LIST-ZIFSTATUS  = C_STATUS_ERR.
        TH_LIST-ZIFMESSAGE = LW_ERR_MSG.
        APPEND TH_LIST TO TD_LIST.
*----- count異常
        CTR_ERR = CTR_ERR + 1.
*----- 次レコード
        CONTINUE.
      ENDIF.
*----- エラーフラグがOFFの場合
      IF LFLG_ERR_TMP = C_FLG_OFF.
        TH_ZPMT0067-ACD01    = TH_ZPMT0067_SUB-ACD01.     "属性内容コード１
        TH_ZPMT0067-ACD02    = TH_ZPMT0067_SUB-ACD02.     "属性内容コード２
        TH_ZPMT0067-STYM     = TH_ZPMT0067_SUB-STYM.      "登録値
        TH_ZPMT0067-JIYM     = TH_ZPMT0067_SUB-JIYM.      "実年月
        TH_ZPMT0067-MDYM     = TH_ZPMT0067_SUB-MDYM.      "MD年度
        TH_ZPMT0067-MDKIKNKB = TH_ZPMT0067_SUB-MDKIKNKB.  "MD期間区分
        TH_ZPMT0067-MDKIKNCD = TH_ZPMT0067_SUB-MDKIKNCD.  "MD期間コード
        TH_ZPMT0067-MDKIKNNM = TH_ZPMT0067_SUB-MDKIKNNM.  "MD期間コード名称
        TH_ZPMT0067-MDGMNNM  = TH_ZPMT0067_SUB-MDGMNNM.   "画面表示用名称
        TH_ZPMT0067-ZTOUSR   = TH_ZPMT0067_SUB-ZTOUSR.    "MDM登録者ID
        TH_ZPMT0067-ZMIDATE  = TH_ZPMT0067_SUB-ZMIDATE.   "MDM登録日時
        TH_ZPMT0067-ZMUUSR   = TH_ZPMT0067_SUB-ZMUUSR.    "MDM更新者ID
        TH_ZPMT0067-ZMUDATE  = TH_ZPMT0067_SUB-ZMUDATE.   "MDM更新日時
        TH_ZPMT0067-ZCRDAT   = SY-DATUM.                  "登録日付
        TH_ZPMT0067-ZCRTIM   = SY-UZEIT.                  "登録時刻
        TH_ZPMT0067-ZCRUSR   = SY-UNAME.                  "登録者
        TH_ZPMT0067-ZUPDAT   = SY-DATUM.                  "更新日付
        TH_ZPMT0067-ZUPTIM   = SY-UZEIT.                  "更新時刻
        TH_ZPMT0067-ZUPUSR   = SY-UNAME.                  "更新者

        INSERT INTO ZPMT0067 VALUES TH_ZPMT0067.
        W_SUBRC = SY-SUBRC.

*----- エラー処理
        IF SY-SUBRC <> 0.
*  ----- エラーフラグon
          FLG_ERR = C_FLG_ON.
          TH_ZPMT0067_SUB-ZIFSTATUS = C_STATUS_ERR.
          MESSAGE E320(ZC01) WITH TEXT-M03
                                  W_SUBRC
                             INTO TH_ZPMT0067_SUB-ZIFMESSAGE.

          MODIFY TD_ZPMT0067_SUB FROM TH_ZPMT0067_SUB
                                INDEX W_CURRENT_IDX
                                TRANSPORTING ZIFSTATUS ZIFMESSAGE.

*  ----- 結果リストITAB作成
          TH_LIST            = TH_ZPMT0067_SUB.
          TH_LIST-ZIFSTATUS  = C_STATUS_ERR.
          TH_LIST-ZIFMESSAGE = TH_ZPMT0067_SUB-ZIFMESSAGE.
          APPEND TH_LIST TO TD_LIST.
*  ----- countエラー
          CTR_ERR = CTR_ERR + 1.
*  ----- 正常終了
        ELSE.
          TH_ZPMT0067_SUB-ZIFSTATUS = C_STATUS_SUC.
          MESSAGE S003(ZC01) WITH SPACE
                                  SPACE
                                  SPACE
                             INTO TH_ZPMT0067_SUB-ZIFMESSAGE.
          MODIFY TD_ZPMT0067_SUB FROM TH_ZPMT0067_SUB
                                INDEX W_CURRENT_IDX
                         TRANSPORTING ZIFSTATUS
                                      ZIFMESSAGE.

*  ----- count正常
          CTR_SUC = CTR_SUC + 1.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " UPDATE_MASTER
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SUB_TABLE
*&---------------------------------------------------------------------*
*       中間テーブル更新処理
*----------------------------------------------------------------------*

FORM UPDATE_SUB_TABLE .
*----- アドオン受信テーブル更新
  UPDATE ZPMT0067_SUB FROM TABLE TD_ZPMT0067_SUB.
  COMMIT WORK.

ENDFORM.                    " UPDATE_SUB_TABLE
*&---------------------------------------------------------------------*
*&      Form  DELETE_FILES
*&---------------------------------------------------------------------*
*       正常終了ファイル削除処理
*----------------------------------------------------------------------*
FORM DELETE_FILES .

  DATA:
    LW_COUNT TYPE I.

*----- 全件終了していたら正常ファイルを削除
  LOOP AT TD_IFID INTO TH_IFID.

    SELECT COUNT(*) FROM ZPMT0067_SUB
      INTO LW_COUNT
     WHERE ZIFID     = TH_IFID-ZIFID
       AND ZIFCOUNT  = TH_IFID-ZIFCOUNT
       AND ZIFSTATUS = C_STATUS_YET.

    IF SY-SUBRC <> 0.
      PERFORM ZCIO_DELETE_SFILE_MAIN IN PROGRAM ZCIO0101
        USING TH_IFID-ZIFID TH_IFID-ZIFCOUNT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DELETE_FILES
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_REPORT
*&---------------------------------------------------------------------*
*       処理結果リスト出力
*----------------------------------------------------------------------*
FORM OUTPUT_REPORT .

*----- 処理結果リスト出力
  LOOP AT TD_LIST INTO TH_LIST.

    WRITE:
       01 TH_LIST-ZIFMESSAGE.  "エラーメッセージ

  ENDLOOP.

ENDFORM.                    " OUTPUT_REPORT
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_REPORT_HEADER
*&---------------------------------------------------------------------*
*       ヘッダ出力処理
*----------------------------------------------------------------------*
FORM OUTPUT_REPORT_HEADER .
*----- 共通ヘッダ出力
  PERFORM INC_COMMON_HEADER_WRITE USING TEXT-R01 W_IFID.
*----- 機能固有ヘッダ出力
  PERFORM HEADER_WRITE.

ENDFORM.                    " OUTPUT_REPORT_HEADER
*&---------------------------------------------------------------------*
*&      Form  HEADER_WRITE
*&---------------------------------------------------------------------*
*       機能固有ヘッダ出力
*----------------------------------------------------------------------*
FORM HEADER_WRITE .
*----- 処理件数出力
  WRITE:
    /01 TEXT-R02, 12 CTR_ALL, 23 TEXT-R06,      "処理件数
    /01 TEXT-R03, 12 CTR_SUC, 23 TEXT-R06,      "正常件数
    /01 TEXT-R04, 12 CTR_ERR, 23 TEXT-R06.      "異常件数

*----- 項目見出し行
  SKIP 1.

  WRITE:
    /01 TEXT-R05.  "エラーメッセージ

  ULINE.

ENDFORM.                    " HEADER_WRITE