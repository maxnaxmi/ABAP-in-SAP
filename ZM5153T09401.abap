*----------------------------------------------------------------------*
* PROGRAM ID  : ZPII_Z_SI_ZM5153T09401_R_01~Z_SI_ZM5153T09401_R_01
* NAME        : 年齢管理テーブル(コルモ)登録機能インターフェース(Inbound)
* AUTHOR      : RS　鈴木
* DATE        : 2018/10/01
* DESCRIPTION : 年齢管理マスタ（コルモ）中間テーブル( ZPMT0067_SUB )
*               にデータを格納する
*----------------------------------------------------------------------*
*& 変更履歴
*& LOG#  DATE        AUTHOR     DESCRIPTION
*& 0001  2018/10/01  RS 鈴木　  新規作成
*----------------------------------------------------------------------*
METHOD ZPII_Z_SI_ZM5153T09401_R_01~Z_SI_ZM5153T09401_R_01.
*----------------------------------------------------------------------*
* CONSTANTS定義
*----------------------------------------------------------------------*
  CONSTANTS:
    C_FLG_ON    TYPE FLAG VALUE 'X',         "フラグ等ON
    C_FLG_OFF   TYPE FLAG VALUE SPACE,       "フラグ等OFF
    C_EOM(3)    TYPE C VALUE 'EOM',          "End-Of-File
    C_TAB       TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,

*----- ログ出力用のPID/テキスト設定
*----- 誤設定防止のため、テキストエレメントではなく定数を使用
    C_CPROG     TYPE ZCECPROG     VALUE 'ZM5153T09401',                 "PID
    C_CPROG_TXT TYPE ZCECPROG_TXT VALUE '年齢管理マスタ(コルモ)受信IF'. "TEXT

*----------------------------------------------------------------------*
* TYPES定義
*----------------------------------------------------------------------*
  TYPES:
*----- プロキシ構造(All)
    TYP_LINE_ALL TYPE ZPZ_DT_ZM5153T09401_R_01,
*----- プロキシ構造(データ部)
    TYP_LINE     TYPE ZPZ_DT_ZM5153T09401_R_01_STRUC,

*----- キー重複チェック用構造
    BEGIN OF TYP_DCHK,
      MANDT     TYPE MANDT,                 "CLIENT
      ZIFID     TYPE ZCEIFID,               "IFID
      ZIFCOUNT  TYPE ZCEIFCOUNT,            "IFCOUNT
      ACD01     TYPE ZPMT0067_SUB-ACD01,    "属性内容コード1
      ACD02     TYPE ZPMT0067_SUB-ACD02,    "属性内容コード2
      MDYM      TYPE ZPMT0067_SUB-MDYM,     "MD年度
      MDKIKNKB  TYPE ZPMT0067_SUB-MDKIKNKB, "MD期間区分
      MDKIKNCD  TYPE ZPMT0067_SUB-MDKIKNCD, "MD期間コード
    END   OF TYP_DCHK,

*----- エラーファイル構造
    BEGIN OF TYP_EFILE,
      ZIFID     TYPE ZCEIFID,               "IFID
      ACD01     TYPE ZPMT0067_SUB-ACD01,    "属性内容コード1
      ACD02     TYPE ZPMT0067_SUB-ACD02,    "属性内容コード2
      MDYM      TYPE ZPMT0067_SUB-MDYM,     "MD年度
      MDKIKNKB  TYPE ZPMT0067_SUB-MDKIKNKB, "MD期間区分
      MDKIKNCD  TYPE ZPMT0067_SUB-MDKIKNCD, "MD期間コード
      ZMESSAGE(128) TYPE C,                 "メッセージ
    END   OF TYP_EFILE.

*----------------------------------------------------------------------*
* DATA定義（内部テーブル/内部テーブルヘッダ）
*----------------------------------------------------------------------*
  DATA:
*----- プロキシ構造(ALL)
    TD_LINE_ALL  TYPE STANDARD TABLE OF TYP_LINE_ALL,
    TH_LINE_ALL  LIKE LINE OF TD_LINE_ALL,

*----- プロキシ構造(データ部)
    TD_LINE TYPE STANDARD TABLE OF TYP_LINE,
    TH_LINE LIKE LINE OF TD_LINE,

*----- I/F受信データ(中間テーブルフォーマット)
    TD_INDATA TYPE STANDARD TABLE OF ZPMT0067_SUB,
    TH_INDATA LIKE LINE OF TD_INDATA,

*----- I/F受信データ(中間テーブルフォーマット)重複チェック
    TD_INDATA_DCHK TYPE HASHED TABLE OF TYP_DCHK
                   WITH UNIQUE KEY MANDT ZIFID ZIFCOUNT
                   ACD01 ACD02 MDYM MDKIKNKB MDKIKNCD,
    TH_INDATA_DCHK LIKE LINE OF TD_INDATA_DCHK,

*----- 中間テーブル
    TD_ZPMT0067_SUB TYPE HASHED TABLE OF ZPMT0067_SUB
                   WITH UNIQUE KEY MANDT ZIFID ZIFCOUNT
                   ACD01 ACD02 MDYM MDKIKNKB MDKIKNCD,

*----- エラーファイル用テーブル
    TD_EFILE TYPE STANDARD TABLE OF TYP_EFILE,
    TH_EFILE LIKE LINE OF TD_EFILE,

*----- ファイル出力用
    TD_FILEREC TYPE STANDARD TABLE OF LINE,
    TH_FILEREC LIKE LINE OF TD_FILEREC.
*----------------------------------------------------------------------*
* DATA定義（構造）
*----------------------------------------------------------------------*
  DATA:
    ST_ZCOTIFLOG TYPE ZCOTIFLOG.  "IF起動ログテーブル

*----------------------------------------------------------------------*
* DATA定義（ワーク）
*----------------------------------------------------------------------*
  DATA:
    FLG_DUMMY TYPE FLAG,                 "ダミー変数(使用しない)
    FLG_ERR   TYPE FLAG VALUE C_FLG_OFF, "エラーフラグ

    W_IFID    TYPE ZCEIFID,              "インターフェースID
    W_IFCOUNT TYPE ZCEIFCOUNT,           "インターフェース連番
    W_IFEOM   TYPE ZCEEOM,               "EOM使用可否フラグ
    W_COUNT   TYPE I,                    "レコード数
    W_EOM(3)  TYPE C,                    "EOM

    W_ERRMSG  TYPE STRING,               "エラーメッセージ
    W_SFNAME  TYPE STRING,               "正常ファイル名
    W_EFNAME  TYPE STRING.               "エラーファイル名

*----------------------------------------------------------------------*
* 処理開始
*----------------------------------------------------------------------*
*----- 処理０.IF起動ログ
  PERFORM ZCIO_IFLOG_START IN PROGRAM ZCIO0101
    USING    C_CPROG C_CPROG_TXT
    CHANGING ST_ZCOTIFLOG.

*----- 処理①.プロキシデータの取得
  TH_LINE_ALL = INPUT-Z_MT_ZM5153T09401_R_01.
  TD_LINE[]   = INPUT-Z_MT_ZM5153T09401_R_01-STRUCT1[].

*----- I/FIDとEOMの取得
  W_IFID = TH_LINE_ALL-IFID-IFID.       "I/FID

*----- 処理②.I/FIDのロック処理
  PERFORM ZCIO_IFID_ENQUEUE IN PROGRAM ZCIO0101
                      USING W_IFID
                   CHANGING FLG_ERR
                            FLG_DUMMY
                            W_ERRMSG.

*----- 処理③I/F連番の取得処理
  IF FLG_ERR = C_FLG_OFF.
    PERFORM ZCIO_GET_IFCOUNT IN PROGRAM ZCIO0101
                       USING W_IFID
                    CHANGING W_IFCOUNT
                             W_IFEOM
                             FLG_ERR
                             W_ERRMSG.
  ENDIF.

*----- 処理④ファイル名生成(ERR&SUC)
  PERFORM ZCIO_MAKE_FAILENAME IN PROGRAM ZCIO0101
                        USING W_IFID W_IFCOUNT
                     CHANGING W_SFNAME W_EFNAME.

*----- 処理⑤前処理エラーチェック
  IF FLG_ERR = C_FLG_OFF.
    PERFORM ZCIO_CHECK_PREERR IN PROGRAM ZCIO0101
                        USING W_IFID
                              W_EFNAME
                     CHANGING FLG_ERR.
  ENDIF.

*----- 処理⑥キー重複チェック処理
  IF FLG_ERR = C_FLG_OFF.
    LOOP AT TD_LINE INTO TH_LINE.
      CLEAR TH_INDATA.
      TH_INDATA-ZIFID    = W_IFID.                   "IFID
      TH_INDATA-ZIFCOUNT = W_IFCOUNT.                "IF連番
      TH_INDATA-ACD01    = TH_LINE-ACD01.            "属性内容コード1
      TH_INDATA-ACD02    = TH_LINE-ACD02.            "属性内容コード2
      TH_INDATA-STYM     = TH_LINE-STYM.             "登録値
      TH_INDATA-JIYM     = TH_LINE-JIYM.             "実績年月
      TH_INDATA-MDYM     = TH_LINE-MDYM.             "年
      TH_INDATA-MDKIKNKB = TH_LINE-MDKIKNKB.         "MD期間区分
      TH_INDATA-MDKIKNCD = TH_LINE-MDKIKNCD.         "MD期間コード
      TH_INDATA-MDKIKNNM = TH_LINE-MDKIKNNM.         "MD期間コード名称
      TH_INDATA-MDGMNNM  = TH_LINE-MDGMNNM.          "表示名称
      TH_INDATA-ZTOUSR   = TH_LINE-ZTOUSR.           "MDM登録者ID
      TH_INDATA-ZMIDATE  = TH_LINE-ZMIDATE.          "MDM登録日時
      TH_INDATA-ZMUUSR   = TH_LINE-ZMUUSR.           "MDM更新者ID
      TH_INDATA-ZMUDATE  = TH_LINE-ZMUDATE.          "MDM更新日時
      TH_INDATA-ZCRDAT   = SY-DATUM.                 "登録日付
      TH_INDATA-ZCRTIM   = SY-UZEIT.                 "登録時刻
      TH_INDATA-ZCRUSR   = SY-UNAME.                 "登録者
      TH_INDATA-ZUPDAT   = SY-DATUM.                 "更新日付
      TH_INDATA-ZUPTIM   = SY-UZEIT.                 "更新時刻
      TH_INDATA-ZUPUSR   = SY-UNAME.                 "更新者

      APPEND TH_INDATA TO TD_INDATA.
    ENDLOOP.

*----- 中間テーブルから同一キーデータ取得
    IF TD_INDATA IS NOT INITIAL.
      SELECT *
        FROM ZPMT0067_SUB
        INTO TABLE TD_ZPMT0067_SUB
         FOR ALL ENTRIES IN TD_INDATA
       WHERE ZIFID    = W_IFID              "IFID
         AND ZIFCOUNT = W_IFCOUNT           "IF連番
         AND ACD01    = TD_INDATA-ACD01     "属性内容コード1
         AND ACD02    = TD_INDATA-ACD02     "属性内容コード2
         AND MDYM     = TD_INDATA-MDYM      "MD年度
         AND MDKIKNKB = TD_INDATA-MDKIKNKB  "MD期間区分
         AND MDKIKNCD = TD_INDATA-MDKIKNCD. "MD期間コード

      SORT TD_ZPMT0067_SUB.
    ENDIF.

*----- 処理④(3)キー比較
    LOOP AT TD_INDATA INTO TH_INDATA.
*----- A)同一処理単位の重複チェック
      READ TABLE TD_INDATA_DCHK INTO TH_INDATA_DCHK
        WITH TABLE KEY MANDT    = SY-MANDT            "CLIENT
                       ZIFID    = TH_INDATA-ZIFID     "IFID
                       ZIFCOUNT = TH_INDATA-ZIFCOUNT  "IF連番
                       ACD01    = TH_INDATA-ACD01     "属性内容コード1
                       ACD02    = TH_INDATA-ACD02     "属性内容コード2
                       MDYM     = TH_INDATA-MDYM      "MD年度
                       MDKIKNKB = TH_INDATA-MDKIKNKB  "MD期間区分
                       MDKIKNCD = TH_INDATA-MDKIKNCD. "MD期間コード

*----- 重複有
      IF SY-SUBRC = 0.
        FLG_ERR = C_FLG_ON.                         "エラーフラグon
*----- エラーファイルデータ生成
        TH_EFILE-ZIFID    = TH_INDATA-ZIFID.        "I/FID
        TH_EFILE-ACD01    = TH_INDATA-ACD01.        "属性内容コード1
        TH_EFILE-ACD02    = TH_INDATA-ACD02.        "属性内容コード2
        TH_EFILE-MDYM     = TH_INDATA-MDYM.         "MD年度
        TH_EFILE-MDKIKNKB = TH_INDATA-MDKIKNKB.     "MD期間区分
        TH_EFILE-MDKIKNCD = TH_INDATA-MDKIKNCD.     "MD期間コード
*----- 同一処理内でキー重複があります
        MESSAGE E304(ZC01) INTO TH_EFILE-ZMESSAGE.  "メッセージ
        APPEND TH_EFILE TO TD_EFILE.
        CONTINUE.
*----- 重複無し
      ELSE.
        TH_INDATA_DCHK-MANDT    = SY-MANDT.           "クライント
        TH_INDATA_DCHK-ZIFID    = TH_INDATA-ZIFID.    "I/FID
        TH_INDATA_DCHK-ZIFCOUNT = TH_INDATA-ZIFCOUNT. "I/F連番
        TH_INDATA_DCHK-ACD01    = TH_INDATA-ACD01.    "属性内容コード1
        TH_INDATA_DCHK-ACD02    = TH_INDATA-ACD02.    "属性内容コード2
        TH_INDATA_DCHK-MDYM     = TH_INDATA-MDYM.     "MD年度
        TH_INDATA_DCHK-MDKIKNKB = TH_INDATA-MDKIKNKB. "MD期間区分
        TH_INDATA_DCHK-MDKIKNCD = TH_INDATA-MDKIKNCD. "MD期間コード
        INSERT TH_INDATA_DCHK INTO TABLE TD_INDATA_DCHK.
      ENDIF.

*----- B)同一処理単位の重複チェック(対 中間テーブル)
      READ TABLE TD_ZPMT0067_SUB TRANSPORTING NO FIELDS
        WITH TABLE KEY MANDT    = SY-MANDT
                       ZIFID    = TH_INDATA-ZIFID     "IFID
                       ZIFCOUNT = TH_INDATA-ZIFCOUNT  "IF連番
                       ACD01    = TH_INDATA-ACD01     "属性内容コード1
                       ACD02    = TH_INDATA-ACD02     "属性内容コード2
                       MDYM     = TH_INDATA-MDYM      "MD年度
                       MDKIKNKB = TH_INDATA-MDKIKNKB  "MD期間区分
                       MDKIKNCD = TH_INDATA-MDKIKNCD. "MD期間コード

*----- 重複有
      IF SY-SUBRC = 0.
        FLG_ERR           = C_FLG_ON.             "エラーフラグon
*----- エラーファイルデータ生成
        TH_EFILE-ZIFID    = TH_INDATA-ZIFID.    "I/FID
        TH_EFILE-ACD01    = TH_INDATA-ACD01.    "属性内容コード1
        TH_EFILE-ACD02    = TH_INDATA-ACD02.    "属性内容コード2
        TH_EFILE-MDYM     = TH_INDATA-MDYM.     "MD年度
        TH_EFILE-MDKIKNKB = TH_INDATA-MDKIKNKB. "MD期間区分
        TH_EFILE-MDKIKNCD = TH_INDATA-MDKIKNCD. "MD期間コード
*----- 同一処理内でキー重複があります
        MESSAGE E304(ZC01) INTO TH_EFILE-ZMESSAGE. "メッセージ
        APPEND TH_EFILE TO TD_EFILE.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.

*----- 処理⑦中間テーブル更新処理
  IF FLG_ERR = C_FLG_OFF.
    MODIFY ZPMT0067_SUB FROM TABLE TD_INDATA.
*----- エラー処理
    IF SY-SUBRC <> 0.
      FLG_ERR = C_FLG_ON.   "エラーフラグon
*----- テーブル &1 の &2 に失敗しました(RC=:&3)
      MESSAGE E306(ZC01)
        WITH TEXT-M01       "年齢管理マスタ（コルモ）中間テーブル(ZPMT0067_SUB)
             TEXT-M02       "更新
             SY-SUBRC       "リターンコード
        INTO W_ERRMSG.
*----- 正常処理
    ELSE.
      COMMIT WORK.          "コミット
*----- 正常ファイル作成
      PERFORM ZCIO_MAKE_SFILE IN PROGRAM ZCIO0101 USING W_IFID
                                                        W_SFNAME.
    ENDIF.
  ENDIF.

*----- 処理⑧エラー処理
  IF FLG_ERR = C_FLG_ON.
*----- エラーファイルの作成
    LOOP AT TD_EFILE INTO TH_EFILE.
*----- 項目見出し作成
      AT FIRST.
        CONCATENATE TEXT-F01   "I/FID
                    TEXT-F02   "属性内容コード1
                    TEXT-F03   "属性内容コード2
                    TEXT-F04   "MD年度
                    TEXT-F05   "MD期間区分
                    TEXT-F06   "MD期間コード
                    TEXT-F07   "メッセージ
           INTO TH_FILEREC SEPARATED BY C_TAB.
        APPEND TH_FILEREC TO TD_FILEREC.
      ENDAT.
*----- 項目内容設定
      CONCATENATE TH_EFILE-ZIFID
                  TH_EFILE-ACD01
                  TH_EFILE-ACD02
                  TH_EFILE-MDYM
                  TH_EFILE-MDKIKNKB
                  TH_EFILE-MDKIKNCD
                  TH_EFILE-ZMESSAGE
             INTO TH_FILEREC SEPARATED BY C_TAB.
      APPEND TH_FILEREC TO TD_FILEREC.
    ENDLOOP.

*----- ファイルレベルのエラーメッセージ追記
    IF NOT W_ERRMSG IS INITIAL.
      APPEND W_ERRMSG TO TD_FILEREC.
    ENDIF.

*----- エラーファイル作成
    PERFORM ZCIO_MAKE_EFILE IN PROGRAM ZCIO0101
                     TABLES TD_FILEREC
                      USING W_IFID
                            W_EFNAME.

*----- 正常ファイル削除処理
    PERFORM ZCIO_DELETE_SFILE IN PROGRAM ZCIO0101
                        USING W_IFID
                              W_SFNAME.

*----- 正常レコード削除(中間TBL)処理
    SELECT COUNT( * )
      FROM ZPMT0067_SUB
      INTO W_COUNT
     WHERE ZIFID    = W_IFID
       AND ZIFCOUNT = W_IFCOUNT.
    IF SY-SUBRC = 0.
      DELETE FROM ZPMT0067_SUB WHERE ZIFID    = W_IFID
                                 AND ZIFCOUNT = W_IFCOUNT.
      COMMIT WORK.
    ENDIF.
  ENDIF.

*----- 処理⑨終了処理
  IF W_IFEOM = C_FLG_OFF    "EOM対象IFではない場合
    OR W_EOM = C_EOM.       "または、EOM対象で、EOMが送信されてきた場合
*----- 処理⑨(1)正常ファイルリネーム処理
    PERFORM ZCIO_REN_FILE_NAME IN PROGRAM ZCIO0101
                         USING W_IFID
                               W_SFNAME.

*----- 処理⑨(2)異常ファイルリネーム処理
    PERFORM ZCIO_REN_FILE_NAME IN PROGRAM ZCIO0101
                         USING W_IFID
                               W_EFNAME.

*----- 処理⑨(3)IF連番インクリメント処理
    PERFORM ZCIO_SET_IFCOUNT IN PROGRAM ZCIO0101
                       USING W_IFID.
  ENDIF.

*----- 処理⑩ロック解除処理
  PERFORM ZCIO_IFID_DEQUEUE IN PROGRAM ZCIO0101
                      USING W_IFID.

*----- IF終了ログ
  PERFORM ZCIO_IFLOG_END IN PROGRAM ZCIO0101
    CHANGING ST_ZCOTIFLOG.

ENDMETHOD.