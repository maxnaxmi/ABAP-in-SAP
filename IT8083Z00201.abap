*----------------------------------------------------------------------*
* PROGRAM ID  : ZPII_Z_SI_IT8053Z00201_R_01~Z_SI_IT8053Z00201_R_01
* NAME        : 本部発注登録ABAPプロキシ(Inbound)
* AUTHOR      : RS 鈴木
* DATE        : 2018/10/31
* DESCRIPTION : ACMSサーバから、PI経由で予約販売データを受領し、
*               本部発注中間テーブル（ZPTT0049_SUB）へ取込む。
*----------------------------------------------------------------------*
*& 変更履歴
*& LOG#  DATE        AUTHOR     DESCRIPTION
*
*----------------------------------------------------------------------*
METHOD ZPII_Z_SI_IT8053Z00201_R_01~Z_SI_IT8053Z00201_R_01.

*----------------------------------------------------------------------*
* CONSTANTS定義
*----------------------------------------------------------------------*
  CONSTANTS:
    C_FLG_ON         TYPE FLAG VALUE 'X',                                  "フラグ等ON
    C_FLG_OFF        TYPE FLAG VALUE SPACE,                                "フラグ等OFF
    C_STATUS_ERR     TYPE ZCESTATUS VALUE 'E',                             "処理異常
    C_EOM(3)         TYPE C VALUE 'EOM',                                   "End-Of-File
    C_LENGTH_4       TYPE I VALUE 4,                        "データの長さ4
    C_CPROG          TYPE ZCECPROG   VALUE 'IT8053Z00201',                 "PID
    C_CPROG_TXT      TYPE ZCECPROG_TXT VALUE '本部発注登録(予約発注)I/F',  "TEXT
    C_JOBDATE        TYPE RVARI_VNAM VALUE 'Z_JOBDATE_DAY',                "処理日取得変数名
    C_KIKAKU_NO      TYPE CHAR1 VALUE '1',                                 "企画No
    C_SAIYO_GENKA    TYPE CHAR1 VALUE '3',                                 "採用原価区分
    C_GENKA_KAKE     TYPE CHAR3 VALUE '000',                               "原価掛率
    C_BIKUB          TYPE CHAR1 VALUE '1',                                 "便区分
    C_HANCD_YOYAK(5) TYPE C VALUE 'YOYAK',                                 "汎用コード(商品コード変換)

* レンジ構造用定数
    C_SIGN_I(1)   TYPE C VALUE 'I',                                       "レンジテーブル（真）
    C_OPT_EQ(2)   TYPE C VALUE 'EQ'.                                      "レンジテーブル（同値）

*----------------------------------------------------------------------*
* TYPES定義
*----------------------------------------------------------------------*
  TYPES:
*   プロキシ構造
*   プロキシ構造(All)
    TYP_LINE_ALL TYPE ZPZ_DT_IT8053Z00201_R_01,
*   プロキシ構造(データ部)
    TYP_LINE     TYPE ZPZ_DT_IT8053Z00201_R_01_STRUC,
"   ファイル出力用構造
    TYP_FILEREC TYPE STRING.
*   共通バリアントテーブル用(ジョブ処理日）
  TYPES: BEGIN OF TYP_TVARVC,
           LOW  TYPE TVARVC-LOW,          "条件
           HIGH TYPE TVARVC-HIGH,         "結果
         END OF TYP_TVARVC,

*   複数POSマスタテーブル構造
         BEGIN OF TYP_ZPMT0003,
           MATNR TYPE ZPMT0003-MATNR,     "SAP商品コード
           POSCD TYPE ZPMT0003-POSCD,     "POSコード
           APDAT TYPE ZPMT0003-APDAT,     "適用日
           BISMT TYPE ZPMT0003-BISMT,     "商品コード
           SPART TYPE ZPMT0003-SPART,     "部門コード
         END OF TYP_ZPMT0003,

*   汎用マスタテーブル構造
         BEGIN OF TYP_ZPMT0030,
           KEY01 TYPE ZPMT0030-KEY01,     "部門コード
           KEY02 TYPE ZPMT0030-KEY02,     "商品コード
           KEY03 TYPE ZPMT0030-KEY03,     "納品日
           ITM01 TYPE ZPMT0030-ITM01,     "変換商品コード
           ITM02 TYPE ZPMT0030-ITM02,     "変換SAP商品コード
         END OF TYP_ZPMT0030.

*----------------------------------------------------------------------*
* DATA定義（内部テーブル/内部テーブルヘッダ）
*----------------------------------------------------------------------*
  DATA:
*   プロキシ構造(ALL)
    TD_LINE_ALL  TYPE STANDARD TABLE OF TYP_LINE_ALL,
    TH_LINE_ALL  LIKE LINE OF TD_LINE_ALL,

*   プロキシ構造(データ部)
    TD_LINE TYPE STANDARD TABLE OF TYP_LINE,
    TH_LINE LIKE LINE OF TD_LINE,

*   I/F受信データ(中間テーブルフォーマット)
    TD_INDATA TYPE STANDARD TABLE OF ZPTT0049_SUB,
    TH_INDATA LIKE LINE OF TD_INDATA,

"   エラーファイル(構造あり)
    TD_EFILE TYPE STANDARD TABLE OF ZPTT0052,
    TH_EFILE LIKE LINE OF TD_EFILE,

"   エラーメッセージ
    TD_EMESSAGE TYPE STANDARD TABLE OF LINE,
    TH_EMESSAGE LIKE LINE OF TD_EMESSAGE,

*   ファイル出力用
    TD_FILEREC TYPE STANDARD TABLE OF LINE,
    TH_FILEREC LIKE LINE OF TD_FILEREC,

"   バリアントテーブル
    TD_TVARVC TYPE STANDARD TABLE OF TYP_TVARVC,
    TH_TVARVC LIKE LINE OF TD_TVARVC,

"   複数POSマスタ
    TD_ZPMT0003 TYPE STANDARD TABLE OF TYP_ZPMT0003,
    TH_ZPMT0003 LIKE LINE OF TD_ZPMT0003,

"   汎用マスタ
    TD_ZPMT0030 TYPE STANDARD TABLE OF TYP_ZPMT0030,
    TH_ZPMT0030 LIKE LINE OF TD_ZPMT0030,

"   POSコードレンジ
    RD_POSCD TYPE RANGE OF ZPMT0003-POSCD,
    RH_POSCD LIKE LINE OF RD_POSCD.

*----------------------------------------------------------------------*
* DATA定義（構造）
*----------------------------------------------------------------------*
  DATA:
    ST_ZCOTIFLOG TYPE ZCOTIFLOG.              "IF起動ログテーブル

*----------------------------------------------------------------------*
* DATA定義（ワーク）
*----------------------------------------------------------------------*
  DATA:
    FLG_DUMMY   TYPE FLAG,                    "ダミー変数(使用しない)
    FLG_ERR     TYPE FLAG VALUE C_FLG_OFF,    "エラーフラグ
    FLG_LENGTH_ERR TYPE FLAG VALUE C_FLG_OFF, "項目長エラーフラグ
    W_IFID      TYPE ZCEIFID,                 "インターフェースID
    W_EOM(3)    TYPE C,                       "EOM
    W_IFCOUNT   TYPE ZCEIFCOUNT,              "インターフェース連番
    W_IFEOM     TYPE ZCEEOM,                  "EOM使用可否フラグ
    W_SFNAME    TYPE STRING,                  "正常ファイル名
    W_EFNAME    TYPE STRING,                  "エラーファイル名
    W_ERRMSG    TYPE STRING,                  "エラーメッセージ
    W_TABIX(6)  TYPE N,                       "内部テーブルの索引
    W_COUNT     TYPE I,                       "レコード数

    W_SPART     TYPE ZPTT0049_SUB-SPART,      "部門コード
    W_BISMT     TYPE ZPMT0003-BISMT,          "商品コード
    W_MATNR     TYPE ZPMT0003-MATNR.          "SAP商品コード

*----------------------------------------------------------------------*
* 処理開始
*----------------------------------------------------------------------*
* IF起動ログ
  PERFORM ZCIO_IFLOG_START IN PROGRAM ZCIO0101
                  USING    C_CPROG C_CPROG_TXT
                  CHANGING ST_ZCOTIFLOG.

*----- プロキシデータの取得
  TH_LINE_ALL =  INPUT-Z_MT_IT8053Z00201_R_01.
  TD_LINE[]   =  INPUT-Z_MT_IT8053Z00201_R_01-STRUCT1[].    "#EC ENHOK

*----- I/FIDとEOMの取得
  W_IFID = TH_LINE_ALL-IFID-IFID.       "I/FID
  W_EOM  = TH_LINE_ALL-EOM-EOM.         "EOM

*----- 部門コードの取得(ファイル名の頭2文字)
  W_SPART = TH_LINE_ALL-FILE-FILENAME(2).

*----- 処理①I/FIDのロック処理
  PERFORM ZCIO_IFID_ENQUEUE IN PROGRAM ZCIO0101
                      USING W_IFID
                   CHANGING FLG_ERR
                            FLG_DUMMY
                            W_ERRMSG.
*----- 処理②I/F連番の取得処理
  IF FLG_ERR = C_FLG_OFF.
    PERFORM ZCIO_GET_IFCOUNT IN PROGRAM ZCIO0101
                       USING W_IFID
                    CHANGING W_IFCOUNT
                             W_IFEOM
                             FLG_ERR
                             W_ERRMSG.
  ENDIF.

*----- 処理②sub ファイル名生成
  PERFORM ZCIO_MAKE_FAILENAME IN PROGRAM ZCIO0101
    USING    W_IFID
             W_IFCOUNT
    CHANGING W_SFNAME
             W_EFNAME.

*----- 処理③前処理エラーチェック
  IF FLG_ERR = C_FLG_OFF.
    PERFORM ZCIO_CHECK_PREERR IN PROGRAM ZCIO0101
      USING    W_IFID
               W_EFNAME
      CHANGING FLG_ERR.
  ENDIF.

*----- 処理④ジョブ処理日取得
  IF FLG_ERR = C_FLG_OFF.
    SELECT LOW                           "条件データ
           HIGH                          "取得データ
      INTO TABLE TD_TVARVC
      FROM TVARVC
     WHERE NAME  = C_JOBDATE.            "バリアント変数名

*----- レコードが取得できなかった場合
    IF SY-SUBRC <> 0 .
*----- エラーフラグon
      FLG_ERR  = C_FLG_ON.
*----- バリアント変数 &1 が バリアント変数テーブル(TVARVC)に存在しません
      MESSAGE E336(ZC01)
        WITH C_JOBDATE "Z_JOBDATE_DAY
        INTO W_ERRMSG.
    ELSE.
*----- 取得できた場合、エラーリストテーブルのJOB処理日として使用する
      READ TABLE TD_TVARVC INDEX 1 INTO TH_TVARVC.
    ENDIF.
  ENDIF.

*----- 処理⑤データ編集処理
  IF FLG_ERR = C_FLG_OFF.

*----- レンジテーブルにPOSコード格納
    LOOP AT TD_LINE INTO TH_LINE.
      RH_POSCD-SIGN   = C_SIGN_I.
      RH_POSCD-OPTION = C_OPT_EQ.
      RH_POSCD-LOW    = TH_LINE-POSCD.
      APPEND RH_POSCD TO RD_POSCD.
    ENDLOOP.

*----- 複数POSマスタ取得
    SELECT MATNR
           POSCD
           APDAT
           BISMT
           SPART
      FROM ZPMT0003
      INTO TABLE TD_ZPMT0003
      WHERE POSCD IN RD_POSCD
        AND SPART =  W_SPART
        AND HAKUB =  0.

    SORT TD_ZPMT0003 BY MATNR ASCENDING
                        APDAT ASCENDING.

*----- 汎用マスタ取得（商品コード変換）
    SELECT KEY01
           KEY02
           KEY03
           ITM01
           ITM02
      FROM ZPMT0030
        INTO TABLE TD_ZPMT0030
      WHERE HANCD = C_HANCD_YOYAK
        AND KEY01 = W_SPART.

*----- プロキシ構造を中間テーブルに変換
    LOOP AT TD_LINE INTO TH_LINE.
      W_TABIX = SY-TABIX.
      CLEAR : TH_INDATA,
              TH_ZPMT0003,
              W_BISMT,
              W_MATNR.

*----- 商品コード割り当て
*      納品日に直近の適用日のレコードを採用
      LOOP AT TD_ZPMT0003 INTO TH_ZPMT0003
        WHERE POSCD =  TH_LINE-POSCD
          AND APDAT <= TH_LINE-FDDAT.
      ENDLOOP.

      "対象のレコードが存在しない場合エラー
      IF SY-SUBRC <> 0.
        MESSAGE E399(ZC01) WITH TEXT-M02 TH_LINE-POSCD SPACE TEXT-M03
          INTO TH_EMESSAGE-LINE.
        APPEND TH_EMESSAGE TO TD_EMESSAGE.

      ELSE.
        W_BISMT = TH_ZPMT0003-BISMT.        "商品コード
        W_MATNR = TH_ZPMT0003-MATNR.        "SAP商品コード

*----- 商品コード変換（納品日設定）
*      汎用マスタに存在する商品コードの場合、納品日ごとに商品コードを変換する
        READ TABLE TD_ZPMT0030 INTO TH_ZPMT0030
        WITH KEY KEY02 = TH_ZPMT0003-BISMT.

        IF SY-SUBRC = 0.
          READ TABLE TD_ZPMT0030 INTO TH_ZPMT0030
          WITH KEY KEY02 = TH_ZPMT0003-BISMT
                   KEY03 = TH_LINE-FDDAT.

          "対象のレコードが存在しない場合エラー
          IF SY-SUBRC <> 0.
            MESSAGE E399(ZC01) WITH TEXT-M05 TH_ZPMT0003-BISMT SPACE TEXT-M06
              INTO TH_EMESSAGE-LINE.
            APPEND TH_EMESSAGE TO TD_EMESSAGE.
          ELSE.
            W_BISMT = TH_ZPMT0030-ITM01.   "商品コード
            W_MATNR = TH_ZPMT0030-ITM02.   "SAP商品コード
          ENDIF.
        ENDIF.
      ENDIF.


*----- 発注数量(HMNGE)のデータ長チェック
      PERFORM ZCIO_CHECK_FIELD_LENGTH IN PROGRAM ZCIO0101
        USING TH_LINE-HMNGE
              TEXT-M04
              C_LENGTH_4
     CHANGING FLG_LENGTH_ERR
              W_ERRMSG.
      IF FLG_LENGTH_ERR = C_FLG_ON.
        "エラーの場合はメッセージを保存する
        TH_EMESSAGE-LINE = W_ERRMSG.
        APPEND TH_EMESSAGE TO TD_EMESSAGE.
      ENDIF.


      TH_INDATA-MANDT           = SY-MANDT.                 "クライアント
      TH_INDATA-ZIFID           = W_IFID.                   "I/FID
      TH_INDATA-ZIFCOUNT        = W_IFCOUNT.                "I/F連番
*----- ユニークキーNO取得処理
      CONCATENATE W_IFCOUNT               "IF連番
                  W_TABIX                 "内部テーブルの索引
             INTO TH_INDATA-Z_UNIQUE_KEY_NO.                "ユニークキーNO
      TH_INDATA-SPART           = W_SPART.                  "部門コード
      TH_INDATA-FDDAT           = TH_LINE-FDDAT.
      TH_INDATA-BISMT           = W_BISMT.                  "商品コード
      TH_INDATA-WERKS           = TH_LINE-WERKS.            "店舗コード
      TH_INDATA-DAET1           = TH_TVARVC-LOW.            "処理日
      TH_INDATA-FADAT           = TH_TVARVC-LOW.            "発注日
      TH_INDATA-HATTYU_KENMEI   = TEXT-M01.                 "発注件名「年末予約発注」
      TH_INDATA-KIKAKU_NO       = C_KIKAKU_NO.              "企画No「1」
      TH_INDATA-HMNGE           = TH_LINE-HMNGE.            "発注数量
      TH_INDATA-SAIYO_GENKA_KBN = C_SAIYO_GENKA.            "採用原価区分「3」
      TH_INDATA-GENKA_KAKERITU  = C_GENKA_KAKE.             "原価掛率「000」
      TH_INDATA-BIKUB           = C_BIKUB.                  "便区分「1」
      TH_INDATA-FLD_YOBI01      = W_MATNR.                  "項目予備01(SAP商品コード)

*----- エラーがある場合、処理ステータスを'E'とする
      IF TD_EMESSAGE IS NOT INITIAL.
        TH_INDATA-ZIFSTATUS     = C_STATUS_ERR.             "処理ステータス
        TH_INDATA-ZIFMESSAGE    = TH_EMESSAGE-LINE.         "メッセージ
      ENDIF.

      TH_INDATA-ZCRDAT          = SY-DATUM.                 "登録日付
      TH_INDATA-ZCRTIM          = SY-UZEIT.                 "登録時刻
      TH_INDATA-ZCRUSR          = SY-UNAME.                 "登録者
      TH_INDATA-ZUPDAT          = SY-DATUM.                 "更新日付
      TH_INDATA-ZUPTIM          = SY-UZEIT.                 "更新時刻
      TH_INDATA-ZUPUSR          = SY-UNAME.                 "更新者

      APPEND TH_INDATA TO TD_INDATA.

      LOOP AT TD_EMESSAGE INTO TH_EMESSAGE.
        TH_EFILE-ZIFID              "I/FID
        = W_IFID.
        TH_EFILE-ZIFCOUNT           "I/F連番
        = TH_INDATA-ZIFCOUNT.
        TH_EFILE-Z_UNIQUE_KEY_NO    "ユニークキーNO
        = TH_INDATA-Z_UNIQUE_KEY_NO.
        TH_EFILE-ERROR_SEQ_NO       "エラーシーケンスNO
        = SY-TABIX.
        TH_EFILE-JOBDATE            "JOB処理日
        = TH_TVARVC-LOW.
        TH_EFILE-SPART_EOS          "EOS部門コード
        = SPACE.
        TH_EFILE-SPART              "部門コード
        = W_SPART.
        TH_EFILE-HATTYU_SHUBETU     "発注種別
        = SPACE.
        TH_EFILE-FDDAT              "納品日
        = TH_INDATA-FDDAT.
        TH_EFILE-BISMT              "商品コード
        = TH_INDATA-BISMT.
        TH_EFILE-MAKTX              "商品名(漢字)
        = SPACE.
        TH_EFILE-SHIKB              "識別コード
        = SPACE.
        TH_EFILE-WERKS              "店舗コード
        = TH_INDATA-WERKS.
        TH_EFILE-TRHCD              "取引先コード
        = SPACE.
        TH_EFILE-LIFAX              "取引先名(カナ)
        = SPACE.
        TH_EFILE-BENUM              "発注単位入数
        = SPACE.
        TH_EFILE-MENUM              "荷姿入数
        = SPACE.
        TH_EFILE-ORCOD              "発注サイクルコード
        = SPACE.
        TH_EFILE-KIKAKU_NO          "企画NO
        = TH_INDATA-KIKAKU_NO.
        TH_EFILE-HMNGE              "発注数量
        = TH_INDATA-HMNGE.
        TH_EFILE-LOSFX              "原価
        = SPACE.
        TH_EFILE-VKPNE              "売価
        = SPACE.
        TH_EFILE-ZIFSTATUS          "ステータス
        = C_STATUS_ERR.
        TH_EFILE-ZIFMESSAGE         "メッセージ
        = TH_EMESSAGE-LINE.
        TH_EFILE-ZCRDAT             "登録日付
        = SY-DATUM.
        TH_EFILE-ZCRTIM             "登録時刻
        = SY-UZEIT.
        TH_EFILE-ZCRUSR             "登録者
        = SY-UNAME.
        TH_EFILE-ZUPDAT             "更新日付
        = SY-DATUM.
        TH_EFILE-ZUPTIM             "更新時刻
        = SY-UZEIT.
        TH_EFILE-ZUPUSR             "更新者
        = SY-UNAME.
        APPEND TH_EFILE TO TD_EFILE.
      ENDLOOP.
*----- エラーメッセージテーブル初期化
      CLEAR TH_EMESSAGE.
      CLEAR TD_EMESSAGE.

    ENDLOOP.
  ENDIF.

  IF FLG_ERR = C_FLG_OFF.
*----- 処理(9)エラーレコード登録
    INSERT ZPTT0052 FROM TABLE TD_EFILE.
*----- エラー処理
    IF SY-SUBRC <> 0.
*----- エラーフラグon
      FLG_ERR = C_FLG_ON.
*----- エラーメッセージ生成
*----- テーブル &1 の &2 に失敗しました(RC=:&3)
      MESSAGE E306(ZC01)
        WITH TEXT-M01 "発注データエラーリストテーブル(ZPTT0052)
             TEXT-M03 "登録
             SY-SUBRC "リターンコード
        INTO W_ERRMSG.
    ENDIF.
  ENDIF.

*----- 処理⑥中間テーブル更新処理
  IF FLG_ERR = C_FLG_OFF.
    MODIFY ZPTT0049_SUB FROM TABLE TD_INDATA.
*   エラー処理
    IF SY-SUBRC <> 0.
*     エラーフラグon
      FLG_ERR = C_FLG_ON.
*     エラーメッセージ生成
*     テーブル &1 の &2 に失敗しました(RC=:&3)
      MESSAGE E306(ZC01)
        WITH TEXT-M08 "移管確定データ中間テーブル(ZPTT0049_SUB)
             TEXT-M09 "更新
             SY-SUBRC "リターンコード
        INTO W_ERRMSG.
*----- ロールバック
      ROLLBACK WORK.

*   正常処理
    ELSE.
*     コミット
      COMMIT WORK.
*     正常ファイル作成
      PERFORM ZCIO_MAKE_SFILE IN PROGRAM ZCIO0101
        USING W_IFID
              W_SFNAME.
    ENDIF.
  ENDIF.

*----- 処理⑦エラー処理
  IF FLG_ERR = C_FLG_ON.

*   ファイルレベルのエラーメッセージ追記
    IF NOT W_ERRMSG IS INITIAL.
      APPEND W_ERRMSG TO TD_FILEREC.
    ENDIF.

*   エラーファイル作成
    PERFORM ZCIO_MAKE_EFILE IN PROGRAM ZCIO0101
                     TABLES TD_FILEREC
                      USING W_IFID
                            W_EFNAME.

*----- 処理⑦(2)正常ファイル削除処理
    PERFORM ZCIO_DELETE_SFILE IN PROGRAM ZCIO0101
                        USING W_IFID
                              W_SFNAME.

*----- 処理⑦(3)正常レコード削除(中間TBL)処理
    SELECT COUNT( * )
      FROM ZPTT0049_SUB
      INTO W_COUNT
     WHERE ZIFID    = W_IFID
       AND ZIFCOUNT = W_IFCOUNT.

    IF SY-SUBRC = 0.
      DELETE FROM ZPTT0049_SUB WHERE ZIFID    = W_IFID
                                 AND ZIFCOUNT = W_IFCOUNT.
      COMMIT WORK.
    ENDIF.

  ENDIF.

*----- 処理⑧EOM処理
  IF W_IFEOM = C_FLG_OFF  "EOM対象IFではない場合
  OR W_EOM = C_EOM.       "または、EOM対象で、EOMが送信されてきた場合
*----- 処理⑧(1)正常ファイルリネーム処理
    PERFORM ZCIO_REN_FILE_NAME IN PROGRAM ZCIO0101
                         USING W_IFID
                               W_SFNAME.
*----- 処理⑧(2)異常ファイルリネーム処理
    PERFORM ZCIO_REN_FILE_NAME IN PROGRAM ZCIO0101
                         USING W_IFID
                               W_EFNAME.
*----- 処理⑧(3)IF連番インクリメント処理
    PERFORM ZCIO_SET_IFCOUNT IN PROGRAM ZCIO0101
                       USING W_IFID.
  ENDIF.

*----- 処⑨ロック解除処理
  PERFORM ZCIO_IFID_DEQUEUE IN PROGRAM ZCIO0101
                      USING W_IFID.

* IF終了ログ
  PERFORM ZCIO_IFLOG_END IN PROGRAM ZCIO0101
    CHANGING ST_ZCOTIFLOG.

ENDMETHOD.