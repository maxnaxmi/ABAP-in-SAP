*----------------------------------------------------------------------*
* PROGRAM ID  : ZPIM0504
* NAME        : 特売アイキャッチマスタ登録機能
* AUTHOR      : RS 鈴木
* DATE        : 2018/07/09
* DESCRIPTION : 【コピープログラム（コピー元：ZPIM0501）】
*               中間テーブル ZPMT0066_SUB から
*               特売マスタテーブル(ZPMT0066)
*               へデータを更新する機能
*----------------------------------------------------------------------*
*& 変更履歴
*& LOG#  DATE        AUTHOR     DESCRIPTION
*& 0001  2010/09/13  IBM土屋    ①SCR424 納品日を連動項目から削除
*&                              ②日次連動と週次連動をわける
*& 0002  2010/09/14  IBM古畑    ①特売年月,部門コード,
*&                                SAP商品コードを抽出条件に入れる
*&                              ②データ作成日の日付チェックをはずす
*&                              ③Insert→Modify　に変更
*& 0003  2010/11/05  IBM湯浅    ZM5153X00102 対応
*&                              上記I/FIDは特売発注データであり、
*&                              ZPTT0080へ登録を実施する
*&                              ･ZM5153X00101→ZPMT0005
*&                              ･ZM5153X00102→ZPTT0080
*& 0004  2010/11/05  IBM湯浅    登録単位を 特売年月/企画No とし、
*&                              Delete/Insertを実施
*& 0005  2010/11/07  IBM古畑    発注数量のマッピング修正
*&                              発注コード採番の番号範囲オブジェクトから
*&                              年度を削除
*& 0006  2010/11/08  IBM湯浅    特売発注所ロジック追加により
*&                              変更多数のため全面改訂
*&                              前Verは"ZPIM0501_1108BK"参照
*& 0007  2010/11/10  IBM湯浅    変更済数量に、発注予定数量と同数を
*&                              マッピング
*& 0008  2010/12/06  IBM004    　同一納品日を一つにまとめる
*& 0009  2010/12/12  IBM004    　①売上高計画値をマッピングする
*&                               ②日次差分を更新する
*& 0010  2010/12/29  IBM004    　初回フラグ退避ロジック修正
*& 0011  2010/12/30  IBM004    　ZPTT0080の更新をいったんはずす
*& 0012  2011/01/06  IBM湯浅    並列実行を可能にするため
*&                              ロック処理を廃止
*& 0013  2011/01/19  RS坂井     特売発注書テーブルの削除は
*&                              ハウスキープの機能で行うため、
*&                              この機能では行わない(SCR487)
*& 0014  2011/01/25  IBM004     EOS部門コード変換
*& 0015  2015/09/01  RS鈴木     MDM特売マスタ　企画削除の場合、
*&                                             ERP特売マスタから企画削除を行う
*& 0016  2018/07/09  RS鈴木     テーブル項目追加（AICTH）のため新規PGへコピー（元PG：ZPIM0501）
*&                              【テーブル変更】
*&                              取得元テーブル　ZPMT0005_SUB　→　ZPMT0066_SUB
*&                              登録テーブル　　ZPMT0005      →  ZPMT0066
*&                              特売発注書テーブル(ZPTT0080)へのデータ更新なし
*&                              データ作成日(CDATE)のデータ型チェック追加
*----------------------------------------------------------------------*
REPORT  ZPIM0504 NO STANDARD PAGE HEADING
                             LINE-SIZE  170
                             LINE-COUNT  58.
*----------------------------------------------------------------------*
* CONSTANTS定義
*----------------------------------------------------------------------*
CONSTANTS:
  C_FLG_ON            TYPE FLAG  VALUE 'X',     "フラグ等ON
  C_FLG_OFF           TYPE FLAG  VALUE SPACE,   "フラグ等OFF

*-----JOBLOG用
  C_INMSG_JOBSTART(4) TYPE N VALUE 10,   "JOBLOG_MESSAGE：ジョブ開始
  C_INMSG_ENQUEUE(4)  TYPE N VALUE 20,   "JOBLOG_MESSAGE：ロック
  C_INMSG_GETFILE(4)  TYPE N VALUE 30,   "JOBLOG_MESSAGE：ファイル取得
  C_INMSG_GETREC(4)   TYPE N VALUE 40,   "JOBLOG_MESSAGE：レコード取得
  C_INMSG_UPD(4)      TYPE N VALUE 50,   "JOBLOG_MESSAGE：更新
  C_INMSG_UPDCNT(4)   TYPE N VALUE 55,   "JOBLOG_MESSAGE：更新件数
  C_INMSG_UPDSUB(4)   TYPE N VALUE 60,   "JOBLOG_MESSAGE：更新(中間TBL)
  C_INMSG_DELFILE(4)  TYPE N VALUE 70,   "JOBLOG_MESSAGE：ファイル削除
  C_INMSG_REPORT(4)   TYPE N VALUE 80,   "JOBLOG_MESSAGE：レポート
  C_INMSG_REPCNT(4)   TYPE N VALUE 85,   "JOBLOG_MESSAGE：レポート(件数)
  C_INMSG_JOBEND(4)   TYPE N VALUE 90,   "JOBLOG_MESSAGE：ジョブ終了
  C_INMSG_VAR_DUMMY   TYPE I VALUE 0,    "メッセージ用ダミー引数

*-----ステータス用
  C_STATUS_YET TYPE ZCESTATUS VALUE SPACE,"処理ステータス：未処理
  C_STATUS_SUC TYPE ZCESTATUS VALUE 'S',  "処理ステータス：正常
  C_STATUS_ERR TYPE ZCESTATUS VALUE 'E',  "処理ステータス：エラー

*-----日付チェック用
  C_DATE_DEFAULT TYPE D VALUE '00000000',  "日付初期値

*-----日付型項目
  C_VKDAB TYPE STRING VALUE '特売期間FROM',
  C_VKDAI TYPE STRING VALUE '特売期間TO',
  C_DAPFR TYPE STRING VALUE '日替期間FROM',
  C_DAPTO TYPE STRING VALUE '日替期間TO',
  C_DEFRM TYPE STRING VALUE '原価適用開始日',
  C_DELTO TYPE STRING VALUE '原価適用終了日',
  C_AKWED TYPE STRING VALUE '店舗修正締切日',
*--> ADD 0016 START 2018/07/09 RS鈴木
  C_CDATE TYPE STRING VALUE 'データ作成日',
*--> ADD 0016 END 2018/07/09 RS鈴木
  C_UDATE TYPE STRING VALUE 'データ更新日',
  C_DLDAT TYPE STRING VALUE '納品日',
  C_AKHED TYPE STRING VALUE '本部締切日',

*-----数値チェック用
  C_HAIRI  TYPE STRING VALUE '発注単位入数',
  C_TOIRI  TYPE STRING VALUE '特売発注単位入数',
  C_MENUM  TYPE STRING VALUE '荷姿単位入数',
  C_CAHAN  TYPE STRING VALUE 'ケース販売',
  C_HACYO  TYPE STRING VALUE '発注予定数量',
  C_LOSFX  TYPE STRING VALUE '通常原価',
  C_VKPNE  TYPE STRING VALUE '通常売価',
  C_PLEKP  TYPE STRING VALUE '特売原価',
  C_PLVKP  TYPE STRING VALUE '特売売価',
  C_ZASAL  TYPE STRING VALUE '続行売価',
  C_HMNGE  TYPE STRING VALUE '発注数量'.

*----- ZM5153X00102(店別特売発注書データ)IFID
CONSTANTS:
  C_IFID_TOKUBAI_TRAN TYPE ZCEIFID VALUE 'ZM5153X00102'.

*-->ADD 0014 START 2011/1/25 IBM004
*----- 汎用コード取得用
CONSTANTS:
  C_HANCD_TEOSC      TYPE ZPEHANCD  VALUE 'TEOSC',
*-->ADD 0014 END 2011/1/25 IBM004

*-->ADD 0015 START 2015/09/01 RS鈴木
*----- 企画にマスタ更新区分 = 3:削除の有り無しチェック用
 C_UPKBN_DEL_ON(1) TYPE C VALUE '1',               "マスタ更新区分 = 3 :削除があり
 C_UPKBN_DEL_OFF(1) TYPE C VALUE '0',             "マスタ更新区分 = 3 :削除がなし

*----- マスタ更新区分用
  C_UPKBN_INS(1) TYPE C VALUE '1',               "マスタ更新区分 = 1 :新規
  C_UPKBN_UPD(1) TYPE C VALUE '2',               "マスタ更新区分 = 2 :更新
  C_UPKBN_DEL(1) TYPE C VALUE '3'.               "マスタ更新区分 = 3 :削除
*-->ADD 0015 END 2015/09/01 RS鈴木

*----------------------------------------------------------------------*
* TYPES定義
*----------------------------------------------------------------------*
TYPES:
*----- 特売マスタ中間テーブル forZPTT0080
*      ZPMT0066登録用とは、ソート順、ブレイクキーが異なるため、
*      構造新設
  BEGIN OF TYP_ZPMT0066_SUB_80,
    MANDT TYPE ZPMT0066_SUB-MANDT,  "クライアント
    ZIFID TYPE ZPMT0066_SUB-ZIFID,  "I/FID
    ZIFCOUNT TYPE ZPMT0066_SUB-ZIFCOUNT,  "I/F連番
    AKYMT TYPE ZPMT0066_SUB-AKYMT,  "特売年月
    AKTNR TYPE ZPMT0066_SUB-AKTNR,  "特売企画No.
    AKSHU TYPE ZPMT0066_SUB-AKSHU,  "特売種別
    VKDAB TYPE ZPMT0066_SUB-VKDAB,  "特売期間FROM
    SPART TYPE ZPMT0066_SUB-SPART,  "部門コード
    BISMT TYPE ZPMT0066_SUB-BISMT,  "商品コード
    DLDAT TYPE ZPMT0066_SUB-DLDAT,  "納品日
    MATNR TYPE ZPMT0066_SUB-MATNR,  "SAP商品コード
    WERKS TYPE ZPMT0066_SUB-WERKS,  "店舗コード
    VKDAI TYPE ZPMT0066_SUB-VKDAI,  "特売期間TO
    DAPFR TYPE ZPMT0066_SUB-DAPFR,  "日替期間FROM
    DAPTO TYPE ZPMT0066_SUB-DAPTO,  "日替期間TO
    DEFRM TYPE ZPMT0066_SUB-DEFRM,  "原価適用開始日
    DELTO TYPE ZPMT0066_SUB-DELTO,  "原価適用終了日
    AKWED TYPE ZPMT0066_SUB-AKWED,  "店舗修正締切日
    AKHED TYPE ZPMT0066_SUB-AKHED,  "本部締切日
    MATKL TYPE ZPMT0066_SUB-MATKL,  "商品カテゴリ
    DAYKB TYPE ZPMT0066_SUB-DAYKB,  "日替区分
    EOSSP TYPE ZPMT0066_SUB-EOSSP,  "EOS部門コード
    DAIMA TYPE ZPMT0066_SUB-DAIMA,  "代表商品コード
    LIFNR TYPE ZPMT0066_SUB-LIFNR,  "取引先コード
    AKTXK TYPE ZPMT0066_SUB-AKTXK,  "種別名称（漢字）
    MAKTX TYPE ZPMT0066_SUB-MAKTX,  "商品名（漢字）
    MAKAX TYPE ZPMT0066_SUB-MAKAX,  "商品名（カナ）
    KIKAJ TYPE ZPMT0066_SUB-KIKAJ,  "規格名（漢字）
    KIKAN TYPE ZPMT0066_SUB-KIKAN,  "規格名（カナ）
    BSTST TYPE ZPMT0066_SUB-BSTST,  "発注単位
    HAIRI TYPE ZPMT0066_SUB-HAIRI,  "発注単位入数
    TOIRI TYPE ZPMT0066_SUB-TOIRI,  "特売発注単位入数
    MEIST TYPE ZPMT0066_SUB-MEIST,  "荷姿単位
    MENUM TYPE ZPMT0066_SUB-MENUM,  "荷姿単位入数
    CAHAN TYPE ZPMT0066_SUB-CAHAN,  "ケース販売
    HACYO TYPE ZPMT0066_SUB-HACYO,  "売上高計画値
    ORCOD TYPE ZPMT0066_SUB-ORCOD,  "発注ｻｲｸﾙ
    SHKBN TYPE ZPMT0066_SUB-SHKBN,  "数量変更可区分
    EOSKB TYPE ZPMT0066_SUB-EOSKB,  "発注可区分
    REKUB TYPE ZPMT0066_SUB-REKUB,  "回収可区分
    ZAKUB TYPE ZPMT0066_SUB-ZAKUB,  "センター直納区分
    MNKBN TYPE ZPMT0066_SUB-MNKBN,  "マークダウン区分
    HMNGE TYPE ZPMT0066_SUB-HMNGE,  "発注数量
    LOSFX TYPE ZPMT0066_SUB-LOSFX,  "通常原価
    VKPNE TYPE ZPMT0066_SUB-VKPNE,  "通常売価
    PLEKP TYPE ZPMT0066_SUB-PLEKP,  "特売原価
    PLVKP TYPE ZPMT0066_SUB-PLVKP,  "特売売価
    ZASAL TYPE ZPMT0066_SUB-ZASAL,  "続行売価
    UPKBN TYPE ZPMT0066_SUB-UPKBN,  "マスタ更新区分
    CDATE TYPE ZPMT0066_SUB-CDATE,  "データ作成日
    UDATE TYPE ZPMT0066_SUB-UDATE,  "データ更新日
    ZIFSTATUS TYPE ZPMT0066_SUB-ZIFSTATUS,  "処理ステータス
    ZIFMESSAGE TYPE ZPMT0066_SUB-ZIFMESSAGE,  "メッセージ
    ZCRDAT TYPE ZPMT0066_SUB-ZCRDAT,  "登録日付
    ZCRTIM TYPE ZPMT0066_SUB-ZCRTIM,  "登録時刻
    ZCRUSR TYPE ZPMT0066_SUB-ZCRUSR,  "登録者
    ZUPDAT TYPE ZPMT0066_SUB-ZUPDAT,  "更新日付
    ZUPTIM TYPE ZPMT0066_SUB-ZUPTIM,  "更新時刻
    ZUPUSR TYPE ZPMT0066_SUB-ZUPUSR,  "更新者
  END   OF TYP_ZPMT0066_SUB_80.

*----- 特売マスタ中間テーブル forZPMT0066
TYPES: BEGIN OF TYP_ZPMT0066_SUB_UNIT.
INCLUDE TYPE ZPMT0066_SUB.             "特売マスタ中間テーブル
TYPES: INDEX TYPE I.                   "元レコードのINDEX
TYPES: HACCD TYPE ZPTT0080-HACCD.      "発注コード
TYPES: AKHED_D TYPE D.                 "本部締切日(日付型)
TYPES: VKDAB_D TYPE D.                 "特売ｆｒ(日付型)
TYPES: VKDAI_D TYPE D.                 "特売ｔｏ(日付型)
TYPES: DLDAT_D TYPE D.                 "納品日(日付型)
TYPES: DAPFR_D TYPE D.                 "日替わりｆｒ(日付型)
TYPES: END   OF TYP_ZPMT0066_SUB_UNIT.

*--> ADD 0014 START 2011/01/13 IBM004
TYPES:
  BEGIN OF TYP_TEOSC_CONV,
    KEY01 TYPE ZPMT0030-KEY01,         "部門コード
    ITM01 TYPE ZPMT0030-ITM01,         "EOS部門コード
  END OF TYP_TEOSC_CONV.
*--> ADD 0014 END   2011/01/13 IBM004


*----------------------------------------------------------------------*
* DATA定義（内部テーブル/内部テーブルヘッダ）
*----------------------------------------------------------------------*
DATA:
*----- I/FID管理TBL
  TD_IFID         TYPE STANDARD TABLE OF ZCOTIFID,    "I/FID管理
  TH_IFID         LIKE LINE OF TD_IFID,               "I/FID管理

*----- 特売マスタ 中間テーブル
  TD_ZPMT0066_SUB TYPE STANDARD TABLE OF ZPMT0066_SUB,
  TH_ZPMT0066_SUB LIKE LINE OF TD_ZPMT0066_SUB,

*----- 特売マスタ中間テーブル(特売年月/企画Noの登録単位)
  TD_ZPMT0066_SUB_UNIT TYPE STANDARD TABLE OF TYP_ZPMT0066_SUB_UNIT,
  TH_ZPMT0066_SUB_UNIT LIKE LINE OF TD_ZPMT0066_SUB_UNIT,

*----- エラーリスト用(エラーメッセージ複数行対応)
  TD_LIST TYPE STANDARD TABLE OF ZPMT0066_SUB,
  TH_LIST LIKE LINE OF  TD_ZPMT0066_SUB,

*----- 特売発注書テーブル
  TD_ZPTT0080 TYPE STANDARD TABLE OF ZPTT0080,
  TH_ZPTT0080 LIKE LINE OF TD_ZPTT0080,

*--> ADD 0009 START 2010/12/12 IBM004
*----- 週次特売データ中間テーブル(特売年月/企画Noの登録単位)
  TD_ZPTT0080_UNIT TYPE STANDARD TABLE OF ZPTT0080,
  TH_ZPTT0080_UNIT LIKE LINE OF TD_ZPTT0080_UNIT,
*--> ADD 0009 END   2010/12/12 IBM004

*--> ADD 0014 START 2011/01/25 IBM004
  TD_TEOSC_CONV TYPE STANDARD TABLE OF TYP_TEOSC_CONV,
  TH_TEOSC_CONV LIKE LINE OF TD_TEOSC_CONV,
*--> ADD 0014 END   2011/01/25 IBM004

*----- ERPテーブル
  TD_ZPMT0066 TYPE STANDARD TABLE OF ZPMT0066,
  TH_ZPMT0066 LIKE LINE OF TD_ZPMT0066.

*----------------------------------------------------------------------*
* DATA定義（フラグ）
*----------------------------------------------------------------------*
DATA:
  FLG_STOP TYPE FLAG,                  "処理終了フラグ
  FLG_ERR  TYPE FLAG VALUE C_FLG_OFF.  "エラーフラグ

*----------------------------------------------------------------------*
* DATA定義（カウンタ）
*----------------------------------------------------------------------*
DATA:
*----- 件数カウンター
  CTR_ALL TYPE I,    "全件
  CTR_SUC TYPE I,    "正常
  CTR_ERR TYPE I.    "異常

*----------------------------------------------------------------------*
* DATA定義（ワーク）
*----------------------------------------------------------------------*
DATA:
  W_JOBDATE TYPE D,                  "ジョブ処理日付
  W_DATUM   TYPE D,                  "実行日付
  W_UZEIT   TYPE T,                  "実行時刻
  W_ERRMSG  TYPE STRING.             "エラーメッセージ

*----------------------------------------------------------------------*
* PARAMETERS・SELECT-OPTIONS定義
*----------------------------------------------------------------------*
PARAMETERS:
*----- IFID
  P_IFID TYPE ZCEIFID DEFAULT 'ZM5153X00103' OBLIGATORY.

SELECT-OPTIONS:
  S_SPART FOR TH_ZPMT0066-SPART,  "部門コード
  S_MATNR FOR TH_ZPMT0066-MATNR.  "SAP商品コード

*----------------------------------------------------------------------*
* INCLUDE定義
*----------------------------------------------------------------------*
INCLUDE:
  ZCCO0101,  "帳票共通機能
  ZCCO0201.  "帳票共通機能
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

*----- システム日時退避
  W_DATUM = SY-DATUM.
  W_UZEIT = SY-UZEIT.

*----- ジョブログ出力(ジョブ開始)
  PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
    USING C_INMSG_JOBSTART    "JOBLOG_MESSAGE：ジョブ開始
          P_IFID              "IF ID
          C_INMSG_VAR_DUMMY   "メッセージ用ダミー引数
          C_INMSG_VAR_DUMMY   "メッセージ用ダミー引数
          C_INMSG_VAR_DUMMY   "メッセージ用ダミー引数
          FLG_ERR.            "エラーフラグ

*--> DEL 0012 START 2011/01/06 IBM湯浅
**----- ジョブログ出力(ロック)
*  PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
*    USING C_INMSG_ENQUEUE     "JOBLOG_MESSAGE：ロック
*          P_IFID              "IF ID
*          C_INMSG_VAR_DUMMY   "メッセージ用ダミー引数
*          C_INMSG_VAR_DUMMY   "メッセージ用ダミー引数
*          C_INMSG_VAR_DUMMY   "メッセージ用ダミー引数
*          FLG_ERR.            "エラーフラグ
*
**----- 処理①I/FIDのロック処理
*  PERFORM ZCIO_IFID_ENQUEUE IN PROGRAM ZCIO0101
*    USING    P_IFID          "選択画面IFID
*    CHANGING FLG_ERR         "エラーフラグ
*             FLG_STOP        "処理終了フラグ
*             W_ERRMSG.       "エラーメッセージ内容
*--> DEL END   START 2011/01/06 IBM湯浅

  IF FLG_STOP = C_FLG_OFF.
*----- ジョブログ出力(ファイル取得)
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
      USING C_INMSG_GETFILE     "JOBLOG_MESSAGE：ファイル取得
            P_IFID              "IF ID
            C_INMSG_VAR_DUMMY   "メッセージ用ダミー引数
            C_INMSG_VAR_DUMMY   "メッセージ用ダミー引数
            C_INMSG_VAR_DUMMY   "メッセージ用ダミー引数
            FLG_ERR.            "エラーフラグ

*----- 処理②正常終了ファイル名取得
    PERFORM ZCIO_GET_SFILE_NAME IN PROGRAM ZCIO0101
      TABLES   TD_IFID
      USING    P_IFID
      CHANGING FLG_ERR
               FLG_STOP.
  ENDIF.

  IF FLG_STOP = C_FLG_OFF.
*----- ジョブログ出力(レコード取得)
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_GETREC
                                 P_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.
*----- 処理③処理対象レコード取得
    PERFORM GET_INDATA.

  ENDIF.

  IF FLG_STOP = C_FLG_OFF.
*----- ジョブログ出力(実マスタ更新)
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_UPD
                                 P_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.
*----- 処理④特売マスタテーブル登録
    PERFORM UPDATE_MASTER.
  ENDIF.

  IF FLG_STOP = C_FLG_OFF.
*----- ジョブログ出力(更新中間TBL)
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_UPDSUB
                                 P_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.
*----- 中間テーブル更新
    PERFORM UPDATE_SUB_TABLE.
  ENDIF.

  IF FLG_STOP = C_FLG_OFF.
*----- ジョブログ出力(ファイル削除)
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_DELFILE
                                 P_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.
*----- 処理⑤正常ファイル削除処理
    PERFORM DELETE_FILES.
  ENDIF.

  IF FLG_STOP = C_FLG_OFF.
*----- ジョブログ出力(レポート)
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_REPORT
                                 P_IFID
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 C_INMSG_VAR_DUMMY
                                 FLG_ERR.

*----- ジョブログ出力(レポート(件数))
    PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                           USING C_INMSG_REPCNT
                                 P_IFID
                                 CTR_ALL
                                 CTR_SUC
                                 CTR_ERR
                                 FLG_ERR.
*----- 処理⑥処理結果リスト出力
    PERFORM OUTPUT_REPORT.
  ENDIF.

*--> DEL 0012 END   2011/01/06 IBM湯浅
**----- 処理⑦I/FIDのロック解除処理
*  PERFORM ZCIO_IFID_DEQUEUE IN PROGRAM ZCIO0101
*                      USING P_IFID.
*--> DEL 0012 START 2011/01/06 IBM湯浅

*----- ジョブログ出力（ジョブ終了）
  PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                         USING C_INMSG_JOBEND
                               P_IFID
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
*----- 中間テーブルレコード取得
  SELECT *
    FROM ZPMT0066_SUB
    INTO TABLE TD_ZPMT0066_SUB
     FOR ALL ENTRIES IN TD_IFID
   WHERE ZIFID     = TD_IFID-ZIFID      "I/FID
     AND ZIFCOUNT  = TD_IFID-ZIFCOUNT   "I/F連番
     AND SPART  IN S_SPART   "部門コード
     AND MATNR  IN S_MATNR   "SAP商品コード
     AND ZIFSTATUS = C_STATUS_YET.      "ステータス未処理

*----- エラー処理
  IF SY-SUBRC <> 0.
    FLG_STOP = C_FLG_ON.              "処理中止フラグon
*-----メッセージ出力
    MESSAGE S001(ZC01) WITH TEXT-M01  "特売マスタ中間テーブル
                            TEXT-M02. "ZPMT0066_SUB
  ENDIF.
ENDFORM.                    " GET_INDATA

*&---------------------------------------------------------------------*
*&      Form  UPDATE_MASTER
*&---------------------------------------------------------------------*
*       特売マスタテーブル登録
*----------------------------------------------------------------------*
FORM UPDATE_MASTER .
*--> DEL 0016 START 2018/07/09 RS鈴木
**----- I/FID(データ送信元)により、登録先テーブルが異なるため、
**----- I/FIDでフォーム分割
**----- ZM5153X00102(店別特売発注書データ)
*  IF P_IFID = C_IFID_TOKUBAI_TRAN.
*
**--> DEL 0013 START 2011/01/19 RS坂井
**--> ADD 0009 START 2010/12/14 IBM004
**----- 週次特売データ登録前に前回分削除
**    PERFORM DELETE_ZPTT0080.
**--> ADD 0009 END 2010/12/14 IBM004
**--> DEL 0013 END 2011/01/19 RS坂井
*
**----- 週次特売データ登録
*    PERFORM UPDATE_ZPTT0080.
*
**----- ZM5153X00101(特売マスタ)
*  ELSE.
*--> DEL 0016 END 2018/07/09 RS鈴木
    PERFORM UPDATE_ZPMT0066.

*--> DEL 0016 START 2018/07/09 RS鈴木
**--> DEL 0011 START 2010/12/30 IBM004
*****--> ADD 0009 START 2010/12/14 IBM004
*****----- 週次特売データ登録
****    PERFORM UPDATE_ZPTT0080.
*****--> ADD 0009 END 2010/12/14 IBM004
**--> DEL 0011 END 2010/12/30 IBM004
*
*  ENDIF.
*--> DEL 0016 END 2018/07/09 RS鈴木

ENDFORM.                    " UPDATE_MASTER

*--> DEL 0016 START 2018/07/09 RS鈴木
**&---------------------------------------------------------------------*
**&      Form  UPDATE_ZPTT0080
**&---------------------------------------------------------------------*
**       ZPTT0080(特売発注データ)登録
**----------------------------------------------------------------------*
*FORM UPDATE_ZPTT0080 .
**----- ローカルデータ定義
*  DATA:
*    LFLG_RECORD_ERR TYPE FLAG,   "レコード単位エラーフラグ
*    LFLG_UNIT_ERR   TYPE FLAG,   "登録単位エラーフラグ
*    LW_MSG          TYPE STRING, "メッセージ内容
*    LW_CURR_IDX     TYPE I.      "現行INDEX
*
**----- 前回値構造
*  DATA:
*    LTH_ZPMT0066_SUB_LAST TYPE ZPMT0066_SUB.
**----- 次回値構造
*  DATA:
*    LW_NEXT_IDX  TYPE I,
*    LTH_ZPMT0066_SUB_NEXT TYPE ZPMT0066_SUB.
*
***----- ZM5153X00102(店別特売発注書データ)の場合のソート処理
**----- 中間テーブルデータソート処理
*  SORT TD_ZPMT0066_SUB BY ZIFID     "IFID
*                          ZIFCOUNT  "IF連番
*                          AKSHU     "特売種別
*                          VKDAB     "特売期間FROM
*                          SPART     "部門コード
*                          BISMT     "商品コード
*                          DLDAT     "納品日
*                          AKYMT     "特売年月
*                          AKTNR     "特売企画No
*                          MATNR     "SAP商品コード
*                          WERKS     "店舗コード
*                          VKDAI     "特売期間TO
*                          DAPFR.    "日替期間FROM
*
**----- 処理対象レコード件数取得
*  CTR_ALL = LINES( TD_ZPMT0066_SUB ).
*
**----- 処理対象レコード全件処理
*  LOOP AT TD_ZPMT0066_SUB INTO TH_ZPMT0066_SUB.
*    LW_CURR_IDX = SY-TABIX.
*    CLEAR TH_ZPMT0066.
*
**----- レコード単位のエラーフラグ初期化
*    LFLG_RECORD_ERR = C_FLG_OFF.
*
**----- キーブレイクで登録単位itab/エラーフラグ初期化
**   (IFID/IF連番/特売種別/特売期間FROM/部門/商品/納品日)
*    IF LTH_ZPMT0066_SUB_LAST-ZIFID <> TH_ZPMT0066_SUB-ZIFID
*    OR LTH_ZPMT0066_SUB_LAST-ZIFCOUNT <> TH_ZPMT0066_SUB-ZIFCOUNT
*    OR LTH_ZPMT0066_SUB_LAST-AKSHU <> TH_ZPMT0066_SUB-AKSHU
*    OR LTH_ZPMT0066_SUB_LAST-VKDAB <> TH_ZPMT0066_SUB-VKDAB
*    OR LTH_ZPMT0066_SUB_LAST-SPART <> TH_ZPMT0066_SUB-SPART
*    OR LTH_ZPMT0066_SUB_LAST-BISMT <> TH_ZPMT0066_SUB-BISMT
*    OR LTH_ZPMT0066_SUB_LAST-DLDAT <> TH_ZPMT0066_SUB-DLDAT
**--> ADD 0005 START 2011/06/13 RS坂井
*    OR LTH_ZPMT0066_SUB_LAST-AKTNR <> TH_ZPMT0066_SUB-AKTNR.
**--> ADD 0005 END 2011/06/13 RS坂井
*      CLEAR TD_ZPMT0066_SUB_UNIT.
*      LFLG_UNIT_ERR = C_FLG_OFF.
*    ENDIF.
*
**----- 次回値の取得
*    LW_NEXT_IDX = LW_CURR_IDX + 1.
*    CLEAR LTH_ZPMT0066_SUB_NEXT.
*    READ TABLE TD_ZPMT0066_SUB INTO LTH_ZPMT0066_SUB_NEXT
*      INDEX LW_NEXT_IDX.
*
**----- エラーチェックが多数あるため別フォームで実装
*    PERFORM ERR_CHECKS USING    LW_CURR_IDX
*                       CHANGING LFLG_RECORD_ERR.
*    IF LFLG_RECORD_ERR = C_FLG_ON.
*      LFLG_UNIT_ERR = C_FLG_ON.
*    ENDIF.
*
**----- 特売年月/企画No 単位で登録するため、別itab作成
**----- そのための構造へ値を退避
*    CLEAR TH_ZPMT0066_SUB_UNIT.
*    TH_ZPMT0066_SUB_UNIT = TH_ZPMT0066_SUB.     "中間テーブルデータ
**   日付項目の日付型変換
*    TH_ZPMT0066_SUB_UNIT-AKHED_D = TH_ZPMT0066_SUB-AKHED.
*    TH_ZPMT0066_SUB_UNIT-VKDAB_D = TH_ZPMT0066_SUB-VKDAB.
*    TH_ZPMT0066_SUB_UNIT-VKDAI_D = TH_ZPMT0066_SUB-VKDAI.
*    TH_ZPMT0066_SUB_UNIT-DLDAT_D = TH_ZPMT0066_SUB-DLDAT.
*    TH_ZPMT0066_SUB_UNIT-DAPFR_D = TH_ZPMT0066_SUB-DAPFR.
*    TH_ZPMT0066_SUB_UNIT-INDEX = LW_CURR_IDX.   "現在行
*
**----- 特売年月/企画No 単位で登録するため、別itab作成
*    IF LFLG_RECORD_ERR = C_FLG_OFF.
*      APPEND TH_ZPMT0066_SUB_UNIT TO TD_ZPMT0066_SUB_UNIT.
*    ENDIF.
*
**----- キーブレイクで登録処理実施
**   (IFID/IF連番/特売種別/特売期間FROM/部門/商品/納品日)
*    IF LTH_ZPMT0066_SUB_NEXT-ZIFID <> TH_ZPMT0066_SUB-ZIFID
*    OR LTH_ZPMT0066_SUB_NEXT-ZIFCOUNT <> TH_ZPMT0066_SUB-ZIFCOUNT
*    OR LTH_ZPMT0066_SUB_NEXT-AKSHU <> TH_ZPMT0066_SUB-AKSHU
*    OR LTH_ZPMT0066_SUB_NEXT-VKDAB <> TH_ZPMT0066_SUB-VKDAB
*    OR LTH_ZPMT0066_SUB_NEXT-SPART <> TH_ZPMT0066_SUB-SPART
*    OR LTH_ZPMT0066_SUB_NEXT-BISMT <> TH_ZPMT0066_SUB-BISMT
*    OR LTH_ZPMT0066_SUB_NEXT-DLDAT <> TH_ZPMT0066_SUB-DLDAT
**--> ADD 0005 START 2011/06/13 RS坂井
*    OR LTH_ZPMT0066_SUB_NEXT-AKTNR <> TH_ZPMT0066_SUB-AKTNR.
**--> ADD 0005 END 2011/06/13 RS坂井
**----- 登録単位でのエラーがない場合、登録処理実施
*      IF LFLG_UNIT_ERR = C_FLG_OFF.
*
**--> MOD 0009 START   2010/12/25 IBM004
**       ZM5153X00102(店別特売発注書データ)の場合、
**       →ZPTT0080(特売発注データ)登録
**        PERFORM CREATE_ZPTT0080.
*        IF P_IFID = C_IFID_TOKUBAI_TRAN.
*          PERFORM CREATE_ZPTT0080.
*        ELSE.
*          PERFORM UPDATE_ZPTT0080_DAILY.
*        ENDIF.
**--> MOD 0009 END   2010/12/25 IBM004
*
**----- 登録単位でのエラーが発生している場合、
**----- チェックエラーがないレコードもエラーステータス設定
**----- (特売年月/企画No単位で登録をするため)
*      ELSE.
*        LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
*          CLEAR LW_MSG.
**         &1チェックにより&2となっています
*          MESSAGE E416(ZC01) WITH TEXT-M06 TEXT-M07 INTO LW_MSG.
*          PERFORM BUILD_LIST_TABLE
*            USING LW_MSG                       "メッセージ内容
*                  TH_ZPMT0066_SUB_UNIT-INDEX.  "現行INDEX
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
*
**----- 現在行を前回値として変数退避
*    LTH_ZPMT0066_SUB_LAST = TH_ZPMT0066_SUB.
*
*  ENDLOOP.
*
**----- 処理中の件数出力
*  PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
*                         USING C_INMSG_UPDCNT
*                               P_IFID
*                               CTR_ALL
*                               CTR_SUC
*                               CTR_ERR
*                               FLG_ERR.
*
*ENDFORM.                    " UPDATE_ZPTT0080
*--> DEL 0016 END 2018/07/09 RS鈴木

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZPMT0066
*&---------------------------------------------------------------------*
*       ZPMT0066(特売マスタ)登録
*----------------------------------------------------------------------*
FORM UPDATE_ZPMT0066 .
*----- ローカルデータ定義
  DATA:
    LFLG_RECORD_ERR TYPE FLAG,   "レコード単位エラーフラグ
    LFLG_UNIT_ERR   TYPE FLAG,   "登録単位エラーフラグ
    LW_MSG          TYPE STRING, "メッセージ内容
    LW_CURR_IDX     TYPE I.      "現行INDEX

**----- AT命令を使用するため、退避変数追加
  DATA:
    LTH_ZPMT0066_SUB_TMP LIKE LINE OF TD_ZPMT0066_SUB.

*----- ZM5153X00101(特売マスタ)の場合のソート処理
*----- 中間テーブルデータソート処理
  SORT TD_ZPMT0066_SUB BY ZIFID     "IFID
                          ZIFCOUNT  "IF連番
                          AKYMT     "特売年月
                          AKTNR     "特売企画No
                          AKSHU     "特売種別
                          SPART     "部門コード
                          MATNR     "SAP商品コード
                          BISMT     "商品コード
                          WERKS     "店舗コード
                          VKDAB     "特売期間FROM
                          VKDAI     "特売期間TO
                          DLDAT     "納品日
                          DAPFR.    "日替期間FROM

*----- 処理対象レコード件数取得
  CTR_ALL = LINES( TD_ZPMT0066_SUB ).

*----- 処理対象レコード全件処理
  LOOP AT TD_ZPMT0066_SUB INTO LTH_ZPMT0066_SUB_TMP.
    LW_CURR_IDX = SY-TABIX.
    CLEAR TH_ZPMT0066.

    TH_ZPMT0066_SUB = LTH_ZPMT0066_SUB_TMP.
*----- レコード単位のエラーフラグ初期化
    LFLG_RECORD_ERR = C_FLG_OFF.

*----- 特売年月/企画Noブレイクで、登録単位itab/エラーフラグ初期化
    AT NEW AKTNR.
      CLEAR TD_ZPMT0066_SUB_UNIT.
      LFLG_UNIT_ERR = C_FLG_OFF.
    ENDAT.

*----- エラーチェックが多数あるため別フォームで実装
    PERFORM ERR_CHECKS USING    LW_CURR_IDX
                       CHANGING LFLG_RECORD_ERR.
    IF LFLG_RECORD_ERR = C_FLG_ON.
      LFLG_UNIT_ERR = C_FLG_ON.
    ENDIF.

*----- 特売年月/企画No 単位で登録するため、別itab作成
*----- そのための構造へ値を退避
    CLEAR TH_ZPMT0066_SUB_UNIT.
    TH_ZPMT0066_SUB_UNIT = TH_ZPMT0066_SUB.     "中間テーブルデータ
    TH_ZPMT0066_SUB_UNIT-INDEX = LW_CURR_IDX.   "現在行

*----- 特売年月/企画No 単位で登録するため、別itab作成
    IF LFLG_RECORD_ERR = C_FLG_OFF.
      APPEND TH_ZPMT0066_SUB_UNIT TO TD_ZPMT0066_SUB_UNIT.
    ENDIF.

*----- 特売年月/企画Noブレイクで登録処理実施
    AT END OF AKTNR.
*----- 登録単位でのエラーがない場合、登録処理実施
      IF LFLG_UNIT_ERR = C_FLG_OFF.

*       ZM5153X00101(特売マスタ)の場合、
*       →ZPMT0066(特売マスタ)登録
        PERFORM CREATE_ZPMT0066.

*----- 登録単位でのエラーが発生している場合、
*----- チェックエラーがないレコードもエラーステータス設定
*----- (特売年月/企画No単位で登録をするため)
      ELSE.
        LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
          CLEAR LW_MSG.
*         &1チェックにより&2となっています
          MESSAGE E416(ZC01) WITH TEXT-M06 TEXT-M07 INTO LW_MSG.
          PERFORM BUILD_LIST_TABLE
            USING LW_MSG                       "メッセージ内容
                  TH_ZPMT0066_SUB_UNIT-INDEX.  "現行INDEX
        ENDLOOP.
      ENDIF.
    ENDAT.
  ENDLOOP.

*----- 処理中の件数出力
  PERFORM ZCIO_INBOUND_MESSAGE IN PROGRAM ZCIO0101
                         USING C_INMSG_UPDCNT
                               P_IFID
                               CTR_ALL
                               CTR_SUC
                               CTR_ERR
                               FLG_ERR.

ENDFORM.                    " UPDATE_ZPMT0066


*&---------------------------------------------------------------------*
*&      Form  ERR_CHECKS
*&---------------------------------------------------------------------*
*       エラーチェック
*----------------------------------------------------------------------*
*      -->I_INDEX    元レコードのindex
*      <--O_ERR_FLG  エラーフラグ
*----------------------------------------------------------------------*
FORM ERR_CHECKS  USING    I_INDEX    TYPE I
                 CHANGING O_ERR_FLAG TYPE FLAG.
*----- データ定義
  DATA:
    LFLG_ERR_TMP  TYPE FLAG,           "ローカルエラーフラグ
    LW_ERR_MSG    TYPE STRING.         "エラーメッセージ

*----- 日付型項目チェック
*-----①特売期間FROM（VKDAB）日付項目チェック
  IF TH_ZPMT0066_SUB-VKDAB <> C_DATE_DEFAULT.
    PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-VKDAB
                                  C_VKDAB
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.

*-----②特売期間TO（VKDAI）日付項目チェック
  IF TH_ZPMT0066_SUB-VKDAI <> C_DATE_DEFAULT.
    PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-VKDAI
                                  C_VKDAI
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.

*-----③日替期間FROM（DAPFR）日付項目チェック
  IF TH_ZPMT0066_SUB-DAPFR <> C_DATE_DEFAULT.
    PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-DAPFR
                                  C_DAPFR
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.

*-----④日替期間TO（DAPTO）日付項目チェック
  IF TH_ZPMT0066_SUB-DAPTO <> C_DATE_DEFAULT.
    PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-DAPTO
                                  C_DAPTO
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.

*-----⑤原価適用開始日（DEFRM）日付項目チェック
  IF TH_ZPMT0066_SUB-DEFRM <> C_DATE_DEFAULT.
    PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-DEFRM
                                  C_DEFRM
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.


*-----⑥原価適用終了日（DELTO）日付項目チェック
  IF TH_ZPMT0066_SUB-DELTO <> C_DATE_DEFAULT.
    PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-DELTO
                                  C_DELTO
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.

*-----⑧店舗修正締切日（AKWED）日付項目チェック
  IF TH_ZPMT0066_SUB-AKWED <> C_DATE_DEFAULT.
    PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-AKWED
                                  C_AKWED
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.

*--> ADD 0016 START 2018/07/09 RS鈴木
*-----⑨データ作成日（CDATE）日付項目チェック
  IF TH_ZPMT0066_SUB-CDATE <> C_DATE_DEFAULT.
    PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-CDATE
                                  C_CDATE
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.
*--> ADD 0016 END 2018/07/09 RS鈴木


*-----⑩データ更新日（UDATE）日付項目チェック
  IF TH_ZPMT0066_SUB-UDATE <> C_DATE_DEFAULT.
    PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-UDATE
                                  C_UDATE
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.

*----- 数値項目チェック
*-----①発注単位入数（HAIRI）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-HAIRI
                                  C_HAIRI
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*-----②特売発注単位入数（TOIRI）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-TOIRI
                                  C_TOIRI
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*-----③荷姿単位入数（MENUM）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-MENUM
                                  C_MENUM
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*-----④ケース販売（CAHAN）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-CAHAN
                                  C_CAHAN
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*-----⑤発注予定数量（HACYO）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-HACYO
                                  C_HACYO
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*-----⑥通常原価（LOSFX）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-LOSFX
                                  C_LOSFX
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*-----⑦通常売価（VKPNE）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-VKPNE
                                  C_VKPNE
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*-----⑧特売原価（PLEKP）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-PLEKP
                                  C_PLEKP
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*-----⑨特売売価（PLVKP）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-PLVKP
                                  C_PLVKP
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*-----⑩続行売価（ZASAL）数値項目チェック
  PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                            USING TH_ZPMT0066_SUB-ZASAL
                                  C_ZASAL
                         CHANGING LFLG_ERR_TMP
                                  LW_ERR_MSG.
  IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
    PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                   I_INDEX.     "現行INDEX
    O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
    EXIT.                   "エラー処理を抜ける
  ENDIF.

*----- I/FID ZM5153X00102(店別特売発注書データ)の場合の追加チェック
  IF P_IFID = C_IFID_TOKUBAI_TRAN.
*----- 納品日（DLDAT）日付項目チェック
    IF TH_ZPMT0066_SUB-DLDAT <> C_DATE_DEFAULT.
      PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                              USING TH_ZPMT0066_SUB-DLDAT
                                    C_DLDAT
                           CHANGING LFLG_ERR_TMP
                                    LW_ERR_MSG.
      IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
        PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                       I_INDEX.     "現行INDEX
        O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
        EXIT.                   "エラー処理を抜ける
      ENDIF.
    ENDIF.

*----- 本部締切日（AKHED）日付項目チェック
    IF TH_ZPMT0066_SUB-AKHED <> C_DATE_DEFAULT.
      PERFORM ZCIO_CHECK_FIELD_DATE IN PROGRAM ZCIO0101
                              USING TH_ZPMT0066_SUB-AKHED
                                    C_AKHED
                           CHANGING LFLG_ERR_TMP
                                    LW_ERR_MSG.
      IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
        PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                       I_INDEX.     "現行INDEX
        O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
        EXIT.                   "エラー処理を抜ける
      ENDIF.
    ENDIF.

*----- 発注数量（HMNGE）数値項目チェック
    PERFORM ZCIO_CHECK_FIELD_NUMBER IN PROGRAM ZCIO0101
                              USING TH_ZPMT0066_SUB-HMNGE
                                    C_HMNGE
                           CHANGING LFLG_ERR_TMP
                                    LW_ERR_MSG.
    IF LFLG_ERR_TMP = C_FLG_ON.
*----- 結果リストITAB作成
      PERFORM BUILD_LIST_TABLE USING LW_ERR_MSG   "メッセージ
                                     I_INDEX.     "現行INDEX
      O_ERR_FLAG = C_FLG_ON.  "上位FORMへエラーフラグ引渡
      EXIT.                   "エラー処理を抜ける
    ENDIF.
  ENDIF.

ENDFORM.                    " ERR_CHECKS

*--> DEL 0016 START 2018/07/09 RS鈴木
**&---------------------------------------------------------------------*
**&      Form  CREATE_ZPTT0080
**&---------------------------------------------------------------------*
**       ZPTT0080(特売発注データ)登録
**----------------------------------------------------------------------*
*FORM CREATE_ZPTT0080 .
**----- ローカル定数定義
*  CONSTANTS:
*    LC_ZIFSTATUS_T(1) TYPE C VALUE '1'.  "特売処理ステータス設定値
*
**----- ローカル変数定義
*  DATA:
*    LW_HACCD      TYPE ZPTT0080-HACCD, "発注コード
*    LW_SUBRC      TYPE SY-SUBRC,       "リターンコード
*    LW_ERR_MSG    TYPE STRING,         "エラーメッセージ
*    LW_MSG        TYPE STRING,         "メッセージ内容
*    LW_COUNT      TYPE I.                                   "#EC NEEDED
*
**----- 発注コード体系の編集処理実施
*  READ TABLE TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT
*    INDEX 1.
*  PERFORM EDIT_CHECK_DIGIT
*    USING    TH_ZPMT0066_SUB_UNIT-SPART  "部門コード
*    CHANGING LW_HACCD                    "発注コード
*             LW_SUBRC.
*  IF LW_SUBRC <> 0.
*    MESSAGE S308(ZC01) WITH TEXT-M03            "NUMBER_GET_NEXT
*                            LW_SUBRC
*                       INTO LW_ERR_MSG.
*    LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
*      CLEAR LW_MSG.
*      MESSAGE E320(ZC01) WITH TEXT-M05 SY-SUBRC INTO LW_MSG.
*      PERFORM BUILD_LIST_TABLE
*        USING LW_MSG                       "メッセージ内容
*              TH_ZPMT0066_SUB_UNIT-INDEX.  "現行INDEX
*    ENDLOOP.
**----- 登録処理を実施せず次レコード処理にうつる
*    EXIT.
*  ENDIF.
*
**--> ADD 0014 START 2011/01/25 IBM004
**----- EOS部門コード変換
* CLEAR: TD_TEOSC_CONV[].
* SELECT KEY01
*        ITM01
*   INTO TABLE TD_TEOSC_CONV
*   FROM ZPMT0030
*   WHERE HANCD = C_HANCD_TEOSC
*   .
**--> ADD 0014 END   2011/01/25 IBM004
*
**----- レコード登録処理
*  CLEAR TD_ZPTT0080.
*  LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
**----- 特売発注書テーブルにマッピング
*    CLEAR TH_ZPTT0080.
*    TH_ZPTT0080-ZIFID = TH_ZPMT0066_SUB_UNIT-ZIFID.  "I/FID
*    TH_ZPTT0080-HACCD = LW_HACCD .                   "発注コード
*    TH_ZPTT0080-AKYMT = TH_ZPMT0066_SUB_UNIT-AKYMT.  "特売年月
*    TH_ZPTT0080-AKTNR = TH_ZPMT0066_SUB_UNIT-AKTNR.  "特売企画No.
*    TH_ZPTT0080-AKSHU = TH_ZPMT0066_SUB_UNIT-AKSHU.  "特売種別
*    TH_ZPTT0080-SPART = TH_ZPMT0066_SUB_UNIT-SPART.  "部門コード
*    TH_ZPTT0080-MATNR = TH_ZPMT0066_SUB_UNIT-MATNR.  "SAP商品コード
*    TH_ZPTT0080-BISMT = TH_ZPMT0066_SUB_UNIT-BISMT.  "商品コード
*    TH_ZPTT0080-WERKS = TH_ZPMT0066_SUB_UNIT-WERKS.  "店舗コード
*    TH_ZPTT0080-VKDAB = TH_ZPMT0066_SUB_UNIT-VKDAB.  "特売期間FROM
*    TH_ZPTT0080-VKDAI = TH_ZPMT0066_SUB_UNIT-VKDAI.  "特売期間TO
*    TH_ZPTT0080-DLDAT = TH_ZPMT0066_SUB_UNIT-DLDAT.  "納品日
*    TH_ZPTT0080-DAPFR = TH_ZPMT0066_SUB_UNIT-DAPFR.  "日替期間FROM
*    TH_ZPTT0080-DAPTO = TH_ZPMT0066_SUB_UNIT-DAPTO.  "日替期間TO
*    TH_ZPTT0080-DEFRM = TH_ZPMT0066_SUB_UNIT-DEFRM.  "原価適用開始日
*    TH_ZPTT0080-DELTO = TH_ZPMT0066_SUB_UNIT-DELTO.  "原価適用終了日
*    TH_ZPTT0080-AKWED = TH_ZPMT0066_SUB_UNIT-AKWED.  "店舗修正締切日
*    TH_ZPTT0080-AKHED = TH_ZPMT0066_SUB_UNIT-AKHED.  "本部締切日
*    TH_ZPTT0080-MATKL = TH_ZPMT0066_SUB_UNIT-MATKL.  "商品カテゴリ
*    TH_ZPTT0080-DAYKB = TH_ZPMT0066_SUB_UNIT-DAYKB.  "日替区分
*
**--> MOD 0014 START 2011/01/25 IBM004
**    TH_ZPTT0080-EOSSP = TH_ZPMT0066_SUB_UNIT-EOSSP.  "EOS部門コード
*    READ TABLE TD_TEOSC_CONV INTO TH_TEOSC_CONV
*      WITH KEY KEY01 = TH_ZPMT0066_SUB_UNIT-SPART.
*    IF SY-SUBRC = 0.
*      TH_ZPTT0080-EOSSP = TH_TEOSC_CONV-ITM01.
*    ENDIF.
**    TH_ZPTT0080-EOSSP = TH_ZPMT0066_SUB_UNIT-EOSSP.  "EOS部門コード
**--> MOD 0014 END 2011/01/25 IBM004
*
*    TH_ZPTT0080-DAIMA = TH_ZPMT0066_SUB_UNIT-DAIMA.  "代表商品コード
*    TH_ZPTT0080-LIFNR = TH_ZPMT0066_SUB_UNIT-LIFNR.  "取引先コード
*    TH_ZPTT0080-AKTXK = TH_ZPMT0066_SUB_UNIT-AKTXK.  "種別名称（漢字）
*    TH_ZPTT0080-MAKTX = TH_ZPMT0066_SUB_UNIT-MAKTX.  "商品名（漢字）
*    TH_ZPTT0080-MAKAX = TH_ZPMT0066_SUB_UNIT-MAKAX.  "商品名（カナ）
*    TH_ZPTT0080-KIKAJ = TH_ZPMT0066_SUB_UNIT-KIKAJ.  "規格名（漢字）
*    TH_ZPTT0080-KIKAN = TH_ZPMT0066_SUB_UNIT-KIKAN.  "規格名（カナ）
*    TH_ZPTT0080-BSTST = TH_ZPMT0066_SUB_UNIT-BSTST.  "発注単位
*    TH_ZPTT0080-HAIRI = TH_ZPMT0066_SUB_UNIT-HAIRI.  "発注単位入数
*    TH_ZPTT0080-TOIRI = TH_ZPMT0066_SUB_UNIT-TOIRI.  "特売発注単位入数
*    TH_ZPTT0080-MEIST = TH_ZPMT0066_SUB_UNIT-MEIST.  "荷姿単位
*    TH_ZPTT0080-MENUM = TH_ZPMT0066_SUB_UNIT-MENUM.  "荷姿単位入数
*    TH_ZPTT0080-CAHAN = TH_ZPMT0066_SUB_UNIT-CAHAN.  "ケース販売
**--> ADD 0009 START 2010/12/12 IBM004
*    TH_ZPTT0080-PLNUM = TH_ZPMT0066_SUB_UNIT-HACYO.  "売上高計画値
**--> ADD 0009 END   2010/12/12 IBM004
*    TH_ZPTT0080-HACYO = TH_ZPMT0066_SUB_UNIT-HMNGE.  "発注数量
*    TH_ZPTT0080-HACYO_U = TH_ZPMT0066_SUB_UNIT-HMNGE."発注変更済数量
*    TH_ZPTT0080-ORCOD
*      = TH_ZPMT0066_SUB_UNIT-ORCOD.  "発注サイクルコード
*    TH_ZPTT0080-SHKBN = TH_ZPMT0066_SUB_UNIT-SHKBN.  "数量変更可区分
*    TH_ZPTT0080-EOSKB = TH_ZPMT0066_SUB_UNIT-EOSKB.  "発注可区分
*    TH_ZPTT0080-REKUB = TH_ZPMT0066_SUB_UNIT-REKUB.  "回収可区分
*    TH_ZPTT0080-ZAKUB = TH_ZPMT0066_SUB_UNIT-ZAKUB.  "センター在庫区分
*    TH_ZPTT0080-MNKBN
*      = TH_ZPMT0066_SUB_UNIT-MNKBN.  "マークダウン対象区分
*    TH_ZPTT0080-HMNGE = TH_ZPMT0066_SUB_UNIT-HMNGE.  "売上数量
*    TH_ZPTT0080-LOSFX = TH_ZPMT0066_SUB_UNIT-LOSFX.  "通常原価
*    TH_ZPTT0080-VKPNE = TH_ZPMT0066_SUB_UNIT-VKPNE.  "通常売価
*    TH_ZPTT0080-PLEKP = TH_ZPMT0066_SUB_UNIT-PLEKP.  "特売原価
*    TH_ZPTT0080-PLVKP = TH_ZPMT0066_SUB_UNIT-PLVKP.  "特売売価
*    TH_ZPTT0080-ZASAL = TH_ZPMT0066_SUB_UNIT-ZASAL.  "続行売価
*
*    TH_ZPTT0080-UPKBN = TH_ZPMT0066_SUB_UNIT-UPKBN.  "マスタ更新区分
*    TH_ZPTT0080-CDATE = TH_ZPMT0066_SUB_UNIT-CDATE.  "データ作成日
*    TH_ZPTT0080-UDATE = TH_ZPMT0066_SUB_UNIT-UDATE.  "データ更新日
*
*    TH_ZPTT0080-ZIFSTATUS_T = LC_ZIFSTATUS_T.        "特売処理ステータス
*
*    TH_ZPTT0080-ZCRDAT = W_DATUM.                      "登録日付
*    TH_ZPTT0080-ZCRTIM = W_UZEIT.                      "登録時刻
*    TH_ZPTT0080-ZCRUSR = TH_ZPMT0066_SUB_UNIT-ZCRUSR.  "登録者
*    TH_ZPTT0080-ZUPDAT = W_DATUM.                      "更新日付
*    TH_ZPTT0080-ZUPTIM = W_UZEIT.                      "更新時刻
*    TH_ZPTT0080-ZUPUSR = TH_ZPMT0066_SUB_UNIT-ZUPUSR.  "更新者
*
*    APPEND TH_ZPTT0080 TO TD_ZPTT0080.
*  ENDLOOP.
*
*  CHECK TD_ZPTT0080 IS NOT INITIAL.
*
**----- 特売年月/特売企画Noをキーに既存レコード削除
*  SELECT COUNT( DISTINCT ZIFID )
*    FROM ZPTT0080
*    INTO LW_COUNT
*    WHERE ZIFID = TH_ZPMT0066_SUB_UNIT-ZIFID   "I/FID
*      AND AKSHU = TH_ZPMT0066_SUB_UNIT-AKSHU   "特売種別
*      AND VKDAB = TH_ZPMT0066_SUB_UNIT-VKDAB   "特売期間FROM
*      AND SPART = TH_ZPMT0066_SUB_UNIT-SPART   "部門
*      AND BISMT = TH_ZPMT0066_SUB_UNIT-BISMT   "商品コード
*      AND DLDAT = TH_ZPMT0066_SUB_UNIT-DLDAT   "納品日
**--> ADD 0005 START 2011/06/13 RS坂井
*      AND AKTNR = TH_ZPMT0066_SUB_UNIT-AKTNR.  "特売企画No
**--> ADD 0005 END 2011/06/13 RS坂井
*  IF SY-SUBRC = 0.
*    DELETE FROM ZPTT0080
*      WHERE ZIFID = TH_ZPMT0066_SUB_UNIT-ZIFID   "I/FID
*        AND AKSHU = TH_ZPMT0066_SUB_UNIT-AKSHU   "特売種別
*        AND VKDAB = TH_ZPMT0066_SUB_UNIT-VKDAB   "特売期間FROM
*        AND SPART = TH_ZPMT0066_SUB_UNIT-SPART   "部門
*        AND BISMT = TH_ZPMT0066_SUB_UNIT-BISMT   "商品コード
*        AND DLDAT = TH_ZPMT0066_SUB_UNIT-DLDAT   "納品日
**--> ADD 0005 START 2011/06/13 RS坂井
*        AND AKTNR = TH_ZPMT0066_SUB_UNIT-AKTNR.  "特売企画No
**--> ADD 0005 END 2011/06/13 RS坂井
*    COMMIT WORK.
*  ENDIF.
*
**----- Insert処理
*  INSERT ZPTT0080 FROM TABLE TD_ZPTT0080.
*
**----- エラーがあった場合
*  IF SY-SUBRC <> 0.
*    LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
*      CLEAR LW_MSG.
*      MESSAGE E320(ZC01) WITH TEXT-M05 SY-SUBRC INTO LW_MSG.
*      PERFORM BUILD_LIST_TABLE
*        USING LW_MSG                       "メッセージ内容
*              TH_ZPMT0066_SUB_UNIT-INDEX.  "現行INDEX
*    ENDLOOP.
*  ELSE.
**----- 正常終了
*    LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
*      TH_ZPMT0066_SUB-ZIFSTATUS = C_STATUS_SUC.
*      MESSAGE S003(ZC01) WITH SPACE
*                              SPACE
*                              SPACE
*                         INTO TH_ZPMT0066_SUB-ZIFMESSAGE.
*      MODIFY TD_ZPMT0066_SUB
*        FROM TH_ZPMT0066_SUB
*        INDEX TH_ZPMT0066_SUB_UNIT-INDEX  "現行INDEX
*      TRANSPORTING ZIFSTATUS
*                   ZIFMESSAGE.
**----- 正常件数カウント
*      CTR_SUC = CTR_SUC + 1.
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.                    " CREATE_ZPTT0080
*--> DEL 0016 END 2018/07/09 RS鈴木

*--> DEL 0016 START 2018/07/09 RS鈴木
**&---------------------------------------------------------------------*
**&      Form  EDIT_CHECK_DIGIT
**&---------------------------------------------------------------------*
**       発注コード体系の編集
**----------------------------------------------------------------------*
**       -->I_SPART    部門コード
**       <--O_HACCD    発注コード
**       <--O_SUBRC    リターンコード
**----------------------------------------------------------------------*
*FORM EDIT_CHECK_DIGIT USING    I_SPART  TYPE ZPESPART
*                      CHANGING O_HACCD  TYPE ZPEHCUCD
*                               O_SUBRC  TYPE SY-SUBRC.
**----- CONSTANTS定義
*  CONSTANTS:
*    LC_NR_RANGE_NR   TYPE INRI-NRRANGENR     "番号範囲番号
*      VALUE '00',
*    LC_OBJECT_52     TYPE INRI-OBJECT        "番号範囲オブジェクト
*      VALUE 'ZPPONO1021',
*    LC_OBJECT_55     TYPE INRI-OBJECT        "番号範囲オブジェクト
*      VALUE 'ZPPONO1022',
*    LC_OBJECT_58     TYPE INRI-OBJECT        "番号範囲オブジェクト
*      VALUE 'ZPPONO1023',
*    LC_OBJECT_60     TYPE INRI-OBJECT        "番号範囲オブジェクト
*      VALUE 'ZPPONO1024',
*    LC_OBJECT_65     TYPE INRI-OBJECT        "番号範囲オブジェクト
*      VALUE 'ZPPONO1025',
*    LC_SPART_52     TYPE ZPMT0066_SUB-SPART  "部門コード
*      VALUE '52',
*    LC_SPART_55     TYPE ZPMT0066_SUB-SPART  "部門コード
*      VALUE '55',
*    LC_SPART_58     TYPE ZPMT0066_SUB-SPART  "部門コード
*      VALUE '58',
*    LC_SPART_60     TYPE ZPMT0066_SUB-SPART  "部門コード
*      VALUE '60',
*    LC_SPART_65     TYPE ZPMT0066_SUB-SPART  "部門コード
*      VALUE '65'.
**----- データ定義
*  DATA:
*   LW_OBJECT        TYPE INRI-OBJECT,     "番号範囲オブジェクト
*   LW_ZSEQNO        TYPE ZPESEQNO,        "シーケンスNo
*   LW_BUSEQ         TYPE CHAR7,           "部門+SeqNo
*   LW_MULT2         TYPE CHAR2,
*   LW_MULT4         TYPE CHAR2,
*   LW_MULT6         TYPE CHAR2,
*   LW_SUM(2)        TYPE N,
*   LW_RESULT        TYPE N.
*
**----- 部門によって、番号範囲オブジェクトを設定する
*  CASE I_SPART.
*    WHEN LC_SPART_52.
*      LW_OBJECT = LC_OBJECT_52.
*    WHEN LC_SPART_55.
*      LW_OBJECT = LC_OBJECT_55.
*    WHEN LC_SPART_58.
*      LW_OBJECT = LC_OBJECT_58.
*    WHEN LC_SPART_60.
*      LW_OBJECT = LC_OBJECT_60.
*    WHEN LC_SPART_65.
*      LW_OBJECT = LC_OBJECT_65.
*  ENDCASE.
*
**----- 部門ごとに管理された連番SeqNoを取得する。
**----- シーケンスNo.の取得
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      NR_RANGE_NR             = LC_NR_RANGE_NR "番号範囲番号
*      OBJECT                  = LW_OBJECT      "番号範囲オブジェクト名
*    IMPORTING
*      NUMBER                  = LW_ZSEQNO       "シーケンスNo.
*    EXCEPTIONS
*      INTERVAL_NOT_FOUND      = 1
*      NUMBER_RANGE_NOT_INTERN = 2
*      OBJECT_NOT_FOUND        = 3
*      QUANTITY_IS_0           = 4
*      QUANTITY_IS_NOT_1       = 5
*      INTERVAL_OVERFLOW       = 6
*      BUFFER_OVERFLOW         = 7
*      OTHERS                  = 8.
*
*  IF SY-SUBRC <> 0.
*    O_SUBRC = SY-SUBRC.
*    RETURN.
*  ENDIF.
*
**-----発注コード体系の取得
*  CONCATENATE I_SPART LW_ZSEQNO+2(4) INTO LW_BUSEQ.
*  LW_MULT2 = LW_BUSEQ+1(1) * 2.
*  LW_MULT4 = LW_BUSEQ+3(1) * 2.
*  LW_MULT6 = LW_BUSEQ+5(1) * 2.
*  LW_SUM   = LW_BUSEQ+0(1) + LW_BUSEQ+2(1) + LW_BUSEQ+4(1)
*              + LW_MULT2+0(1) + LW_MULT2+1(1) + LW_MULT4+0(1) +
*              LW_MULT4+1(1) + LW_MULT6+0(1) + LW_MULT6+1(1).
*  LW_RESULT = LW_SUM MOD 10.
*  LW_RESULT = 10 - LW_RESULT.
*  LW_BUSEQ+6(1) = LW_RESULT.
*  O_HACCD = LW_BUSEQ.
*ENDFORM.                    " EDIT_CHECK_DIGIT
*--> DEL 0016 END 2018/07/09 RS鈴木

*&---------------------------------------------------------------------*
*&      Form  CREATE_ZPMT0066
*&---------------------------------------------------------------------*
*       ZPMT0066(特売マスタ)登録
*----------------------------------------------------------------------*
FORM CREATE_ZPMT0066 .
*----- ローカル変数定義
  DATA:
    LW_MSG   TYPE STRING,                        "メッセージ内容
    LW_COUNT TYPE I.                                        "#EC NEEDED
*--> ADD 0009 START 2010/12/27 IBM湯浅
  DATA:
*--> MOD 0009 START 2010/12/29 IBM004
*    LW_TABIX TYPE I,  "index
    LW_TABIX TYPE I VALUE 1,  "index
*--> MOD 0009 END 2010/12/29 IBM004

*--> ADD 0015 START 2015/09/01 RS鈴木
    LW_UPKBN_DEL TYPE C,  "マスタ更新区分 = 3:削除の場合、ON
*--> ADD 0015 END 2015/09/01 RS鈴木

    LTD_ZPMT0066_OLD TYPE SORTED TABLE OF ZPMT0066  "既存レコード
      WITH UNIQUE KEY MANDT ZIFID AKYMT AKTNR AKSHU SPART
                      MATNR BISMT WERKS VKDAB VKDAI DAPFR,
    LTH_ZPMT0066_OLD LIKE LINE OF LTD_ZPMT0066_OLD.
*--> ADD 0009 END   2010/12/27 IBM湯浅

*-->ADD 0015 START 2015/09/01 RS鈴木
*----- 初期値
  LW_UPKBN_DEL = C_UPKBN_DEL_OFF.
*-->ADD 0015 END 2015/09/01 RS鈴木
*----- レコード登録処理
  CLEAR TD_ZPMT0066.
  LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
*----- 特売マスタテーブルにマッピング
    CLEAR TH_ZPMT0066.

    MOVE-CORRESPONDING TH_ZPMT0066_SUB_UNIT TO TH_ZPMT0066. "#EC ENHOK

    TH_ZPMT0066-ZCRDAT = W_DATUM.                     "登録日付
    TH_ZPMT0066-ZCRTIM = W_UZEIT.                     "登録時刻
    TH_ZPMT0066-ZCRUSR = TH_ZPMT0066_SUB_UNIT-ZCRUSR. "登録者
    TH_ZPMT0066-ZUPDAT = W_DATUM.                     "更新日付
    TH_ZPMT0066-ZUPTIM = W_UZEIT.                     "更新時刻
    TH_ZPMT0066-ZUPUSR = TH_ZPMT0066_SUB_UNIT-ZUPUSR. "更新者
    APPEND TH_ZPMT0066 TO TD_ZPMT0066.

*-->ADD 0015 START 2015/09/01 RS鈴木
    IF ( TH_ZPMT0066-UPKBN = C_UPKBN_DEL ) AND ( LW_UPKBN_DEL = C_UPKBN_DEL_OFF ) .
        LW_UPKBN_DEL = C_UPKBN_DEL_ON.
    ENDIF.
*-->ADD 0015 END 2015/09/01 RS鈴木

  ENDLOOP.

*--> ADD 0008 START 2010/12/06 IBM004
*----- 納品日違いのレコードをまとめる
  DELETE ADJACENT DUPLICATES FROM TD_ZPMT0066
    COMPARING AKYMT AKTNR AKSHU SPART MATNR BISMT WERKS
      VKDAB VKDAI DAPFR.
*--> ADD 0008 END   2010/12/06 IBM004

*--> ADD 0009 START 2010/12/27 IBM湯浅
*----- 初回発注フラグ（FIRST_ORD_FLG）のみ引き継ぐ
*----- 既存レコード取得(全キー検索)
  SELECT * FROM ZPMT0066
    INTO TABLE LTD_ZPMT0066_OLD
    FOR ALL ENTRIES IN TD_ZPMT0066
    WHERE ZIFID = TD_ZPMT0066-ZIFID  "IFID
      AND AKYMT = TD_ZPMT0066-AKYMT  "特売年月
      AND AKTNR = TD_ZPMT0066-AKTNR  "特売企画NO
      AND AKSHU = TD_ZPMT0066-AKSHU  "特売種別
      AND SPART = TD_ZPMT0066-SPART  "部門
      AND MATNR = TD_ZPMT0066-MATNR  "SAP商品
      AND BISMT = TD_ZPMT0066-BISMT  "商品
      AND WERKS = TD_ZPMT0066-WERKS  "店舗
      AND VKDAB = TD_ZPMT0066-VKDAB  "特売期間ｆｒ
      AND VKDAI = TD_ZPMT0066-VKDAI  "特売期間ｔｏ
      AND DAPFR = TD_ZPMT0066-DAPFR. "日替わりｆｒ

  IF SY-SUBRC = 0.

    LOOP AT TD_ZPMT0066 INTO TH_ZPMT0066.
*----- 既存レコード読み込み
      READ TABLE LTD_ZPMT0066_OLD INTO LTH_ZPMT0066_OLD
        WITH TABLE KEY MANDT = SY-MANDT
                       ZIFID = TH_ZPMT0066-ZIFID  "IFID
                       AKYMT = TH_ZPMT0066-AKYMT  "特売年月
                       AKTNR = TH_ZPMT0066-AKTNR  "特売企画NO
                       AKSHU = TH_ZPMT0066-AKSHU  "特売種別
                       SPART = TH_ZPMT0066-SPART  "部門
                       MATNR = TH_ZPMT0066-MATNR  "SAP商品
                       BISMT = TH_ZPMT0066-BISMT  "商品
                       WERKS = TH_ZPMT0066-WERKS  "店舗
                       VKDAB = TH_ZPMT0066-VKDAB  "特売期間ｆｒ
                       VKDAI = TH_ZPMT0066-VKDAI  "特売期間ｔｏ
                       DAPFR = TH_ZPMT0066-DAPFR. "日替わりｆｒ
*----- 既存レコードから初回発注フラグを取得し更新
      IF SY-SUBRC = 0.
        TH_ZPMT0066-FIRST_ORD_FLG = LTH_ZPMT0066_OLD-FIRST_ORD_FLG.
        MODIFY TD_ZPMT0066 FROM TH_ZPMT0066
          INDEX LW_TABIX
          TRANSPORTING FIRST_ORD_FLG.
      ENDIF.

*--> ADD 0010 START 2010/12/29 IBM004
    LW_TABIX = LW_TABIX + 1.
*--> ADD 0010 END 2010/12/29 IBM004

    ENDLOOP.
  ENDIF.
*--> ADD 0009 END   2010/12/27 IBM湯浅

*--> ADD 0010 START 2010/12/29 IBM004
  CLEAR: LW_TABIX.
*--> ADD 0010 END 2010/12/29 IBM004


*----- 特売年月/特売企画Noをキーに既存レコード削除
  SELECT COUNT( DISTINCT ZIFID )
    FROM ZPMT0066
    INTO LW_COUNT
    WHERE ZIFID = TH_ZPMT0066_SUB_UNIT-ZIFID   "I/FID
      AND AKYMT = TH_ZPMT0066_SUB_UNIT-AKYMT   "特売年月
      AND AKTNR = TH_ZPMT0066_SUB_UNIT-AKTNR.  "特売企画No
  IF SY-SUBRC = 0.
    DELETE FROM ZPMT0066
      WHERE ZIFID = TH_ZPMT0066_SUB_UNIT-ZIFID   "I/FID
        AND AKYMT = TH_ZPMT0066_SUB_UNIT-AKYMT   "特売年月
        AND AKTNR = TH_ZPMT0066_SUB_UNIT-AKTNR.  "特売企画No
    COMMIT WORK.
  ENDIF.

*-->MOD 0015 START 2015/09/01 RS鈴木
**----- Insert処理
*  INSERT ZPMT0066 FROM TABLE TD_ZPMT0066.
    IF ( LW_UPKBN_DEL = C_UPKBN_DEL_ON )  .
        SY-SUBRC = 0.
     ELSE.
*----- Insert処理
      INSERT ZPMT0066 FROM TABLE TD_ZPMT0066.
    ENDIF.
*-->MOD 0015 END 2015/09/01 RS鈴木

*----- エラーがあった場合
  IF SY-SUBRC <> 0.
    LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
      CLEAR LW_MSG.
      MESSAGE E320(ZC01) WITH TEXT-M04 SY-SUBRC INTO LW_MSG.
      PERFORM BUILD_LIST_TABLE
        USING LW_MSG                       "メッセージ内容
              TH_ZPMT0066_SUB_UNIT-INDEX.  "現行INDEX
    ENDLOOP.
  ELSE.
*----- 正常終了
    LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
      TH_ZPMT0066_SUB-ZIFSTATUS = C_STATUS_SUC.
      MESSAGE S003(ZC01) WITH SPACE
                              SPACE
                              SPACE
                         INTO TH_ZPMT0066_SUB-ZIFMESSAGE.
      MODIFY TD_ZPMT0066_SUB
        FROM TH_ZPMT0066_SUB
        INDEX TH_ZPMT0066_SUB_UNIT-INDEX  "現行INDEX
      TRANSPORTING ZIFSTATUS
                   ZIFMESSAGE.
*----- 正常件数カウント
      CTR_SUC = CTR_SUC + 1.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " CREATE_ZPMT0066

*--> DEL 0004 START 2010/11/05 IBM湯浅
**&---------------------------------------------------------------------
**
**&      Form  CREATE_MAIN_TABLE
**&---------------------------------------------------------------------
**
**       特売マスタ登録処理
**----------------------------------------------------------------------
**
**       -->I_INDEX     現行INDEX
**----------------------------------------------------------------------
**
*FORM CREATE_MAIN_TABLE USING I_INDEX TYPE I.
*  DATA: LW_MSG TYPE STRING.                        "メッセージ内容
*
*  MOVE-CORRESPONDING TH_ZPMT0066_SUB TO TH_ZPMT0066.        "#EC ENHOK
*
*  TH_ZPMT0066-ZCRDAT = W_DATUM.                    "登録日付
*  TH_ZPMT0066-ZCRTIM = W_UZEIT.                    "登録時刻
*  TH_ZPMT0066-ZCRUSR = TH_ZPMT0066_SUB-ZCRUSR.     "登録者
*  TH_ZPMT0066-ZUPDAT = W_DATUM.                    "更新日付
*  TH_ZPMT0066-ZUPTIM = W_UZEIT.                    "更新時刻
*  TH_ZPMT0066-ZUPUSR = TH_ZPMT0066_SUB-ZUPUSR.     "更新者
*
**-----特売マスタテーブル登録
**--> ADD 0002 START 2010/09/14 IBM004
**  INSERT INTO ZPMT0066 VALUES TH_ZPMT0066.
*  MODIFY  ZPMT0066 FROM TH_ZPMT0066.
**--> ADD 0002 END 2010/09/14 IBM004
*
**----- エラーがあった場合
*  IF SY-SUBRC <> 0.
*    CLEAR LW_MSG.
*    MESSAGE E320(ZC01) WITH TEXT-M04 SY-SUBRC INTO LW_MSG.
*    PERFORM BUILD_LIST_TABLE USING LW_MSG    "メッセージ内容
*                                   I_INDEX.  "現行INDEX
*  ELSE.
**----- 正常終了
*    TH_ZPMT0066_SUB-ZIFSTATUS = C_STATUS_SUC.
*    MESSAGE S003(ZC01) WITH SPACE
*                            SPACE
*                            SPACE
*                       INTO TH_ZPMT0066_SUB-ZIFMESSAGE.
*    MODIFY TD_ZPMT0066_SUB FROM TH_ZPMT0066_SUB
*                          INDEX I_INDEX
*                   TRANSPORTING ZIFSTATUS
*                                ZIFMESSAGE.
**----- 正常件数カウント
*    CTR_SUC = CTR_SUC + 1.
*  ENDIF.
*
*ENDFORM.                    " CREATE_MAIN_TABLE
*--> DEL 0004 END   2010/11/05 IBM湯浅

*&---------------------------------------------------------------------*
*&      Form  BUILD_LIST_TABLE
*&---------------------------------------------------------------------*
*       結果リストITAB作成
*----------------------------------------------------------------------*
*      -->I_MSG       メッセージ内容
*      -->I_INDEX     現行INDEX
*----------------------------------------------------------------------*
FORM BUILD_LIST_TABLE  USING    I_MSG   TYPE STRING
                                I_INDEX TYPE I.
*----- エラーフラグon
  FLG_ERR = C_FLG_ON.
  TH_ZPMT0066_SUB-ZIFSTATUS  = C_STATUS_ERR.
  TH_ZPMT0066_SUB-ZIFMESSAGE = I_MSG.
  MODIFY TD_ZPMT0066_SUB FROM TH_ZPMT0066_SUB
         INDEX I_INDEX
         TRANSPORTING ZIFSTATUS ZIFMESSAGE.

*----- 結果リストITAB作成
  TH_LIST            = TH_ZPMT0066_SUB.
  TH_LIST-ZIFSTATUS  = C_STATUS_ERR.
  TH_LIST-ZIFMESSAGE = I_MSG.
  APPEND TH_LIST TO TD_LIST.

*-----異常件数カウント
  CTR_ERR = CTR_ERR + 1.

ENDFORM.                    " BUILD_LIST_TABLE

*&---------------------------------------------------------------------*
*&      Form  UPDATE_SUB_TABLE
*&---------------------------------------------------------------------*
*       中間テーブル更新
*----------------------------------------------------------------------*
FORM UPDATE_SUB_TABLE .
*----- アドオン中間受信テーブル更新
  UPDATE ZPMT0066_SUB FROM TABLE TD_ZPMT0066_SUB.
  COMMIT WORK.

ENDFORM.                    " UPDATE_SUB_TABLE

*&---------------------------------------------------------------------*
*&      Form  DELETE_FILES
*&---------------------------------------------------------------------*
*       正常終了ファイル削除処理
*----------------------------------------------------------------------*
FORM DELETE_FILES .
  DATA: LW_COUNT TYPE I.

*----- 全件終了していたら正常ファイルを削除
  LOOP AT TD_IFID INTO TH_IFID.
    SELECT COUNT( * ) FROM ZPMT0066_SUB
      INTO LW_COUNT
      WHERE ZIFID     =  TH_IFID-ZIFID
        AND ZIFCOUNT  =  TH_IFID-ZIFCOUNT
        AND ZIFSTATUS =  C_STATUS_YET.
    IF SY-SUBRC <> 0.
      PERFORM ZCIO_DELETE_SFILE_MAIN IN PROGRAM ZCIO0101
        USING TH_IFID-ZIFID
              TH_IFID-ZIFCOUNT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DELETE_FILES

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_REPORT
*&---------------------------------------------------------------------*
*       処理結果リスト出力
*----------------------------------------------------------------------*
FORM OUTPUT_REPORT .
  DATA: LTH_LIST_TMP LIKE TH_LIST.

  SORT TD_LIST BY AKTNR     "特売企画No
                  AKSHU     "特売種別
                  SPART     "部門コード
                  BISMT     "商品コード
                  WERKS     "店舗コード
                  VKDAB     "特売期間From
                  VKDAI.    "特売期間To


*----- 処理結果リスト出力
  LOOP AT TD_LIST INTO LTH_LIST_TMP.
    TH_LIST = LTH_LIST_TMP.
    NEW-LINE.
    AT NEW BISMT.
      WRITE: 01     TH_LIST-AKTNR,      "特売企画No
             12     TH_LIST-AKSHU,      "特売種別
             21     TH_LIST-SPART,      "部門コード
             32     TH_LIST-BISMT,      "商品コード
             51     TH_LIST-WERKS,      "店舗コード
             62     TH_LIST-VKDAB,      "特売期間From
             75     TH_LIST-VKDAI.      "特売期間To
    ENDAT.
    WRITE:   86     TH_LIST-ZIFMESSAGE. "エラーメッセージ
  ENDLOOP.

*----- 共通帳票フッタ（WRITE）
  PERFORM INC_COMMON_FOOTER_WRITE USING TEXT-R01.

ENDFORM.                    " OUTPUT_REPORT

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_REPORT_HEADER
*&---------------------------------------------------------------------*
*       ヘッダ出力処理
*----------------------------------------------------------------------*
FORM OUTPUT_REPORT_HEADER .
*----- 共通ヘッダ出力
  PERFORM INC_COMMON_HEADER_WRITE USING TEXT-R01 P_IFID.

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
    /01 TEXT-R02, 12 CTR_ALL,23 TEXT-R13,     "処理件数
    /01 TEXT-R03, 12 CTR_SUC,23 TEXT-R13,     "正常件数
    /01 TEXT-R04, 12 CTR_ERR,23 TEXT-R13.     "異常件数

*----- 項目見出し行
  SKIP 1.
  WRITE:
     /1  TEXT-R05,   "特売企画No
      12 TEXT-R06,   "特売種別
      21 TEXT-R07,   "部門コード
      32 TEXT-R08,   "商品コード
      51 TEXT-R09,   "店舗コード
      62 TEXT-R10,   "特売期間From
      75 TEXT-R11,   "特売期間To
      86 TEXT-R12.   "エラーメッセージ

  ULINE.

ENDFORM.                    " HEADER_WRITE

*--> DEL 0016 START 2018/07/09 RS鈴木
**&---------------------------------------------------------------------*
**&      Form  DELETE_ZPTT0080
**&---------------------------------------------------------------------*
**       週次の特売データ削除
**----------------------------------------------------------------------*
*FORM DELETE_ZPTT0080 .
*
** ERPテーブルにある既存データを全件削除する
*  DELETE FROM ZPTT0080.
*
** 全件削除エラーが発生の時
*  IF SY-SUBRC <> 0 AND SY-SUBRC <> 4.
**   処理中止フラグon
*    FLG_STOP = C_FLG_ON.
**   エラーフラグon
*    FLG_ERR = C_FLG_ON.
*    MESSAGE S328(ZC01) WITH TEXT-M05 SY-SUBRC.
*  ENDIF.
*
*ENDFORM.                    " DELETE_ZPTT0080
*--> DEL 0016 END 2018/07/09 RS鈴木

*--> DEL 0016 START 2018/07/09 RS鈴木
**--> ADD 0009 START   2010/12/25 IBM004
**&---------------------------------------------------------------------*
**&      FORM  UPDATE_ZPTT0080_DAILY
**&---------------------------------------------------------------------*
**       ZPTT0080(特売発注データ)登録
**----------------------------------------------------------------------*
*FORM UPDATE_ZPTT0080_DAILY.
**----- ローカル定数定義
*  CONSTANTS:
*    LC_ZIFSTATUS_T(1) TYPE C VALUE '1'.  "特売処理ステータス設定値
*
**----- ローカルタイプ定義
*  TYPES:
**   更新対象判定用
*    BEGIN OF LTYP_ZPTT0080,
*      SPART TYPE ZPTT0080-SPART,  "部門
*      AKHED TYPE ZPTT0080-AKHED,  "店舗修正締切日
*    END   OF LTYP_ZPTT0080,
**   既存レコード取得用
*    BEGIN OF LTYP_ZPTT0080_OLD,
*      AKYMT TYPE ZPTT0080-AKYMT,  "特売年月
*      AKTNR TYPE ZPTT0080-AKTNR,  "特売企画NO
*      AKSHU TYPE ZPTT0080-AKSHU,  "特売種別
*      SPART TYPE ZPTT0080-SPART,  "部門
*      MATNR TYPE ZPTT0080-MATNR,  "SAP商品
*      BISMT TYPE ZPTT0080-BISMT,  "商品
*      WERKS TYPE ZPTT0080-WERKS,  "サイト
*      VKDAB TYPE ZPTT0080-VKDAB,  "特売ｆｒ
*      VKDAI TYPE ZPTT0080-VKDAI,  "特売ｔｏ
*      DLDAT TYPE ZPTT0080-DLDAT,  "納品日
*      DAPFR TYPE ZPTT0080-DAPFR,  "日替わりｆｒ
*      ZIFID TYPE ZPTT0080-ZIFID,  "IFID
*      HACCD TYPE ZPTT0080-HACCD,  "発注コード
*    END   OF LTYP_ZPTT0080_OLD.
*
**----- ローカル変数定義
*  DATA:
**   更新対象判定用
*    LTD_ZPTT0080 TYPE SORTED TABLE OF LTYP_ZPTT0080
*      WITH UNIQUE KEY SPART AKHED,
*    LTH_ZPTT0080 LIKE LINE OF LTD_ZPTT0080,
**   既存レコード取得
*    LTD_ZPTT0080_OLD TYPE SORTED TABLE OF LTYP_ZPTT0080_OLD
*      WITH NON-UNIQUE KEY
*        AKYMT   "特売年月
*        AKTNR   "特売企画NO
*        AKSHU   "特売種別
*        SPART   "部門
*        MATNR   "SAP商品
*        BISMT   "商品
*        WERKS   "サイト
*        VKDAB   "特売ｆｒ
*        VKDAI   "特売ｔｏ
*        DLDAT   "納品日
*        DAPFR,  "日替わりｆｒ
*    LTH_ZPTT0080_OLD LIKE LINE OF LTD_ZPTT0080_OLD.
*
*  DATA:
*    LW_ZIFID      TYPE ZPTT0080-ZIFID, "IFID
*    LW_HACCD      TYPE ZPTT0080-HACCD, "発注コード
*    LW_SUBRC      TYPE SY-SUBRC,       "リターンコード
*    LW_ERR_MSG    TYPE STRING,         "エラーメッセージ
*    LW_MSG        TYPE STRING,         "メッセージ内容
*    LW_COUNT      TYPE I.                                   "#EC NEEDED
*
**----- ジョブ処理日付の取得
*  IF W_JOBDATE IS INITIAL.
*    PERFORM GET_JOBDATE.
*  ENDIF.
*
**----- 処理対象の判定レコード取得
*  SELECT SPART  "部門
*         AKHED  "本部締切日
*    FROM ZPTT0080
*    INTO TABLE LTD_ZPTT0080
*    FOR ALL ENTRIES IN TD_ZPMT0066_SUB_UNIT
*    WHERE SPART = TD_ZPMT0066_SUB_UNIT-SPART    "部門
*      AND AKHED = TD_ZPMT0066_SUB_UNIT-AKHED_D. "本部締切日
**----- 既存レコード取得
*  SELECT AKYMT   "特売年月
*         AKTNR   "特売企画NO
*         AKSHU   "特売種別
*         SPART   "部門
*         MATNR   "SAP商品
*         BISMT   "商品
*         WERKS   "サイト
*         VKDAB   "特売ｆｒ
*         VKDAI   "特売ｔｏ
*         DLDAT   "納品日
*         DAPFR   "日替わりｆｒ
*         ZIFID   "IFID
*         HACCD   "発注コード
*    FROM ZPTT0080
*    INTO TABLE LTD_ZPTT0080_OLD
*    FOR ALL ENTRIES IN TD_ZPMT0066_SUB_UNIT
*    WHERE AKYMT = TD_ZPMT0066_SUB_UNIT-AKYMT    "特売年月
*      AND AKTNR = TD_ZPMT0066_SUB_UNIT-AKTNR    "特売企画NO
*      AND AKSHU = TD_ZPMT0066_SUB_UNIT-AKSHU    "特売種別
*      AND SPART = TD_ZPMT0066_SUB_UNIT-SPART    "部門
*      AND MATNR = TD_ZPMT0066_SUB_UNIT-MATNR    "SAP商品
*      AND BISMT = TD_ZPMT0066_SUB_UNIT-BISMT    "商品
*      AND WERKS = TD_ZPMT0066_SUB_UNIT-WERKS    "サイト
*      AND VKDAB = TD_ZPMT0066_SUB_UNIT-VKDAB_D    "特売ｆｒ
*      AND VKDAI = TD_ZPMT0066_SUB_UNIT-VKDAI_D    "特売ｔｏ
*      AND DLDAT = TD_ZPMT0066_SUB_UNIT-DLDAT_D    "納品日
*      AND DAPFR = TD_ZPMT0066_SUB_UNIT-DAPFR_D.   "日替わりｆｒ
*
**--> ADD 0014 START 2011/01/25 IBM004
**----- EOS部門コード変換
* CLEAR: TD_TEOSC_CONV[].
* SELECT KEY01
*        ITM01
*   INTO TABLE TD_TEOSC_CONV
*   FROM ZPMT0030
*   WHERE HANCD = C_HANCD_TEOSC
*   .
**--> ADD 0014 END   2011/01/25 IBM004
*
*
**----- レコード登録処理
*  CLEAR TD_ZPTT0080.
*  LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
*
**----- 更新対象判定
*    READ TABLE LTD_ZPTT0080 INTO LTH_ZPTT0080
*      WITH TABLE KEY SPART = TH_ZPMT0066_SUB_UNIT-SPART    "部門
*                     AKHED = TH_ZPMT0066_SUB_UNIT-AKHED_D. "本部締切日
**   部門/本部締切日がZPTT0080に存在しない場合、
**   更新対象外として次レコード処理
*    IF SY-SUBRC <> 0.
*      CONTINUE.
*    ENDIF.
**   店舗締切日が当日より前の場合、
**   更新対象外として次レコード処理
*    IF TH_ZPMT0066_SUB_UNIT-AKWED < W_JOBDATE.
*      CONTINUE.
*    ENDIF.
*
**----- 既存レコードの読み込み
*    READ TABLE LTD_ZPTT0080_OLD INTO LTH_ZPTT0080_OLD
*      WITH TABLE KEY AKYMT = TH_ZPMT0066_SUB_UNIT-AKYMT    "特売年月
*                     AKTNR = TH_ZPMT0066_SUB_UNIT-AKTNR    "特売企画NO
*                     AKSHU = TH_ZPMT0066_SUB_UNIT-AKSHU    "特売種別
*                     SPART = TH_ZPMT0066_SUB_UNIT-SPART    "部門
*                     MATNR = TH_ZPMT0066_SUB_UNIT-MATNR    "SAP商品
*                     BISMT = TH_ZPMT0066_SUB_UNIT-BISMT    "商品
*                     WERKS = TH_ZPMT0066_SUB_UNIT-WERKS    "サイト
*                     VKDAB = TH_ZPMT0066_SUB_UNIT-VKDAB    "特売ｆｒ
*                     VKDAI = TH_ZPMT0066_SUB_UNIT-VKDAI    "特売ｔｏ
*                     DLDAT = TH_ZPMT0066_SUB_UNIT-DLDAT    "納品日
*                     DAPFR = TH_ZPMT0066_SUB_UNIT-DAPFR.   "日替わりｆｒ
**   既存レコードが存在する場合、更新処理(既存発注番号使用)
*    IF SY-SUBRC = 0.
*      LW_ZIFID = LTH_ZPTT0080_OLD-ZIFID.
*      LW_HACCD = LTH_ZPTT0080_OLD-HACCD.
*
**   既存レコードが存在しない場合、新規登録(発注番号採番)
*    ELSE.
*      LW_ZIFID = TH_ZPMT0066_SUB_UNIT-ZIFID.  "I/FID
*      PERFORM EDIT_CHECK_DIGIT
*        USING    TH_ZPMT0066_SUB_UNIT-SPART  "部門コード
*        CHANGING LW_HACCD                    "発注コード
*                 LW_SUBRC.
*    ENDIF.
*
**----- 特売発注書テーブルにマッピング
*    CLEAR TH_ZPTT0080.
*    TH_ZPTT0080-ZIFID = LW_ZIFID.                    "I/FID
*    TH_ZPTT0080-HACCD = LW_HACCD .                   "発注コード
*    TH_ZPTT0080-AKYMT = TH_ZPMT0066_SUB_UNIT-AKYMT.  "特売年月
*    TH_ZPTT0080-AKTNR = TH_ZPMT0066_SUB_UNIT-AKTNR.  "特売企画No.
*    TH_ZPTT0080-AKSHU = TH_ZPMT0066_SUB_UNIT-AKSHU.  "特売種別
*    TH_ZPTT0080-SPART = TH_ZPMT0066_SUB_UNIT-SPART.  "部門コード
*    TH_ZPTT0080-MATNR = TH_ZPMT0066_SUB_UNIT-MATNR.  "SAP商品コード
*    TH_ZPTT0080-BISMT = TH_ZPMT0066_SUB_UNIT-BISMT.  "商品コード
*    TH_ZPTT0080-WERKS = TH_ZPMT0066_SUB_UNIT-WERKS.  "店舗コード
*    TH_ZPTT0080-VKDAB = TH_ZPMT0066_SUB_UNIT-VKDAB.  "特売期間FROM
*    TH_ZPTT0080-VKDAI = TH_ZPMT0066_SUB_UNIT-VKDAI.  "特売期間TO
*    TH_ZPTT0080-DLDAT = TH_ZPMT0066_SUB_UNIT-DLDAT.  "納品日
*    TH_ZPTT0080-DAPFR = TH_ZPMT0066_SUB_UNIT-DAPFR.  "日替期間FROM
*    TH_ZPTT0080-DAPTO = TH_ZPMT0066_SUB_UNIT-DAPTO.  "日替期間TO
*    TH_ZPTT0080-DEFRM = TH_ZPMT0066_SUB_UNIT-DEFRM.  "原価適用開始日
*    TH_ZPTT0080-DELTO = TH_ZPMT0066_SUB_UNIT-DELTO.  "原価適用終了日
*    TH_ZPTT0080-AKWED = TH_ZPMT0066_SUB_UNIT-AKWED.  "店舗修正締切日
*    TH_ZPTT0080-AKHED = TH_ZPMT0066_SUB_UNIT-AKHED.  "本部締切日
*    TH_ZPTT0080-MATKL = TH_ZPMT0066_SUB_UNIT-MATKL.  "商品カテゴリ
*    TH_ZPTT0080-DAYKB = TH_ZPMT0066_SUB_UNIT-DAYKB.  "日替区分
*
**--> MOD 0014 START 2011/01/25 IBM004
**    TH_ZPTT0080-EOSSP = TH_ZPMT0066_SUB_UNIT-EOSSP.  "EOS部門コード
*    READ TABLE TD_TEOSC_CONV INTO TH_TEOSC_CONV
*      WITH KEY KEY01 = TH_ZPMT0066_SUB_UNIT-SPART.
*    IF SY-SUBRC = 0.
*      TH_ZPTT0080-EOSSP = TH_TEOSC_CONV-ITM01.
*    ENDIF.
**--> MOD 0014 END 2011/01/25 IBM004
*
*    TH_ZPTT0080-DAIMA = TH_ZPMT0066_SUB_UNIT-DAIMA.  "代表商品コード
*    TH_ZPTT0080-LIFNR = TH_ZPMT0066_SUB_UNIT-LIFNR.  "取引先コード
*    TH_ZPTT0080-AKTXK = TH_ZPMT0066_SUB_UNIT-AKTXK.  "種別名称（漢字）
*    TH_ZPTT0080-MAKTX = TH_ZPMT0066_SUB_UNIT-MAKTX.  "商品名（漢字）
*    TH_ZPTT0080-MAKAX = TH_ZPMT0066_SUB_UNIT-MAKAX.  "商品名（カナ）
*    TH_ZPTT0080-KIKAJ = TH_ZPMT0066_SUB_UNIT-KIKAJ.  "規格名（漢字）
*    TH_ZPTT0080-KIKAN = TH_ZPMT0066_SUB_UNIT-KIKAN.  "規格名（カナ）
*    TH_ZPTT0080-BSTST = TH_ZPMT0066_SUB_UNIT-BSTST.  "発注単位
*    TH_ZPTT0080-HAIRI = TH_ZPMT0066_SUB_UNIT-HAIRI.  "発注単位入数
*    TH_ZPTT0080-TOIRI = TH_ZPMT0066_SUB_UNIT-TOIRI.  "特売発注単位入数
*    TH_ZPTT0080-MEIST = TH_ZPMT0066_SUB_UNIT-MEIST.  "荷姿単位
*    TH_ZPTT0080-MENUM = TH_ZPMT0066_SUB_UNIT-MENUM.  "荷姿単位入数
*    TH_ZPTT0080-CAHAN = TH_ZPMT0066_SUB_UNIT-CAHAN.  "ケース販売
**--> ADD 0009 START 2010/12/12 IBM004
*    TH_ZPTT0080-PLNUM = TH_ZPMT0066_SUB_UNIT-HACYO.  "売上高計画値
**--> ADD 0009 END   2010/12/12 IBM004
*    TH_ZPTT0080-HACYO = TH_ZPMT0066_SUB_UNIT-HMNGE.  "発注数量
*    TH_ZPTT0080-HACYO_U = TH_ZPMT0066_SUB_UNIT-HMNGE."発注変更済数量
*    TH_ZPTT0080-ORCOD
*      = TH_ZPMT0066_SUB_UNIT-ORCOD.  "発注サイクルコード
*    TH_ZPTT0080-SHKBN = TH_ZPMT0066_SUB_UNIT-SHKBN.  "数量変更可区分
*    TH_ZPTT0080-EOSKB = TH_ZPMT0066_SUB_UNIT-EOSKB.  "発注可区分
*    TH_ZPTT0080-REKUB = TH_ZPMT0066_SUB_UNIT-REKUB.  "回収可区分
*    TH_ZPTT0080-ZAKUB = TH_ZPMT0066_SUB_UNIT-ZAKUB.  "センター在庫区分
*    TH_ZPTT0080-MNKBN
*      = TH_ZPMT0066_SUB_UNIT-MNKBN.  "マークダウン対象区分
*    TH_ZPTT0080-HMNGE = TH_ZPMT0066_SUB_UNIT-HMNGE.  "売上数量
*    TH_ZPTT0080-LOSFX = TH_ZPMT0066_SUB_UNIT-LOSFX.  "通常原価
*    TH_ZPTT0080-VKPNE = TH_ZPMT0066_SUB_UNIT-VKPNE.  "通常売価
*    TH_ZPTT0080-PLEKP = TH_ZPMT0066_SUB_UNIT-PLEKP.  "特売原価
*    TH_ZPTT0080-PLVKP = TH_ZPMT0066_SUB_UNIT-PLVKP.  "特売売価
*    TH_ZPTT0080-ZASAL = TH_ZPMT0066_SUB_UNIT-ZASAL.  "続行売価
*    TH_ZPTT0080-UPKBN = TH_ZPMT0066_SUB_UNIT-UPKBN.  "マスタ更新区分
*    TH_ZPTT0080-CDATE = TH_ZPMT0066_SUB_UNIT-CDATE.  "データ作成日
*    TH_ZPTT0080-UDATE = TH_ZPMT0066_SUB_UNIT-UDATE.  "データ更新日
*    TH_ZPTT0080-ZIFSTATUS_T = LC_ZIFSTATUS_T.        "特売処理ステータス
*    TH_ZPTT0080-ZCRDAT = W_DATUM.                      "登録日付
*    TH_ZPTT0080-ZCRTIM = W_UZEIT.                      "登録時刻
*    TH_ZPTT0080-ZCRUSR = TH_ZPMT0066_SUB_UNIT-ZCRUSR.  "登録者
*    TH_ZPTT0080-ZUPDAT = W_DATUM.                      "更新日付
*    TH_ZPTT0080-ZUPTIM = W_UZEIT.                      "更新時刻
*    TH_ZPTT0080-ZUPUSR = TH_ZPMT0066_SUB_UNIT-ZUPUSR.  "更新者
*    APPEND TH_ZPTT0080 TO TD_ZPTT0080.
*  ENDLOOP.
*
*  CHECK TD_ZPTT0080 IS NOT INITIAL.
*
**----- 特売年月/特売企画Noをキーに既存レコード削除
*  SELECT COUNT( DISTINCT ZIFID )
*    FROM ZPTT0080
*    INTO LW_COUNT
*    WHERE ZIFID = TH_ZPMT0066_SUB_UNIT-ZIFID   "I/FID
*      AND AKSHU = TH_ZPMT0066_SUB_UNIT-AKSHU   "特売種別
*      AND VKDAB = TH_ZPMT0066_SUB_UNIT-VKDAB   "特売期間FROM
*      AND SPART = TH_ZPMT0066_SUB_UNIT-SPART   "部門
*      AND BISMT = TH_ZPMT0066_SUB_UNIT-BISMT   "商品コード
*      AND DLDAT = TH_ZPMT0066_SUB_UNIT-DLDAT.  "納品日
*  IF SY-SUBRC = 0.
*    DELETE FROM ZPTT0080
*      WHERE ZIFID = TH_ZPMT0066_SUB_UNIT-ZIFID   "I/FID
*        AND AKSHU = TH_ZPMT0066_SUB_UNIT-AKSHU   "特売種別
*        AND VKDAB = TH_ZPMT0066_SUB_UNIT-VKDAB   "特売期間FROM
*        AND SPART = TH_ZPMT0066_SUB_UNIT-SPART   "部門
*        AND BISMT = TH_ZPMT0066_SUB_UNIT-BISMT   "商品コード
*        AND DLDAT = TH_ZPMT0066_SUB_UNIT-DLDAT.  "納品日
*    COMMIT WORK.
*  ENDIF.
*
**----- Insert処理
*  INSERT ZPTT0080 FROM TABLE TD_ZPTT0080.
*
**----- エラーがあった場合
*  IF SY-SUBRC <> 0.
*    LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
*      CLEAR LW_MSG.
*      MESSAGE E320(ZC01) WITH TEXT-M05 SY-SUBRC INTO LW_MSG.
*      PERFORM BUILD_LIST_TABLE
*        USING LW_MSG                       "メッセージ内容
*              TH_ZPMT0066_SUB_UNIT-INDEX.  "現行INDEX
*    ENDLOOP.
*  ELSE.
**----- 正常終了
*    LOOP AT TD_ZPMT0066_SUB_UNIT INTO TH_ZPMT0066_SUB_UNIT.
*      TH_ZPMT0066_SUB-ZIFSTATUS = C_STATUS_SUC.
*      MESSAGE S003(ZC01) WITH SPACE
*                              SPACE
*                              SPACE
*                         INTO TH_ZPMT0066_SUB-ZIFMESSAGE.
*      MODIFY TD_ZPMT0066_SUB
*        FROM TH_ZPMT0066_SUB
*        INDEX TH_ZPMT0066_SUB_UNIT-INDEX  "現行INDEX
*      TRANSPORTING ZIFSTATUS
*                   ZIFMESSAGE.
**----- 正常件数カウント
*      CTR_SUC = CTR_SUC + 1.
*    ENDLOOP.
*  ENDIF.
*
*
*ENDFORM.                    " CREATE_ZPTT0080_DAILY
**--> ADD 0009 END   2010/12/25 IBM004
*--> DEL 0016 END 2018/07/09 RS鈴木

*&---------------------------------------------------------------------*
*&      Form  GET_JOBDATE
*&---------------------------------------------------------------------*
*       ジョブ処理日付取得
*----------------------------------------------------------------------*
FORM GET_JOBDATE .
  DATA:
    LW_LOW TYPE TVARVC-LOW.

  SELECT SINGLE LOW FROM TVARVC
    INTO LW_LOW
    WHERE NAME = 'Z_JOBDATE_DAY'
      AND TYPE = 'P'
      AND NUMB = '0000'.

  IF SY-SUBRC = 0.
    W_JOBDATE = LW_LOW.
  ENDIF.

ENDFORM.                    " GET_JOBDATE