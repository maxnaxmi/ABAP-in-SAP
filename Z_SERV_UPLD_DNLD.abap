*----------------------------------------------------------------------*
* PROGRAM ID  : Z_SERV_UPLD_DNLD
* NAME        : テーブルアップロード/ダウンロード(サーバ)
* AUTHOR      : RS 鈴木
* DATE        : 2018/12/04
* DESCRIPTION :アドオンテーブルのサーバアップロード/ダウンロード
*----------------------------------------------------------------------*
*& 変更履歴
*& LOG#  DATE        AUTHOR     DESCRIPTION
*& 0001
*----------------------------------------------------------------------*

REPORT  Z_SERV_UPLD_DNLD.

*----------------------------------------------------------------------*
*       TYPE-POOLS定義
*----------------------------------------------------------------------*
TYPE-POOLS: RSDS.
*----------------------------------------------------------------------*
*       コンスタント値定義
*----------------------------------------------------------------------*
CONSTANTS:
  CNS_SPOS      TYPE SY-UCOMM  VALUE 'SPOS',    "機能："バリアント保存
  CNS_VALUE     TYPE SY-UCOMM  VALUE '%*',
  CNS_OPTI      TYPE SY-UCOMM  VALUE 'OPTI',
  CNS_DYNS      TYPE SY-UCOMM  VALUE 'DYNS',
  CNS_1(1)      TYPE C         VALUE '1',
  CNS_A(1)      TYPE C         VALUE 'A',
  CNS_C(1)      TYPE C         VALUE 'C',
  CNS_E(1)      TYPE C         VALUE 'E',
  CNS_O(1)      TYPE C         VALUE 'O',
  CNS_S(1)      TYPE C         VALUE 'S',
  CNS_T(1)      TYPE C         VALUE 'T',
  CNS_X(1)      TYPE C         VALUE 'X',
  CNS_Y(1)      TYPE C         VALUE 'Y',
  CNS_Z(1)      TYPE C         VALUE 'Z',
  CNS_00(2)     TYPE C         VALUE '00',
  CNS_01(2)     TYPE C         VALUE '01',
  CNS_23(2)     TYPE C         VALUE '23',
  CNS_59(2)     TYPE C         VALUE '59',
  CNS_Z_AST(2)  TYPE C         VALUE 'Z*',
  CNS_DEC(3)    TYPE C         VALUE 'DEC',
  CNS_108(3)    TYPE C         VALUE '108',
  CNS_CURR(4)   TYPE C         VALUE 'CURR',
  CNS_FLTP(4)   TYPE C         VALUE 'FLTP',
  CNS_INT1(4)   TYPE C         VALUE 'INT1',
  CNS_INT2(4)   TYPE C         VALUE 'INT2',
  CNS_INT4(4)   TYPE C         VALUE 'INT4',
  CNS_NUMC(4)   TYPE C         VALUE 'NUMC',
  CNS_PREC(4)   TYPE C         VALUE 'PREC',
  CNS_QUAN(4)   TYPE C         VALUE 'QUAN',
  CNS_ACCP(4)   TYPE C         VALUE 'ACCP',
  CNS_DATS(4)   TYPE C         VALUE 'DATS',
  CNS_TIMS(4)   TYPE C         VALUE 'TIMS',
  CNS_CUKY(4)   TYPE C         VALUE 'CUKY',
  CNS_LANG(4)   TYPE C         VALUE 'LANG',
  CNS_UNIT(4)   TYPE C         VALUE 'UNIT',
  CNS_100000(6) TYPE C         VALUE '100000',
  CNS_ZC01(6) TYPE C         VALUE 'ZC01',
  CNS_MANDT(5)  TYPE C         VALUE 'MANDT',
  CNS_000000(6) TYPE C         VALUE '000000',
  CNS_PARTITION TYPE CHAR1     VALUE '/',
  CNS_DEFDIR    TYPE CHAR128   VALUE 'APL',
*===== 091222 UPDATE START
*  CNS_ERROR(6)  TYPE C         VALUE '_ERROR',
  CNS_ERROR(4)  TYPE C         VALUE '_ERR',
*===== 091222 UPDATE END
  CNS_TRANSP(6) TYPE C         VALUE 'TRANSP',
  CNS_00000000(8) TYPE C       VALUE '00000000'.
*----------------------------------------------------------------------*
*       TYPE定義
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TYP_ERRDATA,    "エラー情報
    INDEX   TYPE SY-TABIX,  "インデックス
    LINE    TYPE I,         "項目列
    ITEM    TYPE AS4TEXT,   "項目名
    MESSAGE TYPE TEXT100,   "メッセージ
  END   OF TYP_ERRDATA,

  BEGIN OF TYP_ERRFILE,    "エラーファイル
    DATA    TYPE CHAR4000,  "データ
    LINE    TYPE CHAR12,    "項目列
    ITEM    TYPE AS4TEXT,   "項目名
    MESSAGE TYPE TEXT100,   "メッセージ
  END   OF TYP_ERRFILE,

  BEGIN OF TYP_CHARTBL,     "テーブル(CHAR)
    DATA_C(500)  TYPE C,
  END   OF TYP_CHARTBL,

  BEGIN OF TYP_TBLKEY,    "テーブルキー項目
    FIELDNAME  TYPE FIELDNAME,
    POSITION   TYPE TABFDPOS,
  END   OF TYP_TBLKEY,

  BEGIN OF TYP_SVFILE_FL, "SVファイル(フラット)
    DATA_FL    TYPE CHAR4000, "データ(フラット)
  END   OF TYP_SVFILE_FL,

  BEGIN OF TYP_SVFILE,    "SVファイルデータ
    DATA       TYPE TABNAME16,
  END   OF TYP_SVFILE,

  BEGIN OF TYP_ATTRIBUTE, "テーブル項目属性
    DATATYPE   TYPE DATATYPE, "データ型
    SIGN       TYPE SIGN,     "符号フラグ
  END   OF TYP_ATTRIBUTE,

*----- 内部テーブルデータ型
  TYP_TD_ERRDATA     TYPE  STANDARD TABLE OF TYP_ERRDATA,
  TYP_TD_ERRFILE     TYPE  STANDARD TABLE OF TYP_ERRFILE,
  TYP_TD_CHARTBL     TYPE  STANDARD TABLE OF TYP_CHARTBL,
  TYP_TD_TBLKEY      TYPE  STANDARD TABLE OF TYP_TBLKEY,
  TYP_TD_SVFILE_FL   TYPE  STANDARD TABLE OF TYP_SVFILE_FL,
  TYP_TD_SVFILE      TYPE  STANDARD TABLE OF TYP_SVFILE,
  TYP_TD_RSDSTABS    TYPE  STANDARD TABLE OF RSDSTABS,
  TYP_TD_RSDSFIELDS  TYPE  STANDARD TABLE OF RSDSFIELDS,
  TYP_TD_DFIES       TYPE  STANDARD TABLE OF DFIES.
*----------------------------------------------------------------------*
*       DATA宣言
*----------------------------------------------------------------------*
DATA: TD_TB1         TYPE  REF TO DATA, "動的抽出用参照データ
      TH_TB1         TYPE  REF TO DATA. "動的編集用データ
*----------------------------------------------------------------------*
*       FIELD-SYMBOLS定義
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <TAB_FIELD> TYPE STANDARD TABLE,    "動的作業用内部テーブル
  <HDR_FIELD> TYPE ANY,               "動的作業用作業領域
  <FS_VAL> TYPE ANY.                  "動的作業用項目
*----------------------------------------------------------------------*
*       PARAMETERS定義 SELECT-OPTIONS定義
*----------------------------------------------------------------------*
PARAMETERS: P_TABNM TYPE TABNAME16 OBLIGATORY         "テーブル名
                                   DEFAULT CNS_Z_AST.
PARAMETERS: P_FLNAM TYPE DXFILE-FILENAME              "ファイル名
                                   LOWER CASE OBLIGATORY.
PARAMETERS: P_WHERES TYPE STRING.                          "変数・選択条件

SELECTION-SCREEN SKIP.

*----- アップロード/ダウンロード
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS RB_TAB RADIOBUTTON GROUP FORM.          "TABファイル
SELECTION-SCREEN COMMENT 3(15)  FOR FIELD RB_TAB.
SELECTION-SCREEN POSITION 20.
PARAMETERS RB_CSV   RADIOBUTTON GROUP FORM.        "CSVファイル
SELECTION-SCREEN COMMENT 23(15) FOR FIELD RB_CSV.
SELECTION-SCREEN END   OF LINE.
PARAMETERS CB_HEAD  AS CHECKBOX DEFAULT CNS_X.        "ヘッダ有無
SELECTION-SCREEN END   OF BLOCK BLK2.
*----- アップロード
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS RB_UPLD RADIOBUTTON GROUP UD.             "アップロード
SELECTION-SCREEN COMMENT 2(20) FOR FIELD RB_UPLD.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS RB_INS   RADIOBUTTON GROUP MOD.          "登録
SELECTION-SCREEN COMMENT 3(5)  FOR FIELD RB_INS.
SELECTION-SCREEN POSITION 20.
PARAMETERS RB_UPD   RADIOBUTTON GROUP MOD.          "更新
SELECTION-SCREEN COMMENT 23(5) FOR FIELD RB_UPD.
SELECTION-SCREEN POSITION 40.
PARAMETERS RB_DEL   RADIOBUTTON GROUP MOD.          "削除
SELECTION-SCREEN COMMENT 43(5) FOR FIELD RB_DEL.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK BLK3.

*----- ダウンロード
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS RB_DWNLD RADIOBUTTON GROUP UD DEFAULT 'X'.             "ダウンロード
SELECTION-SCREEN COMMENT 2(20) FOR FIELD RB_DWNLD.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK BLK5 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS RB_TXS   RADIOBUTTON GROUP HEAD.        "テキスト(短)
SELECTION-SCREEN COMMENT 3(15)  FOR FIELD RB_TXS.
SELECTION-SCREEN POSITION 20.
PARAMETERS RB_TXR   RADIOBUTTON GROUP HEAD.        "内容説明
SELECTION-SCREEN COMMENT 23(15) FOR FIELD RB_TXR.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS RB_TXM   RADIOBUTTON GROUP HEAD.        "テキスト(中)
SELECTION-SCREEN COMMENT 3(15)  FOR FIELD RB_TXM.
SELECTION-SCREEN POSITION 20.
PARAMETERS RB_TXH   RADIOBUTTON GROUP HEAD.        "ヘッダ
SELECTION-SCREEN COMMENT 23(15) FOR FIELD RB_TXH.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS RB_TXL   RADIOBUTTON GROUP HEAD.       "テキスト(長)
SELECTION-SCREEN COMMENT 3(15)  FOR FIELD RB_TXL.
SELECTION-SCREEN POSITION 20.
PARAMETERS RB_TXI   RADIOBUTTON GROUP HEAD.       "項目名
SELECTION-SCREEN COMMENT 23(15) FOR FIELD RB_TXI.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK BLK5.

SELECTION-SCREEN END   OF BLOCK BLK1.

*----- システム制御用
SELECTION-SCREEN BEGIN OF BLOCK BLK6 WITH FRAME TITLE TEXT-006.
PARAMETERS P_UPMAX   TYPE NUM7 OBLIGATORY DEFAULT CNS_100000. "内容説明
SELECTION-SCREEN END   OF BLOCK BLK6.
*----------------------------------------------------------------------*
*       INCLUDE定義
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*----- ローカル変数定義
  DATA: LW_SUBRC     TYPE SY-SUBRC.

*----- 機能コードチェック
  CHECK SY-UCOMM <> CNS_SPOS      "バリアント保存
    AND SY-UCOMM NP CNS_VALUE     "複数選択
    AND SY-UCOMM <> CNS_OPTI      "選択オプション
    AND SY-UCOMM <> CNS_DYNS.     "動的選択

*----- 入力パラメータチェック
*----- テーブル名の整合性チェック
  IF P_TABNM+0(1) <> CNS_Z.
    MESSAGE E483(ZC01) WITH P_TABNM.
*   テーブル &1 はアドオンテーブル（Z始まり）ではありません
  ENDIF.

*----- テーブル名のチェック
  PERFORM CHECK_TABNAME.

*----- SVファイル存在チェック
  PERFORM A_CHECK_UL_DL_COMMON   USING    P_FLNAM   "ファイル名
                                 CHANGING LW_SUBRC.
*----- SVファイル存在チェックエラーハンドリング
  PERFORM FLCHK_ERROR USING LW_SUBRC.

*----------------------------------------------------------------------*
*       AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FLNAM.

*----- 入力値選択処理
  PERFORM A_FILENAME_GET CHANGING P_FLNAM. "ファイル名
*----------------------------------------------------------------------*
*      START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM  MAIN_PROC.   "主処理
*&---------------------------------------------------------------------*
*&      Form  CHECK_TABNAME
*&---------------------------------------------------------------------*
*       テーブル名のチェック
*----------------------------------------------------------------------*
FORM CHECK_TABNAME .
*----- ローカル変数定義
  DATA: LW_TABNAME TYPE TABNAME16.

*----- テーブル名の存在チェック
  SELECT TABNAME
    FROM DD02L
    INTO LW_TABNAME
    UP TO 1 ROWS
   WHERE TABNAME  = P_TABNM     "テーブル名
     AND AS4LOCAL = CNS_A       "有効化ステータス
     AND TABCLASS = CNS_TRANSP. "テーブルカテゴリ
  ENDSELECT.

  IF SY-SUBRC <> 0.
    MESSAGE E484(ZC01) WITH P_TABNM.
*   データベーステーブル &1 が存在しません
  ENDIF.

ENDFORM.                    " CHECK_TABNAME
*&---------------------------------------------------------------------*
*&      Form  FLCHK_ERROR
*&---------------------------------------------------------------------*
*       SVファイル存在チェックエラーハンドリング
*----------------------------------------------------------------------*
*      -->I_W_SUBRC
*----------------------------------------------------------------------*
FORM FLCHK_ERROR USING    I_W_SUBRC TYPE SY-SUBRC.

  IF  RB_UPLD = CNS_X  "アップロード時
  AND I_W_SUBRC <> 0.
    CASE I_W_SUBRC.
      WHEN 1.
        SET CURSOR FIELD 'P_FLNAM'.
        MESSAGE E485(ZC01).
*   ディレクトリが存在しません
      WHEN 2.
        SET CURSOR FIELD 'P_FLNAM'.
        MESSAGE E486(ZC01).
*   ファイルを指定してください
      WHEN 3.
        SET CURSOR FIELD 'P_FLNAM'.
        MESSAGE E487(ZC01) WITH P_FLNAM.
*   ファイルが存在していません ファイル名： &1
      WHEN 4.
        SET CURSOR FIELD 'P_FLNAM'.
        MESSAGE E488(ZC01) WITH P_FLNAM.
*   ファイルが使用されています ファイル名： &1

    ENDCASE.
  ENDIF.

  IF  RB_DWNLD = CNS_X "ダウンロード時
  AND I_W_SUBRC <> 0
  AND I_W_SUBRC <> 3.
    CASE I_W_SUBRC.
      WHEN 1.
        SET CURSOR FIELD 'P_FLNAM'.
        MESSAGE E485(ZC01).
*   ディレクトリが存在しません
      WHEN 2.
        SET CURSOR FIELD 'P_FLNAM'.
        MESSAGE E486(ZC01).
*   ファイルを指定してください
      WHEN 4.
        SET CURSOR FIELD 'P_FLNAM'.
        MESSAGE E488(ZC01) WITH P_FLNAM.
*   ファイルが使用されています ファイル名： &1
    ENDCASE.
  ENDIF.

ENDFORM.                    " FLCHK_ERROR
*&---------------------------------------------------------------------*
*&      Form  MAIN_PROC
*&---------------------------------------------------------------------*
*       主処理
*----------------------------------------------------------------------*
FORM MAIN_PROC .
*----- 動的データ作成
  CREATE DATA TD_TB1 TYPE STANDARD TABLE OF (P_TABNM).
  CREATE DATA TH_TB1   TYPE (P_TABNM).
  ASSIGN TD_TB1->* TO <TAB_FIELD>.
  ASSIGN TH_TB1->* TO <HDR_FIELD>.

  CASE CNS_X.
    WHEN RB_UPLD.  "アップロード
      PERFORM UPLOAD_METHOD.
    WHEN RB_DWNLD. "ダウンロード
      PERFORM DOWNLOAD_METHOD.
  ENDCASE.

ENDFORM.                    " MAIN_PROC
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_METHOD
*&---------------------------------------------------------------------*
*       アップロード
*----------------------------------------------------------------------*
FORM UPLOAD_METHOD .
*----- ローカル変数定義
  DATA:
        LTD_ULFILE_FL TYPE TYP_TD_SVFILE_FL, "SVファイル(フラット)
        LTH_ULFILE_FL TYPE TYP_SVFILE_FL,    "SVファイル(フラット)ヘッダ
        LTD_ULFILE_HD TYPE TYP_TD_SVFILE,    "SVファイルヘッダ
        LW_SUBRC      TYPE SY-SUBRC.

*----- SVファイルのアップロード
  PERFORM A_SV_UPLOAD USING    P_FLNAM        "ファイル名
                      CHANGING LTD_ULFILE_FL  "SVファイル(フラット)
                               LW_SUBRC.
  IF LW_SUBRC <> 0.
    MESSAGE S389(ZC01) DISPLAY LIKE CNS_E.
*   ファイルアップロードに失敗しました
    LEAVE LIST-PROCESSING.
  ENDIF.

*----- 最大アップロード件数の確認
  PERFORM COUNT_FILEDATA USING LTD_ULFILE_FL. "SVファイル(フラット)

*----- ヘッダ行の退避及び削除
  IF CB_HEAD = CNS_X.
    PERFORM DELETE_HEADER CHANGING
                          LTD_ULFILE_FL "SVファイル(フラット)
                          LTH_ULFILE_FL "SVファイル(フラット)ヘッダ
                          LTD_ULFILE_HD."内部TAB・SVファイルヘッダ
  ENDIF.

*----- データチェック
  PERFORM CHECK_DATA USING LTD_ULFILE_FL  "SVファイル(フラット)
                           LTH_ULFILE_FL  "SVファイル(フラット)ヘッダ
                           LTD_ULFILE_HD. "内部TAB・SVファイルヘッダ

*----- テーブル更新
  PERFORM MODIFY_TABLE.

ENDFORM.                    " UPLOAD_METHOD
*&---------------------------------------------------------------------*
*&      Form  COUNT_FILEDATA
*&---------------------------------------------------------------------*
*       最大アップロード件数の確認
*----------------------------------------------------------------------*
*      -->I_TD_ULFILE_FL  SVファイル(フラット)
*----------------------------------------------------------------------*
FORM COUNT_FILEDATA  USING    I_TD_ULFILE_FL TYPE TYP_TD_SVFILE_FL.
*----- ローカル変数定義
  DATA: LW_NUM   TYPE I, "SVファイルデータ件数
        LW_UPMAX TYPE I. "最大アップロード件数

*----- アップロード用ファイル件数チェック
  LW_NUM = LINES( I_TD_ULFILE_FL ).
  IF CB_HEAD = CNS_X. "ヘッダ行あり
    LW_NUM = LW_NUM - 1.
  ENDIF.

*----- 件数による処理
  LW_UPMAX = P_UPMAX.
  IF LW_NUM > LW_UPMAX. "ファイルデータ件数 > 最大アップロード件数
    MESSAGE S489(ZC01) WITH LW_UPMAX DISPLAY LIKE CNS_E.
*   ファイルレコード数が最大アップロード件数: &1 を超えています
    LEAVE LIST-PROCESSING.
  ELSEIF LW_NUM =< 0.  "ファイルデータ件数 =< 0
    MESSAGE S490(ZC01) WITH 'TABLEID' DISPLAY LIKE CNS_E.
*   処理対象データが存在しません
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " COUNT_FILEDATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_HEADER
*&---------------------------------------------------------------------*
*       ヘッダ行の退避及び削除
*----------------------------------------------------------------------*
*      <--O_TD_ULFILE_FL  SVファイル(フラット)
*      <--O_TH_ULFILE_FL  SVファイル(フラット)ヘッダ
*      <--O_TD_ULFILE_HD  SVファイルヘッダ
*----------------------------------------------------------------------*
FORM DELETE_HEADER  CHANGING    O_TD_ULFILE_FL TYPE TYP_TD_SVFILE_FL
                                O_TH_ULFILE_FL TYPE TYP_SVFILE_FL
                                O_TD_ULFILE_HD TYPE TYP_TD_SVFILE.
*----- ローカル変数定義
  DATA: LW_SEPARATOR(1) TYPE C.
*----- ヘッダ行の退避
  READ TABLE O_TD_ULFILE_FL INDEX 1 INTO O_TH_ULFILE_FL.
*----- 内部テーブルからヘッダ行削除
  DELETE O_TD_ULFILE_FL INDEX 1.
*----- 区切り文字判定
  IF RB_TAB = CNS_X. "タブ区切り
    LW_SEPARATOR = CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
  ELSE.              "カンマ区切り
    LW_SEPARATOR = ','.
  ENDIF.
*----- SVファイルヘッダへ格納
  SPLIT O_TH_ULFILE_FL AT LW_SEPARATOR INTO TABLE O_TD_ULFILE_HD.

ENDFORM.                    " DELETE_HEADER
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       データチェック
*----------------------------------------------------------------------*
*      -->I_TD_ULFILE_FL  SVファイル(フラット)
*      -->I_TH_ULFILE_FL  SVファイル(フラット)ヘッダ
*      -->I_TD_ULFILE_HD  SVファイルヘッダ
*----------------------------------------------------------------------*
FORM CHECK_DATA USING I_TD_ULFILE_FL TYPE TYP_TD_SVFILE_FL
                      I_TH_ULFILE_FL TYPE TYP_SVFILE_FL
                      I_TD_ULFILE_HD TYPE TYP_TD_SVFILE.

*----- ローカル変数定義
  DATA: LTD_DFIES     TYPE TYP_TD_DFIES,     "テーブル項目属性
        LTD_ERRDATA   TYPE TYP_TD_ERRDATA,   "エラー情報
        LTD_SVFILE_FL TYPE TYP_TD_SVFILE_FL, "エラーファイル
        LW_VAL        TYPE I.                "項目数(テーブル)

*----- テーブル属性の取得
  PERFORM GET_FIELDINFO CHANGING LTD_DFIES. "テーブル項目属性
*----- 項目チェック
  PERFORM CHECK_ITEM USING    I_TD_ULFILE_FL "SVファイル(フラット)
                              LTD_DFIES      "テーブル項目属性
                              I_TD_ULFILE_HD "SVファイルヘッダ
                     CHANGING LTD_ERRDATA.   "エラー情報
*----- エラーファイルのダウンロード
  IF LTD_ERRDATA IS NOT INITIAL.
    LW_VAL = LINES( LTD_DFIES ).                  "項目数(テーブル)
    PERFORM ERRFILE_DOWNLOAD USING I_TD_ULFILE_FL "SVファイル(フラット)
                                   I_TH_ULFILE_FL "同ヘッダ
                                   LTD_ERRDATA    "エラー情報
                                   LW_VAL         "項目数(テーブル)
                          CHANGING LTD_SVFILE_FL. "エラーファイル
  ENDIF.
ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDINFO
*&---------------------------------------------------------------------*
*       テーブル属性の取得
*----------------------------------------------------------------------*
*      <--O_TD_DFIES  テーブル項目属性
*----------------------------------------------------------------------*
FORM GET_FIELDINFO  CHANGING O_TD_DFIES TYPE TYP_TD_DFIES.

*----- ローカル変数定義
  DATA: LW_DDOBJNAME TYPE DDOBJNAME.

  LW_DDOBJNAME = P_TABNM. "テーブル名

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = LW_DDOBJNAME "テーブル名
    TABLES
      DFIES_TAB      = O_TD_DFIES   "テーブル項目属性
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE CNS_S NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            DISPLAY LIKE CNS_E.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    " GET_FIELDINFO
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM
*&---------------------------------------------------------------------*
*       項目チェック
*----------------------------------------------------------------------*
*      -->I_TD_ULFILE_FL   SVファイル(フラット)
*      -->I_TD_DFIES       テーブル項目属性
*      -->I_TD_ULFILE_HD   SVファイルヘッダ
*      <--O_TD_ERRDATA     エラー情報
*----------------------------------------------------------------------*
FORM CHECK_ITEM  USING    I_TD_ULFILE_FL TYPE TYP_TD_SVFILE_FL
                          I_TD_DFIES     TYPE TYP_TD_DFIES
                          I_TD_ULFILE_HD TYPE TYP_TD_SVFILE
                 CHANGING O_TD_ERRDATA   TYPE TYP_TD_ERRDATA.
*----- ローカル変数定義
  DATA: LTH_ULFILE_HD   TYPE TYP_SVFILE,     "SVファイルヘッダ
        LTH_ERRDATA     TYPE TYP_ERRDATA,    "エラー情報用構造
        LW_SEPARATOR(1) TYPE C,              "区切り文字
        LW_DFIES        TYPE DFIES,          "テーブル項目属性
        LW_TABIX        TYPE SY-TABIX,       "索引
        LCTR_LINE       TYPE I,              "項目列カウンタ
        LTD_CHARTBL     TYPE TYP_TD_CHARTBL, "テーブル(CHAR)
        LTH_CHARTBL     TYPE TYP_CHARTBL,    "テーブルヘッダ(CHAR)
        LTH_ULFILE_FL   TYPE TYP_SVFILE_FL,  "SVファイル(フラット)ヘッダ
        LFLG_ERR(1)     TYPE C.              "エラーフラグ

*----- 区切り文字判定
  IF RB_TAB = CNS_X. "タブ区切り
    LW_SEPARATOR = CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
  ELSE.              "カンマ区切り
    LW_SEPARATOR = ','.
  ENDIF.

*----- チェック対象項目選択
  LOOP AT I_TD_ULFILE_FL INTO LTH_ULFILE_FL.
    SPLIT LTH_ULFILE_FL AT LW_SEPARATOR INTO TABLE LTD_CHARTBL.
    CLEAR LCTR_LINE.
    LW_TABIX = SY-TABIX.
    CLEAR <HDR_FIELD>.
    LOOP AT LTD_CHARTBL INTO LTH_CHARTBL.
      CLEAR: LW_DFIES,
             LTH_ULFILE_HD,
             LTH_ERRDATA.
      READ TABLE I_TD_DFIES INDEX SY-TABIX
      INTO LW_DFIES.
*===== 091221 ADD START
      IF SY-SUBRC = 0.
*===== 091221 ADD END
*----- エラー情報用項目セット
        LCTR_LINE = LCTR_LINE + CNS_1. "項目列
        IF CB_HEAD = CNS_X.
          READ TABLE I_TD_ULFILE_HD
          INDEX LCTR_LINE
          INTO LTH_ULFILE_HD.          "項目名
        ENDIF.
*----- 各項目チェック
        PERFORM CHECK_ITEM_INDI USING     LW_DFIES     "テーブル項目属性
                                          LTH_CHARTBL  "チェック対象項目
                                          LCTR_LINE     "項目列
                                          LTH_ULFILE_HD "項目名
                                          LW_TABIX      "索引
                                CHANGING  LTH_ERRDATA. "エラー情報用構造
*===== 091221 ADD START
      ENDIF.
*===== 091221 ADD END
      IF LTH_ERRDATA IS NOT INITIAL.
        LFLG_ERR = CNS_X.
        APPEND LTH_ERRDATA TO O_TD_ERRDATA.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF LFLG_ERR <> CNS_X.
      APPEND <HDR_FIELD> TO <TAB_FIELD>.
    ENDIF.
    CLEAR: LFLG_ERR.
  ENDLOOP.
ENDFORM.                    " CHECK_ITEM
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM_INDI
*&---------------------------------------------------------------------*
*       各項目チェック
*----------------------------------------------------------------------*
*      -->I_W_DFIES       テーブル項目属性
*      -->I_TH_CHARTBL    チェック対象項目
*      -->I_CTR_LINE      項目列
*      -->I_TH_ULFILE_HD  項目名
*      -->I_W_TABIX       索引
*      <--O_TH_ERR        エラー情報用構造
*----------------------------------------------------------------------*
FORM CHECK_ITEM_INDI  USING    I_W_DFIES      TYPE DFIES
                               I_TH_CHARTBL   TYPE TYP_CHARTBL
                               I_CTR_LINE     TYPE I
                               I_TH_ULFILE_HD TYPE TYP_SVFILE
                               I_W_TABIX      TYPE SY-TABIX
                      CHANGING O_TH_ERRDATA   TYPE TYP_ERRDATA.
  CASE I_W_DFIES-DATATYPE. "データ型
    WHEN CNS_CURR
      OR CNS_DEC
      OR CNS_FLTP
      OR CNS_INT1
      OR CNS_INT2
      OR CNS_INT4
      OR CNS_NUMC
      OR CNS_PREC
      OR CNS_QUAN.
*----- 数値チェック
      PERFORM CHECK_ITEM_NUM USING    I_TH_CHARTBL "チェック対象項目
                                      I_W_DFIES    "テーブル項目属性
                                      I_CTR_LINE     "項目列
                                      I_TH_ULFILE_HD "項目名
                                      I_W_TABIX      "索引
                             CHANGING O_TH_ERRDATA."エラー情報用構造
    WHEN CNS_ACCP
      OR CNS_DATS.
*----- 日付チェック
      PERFORM CHECK_ITEM_DATE USING I_TH_CHARTBL   "チェック対象項目
                                    I_CTR_LINE     "項目列
                                    I_TH_ULFILE_HD "項目名
                                    I_W_TABIX      "索引
                                    I_W_DFIES      "テーブル属性
                           CHANGING O_TH_ERRDATA.  "エラー情報用構造
    WHEN CNS_TIMS.
*----- 時刻チェック
      PERFORM CHECK_ITEM_TIME USING I_TH_CHARTBL"チェック対象項目
                                    I_CTR_LINE     "項目列
                                    I_TH_ULFILE_HD "項目名
                                    I_W_TABIX      "索引
                           CHANGING O_TH_ERRDATA.  "エラー情報用構造
    WHEN CNS_CUKY.
*----- 通貨コードチェック
      PERFORM CHECK_ITEM_WAERS USING I_TH_CHARTBL "チェック対象項目
                                     I_CTR_LINE     "項目列
                                     I_TH_ULFILE_HD "項目名
                                     I_W_TABIX      "索引
                            CHANGING O_TH_ERRDATA. "エラー情報用構造
    WHEN CNS_LANG.
*----- 言語キーチェック
      PERFORM CHECK_ITEM_SPRAS USING I_TH_CHARTBL"チェック対象項目
                                     I_CTR_LINE     "項目列
                                     I_TH_ULFILE_HD "項目名
                                     I_W_TABIX      "索引
                            CHANGING O_TH_ERRDATA. "エラー情報用構造
    WHEN CNS_UNIT.
*----- 数量単位キーチェック
      PERFORM CHECK_ITEM_MSEHI USING I_TH_CHARTBL "チェック対象項目
                                     I_CTR_LINE     "項目列
                                     I_TH_ULFILE_HD "項目名
                                     I_W_TABIX      "索引
                            CHANGING O_TH_ERRDATA. "エラー情報用構造
    WHEN OTHERS.
      ASSIGN COMPONENT I_CTR_LINE OF STRUCTURE
                                    <HDR_FIELD> TO <FS_VAL>.
      <FS_VAL> = I_TH_CHARTBL.
  ENDCASE.
ENDFORM.                    " CHECK_ITEM_INDI
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM_NUM
*&---------------------------------------------------------------------*
*       数値チェック
*----------------------------------------------------------------------*
*      -->I_TH_CHARTBL   チェック対象項目
*      -->I_W_DFIES      テーブル項目属性
*      -->I_CTR_LINE     項目列カウンタ
*      -->I_TH_ULFILE_HD 項目名
*      -->I_W_TABIX      索引
*      <--O_TH_ERRDATA   エラー情報
*----------------------------------------------------------------------*
FORM CHECK_ITEM_NUM USING    I_TH_CHARTBL   TYPE TYP_CHARTBL
                             I_W_DFIES      TYPE DFIES
                             I_CTR_LINE     TYPE I
                             I_TH_ULFILE_HD TYPE TYP_SVFILE
                             I_W_TABIX      TYPE SY-TABIX
                    CHANGING O_TH_ERRDATA   TYPE TYP_ERRDATA.

*----- ローカル変数
  DATA: LW_VALUE     TYPE N,
        LW_TEXT      TYPE ITEX132,
        LFLG_SIGN(1) TYPE C.

*----- 数値チェック
  IF I_TH_CHARTBL IS NOT INITIAL. "初期値でない場合
    CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
      EXPORTING
        INPUT      = I_TH_CHARTBL
        INTERNAL   = CNS_X
      IMPORTING
        OUTPUT     = LW_VALUE
      EXCEPTIONS
        NO_NUMERIC = 1
        OTHERS     = 2.

*----- エラー情報
    IF SY-SUBRC <> 0.
*----- メッセージ用変数判定
      CASE I_W_DFIES-SIGN. "符号フラグON
        WHEN CNS_X.
          LW_TEXT = TEXT-011.
        WHEN OTHERS.       "符号フラグOFF
          LW_TEXT = TEXT-012.
      ENDCASE.
*----- エラー情報格納
      PERFORM EDIT_ERRDATA USING    I_W_TABIX      "インデックス
                                    I_CTR_LINE     "項目列
                                    I_TH_ULFILE_HD "項目名
                                    CNS_108        "メッセージ番号
                                    LW_TEXT        "メッセージ
                           CHANGING O_TH_ERRDATA.  "エラー情報
    ELSE.
      IF I_W_DFIES-SIGN <> CNS_X. "符号フラグ:OFFの場合
*----- 符号チェック
        CLEAR LFLG_SIGN.
        FIND '+' IN I_TH_CHARTBL.
        IF SY-SUBRC = 0.
          LFLG_SIGN = CNS_X.
        ENDIF.
        FIND '-' IN I_TH_CHARTBL.
        IF SY-SUBRC = 0.
          LFLG_SIGN = CNS_X.
        ENDIF.
        IF LFLG_SIGN = CNS_X.
*----- エラー情報格納
          PERFORM EDIT_ERRDATA USING I_W_TABIX      "インデックス
                                     I_CTR_LINE     "項目列
                                     I_TH_ULFILE_HD "項目名
                                     CNS_108        "メッセージ番号
                                     TEXT-012       "半角数字、ピリオド
                            CHANGING O_TH_ERRDATA.  "エラー情報
        ELSE.
          ASSIGN COMPONENT I_CTR_LINE OF STRUCTURE
                                         <HDR_FIELD> TO <FS_VAL>.
          <FS_VAL> = I_TH_CHARTBL.
        ENDIF.
      ELSE.
        ASSIGN COMPONENT I_CTR_LINE OF STRUCTURE
                                         <HDR_FIELD> TO <FS_VAL>.
        <FS_VAL> = I_TH_CHARTBL.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_ITEM_NUM
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM_DATE
*&---------------------------------------------------------------------*
*       日付チェック
*----------------------------------------------------------------------*
*      -->I_TH_CHARTBL   チェック対象項目
*      -->I_CTR_LINE     項目列カウンタ
*      -->I_TH_ULFILE_HD 項目名
*      -->I_W_TABIX      索引
*      -->I_W_DFIES      テーブル属性
*      <--O_TH_ERRDATA   エラー情報
*----------------------------------------------------------------------*
FORM CHECK_ITEM_DATE USING    I_TH_CHARTBL   TYPE TYP_CHARTBL
                              I_CTR_LINE     TYPE I
                              I_TH_ULFILE_HD TYPE TYP_SVFILE
                              I_W_TABIX      TYPE SY-TABIX
                              I_W_DFIES      TYPE DFIES
                     CHANGING O_TH_ERRDATA   TYPE TYP_ERRDATA.
*----- ローカル変数定義
  DATA: LW_DATUM  TYPE SY-DATUM.
*----- 初期値/ゼロチェック
  IF    I_TH_CHARTBL IS NOT INITIAL
  AND ( I_TH_CHARTBL <> CNS_000000 )
  AND ( I_TH_CHARTBL <> CNS_00000000 ).
*----- 会計期間チェックの場合：日付操作(yyyymm + 01)
    IF I_W_DFIES-DATATYPE = CNS_ACCP.
      CONCATENATE I_TH_CHARTBL CNS_01 INTO LW_DATUM.
    ELSE.
      LW_DATUM = I_TH_CHARTBL.
    ENDIF.
*----- 日付チェック
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        DATE                      = LW_DATUM
      EXCEPTIONS
        PLAUSIBILITY_CHECK_FAILED = 1
        OTHERS                    = 2.
    IF SY-SUBRC <> 0.
      PERFORM EDIT_ERRDATA USING I_W_TABIX      "インデックス
                                 I_CTR_LINE     "項目列
                                 I_TH_ULFILE_HD "項目名
                                 CNS_108        "メッセージ番号
                                 TEXT-013       "メッセージ用変数
                        CHANGING O_TH_ERRDATA.  "エラー情報
    ELSE.
      ASSIGN COMPONENT I_CTR_LINE OF STRUCTURE <HDR_FIELD> TO <FS_VAL>.
      <FS_VAL> = I_TH_CHARTBL.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_ITEM_DATE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM_TIME
*&---------------------------------------------------------------------*
*       時刻チェック
*----------------------------------------------------------------------*
*      -->I_TH_CHARTBL   チェック対象項目
*      -->I_CTR_LINE     項目列カウンタ
*      -->I_TH_ULFILE_HD 項目名
*      -->I_W_TABIX      索引
*      <--O_TH_ERRDATA エラー情報
*----------------------------------------------------------------------*
FORM CHECK_ITEM_TIME USING    I_TH_CHARTBL   TYPE TYP_CHARTBL
                              I_CTR_LINE     TYPE I
                              I_TH_ULFILE_HD TYPE TYP_SVFILE
                              I_W_TABIX      TYPE SY-TABIX
                     CHANGING O_TH_ERRDATA   TYPE TYP_ERRDATA.
*----- ローカル変数定義
  DATA: LW_TIME(8) TYPE C.
*----- 初期値/ゼロチェック
  IF  I_TH_CHARTBL IS NOT INITIAL
  AND I_TH_CHARTBL <> CNS_000000.
*----- 時刻チェック
    LW_TIME = I_TH_CHARTBL.
    IF    LW_TIME+0(2) >= CNS_00   "時チェック
    AND ( LW_TIME+0(2) <= CNS_23 )
    AND ( LW_TIME+2(2) >= CNS_00 ) "分チェック
    AND ( LW_TIME+2(2) <= CNS_59 )
    AND ( LW_TIME+4(2) >= CNS_00 ) "秒チェック
    AND ( LW_TIME+4(2) <= CNS_59 ) .
      ASSIGN COMPONENT I_CTR_LINE OF STRUCTURE <HDR_FIELD> TO <FS_VAL>.
      <FS_VAL> = I_TH_CHARTBL.
    ELSE.
      PERFORM EDIT_ERRDATA USING I_W_TABIX       "インデックス
                                 I_CTR_LINE     "項目列
                                 I_TH_ULFILE_HD "項目名
                                 CNS_108        "メッセージ番号
                                 TEXT-021       "メッセージ用変数
                        CHANGING O_TH_ERRDATA.  "エラー情報
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_ITEM_TIME
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM_WAERS
*&---------------------------------------------------------------------*
*       通貨コードチェック
*----------------------------------------------------------------------*
*      -->I_TH_CHARTBL   チェック対象項目
*      -->I_CTR_LINE     項目列カウンタ
*      -->I_TH_ULFILE_HD 項目名
*      -->I_W_TABIX      索引
*      <--O_TH_ERRDATA   エラー情報
*----------------------------------------------------------------------*
FORM CHECK_ITEM_WAERS USING    I_TH_CHARTBL   TYPE TYP_CHARTBL
                               I_CTR_LINE     TYPE I
                               I_TH_ULFILE_HD TYPE TYP_SVFILE
                               I_W_TABIX      TYPE SY-TABIX
                      CHANGING O_TH_ERRDATA   TYPE TYP_ERRDATA.
*----- ローカル変数定義
  DATA: LW_WAERS TYPE WAERS.
*----- 初期値チェック
  IF I_TH_CHARTBL IS NOT INITIAL.
*----- 通貨コードチェック
    SELECT SINGLE WAERS "通貨コード
      FROM TCURC
      INTO LW_WAERS
     WHERE WAERS = I_TH_CHARTBL.

    IF SY-SUBRC <> 0.
      PERFORM EDIT_ERRDATA USING I_W_TABIX      "インデックス
                                 I_CTR_LINE     "項目列
                                 I_TH_ULFILE_HD "項目名
                                 CNS_108        "メッセージ番号
                                 TEXT-014       "メッセージ用変数
                        CHANGING O_TH_ERRDATA.  "エラー情報
    ELSE.
      ASSIGN COMPONENT I_CTR_LINE OF STRUCTURE <HDR_FIELD> TO <FS_VAL>.
      <FS_VAL> = I_TH_CHARTBL.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_ITEM_WAERS
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM_SPRAS
*&---------------------------------------------------------------------*
*       言語キーチェック
*----------------------------------------------------------------------*
*      -->I_TH_CHARTBL   チェック対象項目
*      -->I_CTR_LINE     項目列カウンタ
*      -->I_TH_ULFILE_HD 項目名
*      -->I_W_TABIX      索引
*      <--O_TH_ERRDATA   エラー情報
*----------------------------------------------------------------------*
FORM CHECK_ITEM_SPRAS USING    I_TH_CHARTBL   TYPE TYP_CHARTBL
                               I_CTR_LINE     TYPE I
                               I_TH_ULFILE_HD TYPE TYP_SVFILE
                               I_W_TABIX      TYPE SY-TABIX
                      CHANGING O_TH_ERRDATA   TYPE TYP_ERRDATA.
*----- ローカル変数定義
  DATA: LW_SPRAS TYPE SPRAS.
*----- 初期値チェック
  IF I_TH_CHARTBL IS NOT INITIAL.
*----- 言語キーチェック
    SELECT SINGLE SPRAS
      FROM T002
      INTO LW_SPRAS
     WHERE SPRAS = I_TH_CHARTBL.

    IF SY-SUBRC <> 0.
      PERFORM EDIT_ERRDATA USING I_W_TABIX      "インデックス
                                 I_CTR_LINE     "項目列
                                 I_TH_ULFILE_HD "項目名
                                 CNS_108        "メッセージ番号
                                 TEXT-015       "メッセージ用変数
                        CHANGING O_TH_ERRDATA.  "エラー情報
    ELSE.
      ASSIGN COMPONENT I_CTR_LINE OF STRUCTURE <HDR_FIELD> TO <FS_VAL>.
      <FS_VAL> = I_TH_CHARTBL.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_ITEM_SPRAS
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM_MSEHI
*&---------------------------------------------------------------------*
*       数量単位チェック
*----------------------------------------------------------------------*
*      -->I_TH_CHARTBL   チェック対象項目
*      -->I_CTR_LINE     項目列カウンタ
*      -->I_TH_ULFILE_HD 項目名
*      -->I_W_TABIX      索引
*      <--O_TH_ERRDATA   エラー情報
*----------------------------------------------------------------------*
FORM CHECK_ITEM_MSEHI USING    I_TH_CHARTBL   TYPE TYP_CHARTBL
                               I_CTR_LINE     TYPE I
                               I_TH_ULFILE_HD TYPE TYP_SVFILE
                               I_W_TABIX      TYPE SY-SUBRC
                      CHANGING O_TH_ERRDATA   TYPE TYP_ERRDATA.
*----- ローカル変数定義
  DATA: LW_MSEHI TYPE MSEHI.
*----- 初期値チェック
  IF I_TH_CHARTBL IS NOT INITIAL.
*----- 数量単位チェック
    SELECT SINGLE MSEHI
      FROM T006
      INTO LW_MSEHI
     WHERE MSEHI = I_TH_CHARTBL.

    IF SY-SUBRC <> 0.
      PERFORM EDIT_ERRDATA USING I_W_TABIX      "インデックス
                                 I_CTR_LINE     "項目列
                                 I_TH_ULFILE_HD "項目名
                                 CNS_108        "メッセージ番号
                                 TEXT-016       "メッセージ用変数
                        CHANGING O_TH_ERRDATA.  "エラー情報
    ELSE.
      ASSIGN COMPONENT I_CTR_LINE OF STRUCTURE <HDR_FIELD> TO <FS_VAL>.
      <FS_VAL> = I_TH_CHARTBL.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_ITEM_MSEHI
*&---------------------------------------------------------------------*
*&      Form  EDIT_ERRDATA
*&---------------------------------------------------------------------*
*       エラー情報格納
*----------------------------------------------------------------------*
*      -->I_W_TABIX      インデックス
*      -->I_CTR_LINE     項目列
*      -->I_TH_ULFILE_HD 項目名
*      -->I_W_NUM        メッセージ番号
*      -->I_W_TEXT       メッセージ
*      <--O_TH_ERRDATA   エラー情報
*----------------------------------------------------------------------*
FORM EDIT_ERRDATA  USING    I_W_TABIX      TYPE SY-TABIX
                            I_CTR_LINE     TYPE I
                            I_TH_ULFILE_HD TYPE TYP_SVFILE
                            I_W_NUM        TYPE C
                            I_W_TEXT       TYPE ITEX132
                   CHANGING O_TH_ERRDATA   TYPE TYP_ERRDATA.

  O_TH_ERRDATA-INDEX   = I_W_TABIX.      "インデックス
  O_TH_ERRDATA-LINE    = I_CTR_LINE.     "項目列
  O_TH_ERRDATA-ITEM    = I_TH_ULFILE_HD. "項目名
  MESSAGE ID CNS_ZC01 TYPE CNS_E NUMBER I_W_NUM WITH I_W_TEXT
  INTO O_TH_ERRDATA-MESSAGE.             "メッセージ

ENDFORM.                    " EDIT_ERRFILE
*&---------------------------------------------------------------------*
*&      Form  ERRFILE_DOWNLOAD
*&---------------------------------------------------------------------*
*       エラーファイルのダウンロード
*----------------------------------------------------------------------*
*      -->I_TD_ULFILE_FL  SVファイル(フラット)
*      -->I_TH_ULFILE_FL  SVファイル(フラット)ヘッダ
*      -->I_TD_ERRDATA    エラー情報
*      -->I_W_VAL         項目数(テーブル)
*      <--O_TD_SVFILE_FL  SVファイル(フラット)
*----------------------------------------------------------------------*
FORM ERRFILE_DOWNLOAD USING    I_TD_ULFILE_FL TYPE TYP_TD_SVFILE_FL
                               I_TH_ULFILE_FL TYPE TYP_SVFILE_FL
                               I_TD_ERRDATA   TYPE TYP_TD_ERRDATA
                               I_W_VAL        TYPE I
                      CHANGING O_TD_SVFILE_FL TYPE TYP_TD_SVFILE_FL.

*----- ローカル変数定義
  DATA: LTD_ERRFILE   TYPE TYP_TD_ERRFILE,   "エラーファイル
        LTH_ERRFILE   TYPE TYP_ERRFILE,      "エラーファイルヘッダ
        LTD_SVFILE_FL TYPE TYP_TD_SVFILE_FL, "SVファイル(フラット)
        LW_FLNAM      TYPE RLGRAP-FILENAME,  "エラーファイル名
        LW_SUBRC      TYPE SY-SUBRC.

*----- ヘッダ部の作成
  IF CB_HEAD = CNS_X.
    LTH_ERRFILE-DATA    = I_TH_ULFILE_FL.
    LTH_ERRFILE-LINE    = TEXT-008.
    LTH_ERRFILE-ITEM    = TEXT-009.
    LTH_ERRFILE-MESSAGE = TEXT-010.
    INSERT LTH_ERRFILE INTO LTD_ERRFILE INDEX 1.
  ENDIF.
*----- データ部の作成
  PERFORM ADD_DELIMITER USING    I_TD_ULFILE_FL
                        CHANGING LTD_SVFILE_FL.

*----- データ行の作成
  PERFORM EDIT_ERRFILE USING    LTD_SVFILE_FL "SVファイル(フラット)
                                I_TD_ERRDATA  "エラー情報
                       CHANGING LTD_ERRFILE.  "エラーファイル

*----- エラーファイル(フラット)の作成
  PERFORM ADJUST_ITEM_NUM USING    I_W_VAL
                          CHANGING LTD_ERRFILE.

  PERFORM ADD_DELIMITER USING    LTD_ERRFILE
                        CHANGING O_TD_SVFILE_FL.
*----- ファイル名の編集
  PERFORM EDIT_FLNAM CHANGING LW_FLNAM. "エラーファイル名
*----- ダウンロード
  PERFORM A_SV_DOWNLOAD USING    LW_FLNAM       "エラーファイル名
                                 SPACE
                        CHANGING O_TD_SVFILE_FL "SVファイル(フラット)
                                 LW_SUBRC.
  IF LW_SUBRC <> 0.
    MESSAGE S491(ZC01) DISPLAY LIKE CNS_E.
*   ファイルのダウンロード時にエラーが発生しました
    LEAVE LIST-PROCESSING.
  ELSE.
    MESSAGE S492(ZC01) DISPLAY LIKE CNS_E.
*   ファイルに不正なレコードがありました エラーファイルを確認して下さい
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " ERRFILE_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  EDIT_ERRFILE
*&---------------------------------------------------------------------*
*       データ行の作成
*----------------------------------------------------------------------*
*      -->I_TD_SVFILE_FL SVファイル(フラット)
*      -->I_TD_ERRDATA   エラー情報
*      <--O_TD_ERRFILE   エラーファイル
*----------------------------------------------------------------------*
FORM EDIT_ERRFILE  USING    I_TD_SVFILE_FL TYPE TYP_TD_SVFILE_FL
                            I_TD_ERRDATA   TYPE TYP_TD_ERRDATA
                   CHANGING O_TD_ERRFILE   TYPE TYP_TD_ERRFILE.
*----- ローカル変数定義
  DATA: LTH_SVFILE_FL TYPE TYP_SVFILE_FL,    "SVファイル(フラット)ヘッダ
        LTH_ERRDATA   TYPE TYP_ERRDATA,      "エラー情報ヘッダ
        LTH_ERRFILE   TYPE TYP_ERRFILE,      "エラーファイルヘッダ
        LW_TABIX      TYPE SY-TABIX,
        LW_LINE       TYPE STRING.           "項目列

  LOOP AT I_TD_SVFILE_FL INTO LTH_SVFILE_FL.
    LW_TABIX = SY-TABIX.
    CLEAR: LTH_ERRDATA,
           LTH_ERRFILE.
*----- エラー情報検索
    READ TABLE I_TD_ERRDATA
    WITH KEY INDEX = LW_TABIX
    INTO LTH_ERRDATA.
*----- データセット
    LTH_ERRFILE-DATA    = LTH_SVFILE_FL.       "データ
    IF LTH_ERRDATA IS NOT INITIAL.
      LW_LINE             = LTH_ERRDATA-LINE.    "項目列
      LTH_ERRFILE-LINE    = LW_LINE.
      LTH_ERRFILE-ITEM    = LTH_ERRDATA-ITEM.    "項目名
      LTH_ERRFILE-MESSAGE = LTH_ERRDATA-MESSAGE. "メッセージ
    ENDIF.
    APPEND LTH_ERRFILE TO O_TD_ERRFILE.
  ENDLOOP.
ENDFORM.                    " EDIT_ERRFILE
*&---------------------------------------------------------------------*
*&      Form  ADJUST_ITEM_NUM
*&---------------------------------------------------------------------*
*       エラーファイル項目数のチェック・調整
*----------------------------------------------------------------------*
*      -->I_W_VAL        項目数(テーブル)
*      <--O_TD_ERRFILE   エラーファイル
*----------------------------------------------------------------------*
FORM ADJUST_ITEM_NUM  USING    I_W_VAL        TYPE I
                      CHANGING O_TD_ERRFILE   TYPE TYP_TD_ERRFILE.
*----- ローカル変数定義
  DATA: LTD_SVFILE_FL  TYPE TYP_TD_SVFILE_FL, "SVファイル(フラット)
        LTD_SVFILE_SP  TYPE TYP_TD_SVFILE_FL,
                                         "エラーファイル(フラット)分割済
        LTH_SVFILE_FL  TYPE TYP_SVFILE_FL,   "SVファイル(フラット)ヘッダ
        LW_SEPARATOR   TYPE CHAR01.           "区切り文字
  FIELD-SYMBOLS:
        <LFS>          TYPE TYP_ERRFILE.

*----- 区切り文字判定
  IF RB_TAB = CNS_X.
    LW_SEPARATOR = CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
  ELSE.
    LW_SEPARATOR = ','.
  ENDIF.
*----- エラーファイル項目数のチェック・調整
  LOOP AT O_TD_ERRFILE ASSIGNING <LFS>.
    REFRESH: LTD_SVFILE_FL,
             LTD_SVFILE_SP.
    SPLIT <LFS>-DATA AT LW_SEPARATOR INTO TABLE LTD_SVFILE_FL.
*----- 項目数が異なる場合、テーブル項目数に合わせる
    DO I_W_VAL TIMES.
      CLEAR LTH_SVFILE_FL.
      READ TABLE LTD_SVFILE_FL INTO LTH_SVFILE_FL INDEX SY-INDEX.
      APPEND LTH_SVFILE_FL TO LTD_SVFILE_SP.
    ENDDO.
    CLEAR <LFS>-DATA.
    LOOP AT LTD_SVFILE_SP INTO LTH_SVFILE_FL.
      IF SY-TABIX = CNS_1.
        <LFS>-DATA = LTH_SVFILE_FL-DATA_FL.
      ELSE.
        CONCATENATE <LFS>-DATA LTH_SVFILE_FL-DATA_FL INTO <LFS>-DATA
        SEPARATED BY LW_SEPARATOR.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " ADJUST_ITEM_NUM
*&---------------------------------------------------------------------*
*&      Form  EDIT_FLNAM
*&---------------------------------------------------------------------*
*       ファイル名の編集
*----------------------------------------------------------------------*
*      <--O_W_FLNAM  ファイル名
*----------------------------------------------------------------------*
FORM EDIT_FLNAM  CHANGING O_W_FLNAM TYPE RLGRAP-FILENAME.

*----- ローカル変数定義
  DATA: LW_NAME TYPE RLGRAP-FILENAME,
        LW_EXT  TYPE RLGRAP-FILENAME.
*===== 091222 ADD START
  DATA: LW_YYMMDD(6)     TYPE C,
        LW_ERR_STAMP(17) TYPE C.
*===== 091222 ADD END

*===== 091222 ADD START
* エラーファイル付加部分作成：_ERRyymmddhhmmss
  WRITE SY-DATLO TO LW_YYMMDD YYMMDD.
  CONCATENATE CNS_ERROR LW_YYMMDD SY-TIMLO INTO LW_ERR_STAMP.
*===== 091222 ADD END
*----- ファイル名を名称と拡張子に分割
  PERFORM A_FILE_NAME_SEPARATE USING      P_FLNAM
                                 CHANGING LW_NAME     " 名称
                                          LW_EXT.     " 拡張子
*===== 091222 UPDATE START
*----- 名称 + '_ERROR' + 拡張子
*  CONCATENATE LW_NAME CNS_ERROR LW_EXT INTO O_W_FLNAM.
  CONCATENATE LW_NAME LW_ERR_STAMP LW_EXT INTO O_W_FLNAM.
*===== 091222 UPDATE END
ENDFORM.                    " EDIT_FLNAM
*&---------------------------------------------------------------------*
*&      Form  MODIFY_TABLE
*&---------------------------------------------------------------------*
*       テーブル更新
*----------------------------------------------------------------------*
FORM MODIFY_TABLE .

*----- ローカル変数
  DATA: LW_MSGTX(255) TYPE C,    "処理件数
        LW_INUM       TYPE INUM. "処理内容
  CASE CNS_X.
*----- 登録のみ
    WHEN RB_INS.
      LW_INUM = TEXT-017.
      INSERT (P_TABNM) FROM TABLE <TAB_FIELD> ACCEPTING DUPLICATE KEYS.
*----- 更新
    WHEN RB_UPD.
      LW_INUM = TEXT-018.
      MODIFY (P_TABNM) FROM TABLE <TAB_FIELD>.
*----- 削除
    WHEN RB_DEL.
      LW_INUM = TEXT-019.
      DELETE (P_TABNM) FROM TABLE <TAB_FIELD>.
  ENDCASE.

  IF SY-SUBRC = 0. "正常
    WRITE SY-DBCNT TO LW_MSGTX LEFT-JUSTIFIED.
    MESSAGE S493(ZC01) WITH LW_MSGTX LW_INUM.
*   &1 件 &2 しました
  ELSE.
*----- ROLLBACK
    ROLLBACK WORK.
    MESSAGE S494(ZC01) WITH 'TABLEID' LW_INUM SY-SUBRC DISPLAY LIKE CNS_E.
*   テーブル &1 の &2 に失敗しました(RC=:&3)
  ENDIF.
ENDFORM.                    " MODIFY_TABLE
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_METHOD
*&---------------------------------------------------------------------*
*       ダウンロード
*----------------------------------------------------------------------*
FORM DOWNLOAD_METHOD .

*----- ローカル変数定義
  DATA: LTD_TBLKEY     TYPE TYP_TD_TBLKEY,    "テーブルキー項目
        LTD_TABLES_TAB TYPE TYP_TD_RSDSTABS,  "対象テーブル
        LTD_FIELDS_TAB TYPE TYP_TD_RSDSFIELDS,"選択項目
        LTD_WHERES     TYPE RSDS_TWHERE,      "内部テーブル・選択条件
        LW_WHERES      TYPE STRING,           "変数・選択条件
        LW_DYNSELID    TYPE DYNSELID.         "動的選択ID

*----- データの抽出
  PERFORM SELECT_DOWNLOAD_DATA USING LW_WHERES. "変数・選択条件

*----- ファイルダウンロード
  PERFORM FILE_DOWNLOAD.

ENDFORM.                    " DOWNLOAD_METHOD
*&---------------------------------------------------------------------*
*&      Form  GET_TBLKEY
*&---------------------------------------------------------------------*
*       キー項目の抽出
*----------------------------------------------------------------------*
*      <--O_TD_TBLKEY  テーブルキー項目
*----------------------------------------------------------------------*
FORM GET_TBLKEY  CHANGING O_TD_TBLKEY TYPE TYP_TD_TBLKEY.

  SELECT FIELDNAME             "項目名
         POSITION              "テーブル内の項目位置
    FROM DD03L
    INTO TABLE O_TD_TBLKEY
   WHERE TABNAME   =  P_TABNM   "テーブル名
     AND FIELDNAME <> CNS_MANDT "項目名
     AND AS4LOCAL  =  CNS_A     "有効化ステータス
     AND KEYFLAG   =  CNS_X.    "キー項目の識別

  IF SY-SUBRC <> 0.
    MESSAGE S495(ZC01) DISPLAY LIKE CNS_E.
*   テーブル項目取得に失敗しました
    LEAVE LIST-PROCESSING.
  ENDIF.
*----- ソート
  SORT O_TD_TBLKEY BY POSITION. "テーブル内の項目位置

ENDFORM.                    " GET_TBLKEY
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
*       選択条件初期値の設定
*----------------------------------------------------------------------*
*      -->I_TD_TBLKEY      テーブルキー項目
*      <--O_TD_TABLES_TAB  対象テーブル
*      <--O_TD_FIELDS_TAB  選択項目
*      <--O_W_DYNSELID     動的選択ID
*----------------------------------------------------------------------*
FORM SET_INIT  USING    I_TD_TBLKEY     TYPE TYP_TD_TBLKEY
               CHANGING O_TD_TABLES_TAB TYPE TYP_TD_RSDSTABS
                        O_TD_FIELDS_TAB TYPE TYP_TD_RSDSFIELDS
                        O_W_DYNSELID    TYPE DYNSELID.
*----- ローカル変数定義
  DATA: LTH_FIELD      TYPE RSDSFIELDS,
        LTH_TBLKEY     TYPE TYP_TBLKEY,
        LTH_TABLES_TAB TYPE RSDSTABS.

  LTH_FIELD-TABLENAME = P_TABNM.

*----- テーブル編集
  LOOP AT I_TD_TBLKEY INTO LTH_TBLKEY.
    LTH_FIELD-FIELDNAME = LTH_TBLKEY-FIELDNAME.
    APPEND LTH_FIELD TO O_TD_FIELDS_TAB.
  ENDLOOP.
  LTH_TABLES_TAB-PRIM_TAB = P_TABNM.
  APPEND LTH_TABLES_TAB TO O_TD_TABLES_TAB.

  CALL FUNCTION 'FREE_SELECTIONS_INIT'
    EXPORTING
      KIND                     = 'T'
    IMPORTING
      SELECTION_ID             = O_W_DYNSELID
    TABLES
      TABLES_TAB               = O_TD_TABLES_TAB
      FIELDS_TAB               = O_TD_FIELDS_TAB
    EXCEPTIONS
      FIELDS_INCOMPLETE        = 1
      FIELDS_NO_JOIN           = 2
      FIELD_NOT_FOUND          = 3
      NO_TABLES                = 4
      TABLE_NOT_FOUND          = 5
      EXPRESSION_NOT_SUPPORTED = 6
      INCORRECT_EXPRESSION     = 7
      ILLEGAL_KIND             = 8
      AREA_NOT_FOUND           = 9
      INCONSISTENT_AREA        = 10
      KIND_F_NO_FIELDS_LEFT    = 11
      KIND_F_NO_FIELDS         = 12
      TOO_MANY_FIELDS          = 13
      DUP_FIELD                = 14
      FIELD_NO_TYPE            = 15
      FIELD_ILL_TYPE           = 16
      DUP_EVENT_FIELD          = 17
      NODE_NOT_IN_LDB          = 18
      AREA_NO_FIELD            = 19
      OTHERS                   = 20.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE CNS_S NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            DISPLAY LIKE CNS_E.
  ENDIF.

ENDFORM.                    " SET_INIT
*&---------------------------------------------------------------------*
*&      Form  SET_DIALOG
*&---------------------------------------------------------------------*
*       選択条件入力ダイアログの出力
*----------------------------------------------------------------------*
*      -->I_W_DYNSELID     動的選択ID
*      <--O_TD_WHERES      選択条件
*      <--O_TD_FIELDS_TAB  選択項目
*----------------------------------------------------------------------*
FORM SET_DIALOG  USING    I_W_DYNSELID    TYPE DYNSELID
                 CHANGING O_TD_WHERES     TYPE RSDS_TWHERE
                          O_TD_FIELDS_TAB TYPE TYP_TD_RSDSFIELDS.

  CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
    EXPORTING
      SELECTION_ID    = I_W_DYNSELID
      TITLE           = TEXT-007 "固定値：データ選択条件
      FRAME_TEXT      = TEXT-007 "固定値：データ選択条件
      AS_WINDOW       = CNS_X
    IMPORTING
      WHERE_CLAUSES   = O_TD_WHERES     "選択条件
    TABLES
      FIELDS_TAB      = O_TD_FIELDS_TAB "選択項目
    EXCEPTIONS
      INTERNAL_ERROR  = 1
      NO_ACTION       = 2
      SELID_NOT_FOUND = 3
      ILLEGAL_STATUS  = 4
      OTHERS          = 5.
  IF SY-SUBRC = 2.
    MESSAGE S496(ZC01).
    LEAVE LIST-PROCESSING.
*   データ選択条件がキャンセルされました
  ELSEIF SY-SUBRC <> 0
     AND SY-SUBRC <> 2.
    MESSAGE S497(ZC01) WITH TEXT-020 SY-SUBRC DISPLAY LIKE CNS_E.
*   汎用モジュール &1 の処理に失敗しました - （ リターンコード＝ &2 ）
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " SET_DIALOG
*&---------------------------------------------------------------------*
*&      Form  SELECT_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       データの抽出
*----------------------------------------------------------------------*
*      -->I_W_WHERES  変数・選択条件
*----------------------------------------------------------------------*
FORM SELECT_DOWNLOAD_DATA  USING    I_W_WHERES TYPE STRING.
*----- 動的抽出
  SELECT *
    INTO TABLE <TAB_FIELD>
    FROM (P_TABNM)
   WHERE (P_WHERES).
ENDFORM.                    " SELECT_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*       ファイルダウンロード
*----------------------------------------------------------------------*
FORM FILE_DOWNLOAD .
*----- ローカル変数定義
  DATA: LTD_SVFILE_FL TYPE TYP_TD_SVFILE_FL, "SVファイル(フラット)
        LW_SUBRC      TYPE SY-SUBRC,
        LW_NUM        TYPE I,
        LW_MSGTX(255) TYPE C.    "処理件数

*----- データ部の作成
  PERFORM ADD_DELIMITER USING    <TAB_FIELD>
                        CHANGING LTD_SVFILE_FL.
*----- ヘッダ部の作成
  PERFORM EDIT_HEAD CHANGING LTD_SVFILE_FL.
*----- データのダウンロード
  PERFORM A_SV_DOWNLOAD USING    P_FLNAM        "ファイル名
                                 SPACE          "固定長
                        CHANGING LTD_SVFILE_FL  "SVデータ(フラット)
                                 LW_SUBRC.
  IF SY-SUBRC = 0.
    LW_NUM = LINES( LTD_SVFILE_FL ).
    IF CB_HEAD = CNS_X. "ヘッダ行ありの場合：行数-1
      LW_NUM = LW_NUM - 1.
    ENDIF.
    WRITE LW_NUM TO LW_MSGTX LEFT-JUSTIFIED.
    MESSAGE S493(ZC01) WITH LW_MSGTX TEXT-022.
*   &1 件 &2 しました
  ELSE.
    MESSAGE S491(ZC01) DISPLAY LIKE CNS_E.
*   ファイルのダウンロード時にエラーが発生しました
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  ADD_DELIMITER
*&---------------------------------------------------------------------*
*       区切り文字の付加(ダウンロード)
*----------------------------------------------------------------------*
*      -->I_TD_ANY     抽出データ
*      <--O_TD_ANY     区切り文字付加データ
*----------------------------------------------------------------------*
FORM ADD_DELIMITER  USING    I_TD_ANY    TYPE ANY
                    CHANGING O_TD_ANY    TYPE ANY.
*----- ローカル変数定義
  DATA: LW_SEPARATE(1) TYPE C. "区切り文字

*----- 区切り文字判定
  IF RB_TAB = CNS_X.
    LW_SEPARATE = CNS_T. "Tab区切り
  ELSE.
    LW_SEPARATE = CNS_C. "カンマ区切り
  ENDIF.
*----- 区切り文字の付加
*  CALL FUNCTION 'Z_AKS_SEP_STRCTER'
  CALL FUNCTION 'ZZ_C_SEP_STRCTER'
    EXPORTING
      I_PROCESS      = CNS_O          "区切り文字付加
      I_SEPARATE     = LW_SEPARATE    "区切り文字
      T_INFILE       = I_TD_ANY       "抽出データ
    IMPORTING
      T_OUTFILE      = O_TD_ANY       "区切り文字付加データ
    EXCEPTIONS
      PARAMETER_ERROR = 1
      CRKUBUN_ERROR   = 2
      INPUTFILE_ERROR = 3
      OTHERS          = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE CNS_S NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            DISPLAY LIKE CNS_E.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " ADD_DELIMITER
*&---------------------------------------------------------------------*
*&      Form  EDIT_HEAD
*&---------------------------------------------------------------------*
*       ヘッダ部の作成
*----------------------------------------------------------------------*
*      <--O_TD_SVFILE_FL SVファイル(フラット)
*----------------------------------------------------------------------*
FORM EDIT_HEAD CHANGING O_TD_SVFILE_FL TYPE TYP_TD_SVFILE_FL.

*----- ローカル変数定義
  DATA: LTD_DFIES     TYPE TYP_TD_DFIES,    "テーブル項目属性
        LTH_DLFILE_FL TYPE TYP_SVFILE_FL.   "SVファイル(フラット)ヘッダ

*----- テーブル属性の取得
  PERFORM GET_FIELDINFO CHANGING LTD_DFIES. "テーブル項目属性
*----- ヘッダ行の作成
  IF CB_HEAD = CNS_X.
    PERFORM MAKE_HEAD USING LTD_DFIES      "テーブル項目属性
                   CHANGING LTH_DLFILE_FL. "SVファイル(フラット)ヘッダ
*----- ヘッダ行の追加
    INSERT LTH_DLFILE_FL INTO O_TD_SVFILE_FL INDEX 1.
  ENDIF.

ENDFORM.                    " EDIT_HEAD
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEAD
*&---------------------------------------------------------------------*
*       ヘッダ行の作成
*----------------------------------------------------------------------*
*      -->I_TD_DFIES      テーブル項目属性
*      <--O_TH_DLFILE_FL  SVファイル(フラット)ヘッダ
*----------------------------------------------------------------------*
FORM MAKE_HEAD  USING    I_TD_DFIES     TYPE TYP_TD_DFIES
                CHANGING O_TH_DLFILE_FL TYPE TYP_SVFILE_FL.
*----- ローカル変数定義
  DATA: LTH_DFIES       TYPE DFIES,        "テーブル項目属性
        LW_SEPARATOR(1) TYPE C,            "区切り文字
        LW_COLM         LIKE DD04T-DDTEXT. "ヘッダテキスト

*----- 区切り文字判定
  IF RB_TAB = CNS_X. "TABファイル
    LW_SEPARATOR = CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
  ELSE.              "CSVファイル
    LW_SEPARATOR = ','.
  ENDIF.

  LOOP AT I_TD_DFIES INTO LTH_DFIES.
    CASE CNS_X.
      WHEN RB_TXS. "テキスト(短)
        LW_COLM = LTH_DFIES-SCRTEXT_S.
      WHEN RB_TXM. "テキスト(中)
        LW_COLM = LTH_DFIES-SCRTEXT_M.
      WHEN RB_TXL. "テキスト(長)
        LW_COLM = LTH_DFIES-SCRTEXT_L.
      WHEN RB_TXR. "リポジトリ
        LW_COLM = LTH_DFIES-FIELDTEXT.
      WHEN RB_TXH. "ヘッダ
        LW_COLM = LTH_DFIES-REPTEXT.
      WHEN RB_TXI. "項目名
        LW_COLM = LTH_DFIES-FIELDNAME.
    ENDCASE.

    AT FIRST.
      O_TH_DLFILE_FL = LW_COLM.
      CONTINUE.
    ENDAT.

    CONCATENATE O_TH_DLFILE_FL LW_COLM
    INTO O_TH_DLFILE_FL        "SVファイル(フラット)ヘッダ
    SEPARATED BY LW_SEPARATOR. "区切り文字
  ENDLOOP.

ENDFORM.                    " MAKE_HEAD

*&---------------------------------------------------------------------*
*&  Include           ZZ_ITATOOL14_I01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   コンスタント値定義
*----------------------------------------------------------------------*
CONSTANTS:
  CNS_A003_YEN            TYPE CHAR1           VALUE '/',
  CNS_A003_ASC            TYPE RLGRAP-FILETYPE VALUE 'ASC',
  CNS_A003_PATH_C         TYPE CHAR4           VALUE  '/apl',
  CNS_A003_AGC20_TYPE     TYPE CHAR20          VALUE 'APPLICATION',  "
  CNS_A003_AGC20_SUBTP    TYPE CHAR20          VALUE 'X-UNKNOWN'  ,  "
  CNS_A003_AGC20_URL      TYPE CHAR7           VALUE 'file://',
  CNS_A003_X              TYPE CHAR1           VALUE 'X',
  CNS_A003_PERIOD         TYPE CHAR1           VALUE '.'.

*----------------------------------------------------------------------*
*   TYPE定義
*----------------------------------------------------------------------*
TYPES:
*----- ダウンロード/アップロードデータ格納用
  BEGIN OF TYP_A003_ITAB,
    DATA                  TYPE CHAR4000,
  END OF TYP_A003_ITAB,
*
  BEGIN OF TYP_A003_HEX_TAB,
    MYHEX                 TYPE BIN1024,
  END OF TYP_A003_HEX_TAB.

*----------------------------------------------------------------------*
*   フィールドシンボル定義
*----------------------------------------------------------------------*
*FIELD-SYMBOLS: <f_agc20> TYPE ANY.

*&---------------------------------------------------------------------*
*&      FORM  A_FILENAME_GET
*&---------------------------------------------------------------------*
*       入力可能値(Pop-up画面)を表示しファイル名に入力された値を取得する
*----------------------------------------------------------------------*
*      <-- O_FN      DL／ULファイル名
*----------------------------------------------------------------------*
FORM A_FILENAME_GET         CHANGING O_FN TYPE ANY.
*
  DATA: LW_ENQNAME  TYPE MSNAME2,
        LW_SUBRC    TYPE I,
        LW_IPATH    TYPE DXLPATH,
        LW_OPATH    TYPE DXLPATH.
*

  LW_IPATH  = CNS_PARTITION.

  CALL FUNCTION 'TH_READ_ENQ_NAME'
    IMPORTING
      ENQ_NAME = LW_ENQNAME.

  CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
    EXPORTING
      I_LOCATION_FLAG = 'A'
      I_SERVER        = LW_ENQNAME
      I_PATH          = LW_IPATH
      FILEMASK        = '*.*'
      FILEOPERATION   = 'R'
    IMPORTING
      O_PATH          = LW_OPATH
    EXCEPTIONS
      RFC_ERROR       = 1
      ERROR_WITH_GUI  = 2
      OTHERS          = 3.

  IF SY-SUBRC = 0.
    O_FN = LW_OPATH.
  ENDIF.
*
ENDFORM.                    " A_FILENAME_GET

*&---------------------------------------------------------------------*
*&      FORM  A_CHECK_UL_DL_COMMON
*&---------------------------------------------------------------------*
*       ディレクトリ存在チェック、ファイル存在チェック、使用中チェック
*----------------------------------------------------------------------*
*      --> I_FULLPATH  フルパス名
*      <-- O_SUBRC     リターンコード:
*                         0 正常終了(ファイルが存在し、未使用)
*                         1 ディレクトリが存在しない
*                         2 ファイル名がディレクトリ
*                         3 ファイルが存在しない
*                         4 ファイルが使用中
*----------------------------------------------------------------------*
FORM A_CHECK_UL_DL_COMMON   USING    I_FULLPATH TYPE ANY
                            CHANGING O_SUBRC    TYPE ANY.
*
  DATA: LW_DIREC     TYPE STRING ,                " ディレクトリパス
        LW_FILENAME  TYPE STRING ,                " ファイル名
        LW_RESULT    TYPE CHAR1,                  " 処理結果
        LW_URL       TYPE CHAR1024,               " URL
        LW_ERROR     TYPE I.
*
  DATA: LTD_HEX_TAB TYPE STANDARD TABLE           " URL(HEX)
                         OF TYP_A003_HEX_TAB.
*
*----- リターンコードの初期化
  O_SUBRC  =  0.
*
*----- 入力パラメータチェック
  CHECK NOT I_FULLPATH  IS INITIAL.               "SVファイル名が初期値

* サーバーファイルの読込み
  OPEN DATASET I_FULLPATH FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  IF SY-SUBRC <> 0.
*----- バッファした自動キューをフロントエンドに送信
    CALL METHOD CL_GUI_CFW=>FLUSH
      EXCEPTIONS
        CNTL_SYSTEM_ERROR = 1
        CNTL_ERROR        = 2
        OTHERS            = 3.
*----- 処理結果の判定
    IF  SY-SUBRC <> 0.
      O_SUBRC  =  3.                              " ファイルが存在しない
    ENDIF.
  ENDIF.

* ファイルのクローズ
  CLOSE DATASET I_FULLPATH.
  IF SY-SUBRC <> 0.
*   ファイル &1 をクローズできません
    MESSAGE E498(ZC01) WITH I_FULLPATH.
  ENDIF.

ENDFORM.                               " A_CHECK_UL_DL_COMMON

*&---------------------------------------------------------------------*
*&      FORM  A_SV_DOWNLOAD
*&---------------------------------------------------------------------*
*       SVへのファイルダウンロード(上書きモード)
*----------------------------------------------------------------------*
*      --> I_FULLPATH     ファイルのフルパス
*      --> I_FIXEDLEN     出力長の区分 (ブランク:可変長、X:固定長)
*      <-- T_DLFILE       ダウンロード用内部テーブル
*      <-- O_SUBRC        リターンコード
*----------------------------------------------------------------------*
FORM A_SV_DOWNLOAD          USING      I_FULLPATH  TYPE ANY
                                       I_FIXEDLEN  TYPE ANY
                            CHANGING   T_DLFILE    TYPE STANDARD TABLE
                                       O_SUBRC     TYPE ANY.
*
  DATA:
*    LTD_ITAB     TYPE STANDARD TABLE OF TYP_A003_ITAB,
    LW_FULLPATH  TYPE STRING,                 " フルパス
    LW_FULLTYPE  TYPE CHAR10,                 " ファイルタイプ
    LW_WRITE_LF  TYPE CHAR01,                 " 行の終端に CR/LF を挿入
    LW_FIXEDLEN  TYPE CHAR01,                 " 最終列の末尾の空白を削除
    LTH_DLFILE   TYPE TYP_SVFILE_FL.

*
  LW_FULLPATH = I_FULLPATH.
  LW_FULLTYPE = CNS_A003_ASC.
  IF I_FIXEDLEN IS INITIAL.                   " 可変長の場合
    LW_FIXEDLEN = CNS_A003_X.
    LW_WRITE_LF = CNS_A003_X.
  ELSE.                                       " 固定長の場合
    CLEAR: LW_FIXEDLEN,
           LW_WRITE_LF.
  ENDIF.
*
  CLEAR : O_SUBRC.
*  LTD_ITAB[] = T_TD_ULFILE[].

* ファイルのオープン
  OPEN DATASET LW_FULLPATH FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC <> 0.
*   戻り値を格納
    O_SUBRC = SY-SUBRC.
*   ファイル &1 をオープンできません
    MESSAGE S499(ZC01) WITH LW_FULLPATH DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* ファイルのデータを出力する
  LOOP AT T_DLFILE INTO LTH_DLFILE.
    TRY.
        TRANSFER LTH_DLFILE TO LW_FULLPATH.
      CATCH CX_ROOT.
        IF SY-SUBRC = 0.
*         ファイル &1 を出力できません
          MESSAGE E500(ZC01) WITH LW_FULLPATH
                  RAISING FILE_WRITE_ERROR.
        ENDIF.
    ENDTRY.
  ENDLOOP.

* ファイルのクローズ
  CLOSE DATASET LW_FULLPATH.
  IF SY-SUBRC <> 0.
*   戻り値を格納
    O_SUBRC = SY-SUBRC.
*   ファイル &1 をクローズできません
    MESSAGE S498(ZC01) WITH LW_FULLPATH DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*
*   戻り値を格納
  O_SUBRC = SY-SUBRC.
*
ENDFORM.                               " A_SV_DOWNLOAD

*&---------------------------------------------------------------------
*&      FORM  A_SV_DOWNLOAD_APPEND
*&---------------------------------------------------------------------
*       SVへのファイルダウンロード（追加モード）
*----------------------------------------------------------------------
*      --> I_FULLPATH   ファイルのフルパス
*      --> I_HEADER     ファイルのヘッダー(ヘッダーなしの場合SPACE指定)
*      --> T_DLFILE     ダウンロード用内部テーブル
*      <-- O_SUBRC      リターンコード
*----------------------------------------------------------------------
FORM A_SV_DOWNLOAD_APPEND   USING    I_FULLPATH  TYPE ANY
*                                     I_HEADER    TYPE ANY
                            CHANGING T_DLFILE    TYPE STANDARD TABLE
                                     O_SUBRC     TYPE ANY.
*
  DATA:
*    ltd_itab     TYPE TABLE OF typ_a003_itab,
*    LW_HEADER    TYPE XSTRING,                   " ヘッダー
    LW_FULLPATH  TYPE STRING,                    " フルパス
    LW_FULLTYPE  TYPE CHAR10.                    " ファイルタイプ
*
*  CLEAR: LW_HEADER.
*  IF I_HEADER IS NOT INITIAL.
*    LW_HEADER = I_HEADER.
*  ENDIF.
  LW_FULLPATH = I_FULLPATH.
  LW_FULLTYPE = CNS_A003_ASC.
*
*----- SVダウンロード
*  ltd_itab[] = t_td_ulfile[].
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
    EXPORTING
      FILENAME                = LW_FULLPATH       " フルパス名
      FILETYPE                = LW_FULLTYPE       " アスキーコード
      APPEND                  = CNS_A003_X        " 追記
*     HEADER                  = LW_HEADER         " ヘッダー
      WRITE_FIELD_SEPARATOR   = CNS_A003_X        " タブ区切り
    CHANGING
      DATA_TAB                = T_DLFILE          " DLデータ格納用TBL
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.
*
*----- 戻り値を格納
  O_SUBRC = SY-SUBRC.
*
ENDFORM.                               " A_SV_DOWNLOAD_APPEND

*&---------------------------------------------------------------------*
*&      FORM  A_SV_UPLOAD
*&---------------------------------------------------------------------*
*       SVファイルをアップロードする
*----------------------------------------------------------------------*
*      --> I_FULLPATH   ファイルのフルパス
*      <-- T_ULFILE     アップロード用内部テーブル
*      <-- O_SUBRC      リターンコード
*----------------------------------------------------------------------*
FORM A_SV_UPLOAD  USING      I_FULLPATH  TYPE ANY
                  CHANGING   T_ULFILE    TYPE STANDARD TABLE
                             O_SUBRC     TYPE ANY.
*
  DATA:
*    ltd_itab     TYPE STANDARD TABLE OF typ_a003_itab,
    LW_FULLPATH  TYPE STRING,                    " Path name
    LW_FULLTYPE  TYPE CHAR10,                    " Type
    LW_SEPARATOR TYPE CHAR1,
    LW_TYPE      TYPE CHAR1,
    LW_COMP      TYPE CHAR1,
    LOBJ_REFSTR1 TYPE REF TO DATA,
    LTH_ULFILE   TYPE TYP_SVFILE_FL,
    LW_EXIT      TYPE CHAR1.
  FIELD-SYMBOLS:
    <LFS_COMPS>  TYPE ANY,                " for work
    <LFS_DDITM>  TYPE ANY.                " for work
*
  LW_FULLPATH =   I_FULLPATH.
  LW_FULLTYPE =   CNS_A003_ASC.
*
*----- 指定されたITABの項目数が1の場合はそのままITABに読込む。
*----- ITABの項目数が1より多い場合は項目別に構造化されているとみなして
*----- レコード内容をTAB文字で分割後の値をITABの各項目に読込む。
  CLEAR: LW_SEPARATOR.
  CREATE DATA LOBJ_REFSTR1 LIKE LINE OF T_ULFILE.
  ASSIGN LOBJ_REFSTR1->* TO <LFS_COMPS>.
  DO.
    ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LFS_COMPS> TO <LFS_DDITM>.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSEIF SY-INDEX > 1.
      LW_SEPARATOR = CNS_A003_X.                  " タブ区切り
      EXIT.
    ENDIF.
  ENDDO.
*
  CLEAR : O_SUBRC.

* サーバーファイルの読込み
  OPEN DATASET LW_FULLPATH FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC <> 0.
*   ファイル & をオープンできません
    MESSAGE S499(ZC01) WITH LW_FULLPATH DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

* データを読み込む
  DO.
    CATCH SYSTEM-EXCEPTIONS CONVT_CODEPAGE = 9.
*     ファイルからデータを取得する
      READ DATASET LW_FULLPATH INTO LTH_ULFILE.
      IF SY-SUBRC <> 0.
        LW_EXIT = 1.
      ENDIF.
*     データが読み込めた場合は、内部テーブルにスタックする
      IF LTH_ULFILE IS NOT INITIAL.
        APPEND LTH_ULFILE TO T_ULFILE.
      ENDIF.
      CLEAR LTH_ULFILE.
    ENDCATCH.
    IF SY-SUBRC <> 0.
      CLOSE DATASET LW_FULLPATH.
*     ファイル &! のデータを読み込めません
      MESSAGE S501(ZC01) WITH LW_FULLPATH DISPLAY LIKE 'E'.
    ENDIF.
*   内部テーブルに保管
    IF LW_EXIT <> 0.
      EXIT.
    ENDIF.
  ENDDO.

* ファイルのクローズ
  CLOSE DATASET LW_FULLPATH.
  IF SY-SUBRC <> 0.
*   ファイル &1 をクローズできません
    MESSAGE S498(ZC01) WITH LW_FULLPATH DISPLAY LIKE 'E'.
  ENDIF.
*
*----- return value
  O_SUBRC = SY-SUBRC.
*
*  T_TD_ULFILE[] = LTD_ITAB[].
*
ENDFORM.                               " A_SV_UPLOAD

*&---------------------------------------------------------------------*
*&      FORM  A_GET_FILE_DIRECTORY
*&---------------------------------------------------------------------*
*       SVファイル名よりディレクトリを取得する
*----------------------------------------------------------------------*
*      --> I_FULLPATH    SVファイル名
*      <-- O_DIREC       ディレクトリ
*----------------------------------------------------------------------*
FORM A_GET_FILE_DIRECTORY   USING     I_FULLPATH  TYPE ANY
                            CHANGING  O_DIREC     TYPE CSEQUENCE.
*
  DATA :
    LW_DIREC     TYPE RLGRAP-FILENAME,           " Dir
    LW_STRLEN    TYPE I,                         " Dir: num.of digits
    LW_CHAR      TYPE CHAR1.                     " Dir:1st char
*    LW_CHAR_PRE  TYPE I,                         " Dir: num.of digits
*    LW_CHAR_LEN  TYPE I.                         " Dir length
*
*----- ファイル名が初期値の場合、処理対象外
  CHECK NOT I_FULLPATH IS INITIAL.
*
*------ ファイルパスをディレクトリパス作成用の変数に格納
  LW_DIREC  = I_FULLPATH.
  LW_STRLEN = STRLEN( LW_DIREC ).
*
  IF LW_STRLEN > 0.
    LW_STRLEN = LW_STRLEN - 1.                    " オフセットの開始位置
  ENDIF.
*
  DO LW_STRLEN TIMES.
    LW_CHAR = LW_DIREC+LW_STRLEN(1).
    IF LW_CHAR = CNS_A003_YEN.
*----- 2バイト文字チェック
*      LW_CHAR_PRE = LW_STRLEN - 1.
*      ASSIGN LW_DIREC+LW_CHAR_PRE(2) TO <F_AGC20>.
*      LW_CHAR_LEN = CHARLEN( <F_AGC20> ).
*      IF LW_CHAR_LEN = 1.                         " 1バイト文字の場合
*        O_DIREC = LW_DIREC(LW_STRLEN).
*        EXIT.
*      ENDIF.
*    ENDIF.
      O_DIREC = LW_DIREC(LW_STRLEN).
      EXIT.
    ENDIF.
    LW_STRLEN = LW_STRLEN - 1.
  ENDDO.
*
ENDFORM.                    " A_GET_FILE_DIRECTORY

*&---------------------------------------------------------------------*
*&      FORM  A_FILE_NAME_SEPARATE
*&---------------------------------------------------------------------*
*       SVファイル名(フルパス)よりファイル名と拡張子に分割する
*----------------------------------------------------------------------*
*      -->  I_FULLPATH    SVファイル名
*      <--  O_FILE        ファイル名
*      <--  O_EXTENSION   拡張子
*----------------------------------------------------------------------*
FORM A_FILE_NAME_SEPARATE   USING     I_FULLPATH  TYPE ANY
                            CHANGING  O_FILE      TYPE ANY
                                      O_EXTENSION TYPE ANY.
*
  DATA:
    LW_DIREC     TYPE RLGRAP-FILENAME,           " Dir
    LW_STRLEN    TYPE I,                         " Dir: num.of digits
    LW_CHAR      TYPE CHAR1.                     " Dir:1st char
*    LW_CHAR_PRE  TYPE I,                         " Dir: num.of digits
*    LW_CHAR_LEN  TYPE I.                         " Dir length
*
  CLEAR O_EXTENSION.
  O_FILE = I_FULLPATH.
*----- ファイル名が初期値の場合、処理対象外
  CHECK NOT I_FULLPATH IS INITIAL.
*
*----- ピリオドが含まれるか確認
  CHECK I_FULLPATH CA CNS_A003_PERIOD.
*
*------ ファイルパスをディレクトリパス作成用の変数に格納
  LW_DIREC  = I_FULLPATH.
  LW_STRLEN = STRLEN( LW_DIREC ).
*
  IF LW_STRLEN > 0.
    LW_STRLEN = LW_STRLEN - 1.                    " Offset
  ENDIF.
*
  DO LW_STRLEN TIMES.
    LW_CHAR = LW_DIREC+LW_STRLEN(1).
    IF LW_CHAR = CNS_A003_PERIOD.
*----- 2バイト文字チェック
*      LW_CHAR_PRE = LW_STRLEN - 1.
*      ASSIGN LW_DIREC+LW_CHAR_PRE(2) TO <F_AGC20>.
*      LW_CHAR_LEN = CHARLEN( <F_AGC20> ).
*      IF LW_CHAR_LEN = 1.                         " 1バイト文字の場合
*        O_FILE = LW_DIREC(LW_STRLEN).
*        O_EXTENSION = LW_DIREC+LW_STRLEN.
*        EXIT.
*      ENDIF.
*    ENDIF.
      O_FILE      = LW_DIREC(LW_STRLEN).
      O_EXTENSION = LW_DIREC+LW_STRLEN.
      EXIT.
    ENDIF.
    LW_STRLEN = LW_STRLEN - 1.
  ENDDO.
*
ENDFORM.                    " A_GET_FILE_DIRECTORY