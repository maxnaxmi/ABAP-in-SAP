*&---------------------------------------------------------------------*
*& Report  ZOTM0001
*& CREATED BY   : IBCS古畑
*& DATE         : 2008/08/28
*& DESCRIPTION  : アドオンテーブルアップロード/ダウンロード
*&---------------------------------------------------------------------*
*& アドオンテーブルにデータをアップロードorダウンロード
*&---------------------------------------------------------------------*
*& 変更履歴
*& LOG#  DATE        AUTHOR     DESCRIPTION
*& 0001  2011/06/22  RS鈴木     SQL文をセットする変数の桁数変更
*&---------------------------------------------------------------------*
REPORT  ZOTM0001  NO  STANDARD PAGE HEADING
                    MESSAGE-ID ZO00
                    LINE-SIZE  170
                    LINE-COUNT 58.

************************************************************************
*DATA DEFINITION
************************************************************************

*-----*VARIABLE
*--> ADD 0001 START 2011/06/22 RS鈴木
*DATA:    V_ALINE(100),                       " Actual line
DATA:    V_ALINE(3000),                       " Actual line
*--> ADD 0001 END 2011/06/22 RS鈴木
         V_LEN          TYPE I,              " Required by contabfield
         V_REPNAME(8),                       " Report name
         V_TABCLASS     TYPE DD02L-TABCLASS,
         V_SUBRC        TYPE SY-SUBRC,       " Return code
         V_MESG(200),                        " Message
         V_LINE TYPE I,                      " Line
         V_WORD(80),                         " Word
         V_MEMORY(47)   TYPE C,              " Memory ID
         V_SPGNAME      TYPE SY-CPROG,       " Subprogram name
         CTR_DATA       TYPE SY-DBCNT,       " Data counter
**20080906 FURUHATA
*         V_PATH         TYPE RLGRAP-FILENAME,
         V_PATH(128)         TYPE C,
**20080906 FURUHATA
         V_LENGTH       TYPE DFIES-LENG,      " Length
         V_CONVEXIT     TYPE DFIES-CONVEXIT.  " ConvExit


*-----*CONSTANT
CONSTANTS : C_SUCC          TYPE C VALUE '0'.
CONSTANTS : C_BATCH         TYPE C VALUE 'X'.
CONSTANTS : C_ZEROLEN(6)       TYPE C VALUE '000000'.

*-----*TABLE
*REPORT PROGRAM DYNAMIC GENERATION SOURCE TABLE
TYPES: BEGIN OF T_REP_TAB,
*--> UPD 0001 START 2011/06/22 RS鈴木
*          LINE(100),
            LINE(3000),
*--> UPD 0001 END 2011/06/22 RS鈴木
       END   OF T_REP_TAB.
DATA : WA_REP_TAB TYPE          T_REP_TAB,
       IT_REP_TAB TYPE TABLE OF T_REP_TAB.

*SQL DYNAMIC GENERATION SOURCE TABLE
TYPES: BEGIN OF T_SQL_TAB,
*--> UPD 0001 START 2011/06/22 RS鈴木
*          LINE(100),
          LINE(3000),
*--> UPD 0001 END 2011/06/22 RS鈴木
       END   OF T_SQL_TAB.
DATA : WA_SQL_TAB TYPE          T_SQL_TAB,
       IT_SQL_TAB TYPE TABLE OF T_SQL_TAB.

*FUNCTION MODULE PROGRAM DYNAMIC GENERATION SOURCE TABLE
TYPES: BEGIN OF T_FUN_TAB,
*--> UPD 0001 START 2011/06/22 RS鈴木
*          LINE(100),
          LINE(3000),
*--> UPD 0001 END 2011/06/22 RS鈴木
       END   OF T_FUN_TAB.
DATA : WA_FUN_TAB TYPE          T_FUN_TAB,
       IT_FUN_TAB TYPE TABLE OF T_FUN_TAB.

*GET TABLE ITEM TABLE
DATA : WA_FIELD_STAT_TAB TYPE          DFIES,
       IT_FIELD_STAT_TAB TYPE TABLE OF DFIES.

*GET TABLE NAME TABLE
TYPES: BEGIN OF T_TABLE_TAB,
          NAME         TYPE DD02V-TABNAME,
          PATH         TYPE RLGRAP-FILENAME,
          WPATH        TYPE RLGRAP-FILENAME,
          FLNAME       TYPE RLGRAP-FILENAME,
       END   OF T_TABLE_TAB.
DATA : WA_TABLE_TAB TYPE          T_TABLE_TAB,
       IT_TABLE_TAB TYPE TABLE OF T_TABLE_TAB.

*LOCAL FILE NAME STORING TABLE
DATA : WA_SDOKPATH  TYPE SDOKPATH,
       IT_SDOKPATH  TYPE STANDARD TABLE OF SDOKPATH.

***20080902 FURUHATA ADD
TYPES: BEGIN OF T_STABLE_TAB,
          NAME         TYPE DD02V-TABNAME,
*          PATH         TYPE FILENAME-PATHINTERN,
*          WPATH        TYPE FILENAME-PATHINTERN,
          FLNAME       TYPE FILE,
       END   OF T_STABLE_TAB.
*SERVER FILE NAME STORING TABLE
**DATA : WA_FLNAME  TYPE FILE.
**DATA : WA_STABLE_TAB TYPE          T_STABLE_TAB,
**       IT_STABLE_TAB TYPE TABLE OF T_STABLE_TAB.
***20080902 FURUHATA ADD


************************************************************************
*SELECTION-SCREEN
************************************************************************

*-----*TABLE ID
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(14) TEXT-004.
***Add 2008/10/10 R.Furuhata Start
PARAMETERS: P_TABNM       TYPE RSRD1-TBMA_VAL VISIBLE LENGTH 10. "DD02V-TABNAME.
*PARAMETERS: P_TABNM       TYPE RSRD1-TBMA_VAL. "DD02V-TABNAME.
***Add 2008/10/10 R.Furuhata Start
SELECTION-SCREEN END   OF LINE.
*--> ADD 0001 START 2011/06/22 RS鈴木
PARAMETERS: P_WHERE(2900)       TYPE C.
*--> ADD 0001 END 2011/06/22 RS鈴木
SELECTION-SCREEN END   OF BLOCK B0.

*-----*DOWNLOAD/UPLOAD
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-017.
SELECTION-SCREEN BEGIN OF LINE.
***-----*Add20080831
PARAMETERS P_LFILE    RADIOBUTTON GROUP RG9.
SELECTION-SCREEN COMMENT 4(8) TEXT-029.
***-----*Add20080831
SELECTION-SCREEN COMMENT 14(12) TEXT-023.
**SELECTION-SCREEN COMMENT 4(12) TEXT-023.
PARAMETERS P_FNAME  TYPE RLGRAP-FILENAME.
SELECTION-SCREEN END   OF LINE.

***-----*Add20080831
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_SFILE    RADIOBUTTON GROUP RG9.
SELECTION-SCREEN COMMENT 4(8) TEXT-028.
SELECTION-SCREEN COMMENT 14(12) TEXT-023.
PARAMETERS P_SFNAME  TYPE FILE.
SELECTION-SCREEN END   OF LINE.
***-----*Add20080831


*-----*FILE FORMAT
SELECTION-SCREEN BEGIN OF BLOCK B6 WITH FRAME TITLE TEXT-024.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_DAT    RADIOBUTTON GROUP RG6 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(15) TEXT-025.
PARAMETERS P_CSV    RADIOBUTTON GROUP RG6.
SELECTION-SCREEN COMMENT 23(12) TEXT-026.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK B6.

*-----*DOWNLOAD
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_DOWN    RADIOBUTTON GROUP RG1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(12) TEXT-002.
SELECTION-SCREEN END   OF LINE.

*-----*ITEM TEXT SELECTION
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-010.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_TEXT_S    RADIOBUTTON GROUP RG2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(12) TEXT-011.
PARAMETERS P_TEXTFT    RADIOBUTTON GROUP RG2.
SELECTION-SCREEN COMMENT 20(25) TEXT-015.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_TEXT_M    RADIOBUTTON GROUP RG2.
SELECTION-SCREEN COMMENT 4(12) TEXT-012.
PARAMETERS P_TEXT_H    RADIOBUTTON GROUP RG2.
SELECTION-SCREEN COMMENT 20(12) TEXT-014.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_TEXT_L    RADIOBUTTON GROUP RG2.
SELECTION-SCREEN COMMENT 4(12) TEXT-013.
PARAMETERS P_TEXTFN    RADIOBUTTON GROUP RG2.
SELECTION-SCREEN COMMENT 20(12) TEXT-016.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK B2.

*-----*UPLOAD
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_UPLOAD    RADIOBUTTON GROUP RG1.
SELECTION-SCREEN COMMENT 4(12) TEXT-003.
SELECTION-SCREEN END   OF LINE.

*-----*IS A HEADER IN A UPLOAD FILE?
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-019.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_H_ON      RADIOBUTTON GROUP RG3 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(7) TEXT-020.
PARAMETERS P_H_NON     RADIOBUTTON GROUP RG3.
SELECTION-SCREEN COMMENT 15(10) TEXT-021.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK B3.

*-----*THE CONTENTS OF PROCESSING
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-022.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_INS      RADIOBUTTON GROUP RG4 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(31) TEXT-009.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_DEL      RADIOBUTTON GROUP RG4.
SELECTION-SCREEN COMMENT 4(45) TEXT-006.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_MOD      RADIOBUTTON GROUP RG4.
SELECTION-SCREEN COMMENT 4(46) TEXT-007.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK B4.

*SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-030.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 4(31) TEXT-031.
*PARAMETERS: P_DEV  TYPE I.
*SELECTION-SCREEN END   OF LINE.
*SELECTION-SCREEN END   OF BLOCK B5.

SELECTION-SCREEN END   OF BLOCK B1.

************************************************************************
*AT SELECTION-SCREEN
************************************************************************

*-----*1-1.FAILE NAME SELECTION DIALOG DISPLAY
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FNAME.

*-----*1-1-1.GET FILE NAME
      PERFORM GET_FILENAME.

************************************************************************
*INITIALIZATION
************************************************************************
INITIALIZATION.
*  CONCATENATE SY-CPROG '_' SY-UZEIT INTO V_MEMORY.
  MOVE V_MEMORY TO V_SPGNAME.
**ADD 20080906
  IF P_LFILE = 'X'.
    MOVE P_FNAME TO V_PATH.
  ELSEIF P_SFILE = 'X'.
    MOVE P_SFNAME TO V_PATH.
  ENDIF.
**ADD 20080906

************************************************************************
*START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

***************1.INITIAL PROCESSING

*-----*1-2.TABLE NAME A NON-INPUTTED CHECK
  IF  P_TABNM IS INITIAL.
    MOVE '7' TO V_SUBRC.
  ENDIF.

* STOP FLAG
  CHECK V_SUBRC <> '7'.

*-----*1-3.FILE PATH A NON-INPUTTED CHECK
**20080906 FURUHATA
*  IF  P_FNAME IS INITIAL.
  IF ( P_LFILE = 'X' AND P_FNAME IS INITIAL )
    OR ( P_SFILE = 'X' AND P_SFNAME IS INITIAL ).
      MOVE '17' TO V_SUBRC.
  ENDIF.
**20080906 FURUHATA

* STOP FLAG
  CHECK V_SUBRC <> '17'.

  CLEAR WA_TABLE_TAB.
  MOVE P_TABNM TO WA_TABLE_TAB-NAME.
  MOVE 'C:\'   TO WA_TABLE_TAB-PATH.
  APPEND WA_TABLE_TAB TO IT_TABLE_TAB.

*-----*1-4.THE TABLE NAME CHECK AT THE TIME OF UPLOAD
  PERFORM TABLE_UPLOAD_CHECK.

* STOP FLAG
  CHECK V_SUBRC <> '8'.

*-----*1-5.TABLE NAME EXISTENCE CHECK
  PERFORM TABLE_EXIT_CHECK.

* STOP FLAG
  CHECK V_SUBRC <> '9'.

  LOOP AT IT_TABLE_TAB INTO WA_TABLE_TAB.


***************2.IN THE CASE OF DOWNLOAD
    CASE 'X'.
      WHEN P_DOWN.

*-----*2-1.GET TABLE ITEM
        PERFORM SQL_TAB_MAKE_DOWNLOAD.

*-----*2-2.DOWNLOAD PLACE SELECTION
        PERFORM LOCAL_DOWNLOAD_PROC.


***************3.IN THE CASE OF UPLOAD
      WHEN P_UPLOAD.

****20080902 FURUHATA

*-----*3-1.UPLOAD PLACE SELECTION
       IF P_LFILE = 'X'.
        PERFORM LOCAL_UP_PROC.
       ELSEIF P_SFILE = 'X'.
        PERFORM SERVER_UP_PROC.
       ENDIF.
****20080902 FURUHATA

*-----*3-2.UPLOAD DATA EDIT
        PERFORM UP_SPLIT_UPLOAD.

*       STOP FLAG
        CHECK V_SUBRC <> '19'.

*-----*3-3.TABLE UPDATING DATA EDIT
        PERFORM SQL_TAB_MAKE_UPLOAD.

    ENDCASE.


***************4.REPORT PROGRAM DYNAMIC GENERATION

*-----*4-1.GET REPORT PROGRAM SOURCE
    PERFORM REP_TAB_MAKE.

*-----*4-2.REPORT PROGRAM DYNAMIC GENERATION
    GENERATE SUBROUTINE POOL IT_REP_TAB NAME V_REPNAME
                        MESSAGE V_MESG LINE V_LINE WORD V_WORD.

    IF  SY-SUBRC = 0.
      PERFORM TAB_BACKUP IN PROGRAM (V_REPNAME).
      IMPORT V_SUBRC CTR_DATA FROM MEMORY ID V_MEMORY.
      FREE MEMORY ID V_MEMORY.
    ELSE.
      MOVE '6' TO V_SUBRC.
    ENDIF.

    IF  V_SUBRC <> 0.
      EXIT.
    ENDIF.

  ENDLOOP.

************************************************************************
*END-OF-SELECTION
************************************************************************
END-OF-SELECTION.

  IF  V_SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  CASE V_SUBRC.
    WHEN 0.
      MESSAGE S005 WITH CTR_DATA.
    WHEN 1.
      MESSAGE S005
              WITH CTR_DATA.
    WHEN 2.
      IF SY-BATCH = C_BATCH.
        MESSAGE E007
                WITH P_FNAME.
      ELSE.
        MESSAGE S007
                WITH P_FNAME.
      ENDIF.
    WHEN 3.
      IF SY-BATCH = C_BATCH.
        MESSAGE E011
                WITH P_FNAME.
      ELSE.
        MESSAGE S011
                WITH P_FNAME.
      ENDIF.
    WHEN 4.
      IF SY-BATCH = C_BATCH.
        MESSAGE E012
                WITH WA_TABLE_TAB-NAME.
      ELSE.
        MESSAGE S012
                WITH WA_TABLE_TAB-NAME.
      ENDIF.
    WHEN 5.
      IF SY-BATCH = C_BATCH.
        MESSAGE E013
                WITH WA_TABLE_TAB-NAME.
      ELSE.
        MESSAGE S013
                WITH WA_TABLE_TAB-NAME.
      ENDIF.
    WHEN 6.
      MESSAGE S019
              WITH V_MESG+0(50) V_MESG+50(50)
                   V_MESG+100(50) V_MESG+150(50).
    WHEN 7.
      IF SY-BATCH = C_BATCH.
        MESSAGE E000.
      ELSE.
        MESSAGE S000.
      ENDIF.
    WHEN 8.
      IF SY-BATCH = C_BATCH.
        MESSAGE E003
                WITH WA_TABLE_TAB-NAME.
      ELSE.
        MESSAGE S003
                WITH WA_TABLE_TAB-NAME.
      ENDIF.
    WHEN 9.
      IF SY-BATCH = C_BATCH.
        MESSAGE E002.
      ELSE.
        MESSAGE S002.
      ENDIF.
    WHEN 10.
      IF SY-BATCH = C_BATCH.
        MESSAGE E008
                WITH CTR_DATA.
      ELSE.
        MESSAGE S008
                WITH CTR_DATA.
      ENDIF.
    WHEN 11.
      IF SY-BATCH = C_BATCH.
        MESSAGE E009.
      ELSE.
        MESSAGE S009.
      ENDIF.
    WHEN 12.
      IF SY-BATCH = C_BATCH.
        MESSAGE E010.
      ELSE.
        MESSAGE S010.
      ENDIF.
    WHEN 13.
      IF SY-BATCH = C_BATCH.
        MESSAGE E015
                WITH P_FNAME.
      ELSE.
        MESSAGE S015
                WITH P_FNAME.
      ENDIF.
    WHEN 14.
      IF SY-BATCH = C_BATCH.
        MESSAGE E016
                WITH P_FNAME.
      ELSE.
        MESSAGE S016
                WITH P_FNAME.
      ENDIF.
    WHEN 15.
      IF SY-BATCH = C_BATCH.
        MESSAGE E016
                WITH P_FNAME.
      ELSE.
        MESSAGE S016
                WITH P_FNAME.
      ENDIF.
    WHEN 16.
      IF SY-BATCH = C_BATCH.
        MESSAGE E017
                WITH P_FNAME.
      ELSE.
        MESSAGE S017
                WITH P_FNAME.
      ENDIF.
    WHEN 17.
      IF SY-BATCH = C_BATCH.
        MESSAGE E001
                WITH P_FNAME.
      ELSE.
        MESSAGE S001
                WITH P_FNAME.
      ENDIF.
    WHEN 18.
      IF SY-BATCH = C_BATCH.
        MESSAGE E018.
      ELSE.
        MESSAGE S018.
      ENDIF.
    WHEN 19.
      IF SY-BATCH = C_BATCH.
        MESSAGE E014.
      ELSE.
        MESSAGE S014.
      ENDIF.
    WHEN 20.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  get_filename
*&---------------------------------------------------------------------*
*     　1-1-1.GET FILE NAME
*----------------------------------------------------------------------*
FORM GET_FILENAME.

  CALL FUNCTION 'TMP_GUI_FILE_OPEN_DIALOG'
       EXPORTING
            FILE_FILTER = 'すべてのファイル(*.*)|*.*|'
       TABLES
            FILE_TABLE  = IT_SDOKPATH
       EXCEPTIONS
            CNTL_ERROR  = 1
            OTHERS      = 2.

  IF SY-SUBRC <> 0.
    MOVE '18' TO V_SUBRC.
  ELSE.
    LOOP AT IT_SDOKPATH INTO WA_SDOKPATH.
      P_FNAME = WA_SDOKPATH-PATHNAME.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TABLE_UPLOAD_CHECK
*&---------------------------------------------------------------------*
*       1-4.THE TABLE NAME CHECK AT THE TIME OF UPLOAD
*&---------------------------------------------------------------------*
FORM TABLE_UPLOAD_CHECK.

  IF  WA_TABLE_TAB-NAME+0(1) <> 'Z'  AND
      WA_TABLE_TAB-NAME+0(1) <> 'Y'  AND
*      WA_TABLE_TAB-NAME+0(2) <> 'P9' AND
*      WA_TABLE_TAB-NAME+0(2) <> 'T9' AND
      P_UPLOAD              EQ 'X'.
    MOVE '8' TO V_SUBRC.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TABLE_EXIT_CHECK
*&---------------------------------------------------------------------*
*       1-5.TABLE NAME EXISTENCE CHECK
*----------------------------------------------------------------------*
FORM TABLE_EXIT_CHECK.

* TABLE EXISTENCE CHECK
  SELECT SINGLE TABCLASS
           INTO V_TABCLASS
           FROM DD02L
          WHERE TABNAME  =  WA_TABLE_TAB-NAME
          AND   AS4LOCAL =  'A'
          AND   AS4VERS  =  0
          AND   TABCLASS <> 'INTTAB'
          AND   TABCLASS <> 'VIEW'
          AND   TABCLASS <> 'APPEND'.

  IF SY-SUBRC <> C_SUCC.
    MOVE '9' TO V_SUBRC.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SQL_TAB_MAKE_DOWNLOAD
*&---------------------------------------------------------------------*
*       2-1.GET TABLE ITEM
*&---------------------------------------------------------------------*
FORM SQL_TAB_MAKE_DOWNLOAD.

  V_ALINE      = 'SELECT * FROM '.
  V_LEN        = STRLEN( V_ALINE ) + 1.
  V_ALINE+V_LEN  = WA_TABLE_TAB-NAME.
  V_LEN        = STRLEN( V_ALINE ) + 1.
*--> UPD 0001 START 2011/06/22 RS鈴木
* V_ALINE+V_LEN  = 'INTO TABLE IT_SELECT_TAB.'.
*  WA_SQL_TAB-LINE    = V_ALINE.
  V_ALINE+V_LEN  = 'INTO TABLE IT_SELECT_TAB'.
  V_LEN        = STRLEN( V_ALINE ) + 1.
  IF P_WHERE IS NOT INITIAL.
    V_ALINE+V_LEN = P_WHERE.
    V_LEN        = STRLEN( V_ALINE ) + 1.
  ENDIF.
  V_ALINE+V_LEN  = '.'.
  WA_SQL_TAB-LINE    = V_ALINE.
*--> UPD 0001 END 2011/06/22 RS鈴木
  APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE            = 'MOVE SY-DBCNT TO CTR_DATA.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

* ■1.SQL-SUBRC PROCESSING
  PERFORM SET_SUBRC_SQL USING '1'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FUN_TAB_MAKE_DOWNLOAD
*&---------------------------------------------------------------------*
*       2-2.DOWNLOAD PLACE SELECTION
*&---------------------------------------------------------------------*
FORM LOCAL_DOWNLOAD_PROC.

  V_ALINE      = '  CALL FUNCTION ''GUI_DOWNLOAD'''.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '       EXPORTING'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '            FILENAME            = V_PATH'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '            FILETYPE            = ''ASC'''.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '       TABLES'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '            DATA_TAB            = IT_DOWNLOAD'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '       EXCEPTIONS'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '            INVALID_FILESIZE    = 1'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '            INVALID_TABLE_WIDTH = 2'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '            INVALID_TYPE        = 3'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '            NO_BATCH            = 4'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '            UNKNOWN_ERROR       = 5'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '            OTHERS              = 6.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.

* ■2.SQL-SUBRC PROCESSING
  PERFORM SET_SUBRC_FUN USING '2'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FUN_TAB_MAKE_UPLOAD
*&---------------------------------------------------------------------*
*       3-1.UPLOAD PLACE SELECTION
*&---------------------------------------------------------------------*
FORM LOCAL_UP_PROC.

  V_ALINE      = 'CALL FUNCTION ''GUI_UPLOAD'''.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'EXPORTING'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'FILENAME           = V_PATH'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'FILETYPE            = ''ASC'''.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'TABLES'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'DATA_TAB            = IT_DOWNLOAD'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  WA_FUN_TAB-LINE    = V_ALINE.
  V_ALINE      = 'EXCEPTIONS'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'FILE_OPEN_ERROR               = 1'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'FILE_READ_ERROR               = 2'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'NO_BATCH                      = 3'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'GUI_REFUSE_FILETRANSFER       = 4'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'INVALID_TYPE                  = 5'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'NO_AUTHORITY                  = 6'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'UNKNOWN_ERROR                 = 7'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'BAD_DATA_FORMAT               = 8'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'HEADER_NOT_ALLOWED            = 9'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'SEPARATOR_NOT_ALLOWED         = 10'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'HEADER_TOO_LONG               = 11'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'UNKNOWN_DP_ERROR              = 12'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'ACCESS_DENIED                 = 13'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'DP_OUT_OF_MEMORY              = 14'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'DISK_FULL                     = 15'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'DP_TIMEOUT                    = 16'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'OTHERS                        = 17.'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.

* ■2.SQL-SUBRC PROCESSING
  PERFORM SET_SUBRC_FUN     USING '3'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UP_SPLIT
*&---------------------------------------------------------------------*
*       3-2.UPLOAD DATA EDIT
*----------------------------------------------------------------------*
FORM UP_SPLIT_UPLOAD.

* GET DYNAMIC TABLE ITEM
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
       EXPORTING
            TABNAME   = WA_TABLE_TAB-NAME
            LANGU     = SY-LANGU
       TABLES
            DFIES_TAB = IT_FIELD_STAT_TAB
       EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.

  IF SY-SUBRC <> 0.
    MOVE '19' TO V_SUBRC.
    EXIT.
  ENDIF.

  V_ALINE      = 'LOOP AT IT_DOWNLOAD INTO WA_DOWNLOAD.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.

*-----*BRANCH BY THE FILE FORMAT
  CASE 'X'.
    WHEN P_DAT.
      V_ALINE      = '  SPLIT WA_DOWNLOAD-DATA AT C_TAB INTO'.
      WA_FUN_TAB-LINE    = V_ALINE.
      APPEND WA_FUN_TAB TO IT_FUN_TAB.
    WHEN P_CSV.
      V_ALINE      = '  SPLIT WA_DOWNLOAD-DATA AT C_CSV INTO'.
      WA_FUN_TAB-LINE    = V_ALINE.
      APPEND WA_FUN_TAB TO IT_FUN_TAB.
  ENDCASE.

* GET DYNAMIC TABLE ITEM
  LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB.
    CLEAR WA_REP_TAB.

    WA_FUN_TAB-LINE  = ' WA_FIELD_TAB-'.
    V_LEN         = STRLEN( WA_FUN_TAB-LINE ).
    WA_FUN_TAB-LINE+V_LEN  = WA_FIELD_STAT_TAB-FIELDNAME.
    APPEND WA_FUN_TAB TO IT_FUN_TAB.

  ENDLOOP.

  V_ALINE      = '.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.

***20080923 FURUHATA ADD START
*  V_ALINE      = '  WA_FIELD_TAB-ZCRDAT = SY-DATUM.'.
*  WA_FUN_TAB-LINE    = V_ALINE.
*  APPEND WA_FUN_TAB TO IT_FUN_TAB.
*
*  V_ALINE      = '  WA_FIELD_TAB-ZCRTIM = SY-UZEIT.'.
*  WA_FUN_TAB-LINE    = V_ALINE.
*  APPEND WA_FUN_TAB TO IT_FUN_TAB.
*
*  V_ALINE      = '  WA_FIELD_TAB-ZCRUSR = SY-UNAME.'.
*  WA_FUN_TAB-LINE    = V_ALINE.
*  APPEND WA_FUN_TAB TO IT_FUN_TAB.
*
*  V_ALINE      = '  WA_FIELD_TAB-ZUPDAT = SY-DATUM.'.
*  WA_FUN_TAB-LINE    = V_ALINE.
*  APPEND WA_FUN_TAB TO IT_FUN_TAB.
*
*  V_ALINE      = '  WA_FIELD_TAB-ZUPTIM = SY-UZEIT.'.
*  WA_FUN_TAB-LINE    = V_ALINE.
*  APPEND WA_FUN_TAB TO IT_FUN_TAB.
*
*  V_ALINE      = '  WA_FIELD_TAB-ZUPUSR = SY-UNAME.'.
*  WA_FUN_TAB-LINE    = V_ALINE.
*  APPEND WA_FUN_TAB TO IT_FUN_TAB.

***20080923 FURUHATA ADD END

  V_ALINE      = '  APPEND WA_FIELD_TAB TO IT_FIELD_TAB.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.

  V_ALINE      = 'ENDLOOP.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SQL_TAB_MAKE_UPLOAD
*&---------------------------------------------------------------------*
*       3-3.TABLE UPDATING DATA EDIT
*&---------------------------------------------------------------------*
FORM SQL_TAB_MAKE_UPLOAD.

  REFRESH IT_SQL_TAB.

  IF P_H_ON = 'X'.
    V_ALINE      = '  DELETE IT_FIELD_TAB INDEX 1.'.
    WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

  ENDIF.

  V_ALINE      = '  REFRESH IT_SELECT_TAB.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

* EXCEPTION-CATCH
  V_ALINE = 'CATCH SYSTEM-EXCEPTIONS CONVT_NO_NUMBER = 99'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE = '          CONVT_OVERFLOW = 98.'.
  WA_SQL_TAB-LINE    = V_ALINE.
  APPEND WA_SQL_TAB TO IT_SQL_TAB.

  V_ALINE      = '  LOOP AT IT_FIELD_TAB INTO WA_FIELD_TAB.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB
                         WHERE
                            INTTYPE = 'D'
                            OR INTTYPE = 'T'.

    V_ALINE      = '    PERFORM CONV_D_T USING WA_FIELD_TAB-'.
    V_LEN        = STRLEN( V_ALINE ).
    V_ALINE+V_LEN  = WA_FIELD_STAT_TAB-FIELDNAME.
    V_LEN        = STRLEN( V_ALINE ).
    V_ALINE+V_LEN  = '.'.
    WA_SQL_TAB-LINE    = V_ALINE.
    APPEND WA_SQL_TAB TO IT_SQL_TAB.

  ENDLOOP.

  V_ALINE = '    MOVE-CORRESPONDING WA_FIELD_TAB TO WA_SELECT_TAB.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = '    APPEND WA_SELECT_TAB TO IT_SELECT_TAB.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = '  ENDLOOP.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

* EXCEPTION-ENDCATCH
  V_ALINE      = 'ENDCATCH.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = 'CASE SY-SUBRC.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = '  WHEN 99.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = '    MOVE ''11'' TO V_SUBRC.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  WA_SQL_TAB-LINE  = '    EXPORT V_SUBRC CTR_DATA TO MEMORY ID '''.
  V_LEN          = STRLEN( WA_SQL_TAB-LINE ).
  WA_SQL_TAB-LINE+V_LEN  = V_MEMORY.
  V_LEN          = STRLEN( WA_SQL_TAB-LINE ).
  WA_SQL_TAB-LINE+V_LEN  = '''.'.
  APPEND WA_SQL_TAB TO IT_SQL_TAB.
  WA_SQL_TAB-LINE      = '    EXIT.'.
  APPEND WA_SQL_TAB TO IT_SQL_TAB.

  V_ALINE      = '  WHEN 98.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = '    MOVE ''12'' TO V_SUBRC.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  WA_SQL_TAB-LINE  = '    EXPORT V_SUBRC CTR_DATA TO MEMORY ID '''.
  V_LEN          = STRLEN( WA_SQL_TAB-LINE ).
  WA_SQL_TAB-LINE+V_LEN  = V_MEMORY.
  V_LEN          = STRLEN( WA_SQL_TAB-LINE ).
  WA_SQL_TAB-LINE+V_LEN  = '''.'.
  APPEND WA_SQL_TAB TO IT_SQL_TAB.

  WA_SQL_TAB-LINE      = '    EXIT.'.
  APPEND WA_SQL_TAB TO IT_SQL_TAB.

  V_ALINE      = 'ENDCASE.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

  V_ALINE      = '  FREE IT_FIELD_TAB.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

  CASE 'X'.
    WHEN P_DEL.
      READ TABLE IT_FIELD_STAT_TAB
          WITH KEY DATATYPE = 'CLNT' INTO WA_FIELD_STAT_TAB.

      V_ALINE      = '  DELETE FROM'.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = WA_TABLE_TAB-NAME.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = 'CLIENT SPECIFIED'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '         WHERE'.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = WA_FIELD_STAT_TAB-FIELDNAME.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = '= SY-MANDT'.
      WA_SQL_TAB-LINE    = V_ALINE.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = '.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

*     ■1.SQL-SUBRC PROCESSING
      PERFORM SET_SUBRC_SQL     USING '4'.

    WHEN OTHERS.

  ENDCASE.
  CASE 'X'.
    WHEN P_DEL OR P_INS.
      V_ALINE      = '  CLEAR CTR_DATA.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '  LOOP AT IT_SELECT_TAB INTO WA_SELECT_TAB.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '    IF WA_SELECT_TAB+0(1) = ''*''.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '      CONTINUE.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '    ENDIF.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

      LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB
                               WHERE CONVEXIT = 'ALPHA'.

        V_ALINE         = 'CONVERSION_EXIT_'.
        V_LEN           = STRLEN( V_ALINE ).
        V_ALINE+V_LEN     = WA_FIELD_STAT_TAB-CONVEXIT.
        V_LEN           = STRLEN( V_ALINE ).
        V_ALINE+V_LEN     = '_INPUT'.
        WA_SQL_TAB-LINE       = '    PERFORM'.
        V_LEN           = STRLEN( WA_SQL_TAB-LINE ) + 1.
        WA_SQL_TAB-LINE+V_LEN   = V_ALINE.
        V_LEN           = STRLEN( WA_SQL_TAB-LINE ).
        WA_SQL_TAB-LINE+V_LEN   = ' USING WA_SELECT_TAB-'.
        V_LEN           = STRLEN( WA_SQL_TAB-LINE ).
        WA_SQL_TAB-LINE+V_LEN   = WA_FIELD_STAT_TAB-FIELDNAME.
        V_LEN           = STRLEN( WA_SQL_TAB-LINE ).
        WA_SQL_TAB-LINE+V_LEN   = '.'.
        APPEND WA_SQL_TAB TO IT_SQL_TAB.
      ENDLOOP.
      CLEAR SY-SUBRC.

      V_ALINE      = '    INSERT '.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = WA_TABLE_TAB-NAME.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = 'FROM WA_SELECT_TAB.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '    ADD 1 TO CTR_DATA.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '    IF  SY-SUBRC NE 0.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '      MOVE SY-SUBRC TO V_SUBRC.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '      EXIT.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '    ENDIF.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '  ENDLOOP.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '  MOVE V_SUBRC TO SY-SUBRC.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

*     ■1.SQL-SUBRC PROCESSING
      PERFORM SET_SUBRC_SQL     USING '10'.

    WHEN P_MOD.
      V_ALINE      = '  LOOP AT IT_SELECT_TAB INTO WA_SELECT_TAB. '.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '    IF WA_SELECT_TAB+0(1) = ''*''.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '      CONTINUE.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '    ENDIF.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

      LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB
                WHERE CONVEXIT = 'ALPHA'.
        V_ALINE         = 'CONVERSION_EXIT_'.
        V_LEN           = STRLEN( V_ALINE ).
        V_ALINE+V_LEN     = WA_FIELD_STAT_TAB-CONVEXIT.
        V_LEN           = STRLEN( V_ALINE ).
        V_ALINE+V_LEN     = '_INPUT'.
        WA_SQL_TAB-LINE       = '    PERFORM'.
        V_LEN           = STRLEN( WA_SQL_TAB-LINE ) + 1.
        WA_SQL_TAB-LINE+V_LEN   = V_ALINE.
        V_LEN           = STRLEN( WA_SQL_TAB-LINE ).
        WA_SQL_TAB-LINE+V_LEN   = ' USING WA_SELECT_TAB-'.
        V_LEN           = STRLEN( WA_SQL_TAB-LINE ).
        WA_SQL_TAB-LINE+V_LEN   = WA_FIELD_STAT_TAB-FIELDNAME.
        V_LEN           = STRLEN( WA_SQL_TAB-LINE ).
        WA_SQL_TAB-LINE+V_LEN   = '.'.
        APPEND WA_SQL_TAB TO IT_SQL_TAB.
      ENDLOOP.
      CLEAR SY-SUBRC.

      V_ALINE      = '    UPDATE '.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = WA_TABLE_TAB-NAME.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = 'FROM WA_SELECT_TAB.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '    IF SY-SUBRC = 0. '.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '    ELSE. '.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE      = '      INSERT INTO '.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = WA_TABLE_TAB-NAME.
      V_LEN        = STRLEN( V_ALINE ) + 1.
      V_ALINE+V_LEN  = 'VALUES WA_SELECT_TAB.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

*     ■1.SQL-SUBRC PROCESSING
      PERFORM SET_SUBRC_SQL USING '5'.

      V_ALINE     = '    ENDIF. '.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE     = '  ENDLOOP. '.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
      V_ALINE     = '  DESCRIBE TABLE IT_SELECT_TAB LINES CTR_DATA.'.
      WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REP_TAB_MAKE
*&---------------------------------------------------------------------*
*       4-1.GET REPORT PROGRAM SOURCE
*&---------------------------------------------------------------------*
FORM REP_TAB_MAKE.

  REFRESH: IT_REP_TAB.
  WA_REP_TAB-LINE       = 'REPORT'.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ) + 2.
  WA_REP_TAB-LINE+V_LEN   = V_SPGNAME.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ).
  WA_REP_TAB-LINE+V_LEN   = '.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE       = '*'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE       = 'FORM TAB_BACKUP.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE       = '*'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

*-----*DATA TYPE GENERATION
  WA_REP_TAB       = 'DATA: WA_SELECT_TAB TYPE '.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ) + 1.
  WA_REP_TAB-LINE+V_LEN   = WA_TABLE_TAB-NAME.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ).
  WA_REP_TAB-LINE+V_LEN   = ','.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE    = '   IT_SELECT_TAB TYPE TABLE OF '.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ) + 1.
  WA_REP_TAB-LINE+V_LEN   = WA_TABLE_TAB-NAME.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ).
  WA_REP_TAB-LINE+V_LEN   = '.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  WA_REP_TAB       = 'DATA: WA_FIELD_STAT_TAB TYPE DFIES,'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = '      IT_FIELD_STAT_TAB TYPE TABLE OF DFIES.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  WA_REP_TAB       = 'TYPES: BEGIN OF T_DOWNLOAD,'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = '      DATA(3000)   TYPE C,'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = '     END OF T_DOWNLOAD.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = 'DATA: WA_DOWNLOAD TYPE T_DOWNLOAD,'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = '      IT_DOWNLOAD TYPE TABLE OF T_DOWNLOAD.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = 'DATA: V_TMPFIELD(128)  TYPE C.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = 'DATA: V_TMPFIELD_DASH(129) TYPE C.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = 'DATA: V_DTYPE TYPE C.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

****080902 FURUHATA DELETED
**  WA_REP_TAB       = 'CONSTANTS: C_TAB TYPE C VALUE ''09'','.
**  APPEND WA_REP_TAB TO IT_REP_TAB.
****080902 FURUHATA DELETED

  WA_REP_TAB       = 'CONSTANTS: C_CSV TYPE C VALUE '',''.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

****080902 FURUHATA ADD
  WA_REP_TAB       = 'CLASS CL_ABAP_CHAR_UTILITIES DEFINITION LOAD.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = 'DATA :C_TAB TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
****080902 FURUHATA ADD

  WA_REP_TAB-LINE       = 'TYPES: BEGIN OF T_FIELD_TAB,'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  REFRESH IT_FIELD_STAT_TAB.

* GET DYNAMIC TABLE ITEM
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
       EXPORTING
            TABNAME   = WA_TABLE_TAB-NAME
            LANGU     = SY-LANGU
       TABLES
            DFIES_TAB = IT_FIELD_STAT_TAB
       EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.

  CASE SY-SUBRC.
    WHEN 1.
    WHEN 2.
  ENDCASE.

*-----*TABLE ITEM DEFINITION DYNAMIC GENERATION
  LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB.
    CLEAR WA_REP_TAB.
    WA_REP_TAB-LINE+15  = WA_FIELD_STAT_TAB-FIELDNAME.
    V_LEN         = STRLEN( WA_REP_TAB-LINE ).

*　 ITEM TEXT SELECTION AT THE TIME OF DOWNLOAD
    CASE 'X'.
      WHEN P_DOWN.
        CASE 'X'.
          WHEN P_TEXT_S.
            WA_REP_TAB-LINE+V_LEN = '(10),'.
          WHEN P_TEXT_M.
            WA_REP_TAB-LINE+V_LEN = '(20),'.
          WHEN P_TEXT_L.
            WA_REP_TAB-LINE+V_LEN = '(40),'.
          WHEN P_TEXT_H.
            WA_REP_TAB-LINE+V_LEN = '(55),'.
          WHEN P_TEXTFT.
            WA_REP_TAB-LINE+V_LEN = '(60),'.
          WHEN OTHERS.
            WA_REP_TAB-LINE+V_LEN = '(30),'.
        ENDCASE.
      WHEN P_UPLOAD.
        CLEAR V_LENGTH.
        V_LENGTH = WA_FIELD_STAT_TAB-OUTPUTLEN.
*0805 ADDED(04)BEGIN
      IF V_LENGTH = C_ZEROLEN.
          CLEAR V_LENGTH.
          WA_REP_TAB-LINE+V_LEN = ','.
      ELSE.
*0805 ADDED(04)BEGIN
          WA_REP_TAB-LINE+V_LEN = '('.
          V_LEN         = STRLEN( WA_REP_TAB-LINE ).
          WA_REP_TAB-LINE+V_LEN = V_LENGTH.
          V_LEN         = STRLEN( WA_REP_TAB-LINE ).
          WA_REP_TAB-LINE+V_LEN = '),'.
*0805 ADDED(05)BEGIN
       ENDIF.
*0805 ADDED(05)END
    ENDCASE.

    APPEND WA_REP_TAB TO IT_REP_TAB.
  ENDLOOP.

*    WA_REP_TAB-LINE       = '      END   OF FIELD_TAB.'.
  WA_REP_TAB-LINE  = '      END   OF T_FIELD_TAB.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = 'DATA: WA_FIELD_TAB TYPE T_FIELD_TAB,'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB       = '      IT_FIELD_TAB TYPE TABLE OF T_FIELD_TAB.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

* TABLE DECLARATION FOR DOWNLOAD AT THE TIME OF DOWNLOAD
  IF P_DOWN = 'X'.
    WA_REP_TAB-LINE       = 'TYPES: BEGIN OF T_CONCA_TAB,'.
    APPEND WA_REP_TAB TO IT_REP_TAB.

    LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB.
      CLEAR WA_REP_TAB.
      WA_REP_TAB-LINE+15  = WA_FIELD_STAT_TAB-FIELDNAME.
      V_LEN         = STRLEN( WA_REP_TAB-LINE ).

      CLEAR V_LENGTH.

      V_LENGTH = WA_FIELD_STAT_TAB-OUTPUTLEN.
*0805 ADDED(02)BEGIN
      IF V_LENGTH = C_ZEROLEN.
          CLEAR V_LENGTH.
          WA_REP_TAB-LINE+V_LEN = ','.
      ELSE.
*0805 ADDED(02)END
          WA_REP_TAB-LINE+V_LEN = '('.
        V_LEN         = STRLEN( WA_REP_TAB-LINE ).
        WA_REP_TAB-LINE+V_LEN = V_LENGTH.
        V_LEN         = STRLEN( WA_REP_TAB-LINE ).
        WA_REP_TAB-LINE+V_LEN = '),'.
*0805 ADDED(06)BEGIN
      ENDIF.
*0805 ADDED(06)END
      APPEND WA_REP_TAB TO IT_REP_TAB.
    ENDLOOP.

    WA_REP_TAB-LINE  = '      END   OF T_CONCA_TAB.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB       = 'DATA: WA_CONCA_TAB TYPE T_CONCA_TAB,'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB   = '      IT_CONCA_TAB TYPE TABLE OF T_CONCA_TAB.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.

  ENDIF.

  WA_REP_TAB-LINE  = 'DATA: V_SUBRC     LIKE SY-SUBRC VALUE ''0''.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE = 'DATA: CTR_DATA     LIKE SY-DBCNT VALUE ''0''.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE  = 'DATA: V_PATH TYPE STRING.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE = 'MOVE '.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE       = ''''.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ).
IF P_LFILE = 'X'.
  WA_REP_TAB-LINE+V_LEN   = P_FNAME.
ELSEIF P_SFILE = 'X'.
  WA_REP_TAB-LINE+V_LEN   = P_SFNAME.
ENDIF.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ).
  WA_REP_TAB-LINE+V_LEN   = ''' TO V_PATH.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE = 'CONSTANTS: P_TABNM      LIKE DD02V-TABNAME VALUE'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE       = ''''.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ).
  WA_REP_TAB-LINE+V_LEN   = WA_TABLE_TAB-NAME.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ).
  WA_REP_TAB-LINE+V_LEN   = '''.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  WA_REP_TAB-LINE       = '*'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  CASE 'X'.
    WHEN P_DOWN.

*-----*FUNCTION MODULE FOR GET TABLE HEADER DYNAMIC GENERATION
      WA_REP_TAB-LINE    = '  CALL FUNCTION ''DDIF_FIELDINFO_GET'''.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE    = '       EXPORTING'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE    = '            TABNAME          = P_TABNM'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE    = '            LANGU            = SY-LANGU'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE    = '       TABLES'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE = '        DFIES_TAB      = IT_FIELD_STAT_TAB'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE    = '       EXCEPTIONS'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE    = '            NOT_FOUND       = 1'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE    = '            OTHERS          = 2.'.
      APPEND WA_REP_TAB TO IT_REP_TAB.

      LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB.
        WA_REP_TAB-LINE
               = '  READ TABLE IT_FIELD_STAT_TAB WITH KEY '.
        APPEND WA_REP_TAB TO IT_REP_TAB.
        WA_REP_TAB-LINE     = '       TABNAME   = P_TABNM'.
        APPEND WA_REP_TAB TO IT_REP_TAB.
        WA_REP_TAB-LINE     = '       FIELDNAME = '''.
        V_LEN         = STRLEN( WA_REP_TAB-LINE ).
        WA_REP_TAB-LINE+V_LEN = WA_FIELD_STAT_TAB-FIELDNAME.
        V_LEN         = STRLEN( WA_REP_TAB-LINE ).
        WA_REP_TAB-LINE+V_LEN = ''''.
        APPEND WA_REP_TAB TO IT_REP_TAB.
        WA_REP_TAB-LINE
              = 'LANGU    = SY-LANGU INTO WA_FIELD_STAT_TAB.'.
        APPEND WA_REP_TAB TO IT_REP_TAB.
        WA_REP_TAB-LINE     = '  MOVE WA_FIELD_STAT_TAB-'.
        V_LEN         = STRLEN( WA_REP_TAB-LINE ).
        CASE 'X'.
          WHEN P_TEXT_S.
            WA_REP_TAB-LINE+V_LEN = 'SCRTEXT_S'.
          WHEN P_TEXT_M.
            WA_REP_TAB-LINE+V_LEN = 'SCRTEXT_M'.
          WHEN P_TEXT_L.
            WA_REP_TAB-LINE+V_LEN = 'SCRTEXT_L'.
          WHEN P_TEXT_H.
            WA_REP_TAB-LINE+V_LEN = 'REPTEXT'.
          WHEN P_TEXTFT.
            WA_REP_TAB-LINE+V_LEN = 'FIELDTEXT'.
          WHEN OTHERS.
            WA_REP_TAB-LINE+V_LEN = 'FIELDNAME'.
        ENDCASE.
        APPEND WA_REP_TAB TO IT_REP_TAB.
        WA_REP_TAB-LINE     = '    TO'.
        V_LEN         = STRLEN( WA_REP_TAB-LINE ).
        WA_REP_TAB-LINE+V_LEN = ' WA_FIELD_TAB-'.
        V_LEN         = STRLEN( WA_REP_TAB-LINE ).
        WA_REP_TAB-LINE+V_LEN = WA_FIELD_STAT_TAB-FIELDNAME.
        V_LEN         = STRLEN( WA_REP_TAB-LINE ).
        WA_REP_TAB-LINE+V_LEN = '.'.
        APPEND WA_REP_TAB TO IT_REP_TAB.
      ENDLOOP.
      WA_REP_TAB-LINE    = ' APPEND WA_FIELD_TAB TO IT_FIELD_TAB.'.
      APPEND WA_REP_TAB TO IT_REP_TAB.

*-----*4-1-1.GET DOWNLOAD HEADER DATA FOR EVERY FILE TYPE
      PERFORM DOWN_HEAD_EDIT.

*-----*OPEN SQL DYNAMIC GENERATION
      LOOP AT IT_SQL_TAB INTO WA_SQL_TAB.
        MOVE WA_SQL_TAB TO WA_REP_TAB.
        APPEND WA_REP_TAB TO IT_REP_TAB.
      ENDLOOP.

*-----*4-1-2.GET DOWNLOAD DATA FOR EVERY FILE TYPE
      PERFORM DOWN_DATA_EDIT.

*-----*FUNCTION MODULE FOR DATA DOWNLOAD DYNAMIC GENERATION
      LOOP AT IT_FUN_TAB INTO WA_FUN_TAB.
        MOVE WA_FUN_TAB TO WA_REP_TAB.
        APPEND WA_REP_TAB TO IT_REP_TAB.
      ENDLOOP.

    WHEN P_UPLOAD.
      LOOP AT IT_FUN_TAB INTO WA_FUN_TAB.
        MOVE WA_FUN_TAB TO WA_REP_TAB.
        APPEND WA_REP_TAB TO IT_REP_TAB.
      ENDLOOP.
      LOOP AT IT_SQL_TAB INTO WA_SQL_TAB.
        MOVE WA_SQL_TAB TO WA_REP_TAB.
        APPEND WA_REP_TAB TO IT_REP_TAB.
      ENDLOOP.
  ENDCASE.
  WA_REP_TAB-LINE       = '  EXPORT V_SUBRC CTR_DATA TO MEMORY ID '''.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ).
  WA_REP_TAB-LINE+V_LEN   = V_MEMORY.
  V_LEN           = STRLEN( WA_REP_TAB-LINE ).
  WA_REP_TAB-LINE+V_LEN   = '''.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE       = '*'.
  APPEND WA_REP_TAB TO IT_REP_TAB.
  WA_REP_TAB-LINE       = 'ENDFORM.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  IF  P_UPLOAD = 'X'.

    WA_REP_TAB-LINE       = '*'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = 'FORM CONV_D_T USING F_D_T.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '*'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = 'DATA : WF_SUBRC.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '*'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WHILE WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    REPLACE '':'' WITH '''' INTO F_D_T.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    MOVE SY-SUBRC TO WF_SUBRC.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  ENDWHILE.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WHILE WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    REPLACE '';'' WITH '''' INTO F_D_T.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    MOVE SY-SUBRC TO WF_SUBRC.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  ENDWHILE.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WHILE WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    REPLACE ''.'' WITH '''' INTO F_D_T.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    MOVE SY-SUBRC TO WF_SUBRC.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  ENDWHILE.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WHILE WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    REPLACE ''-'' WITH '''' INTO F_D_T.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    MOVE SY-SUBRC TO WF_SUBRC.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  ENDWHILE.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  WHILE WF_SUBRC = 0.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    REPLACE ''/'' WITH '''' INTO F_D_T.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '    MOVE SY-SUBRC TO WF_SUBRC.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  ENDWHILE.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '  CONDENSE F_D_T NO-GAPS.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '*'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = 'ENDFORM.'.
    APPEND WA_REP_TAB TO IT_REP_TAB.
    WA_REP_TAB-LINE       = '*'.
    APPEND WA_REP_TAB TO IT_REP_TAB.

    SORT IT_FIELD_STAT_TAB BY CONVEXIT.
    CLEAR V_CONVEXIT.
    LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB
                      WHERE CONVEXIT = 'ALPHA'.

      IF  V_CONVEXIT EQ WA_FIELD_STAT_TAB-CONVEXIT.
        CONTINUE.
      ELSE.
        MOVE WA_FIELD_STAT_TAB-CONVEXIT TO V_CONVEXIT.
      ENDIF.
      V_ALINE         = 'CONVERSION_EXIT_'.
      V_LEN           = STRLEN( V_ALINE ).
      V_ALINE+V_LEN     = WA_FIELD_STAT_TAB-CONVEXIT.
      V_LEN           = STRLEN( V_ALINE ).
      V_ALINE+V_LEN     = '_INPUT'.
      WA_REP_TAB-LINE       = 'FORM'.
      V_LEN           = STRLEN( WA_REP_TAB-LINE ) + 1.
      WA_REP_TAB-LINE+V_LEN   = V_ALINE.
      V_LEN           = STRLEN( WA_REP_TAB-LINE ).
      WA_REP_TAB-LINE+V_LEN   = ' USING P_CONV_FIELD.'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE       = '*'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE       = '  CALL FUNCTION '''.
      V_LEN           = STRLEN( WA_REP_TAB-LINE ).
      WA_REP_TAB-LINE+V_LEN   = V_ALINE.
      V_LEN           = STRLEN( WA_REP_TAB-LINE ).
      WA_REP_TAB-LINE+V_LEN   = ''''.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE       = '    EXPORTING'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE       = '      INPUT         = P_CONV_FIELD'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE       = '    IMPORTING'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE       = '      OUTPUT        = P_CONV_FIELD.'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE       = '*'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE       = 'ENDFORM.'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
      WA_REP_TAB-LINE       = '*'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
    ENDLOOP.
    CLEAR SY-SUBRC.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWN_HEAD_EDIT
*&---------------------------------------------------------------------*
*       4-1-1.GET DOWNLOAD DATA FOR EVERY FILE TYPE
*----------------------------------------------------------------------*
FORM DOWN_HEAD_EDIT.

  WA_REP_TAB-LINE = 'LOOP AT IT_FIELD_TAB INTO WA_FIELD_TAB.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  WA_REP_TAB-LINE = '  CONCATENATE'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB.
    CLEAR WA_REP_TAB.
    WA_REP_TAB-LINE  = '      WA_FIELD_TAB-'.
    V_LEN         = STRLEN( WA_REP_TAB-LINE ).
    WA_REP_TAB-LINE+V_LEN  = WA_FIELD_STAT_TAB-FIELDNAME.
    APPEND WA_REP_TAB TO IT_REP_TAB.
  ENDLOOP.

  WA_REP_TAB-LINE = '    INTO WA_DOWNLOAD'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

*-----*BRANCH BY THE FILE FORMAT
  CASE 'X'.
    WHEN P_DAT.
      WA_REP_TAB-LINE = '    SEPARATED BY C_TAB.'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
    WHEN P_CSV.
      WA_REP_TAB-LINE = '    SEPARATED BY C_CSV.'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
  ENDCASE.

  WA_REP_TAB-LINE = '    APPEND WA_DOWNLOAD TO IT_DOWNLOAD.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  WA_REP_TAB-LINE = 'ENDLOOP.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOWN_DATA_EDIT
*&---------------------------------------------------------------------*
*       4-1-2.GET DOWNLOAD DATA FOR EVERY FILE TYPE
*----------------------------------------------------------------------*
FORM DOWN_DATA_EDIT.

  WA_REP_TAB-LINE = 'LOOP AT IT_SELECT_TAB INTO WA_SELECT_TAB.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  WA_REP_TAB-LINE
         = 'MOVE-CORRESPONDING WA_SELECT_TAB TO WA_CONCA_TAB.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  WA_REP_TAB-LINE = '  CONCATENATE'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  LOOP AT IT_FIELD_STAT_TAB INTO WA_FIELD_STAT_TAB.
    CLEAR WA_REP_TAB.
    WA_REP_TAB-LINE  = '      WA_CONCA_TAB-'.
    V_LEN         = STRLEN( WA_REP_TAB-LINE ).
    WA_REP_TAB-LINE+V_LEN  = WA_FIELD_STAT_TAB-FIELDNAME.
    APPEND WA_REP_TAB TO IT_REP_TAB.
  ENDLOOP.

  WA_REP_TAB-LINE = '    INTO WA_DOWNLOAD'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

*-----*BRANCH BY THE FILE FORMAT
  CASE 'X'.
    WHEN P_DAT.
      WA_REP_TAB-LINE = '    SEPARATED BY C_TAB.'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
    WHEN P_CSV.
      WA_REP_TAB-LINE = '    SEPARATED BY C_CSV.'.
      APPEND WA_REP_TAB TO IT_REP_TAB.
  ENDCASE.

  WA_REP_TAB-LINE = '    APPEND WA_DOWNLOAD TO IT_DOWNLOAD.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

  WA_REP_TAB-LINE = 'ENDLOOP.'.
  APPEND WA_REP_TAB TO IT_REP_TAB.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_SUBRC_SQL
*&---------------------------------------------------------------------*
*       ■1.SQL-SUBRC PROCESSING
*&---------------------------------------------------------------------*
*       →　P_ERR_NO  ERROR CODE
*&---------------------------------------------------------------------*
FORM SET_SUBRC_SQL USING P_ERR_NO.

  CASE P_ERR_NO.
    WHEN '1'.
      V_ALINE      = '  IF  SY-SUBRC <> 0 OR CTR_DATA = 0.'.
    WHEN '4'.
      V_ALINE      = '  IF  SY-SUBRC <> 0 AND SY-SUBRC <> 4.'.
    WHEN '10'.
      V_ALINE      = '  IF  SY-SUBRC <> 0.'.
    WHEN OTHERS.
      V_ALINE      = '  IF  SY-SUBRC <> 0.'.
  ENDCASE.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = '    MOVE '''.
  V_LEN        = STRLEN( V_ALINE ).
  V_ALINE+V_LEN  = P_ERR_NO.
  V_LEN        = STRLEN( V_ALINE ).
  V_ALINE+V_LEN  = ''' TO V_SUBRC.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = '    EXPORT V_SUBRC CTR_DATA TO MEMORY ID '''.
  V_LEN        = STRLEN( V_ALINE ).
  V_ALINE+V_LEN  = V_MEMORY.
  V_LEN        = STRLEN( V_ALINE ).
  V_ALINE+V_LEN  = '''.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = '    EXIT.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.
  V_ALINE      = '  ENDIF.'.
  WA_SQL_TAB-LINE    = V_ALINE. APPEND WA_SQL_TAB TO IT_SQL_TAB.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_SUBRC_FUN
*&---------------------------------------------------------------------*
*　     ■2.SQL-SUBRC PROCESSING
*&---------------------------------------------------------------------*
FORM SET_SUBRC_FUN USING P_ERR_NO.

  V_ALINE      = '  IF  SY-SUBRC <> 0.'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '    MOVE '''.
  V_LEN        = STRLEN( V_ALINE ).
  V_ALINE+V_LEN  = P_ERR_NO.
  V_LEN        = STRLEN( V_ALINE ).
  V_ALINE+V_LEN  = ''' TO V_SUBRC.'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '    EXPORT V_SUBRC CTR_DATA TO MEMORY ID '''.
  V_LEN        = STRLEN( V_ALINE ).
  V_ALINE+V_LEN  = V_MEMORY.
  V_LEN        = STRLEN( V_ALINE ).
  V_ALINE+V_LEN  = '''.'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '    EXIT.'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = '  ENDIF.'.
  WA_FUN_TAB-LINE    = V_ALINE. APPEND WA_FUN_TAB TO IT_FUN_TAB.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SERVER_UP_PROC
*&---------------------------------------------------------------------*
*
*&---------------------------------------------------------------------*
FORM SERVER_UP_PROC.

  V_ALINE      = 'OPEN DATASET V_PATH FOR INPUT IN TEXT MODE ENCODING DEFAULT.'.
*  V_ALINE      = 'OPEN DATASET V_PATH FOR INPUT IN TEXT MODE ENCODING non-Unicode.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.

* ■2.SQL-SUBRC PROCESSING
  PERFORM SET_SUBRC_FUN     USING '3'.

  V_ALINE      = 'CLEAR IT_DOWNLOAD.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'DO.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'READ DATASET V_PATH INTO WA_DOWNLOAD.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'IF SY-SUBRC <> 0.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'EXIT.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'ELSE.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'APPEND WA_DOWNLOAD TO IT_DOWNLOAD.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'CLEAR WA_DOWNLOAD.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'ENDIF.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'ENDDO.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.
  V_ALINE      = 'CLOSE DATASET V_PATH.'.
  WA_FUN_TAB-LINE    = V_ALINE.
  APPEND WA_FUN_TAB TO IT_FUN_TAB.

* ■2.SQL-SUBRC PROCESSING
  PERFORM SET_SUBRC_FUN     USING '3'.

ENDFORM.