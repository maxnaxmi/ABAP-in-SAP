*----------------------------------------------------------------------*
* PROGRAM ID  : ZPII_Z_SI_IT8053Z00201_R_01~Z_SI_IT8053Z00201_R_01
* NAME        : �{�������o�^ABAP�v���L�V(Inbound)
* AUTHOR      : RS ���
* DATE        : 2018/10/31
* DESCRIPTION : ACMS�T�[�o����API�o�R�ŗ\��̔��f�[�^����̂��A
*               �{���������ԃe�[�u���iZPTT0049_SUB�j�֎捞�ށB
*----------------------------------------------------------------------*
*& �ύX����
*& LOG#  DATE        AUTHOR     DESCRIPTION
*
*----------------------------------------------------------------------*
METHOD ZPII_Z_SI_IT8053Z00201_R_01~Z_SI_IT8053Z00201_R_01.

*----------------------------------------------------------------------*
* CONSTANTS��`
*----------------------------------------------------------------------*
  CONSTANTS:
    C_FLG_ON         TYPE FLAG VALUE 'X',                                  "�t���O��ON
    C_FLG_OFF        TYPE FLAG VALUE SPACE,                                "�t���O��OFF
    C_STATUS_ERR     TYPE ZCESTATUS VALUE 'E',                             "�����ُ�
    C_EOM(3)         TYPE C VALUE 'EOM',                                   "End-Of-File
    C_LENGTH_4       TYPE I VALUE 4,                        "�f�[�^�̒���4
    C_CPROG          TYPE ZCECPROG   VALUE 'IT8053Z00201',                 "PID
    C_CPROG_TXT      TYPE ZCECPROG_TXT VALUE '�{�������o�^(�\�񔭒�)I/F',  "TEXT
    C_JOBDATE        TYPE RVARI_VNAM VALUE 'Z_JOBDATE_DAY',                "�������擾�ϐ���
    C_KIKAKU_NO      TYPE CHAR1 VALUE '1',                                 "���No
    C_SAIYO_GENKA    TYPE CHAR1 VALUE '3',                                 "�̗p�����敪
    C_GENKA_KAKE     TYPE CHAR3 VALUE '000',                               "�����|��
    C_BIKUB          TYPE CHAR1 VALUE '1',                                 "�֋敪
    C_HANCD_YOYAK(5) TYPE C VALUE 'YOYAK',                                 "�ėp�R�[�h(���i�R�[�h�ϊ�)

* �����W�\���p�萔
    C_SIGN_I(1)   TYPE C VALUE 'I',                                       "�����W�e�[�u���i�^�j
    C_OPT_EQ(2)   TYPE C VALUE 'EQ'.                                      "�����W�e�[�u���i���l�j

*----------------------------------------------------------------------*
* TYPES��`
*----------------------------------------------------------------------*
  TYPES:
*   �v���L�V�\��
*   �v���L�V�\��(All)
    TYP_LINE_ALL TYPE ZPZ_DT_IT8053Z00201_R_01,
*   �v���L�V�\��(�f�[�^��)
    TYP_LINE     TYPE ZPZ_DT_IT8053Z00201_R_01_STRUC,
"   �t�@�C���o�͗p�\��
    TYP_FILEREC TYPE STRING.
*   ���ʃo���A���g�e�[�u���p(�W���u�������j
  TYPES: BEGIN OF TYP_TVARVC,
           LOW  TYPE TVARVC-LOW,          "����
           HIGH TYPE TVARVC-HIGH,         "����
         END OF TYP_TVARVC,

*   ����POS�}�X�^�e�[�u���\��
         BEGIN OF TYP_ZPMT0003,
           MATNR TYPE ZPMT0003-MATNR,     "SAP���i�R�[�h
           POSCD TYPE ZPMT0003-POSCD,     "POS�R�[�h
           APDAT TYPE ZPMT0003-APDAT,     "�K�p��
           BISMT TYPE ZPMT0003-BISMT,     "���i�R�[�h
           SPART TYPE ZPMT0003-SPART,     "����R�[�h
         END OF TYP_ZPMT0003,

*   �ėp�}�X�^�e�[�u���\��
         BEGIN OF TYP_ZPMT0030,
           KEY01 TYPE ZPMT0030-KEY01,     "����R�[�h
           KEY02 TYPE ZPMT0030-KEY02,     "���i�R�[�h
           KEY03 TYPE ZPMT0030-KEY03,     "�[�i��
           ITM01 TYPE ZPMT0030-ITM01,     "�ϊ����i�R�[�h
           ITM02 TYPE ZPMT0030-ITM02,     "�ϊ�SAP���i�R�[�h
         END OF TYP_ZPMT0030.

*----------------------------------------------------------------------*
* DATA��`�i�����e�[�u��/�����e�[�u���w�b�_�j
*----------------------------------------------------------------------*
  DATA:
*   �v���L�V�\��(ALL)
    TD_LINE_ALL  TYPE STANDARD TABLE OF TYP_LINE_ALL,
    TH_LINE_ALL  LIKE LINE OF TD_LINE_ALL,

*   �v���L�V�\��(�f�[�^��)
    TD_LINE TYPE STANDARD TABLE OF TYP_LINE,
    TH_LINE LIKE LINE OF TD_LINE,

*   I/F��M�f�[�^(���ԃe�[�u���t�H�[�}�b�g)
    TD_INDATA TYPE STANDARD TABLE OF ZPTT0049_SUB,
    TH_INDATA LIKE LINE OF TD_INDATA,

"   �G���[�t�@�C��(�\������)
    TD_EFILE TYPE STANDARD TABLE OF ZPTT0052,
    TH_EFILE LIKE LINE OF TD_EFILE,

"   �G���[���b�Z�[�W
    TD_EMESSAGE TYPE STANDARD TABLE OF LINE,
    TH_EMESSAGE LIKE LINE OF TD_EMESSAGE,

*   �t�@�C���o�͗p
    TD_FILEREC TYPE STANDARD TABLE OF LINE,
    TH_FILEREC LIKE LINE OF TD_FILEREC,

"   �o���A���g�e�[�u��
    TD_TVARVC TYPE STANDARD TABLE OF TYP_TVARVC,
    TH_TVARVC LIKE LINE OF TD_TVARVC,

"   ����POS�}�X�^
    TD_ZPMT0003 TYPE STANDARD TABLE OF TYP_ZPMT0003,
    TH_ZPMT0003 LIKE LINE OF TD_ZPMT0003,

"   �ėp�}�X�^
    TD_ZPMT0030 TYPE STANDARD TABLE OF TYP_ZPMT0030,
    TH_ZPMT0030 LIKE LINE OF TD_ZPMT0030,

"   POS�R�[�h�����W
    RD_POSCD TYPE RANGE OF ZPMT0003-POSCD,
    RH_POSCD LIKE LINE OF RD_POSCD.

*----------------------------------------------------------------------*
* DATA��`�i�\���j
*----------------------------------------------------------------------*
  DATA:
    ST_ZCOTIFLOG TYPE ZCOTIFLOG.              "IF�N�����O�e�[�u��

*----------------------------------------------------------------------*
* DATA��`�i���[�N�j
*----------------------------------------------------------------------*
  DATA:
    FLG_DUMMY   TYPE FLAG,                    "�_�~�[�ϐ�(�g�p���Ȃ�)
    FLG_ERR     TYPE FLAG VALUE C_FLG_OFF,    "�G���[�t���O
    FLG_LENGTH_ERR TYPE FLAG VALUE C_FLG_OFF, "���ڒ��G���[�t���O
    W_IFID      TYPE ZCEIFID,                 "�C���^�[�t�F�[�XID
    W_EOM(3)    TYPE C,                       "EOM
    W_IFCOUNT   TYPE ZCEIFCOUNT,              "�C���^�[�t�F�[�X�A��
    W_IFEOM     TYPE ZCEEOM,                  "EOM�g�p�ۃt���O
    W_SFNAME    TYPE STRING,                  "����t�@�C����
    W_EFNAME    TYPE STRING,                  "�G���[�t�@�C����
    W_ERRMSG    TYPE STRING,                  "�G���[���b�Z�[�W
    W_TABIX(6)  TYPE N,                       "�����e�[�u���̍���
    W_COUNT     TYPE I,                       "���R�[�h��

    W_SPART     TYPE ZPTT0049_SUB-SPART,      "����R�[�h
    W_BISMT     TYPE ZPMT0003-BISMT,          "���i�R�[�h
    W_MATNR     TYPE ZPMT0003-MATNR.          "SAP���i�R�[�h

*----------------------------------------------------------------------*
* �����J�n
*----------------------------------------------------------------------*
* IF�N�����O
  PERFORM ZCIO_IFLOG_START IN PROGRAM ZCIO0101
                  USING    C_CPROG C_CPROG_TXT
                  CHANGING ST_ZCOTIFLOG.

*----- �v���L�V�f�[�^�̎擾
  TH_LINE_ALL =  INPUT-Z_MT_IT8053Z00201_R_01.
  TD_LINE[]   =  INPUT-Z_MT_IT8053Z00201_R_01-STRUCT1[].    "#EC ENHOK

*----- I/FID��EOM�̎擾
  W_IFID = TH_LINE_ALL-IFID-IFID.       "I/FID
  W_EOM  = TH_LINE_ALL-EOM-EOM.         "EOM

*----- ����R�[�h�̎擾(�t�@�C�����̓�2����)
  W_SPART = TH_LINE_ALL-FILE-FILENAME(2).

*----- �����@I/FID�̃��b�N����
  PERFORM ZCIO_IFID_ENQUEUE IN PROGRAM ZCIO0101
                      USING W_IFID
                   CHANGING FLG_ERR
                            FLG_DUMMY
                            W_ERRMSG.
*----- �����AI/F�A�Ԃ̎擾����
  IF FLG_ERR = C_FLG_OFF.
    PERFORM ZCIO_GET_IFCOUNT IN PROGRAM ZCIO0101
                       USING W_IFID
                    CHANGING W_IFCOUNT
                             W_IFEOM
                             FLG_ERR
                             W_ERRMSG.
  ENDIF.

*----- �����Asub �t�@�C��������
  PERFORM ZCIO_MAKE_FAILENAME IN PROGRAM ZCIO0101
    USING    W_IFID
             W_IFCOUNT
    CHANGING W_SFNAME
             W_EFNAME.

*----- �����B�O�����G���[�`�F�b�N
  IF FLG_ERR = C_FLG_OFF.
    PERFORM ZCIO_CHECK_PREERR IN PROGRAM ZCIO0101
      USING    W_IFID
               W_EFNAME
      CHANGING FLG_ERR.
  ENDIF.

*----- �����C�W���u�������擾
  IF FLG_ERR = C_FLG_OFF.
    SELECT LOW                           "�����f�[�^
           HIGH                          "�擾�f�[�^
      INTO TABLE TD_TVARVC
      FROM TVARVC
     WHERE NAME  = C_JOBDATE.            "�o���A���g�ϐ���

*----- ���R�[�h���擾�ł��Ȃ������ꍇ
    IF SY-SUBRC <> 0 .
*----- �G���[�t���Oon
      FLG_ERR  = C_FLG_ON.
*----- �o���A���g�ϐ� &1 �� �o���A���g�ϐ��e�[�u��(TVARVC)�ɑ��݂��܂���
      MESSAGE E336(ZC01)
        WITH C_JOBDATE "Z_JOBDATE_DAY
        INTO W_ERRMSG.
    ELSE.
*----- �擾�ł����ꍇ�A�G���[���X�g�e�[�u����JOB�������Ƃ��Ďg�p����
      READ TABLE TD_TVARVC INDEX 1 INTO TH_TVARVC.
    ENDIF.
  ENDIF.

*----- �����D�f�[�^�ҏW����
  IF FLG_ERR = C_FLG_OFF.

*----- �����W�e�[�u����POS�R�[�h�i�[
    LOOP AT TD_LINE INTO TH_LINE.
      RH_POSCD-SIGN   = C_SIGN_I.
      RH_POSCD-OPTION = C_OPT_EQ.
      RH_POSCD-LOW    = TH_LINE-POSCD.
      APPEND RH_POSCD TO RD_POSCD.
    ENDLOOP.

*----- ����POS�}�X�^�擾
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

*----- �ėp�}�X�^�擾�i���i�R�[�h�ϊ��j
    SELECT KEY01
           KEY02
           KEY03
           ITM01
           ITM02
      FROM ZPMT0030
        INTO TABLE TD_ZPMT0030
      WHERE HANCD = C_HANCD_YOYAK
        AND KEY01 = W_SPART.

*----- �v���L�V�\���𒆊ԃe�[�u���ɕϊ�
    LOOP AT TD_LINE INTO TH_LINE.
      W_TABIX = SY-TABIX.
      CLEAR : TH_INDATA,
              TH_ZPMT0003,
              W_BISMT,
              W_MATNR.

*----- ���i�R�[�h���蓖��
*      �[�i���ɒ��߂̓K�p���̃��R�[�h���̗p
      LOOP AT TD_ZPMT0003 INTO TH_ZPMT0003
        WHERE POSCD =  TH_LINE-POSCD
          AND APDAT <= TH_LINE-FDDAT.
      ENDLOOP.

      "�Ώۂ̃��R�[�h�����݂��Ȃ��ꍇ�G���[
      IF SY-SUBRC <> 0.
        MESSAGE E399(ZC01) WITH TEXT-M02 TH_LINE-POSCD SPACE TEXT-M03
          INTO TH_EMESSAGE-LINE.
        APPEND TH_EMESSAGE TO TD_EMESSAGE.

      ELSE.
        W_BISMT = TH_ZPMT0003-BISMT.        "���i�R�[�h
        W_MATNR = TH_ZPMT0003-MATNR.        "SAP���i�R�[�h

*----- ���i�R�[�h�ϊ��i�[�i���ݒ�j
*      �ėp�}�X�^�ɑ��݂��鏤�i�R�[�h�̏ꍇ�A�[�i�����Ƃɏ��i�R�[�h��ϊ�����
        READ TABLE TD_ZPMT0030 INTO TH_ZPMT0030
        WITH KEY KEY02 = TH_ZPMT0003-BISMT.

        IF SY-SUBRC = 0.
          READ TABLE TD_ZPMT0030 INTO TH_ZPMT0030
          WITH KEY KEY02 = TH_ZPMT0003-BISMT
                   KEY03 = TH_LINE-FDDAT.

          "�Ώۂ̃��R�[�h�����݂��Ȃ��ꍇ�G���[
          IF SY-SUBRC <> 0.
            MESSAGE E399(ZC01) WITH TEXT-M05 TH_ZPMT0003-BISMT SPACE TEXT-M06
              INTO TH_EMESSAGE-LINE.
            APPEND TH_EMESSAGE TO TD_EMESSAGE.
          ELSE.
            W_BISMT = TH_ZPMT0030-ITM01.   "���i�R�[�h
            W_MATNR = TH_ZPMT0030-ITM02.   "SAP���i�R�[�h
          ENDIF.
        ENDIF.
      ENDIF.


*----- ��������(HMNGE)�̃f�[�^���`�F�b�N
      PERFORM ZCIO_CHECK_FIELD_LENGTH IN PROGRAM ZCIO0101
        USING TH_LINE-HMNGE
              TEXT-M04
              C_LENGTH_4
     CHANGING FLG_LENGTH_ERR
              W_ERRMSG.
      IF FLG_LENGTH_ERR = C_FLG_ON.
        "�G���[�̏ꍇ�̓��b�Z�[�W��ۑ�����
        TH_EMESSAGE-LINE = W_ERRMSG.
        APPEND TH_EMESSAGE TO TD_EMESSAGE.
      ENDIF.


      TH_INDATA-MANDT           = SY-MANDT.                 "�N���C�A���g
      TH_INDATA-ZIFID           = W_IFID.                   "I/FID
      TH_INDATA-ZIFCOUNT        = W_IFCOUNT.                "I/F�A��
*----- ���j�[�N�L�[NO�擾����
      CONCATENATE W_IFCOUNT               "IF�A��
                  W_TABIX                 "�����e�[�u���̍���
             INTO TH_INDATA-Z_UNIQUE_KEY_NO.                "���j�[�N�L�[NO
      TH_INDATA-SPART           = W_SPART.                  "����R�[�h
      TH_INDATA-FDDAT           = TH_LINE-FDDAT.
      TH_INDATA-BISMT           = W_BISMT.                  "���i�R�[�h
      TH_INDATA-WERKS           = TH_LINE-WERKS.            "�X�܃R�[�h
      TH_INDATA-DAET1           = TH_TVARVC-LOW.            "������
      TH_INDATA-FADAT           = TH_TVARVC-LOW.            "������
      TH_INDATA-HATTYU_KENMEI   = TEXT-M01.                 "���������u�N���\�񔭒��v
      TH_INDATA-KIKAKU_NO       = C_KIKAKU_NO.              "���No�u1�v
      TH_INDATA-HMNGE           = TH_LINE-HMNGE.            "��������
      TH_INDATA-SAIYO_GENKA_KBN = C_SAIYO_GENKA.            "�̗p�����敪�u3�v
      TH_INDATA-GENKA_KAKERITU  = C_GENKA_KAKE.             "�����|���u000�v
      TH_INDATA-BIKUB           = C_BIKUB.                  "�֋敪�u1�v
      TH_INDATA-FLD_YOBI01      = W_MATNR.                  "���ڗ\��01(SAP���i�R�[�h)

*----- �G���[������ꍇ�A�����X�e�[�^�X��'E'�Ƃ���
      IF TD_EMESSAGE IS NOT INITIAL.
        TH_INDATA-ZIFSTATUS     = C_STATUS_ERR.             "�����X�e�[�^�X
        TH_INDATA-ZIFMESSAGE    = TH_EMESSAGE-LINE.         "���b�Z�[�W
      ENDIF.

      TH_INDATA-ZCRDAT          = SY-DATUM.                 "�o�^���t
      TH_INDATA-ZCRTIM          = SY-UZEIT.                 "�o�^����
      TH_INDATA-ZCRUSR          = SY-UNAME.                 "�o�^��
      TH_INDATA-ZUPDAT          = SY-DATUM.                 "�X�V���t
      TH_INDATA-ZUPTIM          = SY-UZEIT.                 "�X�V����
      TH_INDATA-ZUPUSR          = SY-UNAME.                 "�X�V��

      APPEND TH_INDATA TO TD_INDATA.

      LOOP AT TD_EMESSAGE INTO TH_EMESSAGE.
        TH_EFILE-ZIFID              "I/FID
        = W_IFID.
        TH_EFILE-ZIFCOUNT           "I/F�A��
        = TH_INDATA-ZIFCOUNT.
        TH_EFILE-Z_UNIQUE_KEY_NO    "���j�[�N�L�[NO
        = TH_INDATA-Z_UNIQUE_KEY_NO.
        TH_EFILE-ERROR_SEQ_NO       "�G���[�V�[�P���XNO
        = SY-TABIX.
        TH_EFILE-JOBDATE            "JOB������
        = TH_TVARVC-LOW.
        TH_EFILE-SPART_EOS          "EOS����R�[�h
        = SPACE.
        TH_EFILE-SPART              "����R�[�h
        = W_SPART.
        TH_EFILE-HATTYU_SHUBETU     "�������
        = SPACE.
        TH_EFILE-FDDAT              "�[�i��
        = TH_INDATA-FDDAT.
        TH_EFILE-BISMT              "���i�R�[�h
        = TH_INDATA-BISMT.
        TH_EFILE-MAKTX              "���i��(����)
        = SPACE.
        TH_EFILE-SHIKB              "���ʃR�[�h
        = SPACE.
        TH_EFILE-WERKS              "�X�܃R�[�h
        = TH_INDATA-WERKS.
        TH_EFILE-TRHCD              "�����R�[�h
        = SPACE.
        TH_EFILE-LIFAX              "����於(�J�i)
        = SPACE.
        TH_EFILE-BENUM              "�����P�ʓ���
        = SPACE.
        TH_EFILE-MENUM              "�׎p����
        = SPACE.
        TH_EFILE-ORCOD              "�����T�C�N���R�[�h
        = SPACE.
        TH_EFILE-KIKAKU_NO          "���NO
        = TH_INDATA-KIKAKU_NO.
        TH_EFILE-HMNGE              "��������
        = TH_INDATA-HMNGE.
        TH_EFILE-LOSFX              "����
        = SPACE.
        TH_EFILE-VKPNE              "����
        = SPACE.
        TH_EFILE-ZIFSTATUS          "�X�e�[�^�X
        = C_STATUS_ERR.
        TH_EFILE-ZIFMESSAGE         "���b�Z�[�W
        = TH_EMESSAGE-LINE.
        TH_EFILE-ZCRDAT             "�o�^���t
        = SY-DATUM.
        TH_EFILE-ZCRTIM             "�o�^����
        = SY-UZEIT.
        TH_EFILE-ZCRUSR             "�o�^��
        = SY-UNAME.
        TH_EFILE-ZUPDAT             "�X�V���t
        = SY-DATUM.
        TH_EFILE-ZUPTIM             "�X�V����
        = SY-UZEIT.
        TH_EFILE-ZUPUSR             "�X�V��
        = SY-UNAME.
        APPEND TH_EFILE TO TD_EFILE.
      ENDLOOP.
*----- �G���[���b�Z�[�W�e�[�u��������
      CLEAR TH_EMESSAGE.
      CLEAR TD_EMESSAGE.

    ENDLOOP.
  ENDIF.

  IF FLG_ERR = C_FLG_OFF.
*----- ����(9)�G���[���R�[�h�o�^
    INSERT ZPTT0052 FROM TABLE TD_EFILE.
*----- �G���[����
    IF SY-SUBRC <> 0.
*----- �G���[�t���Oon
      FLG_ERR = C_FLG_ON.
*----- �G���[���b�Z�[�W����
*----- �e�[�u�� &1 �� &2 �Ɏ��s���܂���(RC=:&3)
      MESSAGE E306(ZC01)
        WITH TEXT-M01 "�����f�[�^�G���[���X�g�e�[�u��(ZPTT0052)
             TEXT-M03 "�o�^
             SY-SUBRC "���^�[���R�[�h
        INTO W_ERRMSG.
    ENDIF.
  ENDIF.

*----- �����E���ԃe�[�u���X�V����
  IF FLG_ERR = C_FLG_OFF.
    MODIFY ZPTT0049_SUB FROM TABLE TD_INDATA.
*   �G���[����
    IF SY-SUBRC <> 0.
*     �G���[�t���Oon
      FLG_ERR = C_FLG_ON.
*     �G���[���b�Z�[�W����
*     �e�[�u�� &1 �� &2 �Ɏ��s���܂���(RC=:&3)
      MESSAGE E306(ZC01)
        WITH TEXT-M08 "�ڊǊm��f�[�^���ԃe�[�u��(ZPTT0049_SUB)
             TEXT-M09 "�X�V
             SY-SUBRC "���^�[���R�[�h
        INTO W_ERRMSG.
*----- ���[���o�b�N
      ROLLBACK WORK.

*   ���폈��
    ELSE.
*     �R�~�b�g
      COMMIT WORK.
*     ����t�@�C���쐬
      PERFORM ZCIO_MAKE_SFILE IN PROGRAM ZCIO0101
        USING W_IFID
              W_SFNAME.
    ENDIF.
  ENDIF.

*----- �����F�G���[����
  IF FLG_ERR = C_FLG_ON.

*   �t�@�C�����x���̃G���[���b�Z�[�W�ǋL
    IF NOT W_ERRMSG IS INITIAL.
      APPEND W_ERRMSG TO TD_FILEREC.
    ENDIF.

*   �G���[�t�@�C���쐬
    PERFORM ZCIO_MAKE_EFILE IN PROGRAM ZCIO0101
                     TABLES TD_FILEREC
                      USING W_IFID
                            W_EFNAME.

*----- �����F(2)����t�@�C���폜����
    PERFORM ZCIO_DELETE_SFILE IN PROGRAM ZCIO0101
                        USING W_IFID
                              W_SFNAME.

*----- �����F(3)���탌�R�[�h�폜(����TBL)����
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

*----- �����GEOM����
  IF W_IFEOM = C_FLG_OFF  "EOM�Ώ�IF�ł͂Ȃ��ꍇ
  OR W_EOM = C_EOM.       "�܂��́AEOM�ΏۂŁAEOM�����M����Ă����ꍇ
*----- �����G(1)����t�@�C�����l�[������
    PERFORM ZCIO_REN_FILE_NAME IN PROGRAM ZCIO0101
                         USING W_IFID
                               W_SFNAME.
*----- �����G(2)�ُ�t�@�C�����l�[������
    PERFORM ZCIO_REN_FILE_NAME IN PROGRAM ZCIO0101
                         USING W_IFID
                               W_EFNAME.
*----- �����G(3)IF�A�ԃC���N�������g����
    PERFORM ZCIO_SET_IFCOUNT IN PROGRAM ZCIO0101
                       USING W_IFID.
  ENDIF.

*----- ���H���b�N��������
  PERFORM ZCIO_IFID_DEQUEUE IN PROGRAM ZCIO0101
                      USING W_IFID.

* IF�I�����O
  PERFORM ZCIO_IFLOG_END IN PROGRAM ZCIO0101
    CHANGING ST_ZCOTIFLOG.

ENDMETHOD.