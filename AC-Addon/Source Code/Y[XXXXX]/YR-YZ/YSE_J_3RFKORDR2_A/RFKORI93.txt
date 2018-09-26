***INCLUDE RFKORI93 .

*NCLUDE RFKORI00.

*=======================================================================
*       Interne Perform-Routinen
*=======================================================================

*----------------------------------------------------------------------*
* FORM  MESSAGE_APPEND
*----------------------------------------------------------------------*
FORM message_append.

  CALL FUNCTION 'FI_MESSAGE_COLLECT'
    EXPORTING
      i_fimsg       = fimsg
      i_xappn       = 'X'
    EXCEPTIONS
      msgid_missing = 01
      msgno_missing = 02
      msgty_missing = 03.

  xnach = 'X'.
ENDFORM.                    "MESSAGE_APPEND

*----------------------------------------------------------------------*
* FORM  MESSAGE_COLLECT
*----------------------------------------------------------------------*
FORM message_collect.

  CALL FUNCTION 'FI_MESSAGE_COLLECT'
    EXPORTING
      i_fimsg       = fimsg
      i_xappn       = ' '
    EXCEPTIONS
      msgid_missing = 01
      msgno_missing = 02
      msgty_missing = 03.

  xnach = 'X'.
ENDFORM.                    "MESSAGE_COLLECT

*----------------------------------------------------------------------*
* FORM  MESSAGE_CHECK
*----------------------------------------------------------------------*
FORM message_check.

  CALL FUNCTION 'FI_MESSAGE_CHECK'
    EXCEPTIONS
      no_message = 01.

ENDFORM.                    "MESSAGE_CHECK

*----------------------------------------------------------------------*
* FORM  MESSAGE_ELEMENT
*----------------------------------------------------------------------*
FORM message_element.
  CLEAR fimsg.
  fimsg-msort = countm. fimsg-msgid = 'FB'.
  fimsg-msgty = 'I'.
  fimsg-msgno = '287'.
  fimsg-msgv1 = save_form.
  fimsg-msgv2 = window.
  fimsg-msgv3 = ereignis.
  PERFORM message_collect.
ENDFORM.                    "MESSAGE_ELEMENT

*----------------------------------------------------------------------*
* FORM  MESSAGE_GET
*----------------------------------------------------------------------*
FORM message_get.

  CALL FUNCTION 'FI_MESSAGE_GET'
    TABLES
      t_fimsg    = hfimsg
    EXCEPTIONS
      no_message = 01.

ENDFORM.                    "MESSAGE_GET

*----------------------------------------------------------------------*
* FORM  MESSAGE_GET_MSORT
*----------------------------------------------------------------------*
FORM message_get_msort.

  CALL FUNCTION 'FI_MESSAGE_GET_MSORT'
    IMPORTING
      e_xinit    = xinit
    TABLES
      s_fimsg    = msort_tab
    EXCEPTIONS
      no_message = 01.

ENDFORM.                    "MESSAGE_GET_MSORT

*----------------------------------------------------------------------*
* FORM  MESSAGE_INIT
*----------------------------------------------------------------------*
FORM  message_init.

  CALL FUNCTION 'FI_MESSAGE_INIT'.

ENDFORM.                    "MESSAGE_INIT

*----------------------------------------------------------------------*
* FORM MESSAGE_NO_SELECTION
*----------------------------------------------------------------------*
FORM message_no_selection.
  PERFORM message_check.
  IF sy-subrc = 0.
    CLEAR buklines.
    DESCRIBE TABLE hbukrs LINES buklines.
    IF buklines = '1'.
      IF hbukrs-high IS INITIAL.
        mbukrs = hbukrs-low.
      ENDIF.
    ENDIF.
    PERFORM message_print.
  ELSE.
    CLEAR buklines.
    DESCRIBE TABLE hbukrs LINES buklines.
    IF buklines = '1'.
      IF hbukrs-high IS INITIAL.
        IF save_event IS INITIAL.
          MESSAGE s474 WITH save_repid hbukrs-low.
        ELSE.
          MESSAGE s475 WITH save_repid hbukrs-low save_event.
        ENDIF.
      ELSE.
        IF save_event IS INITIAL.
          MESSAGE s472 WITH save_repid.
        ELSE.
          MESSAGE s473 WITH save_repid save_event.
        ENDIF.
      ENDIF.
    ELSE.
      IF save_event IS INITIAL.
        MESSAGE s472 WITH save_repid.
      ELSE.
        MESSAGE s473 WITH save_repid save_event.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "MESSAGE_NO_SELECTION

*----------------------------------------------------------------------*
* FORM  MESSAGE_OUTPUT
*----------------------------------------------------------------------*
FORM  message_output.
  IF NOT xnach IS INITIAL.
    CLEAR hbkormkey.
    CLEAR herdata.
    CLEAR hkokon.
    hbkormkey-bukrs = hdbukrs.
    hbkormkey-koart = hdkoart.
    hbkormkey-konto = hdkonto.
    hbkormkey-belnr = dabelnr.
    hbkormkey-gjahr = dagjahr.
    CONDENSE hbkormkey.
    herdata-usnam = hdusnam.
    herdata-datum = hddatum.
    herdata-uzeit = hduzeit.
    IF NOT hdkoar2 IS INITIAL
    OR NOT hdkont2 IS INITIAL.
      hkokon-koart = hdkoar2.
      hkokon-konto = hdkont2.
    ENDIF.
    IF xkausg IS INITIAL.
      IF NOT hdkoar2 IS INITIAL
      OR NOT hdkont2 IS INITIAL.
        CLEAR fimsg.
        fimsg-msort = '    '. fimsg-msgid = 'FB'.
        fimsg-msgty = 'I'.
        fimsg-msgno = '826'.
        fimsg-msgv1 = '          '.
        fimsg-msgv2 = hbkormkey.
        fimsg-msgv3 = herdata.
        fimsg-msgv4 = hkokon.
        PERFORM message_append.
      ELSE.
        CLEAR fimsg.
        fimsg-msort = '    '. fimsg-msgid = 'FB'.
        fimsg-msgty = 'I'.
        fimsg-msgno = '828'.
        fimsg-msgv1 = '          '.
        fimsg-msgv2 = hbkormkey.
        fimsg-msgv3 = herdata.
        PERFORM message_append.
      ENDIF.
    ELSE.
      IF NOT hdkoar2 IS INITIAL
      OR NOT hdkont2 IS INITIAL.
        CLEAR fimsg.
        fimsg-msort = '    '. fimsg-msgid = 'FB'.
        fimsg-msgty = 'I'.
        fimsg-msgno = '827'.
        fimsg-msgv1 = '          '.
        fimsg-msgv2 = hbkormkey.
        fimsg-msgv3 = herdata.
        fimsg-msgv4 = hkokon.
        PERFORM message_append.
      ELSE.
        CLEAR fimsg.
        fimsg-msort = '    '. fimsg-msgid = 'FB'.
        fimsg-msgty = 'I'.
        fimsg-msgno = '548'.
        fimsg-msgv1 = '          '.
        fimsg-msgv2 = hbkormkey.
        fimsg-msgv3 = herdata.
        PERFORM message_append.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "MESSAGE_OUTPUT

*----------------------------------------------------------------------*
* FORM  MESSAGE_PRINT
*----------------------------------------------------------------------*
FORM message_print.

  CLEAR xkausg.
  CLEAR prolistn.
  prolistn      = save_event.
  IF NOT mbukrs IS INITIAL.
    prolistn+6(4) = mbukrs.
  ENDIF.

*  IF print IS INITIAL
*  OR NOT xonli IS INITIAL.
*    NEW-PAGE
*      LINE-SIZE               132.
*  ELSE.
*    IF sy-batch IS INITIAL.
*      IF save_prdest IS INITIAL.
*        SELECT SINGLE * FROM usr01
*          WHERE bname = sy-uname.
*        IF sy-subrc = 0.
*          save_prdest = usr01-spld.
*        ENDIF.
*      ENDIF.
*      IF NOT save_prdest IS INITIAL.
*        NEW-PAGE
*          PRINT ON
*          LINE-SIZE               132
*          DESTINATION             save_prdest
*          LIST NAME               prolistn
*          IMMEDIATELY             ' '
*          NEW LIST IDENTIFICATION 'X'
*          KEEP IN SPOOL           'X'
*          LIST DATASET            'F140ER'
*          SAP COVER PAGE          pri_params-prsap
*          RECEIVER                pri_params-prrec
*          DEPARTMENT              pri_params-prabt
*          NO DIALOG.
*      ELSE.
*        NEW-PAGE
*          PRINT ON
*          LINE-SIZE               132
*          LIST NAME               prolistn
*          IMMEDIATELY             ' '
*          NEW LIST IDENTIFICATION 'X'
*          KEEP IN SPOOL           'X'
*          SAP COVER PAGE          pri_params-prsap
*          RECEIVER                pri_params-prrec
*          DEPARTMENT              pri_params-prabt
*          LIST DATASET            'F140ER'.
*      ENDIF.
*    ELSE.
*      IF save_prdest IS INITIAL.
*        IF NOT syst-pdest  IS INITIAL.
*          save_prdest = syst-pdest.
*        ELSE.
*          IF NOT save_pdest IS INITIAL.
*            save_prdest = save_pdest.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*      IF NOT save_prdest IS INITIAL.
*        NEW-PAGE
*          PRINT ON
*          LINE-SIZE               132
*          DESTINATION             save_prdest
*          LIST NAME               prolistn
*          IMMEDIATELY             ' '
*          NEW LIST IDENTIFICATION 'X'
*          KEEP IN SPOOL           'X'
*          LIST DATASET            'F140ER'
*          SAP COVER PAGE          pri_params-prsap
*          RECEIVER                pri_params-prrec
*          DEPARTMENT              pri_params-prabt
*          NO DIALOG.
*      ELSE.
**       MESSAGE i829.
*        CLEAR fimsg.
*        fimsg-msort = '    '. fimsg-msgid = 'FB'.
*        fimsg-msgty = 'I'.
*        fimsg-msgno = '829'.
*        PERFORM message_append.
*
*        CALL FUNCTION 'FI_MESSAGE_PROTOCOL'
**            EXPORTING
**                 TYPE_S_ONLY = 'X'
*             EXCEPTIONS
**                 NO_MESSAGE  = 1
**                 NOT_BATCH   = 2
*                  OTHERS      = 3
*                  .
*        IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.
*        xkausg = 'X'.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IF xkausg IS INITIAL.
*    IF mbukrs IS INITIAL.
*      MOVE: '    '   TO bhdgd-bukrs.
*    ELSE.
*      MOVE: mbukrs   TO bhdgd-bukrs.
*    ENDIF.
*    MOVE: '0'      TO bhdgd-inifl,
*          sy-linsz TO bhdgd-lines,     "Zeilenbreite der Liste aus Rep.
*          '132'    TO bhdgd-lines,     "Zeilenbreite der Liste aus Rep.
*          sy-uname TO bhdgd-uname,     "Benutzername
*          sy-repid TO bhdgd-repid,     "Name des ABAP-Programmes
*          sy-title TO bhdgd-line1,     "Titel des ABAP-Programmes
*          text-205 TO bhdgd-line2,     "Fehlerliste
**         MI-FICHE TO BHDGD-MIFFL,     "Mikrofiche-Zeile
*          space    TO bhdgd-separ,     "keine Listseparation
*          'BUKRS'  TO bhdgd-domai.
*
*    CLEAR t048t-ltext.
*    IF NOT save_event IS INITIAL.
*      SELECT SINGLE * FROM t048t
*        WHERE spras = sy-langu
*        AND   event = save_event.
*      IF sy-subrc NE 0.
*        t048t-ltext = text-203.
*      ENDIF.
*    ELSE.
*      IF NOT save_repid IS INITIAL.
*        t048t-ltext = text-204.
*      ENDIF.
*    ENDIF.
*    IF NOT t048t-ltext IS INITIAL.
*      WRITE: / t048t-ltext.
*      ULINE.
*    ENDIF.
*
*    CALL FUNCTION 'FI_MESSAGE_PRINT'
*      EXPORTING
*        i_msort = ' '
*        i_xausn = 'X'
*        i_xeaus = ' '
*        i_xskip = ' '.
*
*    NEW-PAGE
*      PRINT OFF.

  PERFORM output_error.

  IF  NOT sy-spono IS INITIAL.
    CLEAR prot_ausgabe.
    IF mbukrs IS INITIAL.
      prot_ausgabe-bukrs     = '    '.
    ELSE.
      prot_ausgabe-bukrs     = mbukrs.
    ENDIF.
    prot_ausgabe-event     = save_event.
    prot_ausgabe-repid     = save_repid.
    prot_ausgabe-tdspoolid = sy-spono.
    prot_ausgabe-tddevice  = sy-pdest.
    prot_ausgabe-tdpreview = ' '.
    prot_ausgabe-tddataset = sy-prdsn.
    prot_ausgabe-tdsuffix1 = sy-pdest.
    prot_ausgabe-tdsuffix2 = sy-plist.
    prot_ausgabe-countp    = 0.
    COLLECT prot_ausgabe.
  ENDIF.
*  ENDIF.
ENDFORM.                    "MESSAGE_PRINT

*-------------------- -------------------------------------------------*
* FORM  MESSAGE_PRINT_MSORT
*----------------------------------------------------------------------*
FORM message_print_msort.

  CALL FUNCTION 'FI_MESSAGE_PRINT'
    EXPORTING
      i_msort = hmsort
      i_xausn = 'X'
      i_xeaus = ' '
      i_xskip = ' '.

ENDFORM.                    "MESSAGE_PRINT_MSORT

*----------------------------------------------------------------------*
* FORM  MESSAGE_SET
*----------------------------------------------------------------------*
FORM message_set.

  CALL FUNCTION 'FI_MESSAGE_SET'
    TABLES
      t_fimsg    = hfimsg
    EXCEPTIONS
      no_message = 01.

ENDFORM.                    "MESSAGE_SET

*----------------------------------------------------------------------*
* FORM  MESSAGE_SORT
*----------------------------------------------------------------------*
FORM message_sort.

  CALL FUNCTION 'FI_MESSAGE_SORT'
    EXCEPTIONS
      no_message = 01.

ENDFORM.                    "MESSAGE_SORT

*----------------------------------------------------------------------*
* FORM  MESSAGE_WINDOW
*----------------------------------------------------------------------*
FORM message_window.
  CLEAR fimsg.
  fimsg-msort = countm. fimsg-msgid = 'FB'.
  fimsg-msgty = 'I'.
  fimsg-msgno = '286'.
  fimsg-msgv1 = save_form.
  fimsg-msgv2 = window.
  PERFORM message_collect.
ENDFORM.                    "MESSAGE_WINDOW

*&---------------------------------------------------------------------*
*&      Form  message_pdf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM message_pdf.
  DATA: ld_error_string TYPE string.
  CALL FUNCTION 'FP_GET_LAST_ADS_ERRSTR'
    IMPORTING
      e_adserrstr = ld_error_string.
  CLEAR fimsg.
  fimsg-msort = space. fimsg-msgid = 'FB'.
  fimsg-msgty = 'I'.
  fimsg-msgno = '874'.
  fimsg-msgv1 = ld_error_string.
  PERFORM message_collect.
ENDFORM.                    "message_pdf

*----------------------------------------------------------------------*
* FORM PRI_PARAM_GET
*----------------------------------------------------------------------*
FORM pri_param_get.
  CLEAR pri_params.
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      mode           = 'CURRENT'
      no_dialog      = 'X'
    IMPORTING
      out_parameters = pri_params
    EXCEPTIONS
      OTHERS         = 4.
ENDFORM.                    "PRI_PARAM_GET

*----------------------------------------------------------------------*
* FORM PRI_PARAM_EXPORT
*----------------------------------------------------------------------*
FORM pri_param_export.
  memokey(8)    = 'F140PRIP'.
  memokey+8(12) = sy-uname.
  EXPORT pri_params TO MEMORY ID memokey.
ENDFORM.                    "PRI_PARAM_EXPORT

*----------------------------------------------------------------------*
* FORM  PRI_PARAM_IMPORT
*----------------------------------------------------------------------*
FORM pri_param_import.
  CLEAR pri_params.
  memokey(8)    = 'F140PRIP'.
  memokey+8(12) = sy-uname.
  IMPORT pri_params FROM MEMORY ID memokey.
ENDFORM.                    "PRI_PARAM_IMPORT

*----------------------------------------------------------------------*
* FORM PROT_EXPORT.
*----------------------------------------------------------------------*
FORM prot_export.
  memokey(8)    = 'F140PROT'.
  memokey+8(12) = sy-uname.
  EXPORT prot_ausgabe TO MEMORY ID memokey.
ENDFORM.                    "PROT_EXPORT

*----------------------------------------------------------------------*
* FORM  PROT_IMPORT
*----------------------------------------------------------------------*
FORM prot_import.
  memokey(8)    = 'F140PROT'.
  memokey+8(12) = sy-uname.
  IMPORT prot_ausgabe FROM MEMORY ID memokey.
ENDFORM.                    "PROT_IMPORT

*----------------------------------------------------------------------*
* FORM PROT_PRINT
*----------------------------------------------------------------------*
FORM prot_print.
  CLEAR xkausg.
  CLEAR prolistn.
  prolistn(8)   = sy-datum.
  prolistn+8(4) = save_proid.
  IF print IS INITIAL
  OR NOT xonli IS INITIAL.
    NEW-PAGE
      LINE-SIZE               132.
  ELSE.
    IF sy-batch IS INITIAL.
      IF save_prdest IS INITIAL.
        SELECT SINGLE * FROM usr01
          WHERE bname = sy-uname.
        IF sy-subrc = 0.
          save_prdest = usr01-spld.
        ENDIF.
      ENDIF.
      IF NOT save_prdest IS INITIAL.
        NEW-PAGE
          PRINT ON
          LINE-SIZE               132
          DESTINATION             save_prdest
          LIST NAME               prolistn
          IMMEDIATELY             save_rimmd_prot
          NEW LIST IDENTIFICATION 'X'
          KEEP IN SPOOL           'X'
          LIST DATASET            'F140'
          SAP COVER PAGE          pri_params-prsap
          RECEIVER                pri_params-prrec
          DEPARTMENT              pri_params-prabt
          NO DIALOG.
      ELSE.
        NEW-PAGE
          PRINT ON
          LINE-SIZE               132
          LIST NAME               prolistn
          IMMEDIATELY             save_rimmd_prot
          NEW LIST IDENTIFICATION 'X'
          KEEP IN SPOOL           'X'
          SAP COVER PAGE          pri_params-prsap
          RECEIVER                pri_params-prrec
          DEPARTMENT              pri_params-prabt
          LIST DATASET            'F140'.
      ENDIF.
    ELSE.
      IF save_prdest IS INITIAL.
        IF NOT syst-pdest  IS INITIAL.
          save_prdest = syst-pdest.
        ELSE.
          IF NOT save_pdest IS INITIAL.
            save_prdest = save_pdest.
          ENDIF.
        ENDIF.
      ENDIF.
      IF NOT save_prdest IS INITIAL.
        NEW-PAGE
          PRINT ON
          LINE-SIZE               132
          DESTINATION             save_prdest
          LIST NAME               prolistn
          IMMEDIATELY             save_rimmd_prot
          NEW LIST IDENTIFICATION 'X'
          KEEP IN SPOOL           'X'
          LIST DATASET            'F140'
          SAP COVER PAGE          pri_params-prsap
          RECEIVER                pri_params-prrec
          DEPARTMENT              pri_params-prabt
          NO DIALOG.
      ELSE.
        MESSAGE i544.
        xkausg = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF xkausg IS INITIAL.
    MOVE: '    '   TO bhdgd-bukrs,
          '0'      TO bhdgd-inifl,
          sy-linsz TO bhdgd-lines,     "Zeilenbreite der Liste aus Rep.
          '132'    TO bhdgd-lines,     "Zeilenbreite der Liste aus Rep.
          sy-uname TO bhdgd-uname,     "Benutzername
          sy-repid TO bhdgd-repid,     "Name des ABAP-Programmes
          sy-title TO bhdgd-line1,     "Titel des ABAP-Programmes
          text-200 TO bhdgd-line2,     "Protokoll
*         MI-FICHE TO BHDGD-MIFFL,     "Mikrofiche-Zeile
          space    TO bhdgd-separ,     "keine Listseparation
          'BUKRS'  TO bhdgd-domai.

    SORT prot_ausgabe.
    loop at prot_ausgabe.
      PERFORM print_immediately.
    endloop.
    PERFORM output_alv.
  ENDIF.

ENDFORM.                    "PROT_PRINT

*---------------------------------------------------------------------*
*       FORM PRINT_IMMEDIATELY                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM print_immediately.
* LOOP AT PROT_AUSGABE.
  DATA: hrqident  LIKE tsp01-rqident.
  IF    ( prot_ausgabe-tddevice  = 'PRINTER'
  OR      not save_outputdone is initial )      "PDF print successful
  AND NOT prot_ausgabe-tdspoolid IS INITIAL
  AND NOT prot_ausgabe-tdimmed IS INITIAL.
    hrqident = prot_ausgabe-tdspoolid.
    CALL FUNCTION 'RSPO_OUTPUT_SPOOL_REQUEST'
      EXPORTING
        spool_request_id = hrqident
      EXCEPTIONS
        OTHERS           = 0.
  ENDIF.
* ENDLOOP.
ENDFORM.                    "PRINT_IMMEDIATELY

*&--------------------------------------------------------------------*
*&      Form  output_alv
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM output_alv.

  DATA: lt_fieldcat  TYPE slis_t_fieldcat_alv,
        ls_fieldcat  TYPE slis_fieldcat_alv,
        ls_print     TYPE slis_print_alv,         " Print Options
        ls_layout    TYPE slis_layout_alv,
        lt_eventtab  TYPE slis_t_event,
        l_tabname    TYPE slis_tabname,
        lt_sorttab   TYPE slis_t_sortinfo_alv,
        wa_prot_ausgabe TYPE fagl_s_rfkord,
        lt_prot_ausgabe TYPE STANDARD TABLE OF fagl_s_rfkord.
  DATA: ls_events TYPE slis_alv_event.
  DATA: ls_variant LIKE disvariant.

  CONSTANTS: lc_strucname TYPE dd02l-tabname VALUE 'FAGL_S_RFKORD'.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = lc_strucname
    CHANGING
      ct_fieldcat      = lt_fieldcat
    EXCEPTIONS
      OTHERS           = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  ls_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = ls_variant
    EXCEPTIONS
      OTHERS     = 4.
  IF sy-subrc <> 0.
* create default layout
    CLEAR ls_variant.
    ls_fieldcat-no_out = 'X'.
    MODIFY lt_fieldcat FROM ls_fieldcat TRANSPORTING no_out
    WHERE fieldname NE 'BUKRS' AND
          fieldname NE 'LTEXT' AND
          fieldname NE 'TDSPOOLID' AND
          fieldname NE 'TDDATASET' AND
          fieldname NE 'TDSUFFIX1' AND
          fieldname NE 'TDSUFFIX2' AND
          fieldname NE 'COUNTP' AND
          fieldname NE 'COUTPUT'.
    ls_fieldcat-outputlen = 41.
    MODIFY lt_fieldcat FROM ls_fieldcat TRANSPORTING outputlen
    WHERE fieldname EQ 'COUTPUT'.
  ENDIF.

* build eventtab
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = lt_eventtab
    EXCEPTIONS
      OTHERS      = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* activate ALV event top_of_page
  READ TABLE lt_eventtab INTO ls_events WITH
       KEY name = slis_ev_top_of_page.
  IF sy-subrc = 0.
    ls_events-form = 'TOP_OF_PAGE_COR'.
    MODIFY lt_eventtab FROM ls_events TRANSPORTING form
           WHERE name = slis_ev_top_of_page.
  ENDIF.

* create device dependent protocol
  LOOP AT prot_ausgabe.
    CLEAR wa_prot_ausgabe.
    MOVE-CORRESPONDING prot_ausgabe TO wa_prot_ausgabe.
    IF NOT prot_ausgabe-event IS INITIAL.
      SELECT SINGLE * FROM t048t
        WHERE spras = sy-langu
        AND   event = prot_ausgabe-event.
      IF sy-subrc NE 0.
        t048t-ltext = text-203.
      ENDIF.
    ELSE.
      IF sy-repid = prot_ausgabe-repid.
        t048t-ltext = text-204.
      ELSE.
        MESSAGE e543.
      ENDIF.
    ENDIF.
    wa_prot_ausgabe-ltext = t048t-ltext.

    IF prot_ausgabe-intad IS INITIAL.
      IF prot_ausgabe-tddevice EQ 'TELEFAX'.
        CONCATENATE prot_ausgabe-tdfaxid space
                    prot_ausgabe-tdteleland space
                    prot_ausgabe-tdtelenum INTO
                    wa_prot_ausgabe-coutput.
      ENDIF.
    ELSE.
      wa_prot_ausgabe-coutput = prot_ausgabe-intad.
    ENDIF.
    APPEND wa_prot_ausgabe TO lt_prot_ausgabe.
  ENDLOOP.

  IF NOT lt_prot_ausgabe IS INITIAL.
    ls_print-no_print_listinfos = 'X'.
    ls_print-no_print_selinfos = 'X'.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = ls_layout
        it_fieldcat        = lt_fieldcat
        i_save             = 'A'
        is_variant         = ls_variant
        it_events          = lt_eventtab
        is_print           = ls_print
      TABLES
        t_outtab           = lt_prot_ausgabe
      EXCEPTIONS
        OTHERS             = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDFORM.                    "output_alv

*&--------------------------------------------------------------------*
*&      Form  top_of_page
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_cor.                                       "#EC CALLED
  DATA : lv_width TYPE        sy-linsz,
         lo_grid  TYPE REF TO cl_salv_form_layout_grid.

* Function module to get width of list
  CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
    IMPORTING
      e_width       = lv_width
    EXCEPTIONS
      no_infos      = 1
      program_error = 2
      OTHERS        = 3.

  MOVE lv_width TO bhdgd-lines.

  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd     = bhdgd
    IMPORTING
      eo_form_grid = lo_grid.

  cl_salv_form_content=>set( lo_grid ).

ENDFORM.                    "top_of_page

*&--------------------------------------------------------------------*
*&      Form  output_error
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM output_error.

  DATA : gt_log_handle TYPE bal_t_logh,
         gs_log_handle TYPE balloghndl,
         gs_log        TYPE bal_s_log.

  DATA : ls_fimsg LIKE fimsg,
         lt_fimsg LIKE fimsg OCCURS 0,
         ls_message TYPE bal_s_msg.

  DATA ls_print_options TYPE  slis_print_alv.
  DATA ls_profile TYPE bal_s_prof.
  DATA ls_clbk_top_of_page TYPE bal_s_clbk.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = gs_log
    IMPORTING
      e_log_handle            = gs_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'FI_MESSAGE_CHECK'
    EXCEPTIONS
      no_message = 04.

  IF sy-subrc = 0.
    CALL FUNCTION 'FI_MESSAGE_SORT'.
    CALL FUNCTION 'FI_MESSAGE_GET'
      TABLES
        t_fimsg = lt_fimsg.

* move collected messages from FIMSG to appl-log
    LOOP AT lt_fimsg INTO ls_fimsg.
      CLEAR ls_message.
      MOVE-CORRESPONDING ls_fimsg TO ls_message.
      ls_message-msgty = 'W'.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = gs_log_handle
          i_s_msg      = ls_message.
    ENDLOOP.
  ENDIF.

  APPEND gs_log_handle TO gt_log_handle.

* To get standard display profile
  CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
    IMPORTING
      e_s_display_profile = ls_profile
    EXCEPTIONS
      OTHERS              = 1.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* 1. use the list display.
  CLEAR ls_profile-use_grid.

* 3. Add title to application log
  ls_profile-title = sy-title.
  ls_profile-no_toolbar = 'X'.

* change appl-log profile
  DATA ls_mess_fcat TYPE bal_s_fcat.
  ls_mess_fcat-col_pos   = 2.
  ls_mess_fcat-outputlen = 5.
  ls_mess_fcat-no_out    = space.
  MODIFY ls_profile-mess_fcat FROM ls_mess_fcat
    TRANSPORTING col_pos outputlen no_out
  WHERE ref_field EQ 'MSGID'.

  ls_mess_fcat-col_pos = 3.
  MODIFY ls_profile-mess_fcat FROM ls_mess_fcat
    TRANSPORTING col_pos no_out
  WHERE ref_field EQ 'MSGNO'.

  ls_mess_fcat-col_pos   = 4.
  ls_mess_fcat-outputlen = 116.                      "Note 1565993
  MODIFY ls_profile-mess_fcat FROM ls_mess_fcat
    TRANSPORTING col_pos outputlen
    WHERE ref_field EQ 'T_MSG'.

* set_print_list_options
  ls_print_options-no_print_listinfos     = 'X'.
  ls_print_options-no_print_selinfos      = 'X'.

* initiate top_of_page_log
  ls_clbk_top_of_page-userexitp = sy-repid.
  ls_clbk_top_of_page-userexitf = 'TOP_OF_PAGE_LOG'.
  ls_profile-clbk_top_of_page = ls_clbk_top_of_page.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile = ls_profile
      i_t_log_handle      = gt_log_handle.

ENDFORM.                    "output_error

*&--------------------------------------------------------------------*
*&      Form  top_of_page_log
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_log.
  DATA : lv_width TYPE        sy-linsz,
         lo_grid  TYPE REF TO cl_salv_form_layout_grid.

  bhdgd-lines = 132.
  bhdgd-repid = sy-repid.
  bhdgd-uname = sy-uname.
  bhdgd-line1 = sy-title.
  bhdgd-line2 = text-205.

  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd     = bhdgd
    IMPORTING
      eo_form_grid = lo_grid.

  cl_salv_form_content=>set( lo_grid ).
ENDFORM.                    "top_of_page_log
