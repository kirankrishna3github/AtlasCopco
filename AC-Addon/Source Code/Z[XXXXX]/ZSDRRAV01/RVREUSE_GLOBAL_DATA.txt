*
tables: ltdx.
type-pools: rsfs.
type-pools: rsds.
type-pools: slis.
type-pools: kkblo.

types: begin of sd_alv,
*
         grid_top_of_page type slis_t_listheader,
         fieldcat     type slis_t_fieldcat_alv,
         variant      like disvariant,
         events       type slis_t_event,
         event_exit  type  slis_t_event_exit,
         layout       type slis_layout_alv,
         sort         type slis_t_sortinfo_alv,
         filter       type slis_t_filter_alv,
         special_groups type slis_t_sp_group_alv,
         excluding      type slis_t_extab,
         sel_hide       type slis_sel_hide_alv,
         program        like sy-repid,
         pf_status_set  type slis_formname,
         user_command   type slis_formname,
         structure      like dd02l-tabname,
         structure_item like dd02l-tabname,
         keyinfo        type slis_keyinfo_alv,
         default        type c,
         save           type c,
         print          type slis_print_alv,
         start_column   type i,
         start_line     type i,
         end_column     type i,
         end_line       type i,
         exit           type c,
         user_exit      type slis_exit_by_user ,
* select
         title(100)     type c,
         selection      type c,
         zebra          type c,
         checkbox       like dd03p-fieldname,
         linemark       like dd03p-fieldname,
         tabname        like dd02l-tabname,
* ok_code
         selfield       type slis_selfield,
*
         tabname_header like dd02l-tabname,
         tabname_item   like dd02l-tabname,
* List_loayout_more
         filtered_entries type  kkblo_t_sfinfo,
         filtered_entries_item type  kkblo_t_sfinfo,
         list_scroll type  kkblo_list_scroll,
*
         grid_display   type c,
         subrc          like sy-subrc,

end of sd_alv.

types: begin of list_variant,
         kind,
         tabname_header like dd02l-tabname,
         tabname_item   like dd02l-tabname,
       end   of list_variant.


types: begin of events,
       node like dd02l-tabname,
       kind,
       end   of events.

types: t_events type events occurs 1.

data:  gs_sd_alv type sd_alv.
data:  gs_list_variant type list_variant.

data:  gv_old_list_layout.

data:  gv_ucomm like sy-ucomm.
data:  gv_subrc like sy-subrc.
data:  gv_exit.
data:  d_save(1) type c.

constants:
      gc_chara type c value 'A',
      gc_charb type c value 'B',
      gc_charc type c value 'C',
      gc_chard type c value 'D',
      gc_chare type c value 'E',
      gc_charf type c value 'F',
      gc_charg type c value 'G',
      gc_charh type c value 'H',
      gc_chari type c value 'I',
      gc_charj type c value 'J',
      gc_chark type c value 'K',
      gc_charl type c value 'L',
      gc_charm type c value 'M',
      gc_charn type c value 'N',
      gc_charo type c value 'O',
      gc_charp type c value 'P',
      gc_charq type c value 'Q',
      gc_charr type c value 'R',
      gc_chars type c value 'S',
      gc_chart type c value 'T',
      gc_charu type c value 'U',
      gc_charv type c value 'V',
      gc_charw type c value 'W',
      gc_charx type c value 'X',
      gc_chary type c value 'Y',
      gc_charz type c value 'Z',
      gc_char0 type c value '0',
      gc_char1 type c value '1',
      gc_char2 type c value '2',
      gc_char3 type c value '3',
      gc_char4 type c value '4',
      gc_char5 type c value '5',
      gc_char6 type c value '6',
      gc_char7 type c value '7',
      gc_char8 type c value '8',
      gc_char9 type c value '9',
      gc_char* type c value '*',
      gc_char$ type c value '$',
      gc_char- type c value '-',                            "#EC *
      gc_char_plus type c value '+',
      gc_space type c value ' '.


data : gs_selfield type slis_selfield.
