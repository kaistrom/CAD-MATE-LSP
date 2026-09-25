;; =========================================================================
;; UNIVERSAL QUANTITY TAKEOFF (QTO) & SHUTTERING TABLE MANAGER
;; =========================================================================

;; Register Application Name for XData
(regapp "QTO_DATA_APP")

;; Global category list setup - Default List
(setq *qto_default_categories*
   '(("Footings" "FTG" 1 "Depth")
     ("Columns"  "COL" 4 "Height")
     ("Beams"    "BM"  2 "Depth")
     ("Slabs"    "SLAB" 3 "Thickness")
     ("Coping"   "COP" 6 "Thickness")))

(if (not *qto_csv_saved_path*) (setq *qto_csv_saved_path* nil))
(if (not *qto_last_vert_dim*) (setq *qto_last_vert_dim* 0.50))
(if (not *qto_shutter_deductions*) (setq *qto_shutter_deductions* nil))

;; Helper: Save custom categories to DWG Named Object Dictionary
(defun qto_save_categories_to_dwg ()
  (vl-load-com)
  (vlax-ldata-put "QTO_SETTINGS_DICT" "CATEGORIES" *qto_categories*)
)

;; Helper: Load categories from DWG (Merge DWG Saved list with Defaults)
(defun qto_load_categories_from_dwg (/ saved_cats item)
  (vl-load-com)
  (setq saved_cats (vlax-ldata-get "QTO_SETTINGS_DICT" "CATEGORIES"))
  (if saved_cats
    (progn
      (setq *qto_categories* saved_cats)
      (foreach item *qto_default_categories*
        (if (not (assoc (car item) *qto_categories*))
          (setq *qto_categories* (append *qto_categories* (list item)))
        )
      )
    )
    (setq *qto_categories* *qto_default_categories*)
  )
)

;; Initialize Categories on Load
(qto_load_categories_from_dwg)
(if (not *qto_active_cat*) (setq *qto_active_cat* "Footings"))

;; Main Command
(defun c:MYQTO (/ result)
  (vl-load-com)
  (regapp "QTO_DATA_APP")
  (qto_load_categories_from_dwg)
  (qto_sync_data_from_dwg)
  (setq result (qto_dialog_category_select))
  (if (= result 1)
    (qto_dialog_main_manager)
  )
  (princ)
)

;; =========================================================================
;; XDATA PERSISTENCE ENGINE
;; =========================================================================
(defun qto_write_xdata (ent cat_name tag_name vert_dim shape_label nos work_type / exdata)
  (regapp "QTO_DATA_APP")
  (setq exdata
    (list 
      (list -3
        (list "QTO_DATA_APP"
          (cons 1000 cat_name)
          (cons 1000 tag_name)
          (cons 1040 (float vert_dim))
          (cons 1000 shape_label)
          (cons 1070 (fix nos))
          (cons 1000 work_type)
        )
      )
    )
  )
  (entmod (append (entget ent) exdata))
)

(defun qto_sync_data_from_dwg (/ ss i ent obj xdata cat_name tag_name vert_dim shape_label nos work_type area dims len wid minpt maxpt text_pt ent_txt)
  (setq *qto_data* nil)
  (setq ss (ssget "X" '((0 . "LWPOLYLINE,POLYLINE") (-3 ("QTO_DATA_APP")))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq xdata (assoc "QTO_DATA_APP" (cdr (assoc -3 (entget ent '("QTO_DATA_APP"))))))
        (if xdata
          (progn
            (setq cat_name (cdr (nth 1 xdata)))
            (setq tag_name (cdr (nth 2 xdata)))
            (setq vert_dim (cdr (nth 3 xdata)))
            (setq shape_label (cdr (nth 4 xdata)))
            (setq nos (if (nth 5 xdata) (cdr (nth 5 xdata)) 1))
            (setq work_type (if (nth 6 xdata) (cdr (nth 6 xdata)) "Regular"))
            (setq area (vla-get-area obj))
            (setq dims (get_accurate_len_wid ent))
            (setq len (car dims))
            (setq wid (cadr dims))
            (vla-getboundingbox obj 'minpt 'maxpt)
            (setq minpt (vlax-safearray->list minpt))
            (setq maxpt (vlax-safearray->list maxpt))
            (setq text_pt (list (/ (+ (car minpt) (car maxpt)) 2.0) (/ (+ (cadr minpt) (cadr maxpt)) 2.0) 0.0))
            (setq ent_txt (qto_find_associated_text text_pt tag_name (strcat "QTO_" (strcase cat_name))))
            
            (setq *qto_data* 
              (append *qto_data* 
                (list (list cat_name tag_name len wid area vert_dim (* area vert_dim nos) shape_label nos work_type ent ent_txt))
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
)

(defun qto_find_associated_text (pt tag_str layer_name / ss i text_ent found)
  (setq ss (ssget "X" (list '(0 . "TEXT") (cons 8 layer_name) (cons 1 tag_str))))
  (if ss
    (progn
      (setq i 0 found nil)
      (repeat (sslength ss)
        (setq text_ent (ssname ss i))
        (if (< (distance pt (cdr (assoc 10 (entget text_ent)))) 5.0)
          (setq found text_ent)
        )
        (setq i (1+ i))
      )
      found
    )
    nil
  )
)

;; =========================================================================
;; DIALOG 1: CATEGORY SELECTION & LAYER SETUP
;; =========================================================================
(defun qto_dialog_category_select (/ dcl_file f dcl_id what_next cat_names)
  (setq dcl_file (vl-filename-mktemp "qto_cat.dcl"))
  (setq f (open dcl_file "w"))
  (write-line "qto_cat : dialog { label = \"QTO - Select Category & Layer Setup\";" f)
  (write-line "  : boxed_column { label = \"Active Category Selection\";" f)
  (write-line "    : popup_list { key = \"pop_cat\"; width = 35; }" f)
  (write-line "    : text { key = \"lbl_layer_info\"; label = \"Target Layer: QTO_FOOTINGS\"; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_row { label = \"Create Custom Category & Layer\";" f)
  (write-line "    : edit_box { label = \"Name:\"; key = \"eb_new_name\"; edit_width = 12; }" f)
  (write-line "    : edit_box { label = \"Prefix:\"; key = \"eb_new_prefix\"; edit_width = 6; }" f)
  (write-line "    : popup_list { label = \"Color:\"; key = \"pop_color\"; edit_width = 10; }" f)
  (write-line "    : button { label = \"+ Add Category\"; key = \"btn_add_cat\"; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_row { label = \"Layer Visibility Quick Controls\";" f)
  (write-line "    : button { label = \"Isolate Active Layer\"; key = \"btn_iso_layer\"; }" f)
  (write-line "    : button { label = \"Turn ON All QTO Layers\"; key = \"btn_show_all\"; }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : button { label = \"Open Takeoff Manager\"; key = \"btn_open\"; is_default = true; }" f)
  (write-line "    : cancel_button { label = \"Cancel\"; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "qto_cat" dcl_id))
    (progn (if (findfile dcl_file) (vl-file-delete dcl_file)) (exit))
  )
  (setq cat_names (mapcar 'car *qto_categories*))
  (start_list "pop_cat")
  (foreach item cat_names (add_list item))
  (end_list)
  (start_list "pop_color")
  (add_list "1 - Red") (add_list "2 - Yellow") (add_list "3 - Green")
  (add_list "4 - Cyan") (add_list "5 - Blue") (add_list "6 - Magenta") (add_list "7 - White")
  (end_list)
  (set_tile "pop_cat" (itoa (vl-position *qto_active_cat* cat_names)))
  (update_cat_layer_label)
  (action_tile "pop_cat" "(setq *qto_active_cat* (nth (atoi $value) (mapcar 'car *qto_categories*))) (update_cat_layer_label)")
  (action_tile "btn_add_cat" "(add_custom_category)")
  (action_tile "btn_iso_layer" "(isolate_active_qto_layer)")
  (action_tile "btn_show_all" "(show_all_qto_layers)")
  (action_tile "btn_open" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq what_next (start_dialog))
  (unload_dialog dcl_id)
  (if (findfile dcl_file) (vl-file-delete dcl_file))
  what_next
)

(defun update_cat_layer_label (/ info cat_name layer_name color_code)
  (setq cat_name *qto_active_cat*)
  (setq info (assoc cat_name *qto_categories*))
  (setq layer_name (strcat "QTO_" (strcase cat_name)))
  (setq color_code (if info (nth 2 info) 1))
  (set_tile "lbl_layer_info" (strcat "Target Layer: " layer_name " (Color Index: " (itoa color_code) ")"))
)

(defun add_custom_category (/ new_name new_prefix new_color_idx cat_names)
  (setq new_name (get_tile "eb_new_name"))
  (setq new_prefix (get_tile "eb_new_prefix"))
  (setq new_color_idx (1+ (atoi (get_tile "pop_color"))))
  (if (and (> (strlen new_name) 0) (> (strlen new_prefix) 0))
    (progn
      (setq *qto_categories* (append *qto_categories* (list (list new_name new_prefix new_color_idx "Depth"))))
      (qto_save_categories_to_dwg)
      (setq *qto_active_cat* new_name)
      (setq cat_names (mapcar 'car *qto_categories*))
      (start_list "pop_cat")
      (foreach item cat_names (add_list item))
      (end_list)
      (set_tile "pop_cat" (itoa (vl-position new_name cat_names)))
      (update_cat_layer_label)
      (set_tile "eb_new_name" "")
      (set_tile "eb_new_prefix" "")
      (alert (strcat "New Category Added & Saved to DWG!\nCategory: " new_name "\nLayer Created: QTO_" (strcase new_name)))
    )
    (alert "Please enter both Category Name and Prefix.")
  )
)

(defun ensure_qto_layer_exists (cat_name / info layer_name color_code)
  (setq info (assoc cat_name *qto_categories*))
  (setq layer_name (strcat "QTO_" (strcase cat_name)))
  (setq color_code (if info (nth 2 info) 1))
  (if (not (tblsearch "LAYER" layer_name))
    (command "_LAYER" "_N" layer_name "_C" color_code layer_name "")
  )
  layer_name
)

(defun isolate_active_qto_layer (/ active_layer)
  (setq active_layer (strcat "QTO_" (strcase *qto_active_cat*)))
  (command "_LAYER" "_OFF" "QTO_*" "_Y" "_ON" active_layer "")
  (alert (strcat "Layer Isolation Complete:\nOnly Layer [" active_layer "] is visible."))
)

(defun show_all_qto_layers ()
  (command "_LAYER" "_ON" "QTO_*" "")
  (alert "All QTO structural takeoff layers turned ON.")
)

(defun confirm_action_prompt (dialog_title message / dcl_file f dcl_id user_choice)
  (setq dcl_file (vl-filename-mktemp "qto_confirm.dcl"))
  (setq f (open dcl_file "w"))
  (write-line (strcat "qto_confirm : dialog { label = \"" dialog_title "\";") f)
  (write-line "  : boxed_column { label = \"Warning!\";" f)
  (write-line (strcat "    : text { label = \"" message "\"; alignment = center; }") f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : button { label = \"Yes\"; key = \"btn_yes\"; }" f)
  (write-line "    : button { label = \"No (Cancel)\"; key = \"btn_no\"; is_default = true; is_cancel = true; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  (setq dcl_id (load_dialog dcl_file))
  (if (new_dialog "qto_confirm" dcl_id)
    (progn
      (action_tile "btn_yes" "(done_dialog 1)")
      (action_tile "btn_no" "(done_dialog 0)")
      (setq user_choice (start_dialog))
      (unload_dialog dcl_id)
    )
    (setq user_choice 0)
  )
  (if (findfile dcl_file) (vl-file-delete dcl_file))
  (= user_choice 1)
)

;; =========================================================================
;; DIALOG 2: MAIN TAKEOFF MANAGER
;; =========================================================================
(defun qto_dialog_main_manager (/ dcl_file f dcl_id what_next cur_sel)
  (setq dcl_file (vl-filename-mktemp "qto_mgr.dcl"))
  (setq f (open dcl_file "w"))
  (write-line "qto_mgr : dialog { label = \"Universal Multi-Category Takeoff Manager\";" f)
  (write-line "  : text { key = \"lbl_active_header\"; label = \"ACTIVE CATEGORY: FOOTINGS\"; alignment = left; }" f)
  (write-line "  : boxed_column { label = \"Filtered Category Records\";" f)
  (write-line "    : list_box { key = \"data_list\"; width = 90; height = 10; fixed_width_font = true; }" f)
  (write-line "    : text { key = \"lbl_total\"; label = \"Total Concrete: 0.000 m3\"; alignment = right; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_row { label = \"Edit / Delete Selected Record\";" f)
  (write-line "    : edit_box { label = \"Tag Name:\"; key = \"eb_tag\"; edit_width = 10; }" f)
  (write-line "    : edit_box { key = \"eb_vert_dim\"; label = \"Depth (m):\"; edit_width = 8; }" f)
  (write-line "    : edit_box { label = \"Nos:\"; key = \"eb_nos\"; edit_width = 5; }" f)
  (write-line "    : popup_list { label = \"Work:\"; key = \"pop_mgr_work\"; edit_width = 9; }" f)
  (write-line "    : button { label = \"Apply Changes\"; key = \"btn_update\"; }" f)
  (write-line "    : button { label = \"Delete Item\"; key = \"btn_delete_item\"; }" f)
  (write-line "    : button { label = \"Zoom & Highlight\"; key = \"btn_locate\"; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_row { label = \"Takeoff Input Commands\";" f)
  (write-line "    : button { label = \"Draw Polyline\"; key = \"btn_draw_poly\"; is_default = true; }" f)
  (write-line "    : button { label = \"Select Polyline\"; key = \"btn_pick_poly\"; }" f)
  (write-line "    : button { label = \"2-Point Rect\"; key = \"btn_add_rect\"; }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : button { label = \"Switch Category / Layer\"; key = \"btn_switch\"; }" f)
  (write-line "    : button { label = \"Shuttering Manager\"; key = \"btn_shutter\"; }" f)
  (write-line "    : button { label = \"Export CSV\"; key = \"btn_export\"; }" f)
  (write-line "    : button { label = \"Clear Category\"; key = \"btn_clear\"; }" f)
  (write-line "    : ok_button { label = \"Close\"; is_cancel = true; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)

  (setq what_next 2)
  (setq cur_sel nil)
  (while (>= what_next 2)
    (setq dcl_id (load_dialog dcl_file))
    (if (not (new_dialog "qto_mgr" dcl_id))
      (progn (if (findfile dcl_file) (vl-file-delete dcl_file)) (exit))
    )
    (set_tile "lbl_active_header" (strcat "ACTIVE CATEGORY: " (strcase *qto_active_cat*) " | LAYER: QTO_" (strcase *qto_active_cat*)))
    (set_tile "eb_vert_dim" (strcat (nth 3 (assoc *qto_active_cat* *qto_categories*)) " (m):"))
    (update_active_qto_list_ui)
    (if cur_sel (handle_mgr_list_select (itoa (+ cur_sel 2))))
    
    (action_tile "data_list" "(handle_mgr_list_select $value)")
    (action_tile "btn_update" "(handle_apply_changes)")
    (action_tile "btn_delete_item" "(handle_delete_single_item)")
    (action_tile "btn_locate" "(if cur_sel (done_dialog 3) (alert \"Select a takeoff record first!\"))")
    (action_tile "btn_draw_poly" "(done_dialog 2)")
    (action_tile "btn_pick_poly" "(done_dialog 4)")
    (action_tile "btn_add_rect" "(done_dialog 5)")
    (action_tile "btn_switch" "(done_dialog 6)")
    (action_tile "btn_shutter" "(done_dialog 7)")
    (action_tile "btn_export" "(export_all_qto_csv)")
    (action_tile "btn_clear" "(handle_clear_category_with_warning)")

    (setq what_next (start_dialog))
    (unload_dialog dcl_id)

    (cond
      ((= what_next 2) (draw_custom_polyline_qto) (setq what_next 2))
      ((= what_next 3) (locate_and_flash_qto cur_sel) (setq what_next 2))
      ((= what_next 4) (select_existing_polyline_qto) (setq what_next 2))
      ((= what_next 5) (pick_and_calc_rect_qto) (setq what_next 2))
      ((= what_next 6)
       (if (= (qto_dialog_category_select) 1)
         (setq what_next 2)
         (setq what_next 0)
       )
      )
      ((= what_next 7)
       (qto_dialog_shuttering_manager)
       (setq what_next 2)
      )
    )
  )
  (if (findfile dcl_file) (vl-file-delete dcl_file))
)

(defun update_active_qto_list_ui (/ total_vol display_list item vol shape_str tag_str filtered_data vert_lbl nos_val work_val)
  (setq total_vol 0.0)
  (setq vert_lbl (strcase (substr (nth 3 (assoc *qto_active_cat* *qto_categories*)) 1 1)))
  (setq display_list (list (format_col "TAG" 8)
                           (format_col "SHAPE" 13)
                           (format_col "NOS" 5)
                           (format_col "L(m)" 8)
                           (format_col "W(m)" 8)
                           (format_col "AREA(m2)" 11)
                           (format_col (strcat vert_lbl "(m)") 7)
                           (format_col "VOL(m3)" 11)
                           (format_col "WORK" 8)))
  (start_list "data_list")
  (add_list (apply 'strcat display_list))
  (add_list "------------------------------------------------------------------------------------------------------")
  (setq filtered_data (vl-remove-if-not '(lambda (x) (equal (nth 0 x) *qto_active_cat*)) *qto_data*))
  (foreach item filtered_data
    (setq vol (nth 6 item))
    (setq total_vol (+ total_vol vol))
    (setq shape_str (nth 7 item))
    (setq tag_str (nth 1 item))
    (setq nos_val (if (and (> (length item) 8) (numberp (nth 8 item))) (nth 8 item) 1))
    (setq work_val (if (> (length item) 9) (nth 9 item) "Regular"))
    
    (add_list (strcat 
      (format_col tag_str 8)
      (format_col shape_str 13)
      (format_col (itoa nos_val) 5)
      (format_col (rtos (nth 2 item) 2 2) 8)
      (format_col (rtos (nth 3 item) 2 2) 8)
      (format_col (rtos (nth 4 item) 2 2) 11)
      (format_col (rtos (nth 5 item) 2 2) 7)
      (format_col (rtos vol 2 3) 11)
      (format_col work_val 8)
    ))
  )
  (end_list)
  (set_tile "lbl_total" (strcat "Total Concrete (" *qto_active_cat* "): " (rtos total_vol 2 3) " m3"))
)

(defun format_col (str width)
  (if (not (eq (type str) 'STR)) (setq str (vl-prin1-to-string str)))
  (while (< (strlen str) width) (setq str (strcat str " ")))
  str
)

(defun register_qto_entity (ent shape_label / cat_info prefix layer_name count area tag val_res vert_dim nos work_type vol minpt maxpt dims len wid text_pt txt_h ent_txt obj)
  (setq obj (vlax-ename->vla-object ent))
  (if (and (vlax-property-available-p obj 'Area) (> (vla-get-area obj) 0.0))
    (progn
      (setq layer_name (ensure_qto_layer_exists *qto_active_cat*))
      (vla-put-layer obj layer_name)
      (setq cat_info (assoc *qto_active_cat* *qto_categories*))
      (setq prefix (nth 1 cat_info))
      
      (setq count (1+ (length (vl-remove-if-not '(lambda (x) (equal (nth 0 x) *qto_active_cat*)) *qto_data*))))
      (setq tag (strcat prefix "-" (itoa count)))
      (setq area (vla-get-area obj))
      (setq dims (get_accurate_len_wid ent))
      (setq len (car dims))
      (setq wid (cadr dims))
      (vla-getboundingbox obj 'minpt 'maxpt)
      (setq minpt (vlax-safearray->list minpt))
      (setq maxpt (vlax-safearray->list maxpt))
      (setq text_pt (list (/ (+ (car minpt) (car maxpt)) 2.0) (/ (+ (cadr minpt) (cadr maxpt)) 2.0) 0.0))
      (setq txt_h (max 0.15 (* (min (- (car maxpt) (car minpt)) (- (cadr maxpt) (cadr minpt))) 0.40)))
      (command "_TEXT" "_J" "_MC" "_non" text_pt txt_h "0" tag)
      (setq ent_txt (entlast))
      (vla-put-layer (vlax-ename->vla-object ent_txt) layer_name)
      
      (setq val_res (qto_dialog_set_vert_dim))
      (setq vert_dim (nth 0 val_res))
      (setq nos      (nth 1 val_res))
      (setq work_type(nth 2 val_res))
      (setq vol (* area vert_dim nos))
      
      (qto_write_xdata ent *qto_active_cat* tag vert_dim shape_label nos work_type)
      (setq *qto_data* (append *qto_data* (list (list *qto_active_cat* tag len wid area vert_dim vol shape_label nos work_type ent ent_txt))))
    )
    (alert "Selected entity is not closed or has zero area.")
  )
)

(defun get_clean_poly_vertices (ent / elist raw_pts clean_pts pt)
  (setq elist (entget ent))
  (setq raw_pts nil)
  (foreach item elist (if (= (car item) 10) (setq raw_pts (append raw_pts (list (cdr item))))))
  (setq clean_pts nil)
  (foreach pt raw_pts
    (if (not (and clean_pts (equal pt (car clean_pts) 0.0001)))
      (if (not (and clean_pts (equal pt (last clean_pts) 0.0001)))
        (setq clean_pts (append clean_pts (list pt)))
      )
    )
  )
  clean_pts
)

(defun get_accurate_len_wid (ent / pts n p0 p1 p2 d1 d2 side_a side_b area_calc)
  (setq pts (get_clean_poly_vertices ent))
  (setq n (length pts))
  (if (= n 4)
    (progn
      (setq p0 (nth 0 pts) p1 (nth 1 pts) p2 (nth 2 pts))
      (setq d1 (distance p0 p1) d2 (distance p1 p2))
      (setq side_a (max d1 d2) side_b (min d1 d2))
      (setq area_calc (vla-get-area (vlax-ename->vla-object ent)))
      (if (and (> side_a 0.0) (> side_b 0.0) (< (abs (- (* side_a side_b) area_calc)) 0.05))
        (list side_a side_b)
        (list 1.0 1.0)
      )
    )
    (list 1.0 1.0)
  )
)

(defun draw_custom_polyline_qto (/ last_ent new_ent obj)
  (setq last_ent (entlast))
  (princ (strcat "\nDraw " *qto_active_cat* " outline (Click points, ENTER or 'C' to close): "))
  (command "_PLINE")
  (while (> (getvar "CMDACTIVE") 0) (command pause))
  (setq new_ent (entlast))
  (if (and new_ent (not (equal last_ent new_ent)))
    (progn
      (setq obj (vlax-ename->vla-object new_ent))
      (if (vlax-property-available-p obj 'Closed) (vla-put-closed obj :vlax-true))
      (register_qto_entity new_ent "Polyline")
    )
  )
)

(defun select_existing_polyline_qto (/ ent)
  (setq ent (car (entsel (strcat "\nSelect existing closed polyline for " *qto_active_cat* ": "))))
  (if ent (register_qto_entity ent "Picked Poly"))
)

(defun pick_and_calc_rect_qto (/ old_echo old_dyn old_prmpt old_divis old_pi last_ent new_ent)
  (setq old_echo  (getvar "CMDECHO"))
  (setq old_dyn   (getvar "DYNMODE"))
  (setq old_prmpt (getvar "DYNPROMPT"))
  (setq old_divis (getvar "DYNDIVIS"))
  (setq old_pi    (getvar "DYNPICOORDS"))
  (setvar "CMDECHO" 1)
  (setvar "DYNMODE" 3)
  (setvar "DYNPROMPT" 1)
  (setvar "DYNDIVIS" 2)
  (setvar "DYNPICOORDS" 0)
  (setq last_ent (entlast))
  (command "._RECTANG")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )
  (setq new_ent (entlast))
  (setvar "CMDECHO" old_echo)
  (setvar "DYNMODE" old_dyn)
  (setvar "DYNPROMPT" old_prmpt)
  (setvar "DYNDIVIS" old_divis)
  (setvar "DYNPICOORDS" old_pi)
  (if (and new_ent (not (equal last_ent new_ent)))
    (register_qto_entity new_ent "Rectangular")
  )
)

(defun qto_dialog_set_vert_dim (/ dcl_file f dcl_id user_val user_nos user_work what_next dim_name)
  (setq dim_name (nth 3 (assoc *qto_active_cat* *qto_categories*)))
  (if (null dim_name) (setq dim_name "Depth"))
  (setq dcl_file (vl-filename-mktemp "qto_val.dcl"))
  (setq f (open dcl_file "w"))
  (write-line "qto_set_val : dialog { label = \"Set Value\";" f)
  (write-line "  : boxed_column {" f)
  (write-line (strcat "    label = \"Specify Parameters for " *qto_active_cat* "\";") f)
  (write-line (strcat "    : edit_box { label = \"" dim_name " (m): \"; key = \"eb_val\"; edit_width = 12; }") f)
  (write-line "    : edit_box { label = \"Nos / Layers: \"; key = \"eb_nos\"; edit_width = 12; }" f)
  (write-line "    : popup_list { label = \"Work Type: \"; key = \"pop_work\"; edit_width = 12; }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : ok_button { label = \"Set & Continue\"; is_default = true; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "qto_set_val" dcl_id))
    (progn (if (findfile dcl_file) (vl-file-delete dcl_file)) (list *qto_last_vert_dim* 1 "Regular"))
    (progn
      (set_tile "eb_val" (rtos *qto_last_vert_dim* 2 3))
      (set_tile "eb_nos" "1")
      (start_list "pop_work")
      (add_list "Regular")
      (add_list "Rework")
      (end_list)
      (set_tile "pop_work" "0")
      (mode_tile "eb_val" 2)
      (action_tile "accept"
        "(setq user_val (atof (get_tile \"eb_val\")))
         (setq user_nos (atoi (get_tile \"eb_nos\")))
         (setq user_work (if (= (get_tile \"pop_work\") \"1\") \"Rework\" \"Regular\"))
         (done_dialog 1)")
      (setq what_next (start_dialog))
      (unload_dialog dcl_id)
      (if (findfile dcl_file) (vl-file-delete dcl_file))
      (if (and user_val (> user_val 0.0))
        (setq *qto_last_vert_dim* user_val)
        (setq user_val *qto_last_vert_dim*)
      )
      (if (or (null user_nos) (<= user_nos 0)) (setq user_nos 1))
      (if (null user_work) (setq user_work "Regular"))
      (list user_val user_nos user_work)
    )
  )
)

(defun get_active_category_records ()
  (vl-remove-if-not '(lambda (x) (equal (nth 0 x) *qto_active_cat*)) *qto_data*)
)

(defun handle_mgr_list_select (val / idx records item)
  (setq idx (- (atoi val) 2))
  (setq records (get_active_category_records))
  (if (and (>= idx 0) (< idx (length records)))
    (progn
      (setq cur_sel idx)
      (setq item (nth idx records))
      (set_tile "eb_tag" (nth 1 item))
      (set_tile "eb_vert_dim" (rtos (nth 5 item) 2 2))
      (set_tile "eb_nos" (itoa (if (nth 8 item) (nth 8 item) 1)))
      (start_list "pop_mgr_work")
      (add_list "Regular")
      (add_list "Rework")
      (end_list)
      (set_tile "pop_mgr_work" (if (equal (nth 9 item) "Rework") "1" "0"))
    )
    (progn
      (setq cur_sel nil)
      (set_tile "eb_tag" "")
      (set_tile "eb_vert_dim" "")
      (set_tile "eb_nos" "")
    )
  )
)

(defun handle_apply_changes (/ records item global_idx new_tag new_v new_nos new_work area new_vol ent_poly ent_txt txt_obj)
  (setq records (get_active_category_records))
  (if (and cur_sel (< cur_sel (length records)))
    (progn
      (setq item (nth cur_sel records))
      (setq global_idx (vl-position item *qto_data*))
      (setq new_tag (get_tile "eb_tag"))
      (setq new_v (atof (get_tile "eb_vert_dim")))
      (setq new_nos (atoi (get_tile "eb_nos")))
      (setq new_work (if (= (get_tile "pop_mgr_work") "1") "Rework" "Regular"))
      (if (<= new_nos 0) (setq new_nos 1))
      
      (if (and (> (strlen new_tag) 0) (> new_v 0.0))
        (progn
          (setq area (nth 4 item))
          (setq new_vol (* area new_v new_nos))
          (setq ent_poly (nth 10 item))
          (setq ent_txt  (nth 11 item))
          (if (and ent_txt (entget ent_txt))
            (progn
              (setq txt_obj (vlax-ename->vla-object ent_txt))
              (vla-put-textstring txt_obj new_tag)
              (vla-update txt_obj)
            )
          )
          (qto_write_xdata ent_poly *qto_active_cat* new_tag new_v (nth 7 item) new_nos new_work)
          (setq item (list *qto_active_cat* new_tag (nth 2 item) (nth 3 item) area new_v new_vol (nth 7 item) new_nos new_work ent_poly ent_txt))
          (setq *qto_data* (subst_nth global_idx item *qto_data*))
          (update_active_qto_list_ui)
          (alert (strcat "Record Updated!\nTag: " new_tag "\nWork: " new_work "\nVolume: " (rtos new_vol 2 3) " m3"))
        )
      )
    )
  )
)

(defun handle_delete_single_item (/ records item tag_name confirm ent_poly ent_txt)
  (setq records (get_active_category_records))
  (if (and cur_sel (< cur_sel (length records)))
    (progn
      (setq item (nth cur_sel records))
      (setq tag_name (nth 1 item))
      (setq confirm (confirm_action_prompt "Confirm Delete Item"
                       (strcat "Are you sure you want to delete item [" tag_name "]?")))
      (if confirm
        (progn
          (setq ent_poly (nth 10 item))
          (setq ent_txt  (nth 11 item))
          (if (and ent_poly (entget ent_poly)) (entdel ent_poly))
          (if (and ent_txt (entget ent_txt)) (entdel ent_txt))
          (setq *qto_data* (vl-remove item *qto_data*))
          (setq cur_sel nil)
          (set_tile "eb_tag" "")
          (set_tile "eb_vert_dim" "")
          (update_active_qto_list_ui)
          (alert (strcat "Item [" tag_name "] deleted."))
        )
      )
    )
    (alert "Please select an item row from the list first.")
  )
)

(defun handle_clear_category_with_warning (/ confirm active_records item)
  (setq active_records (get_active_category_records))
  (if active_records
    (progn
      (setq confirm (confirm_action_prompt "Confirm Clear Category"
                       (strcat "Are you sure you want to delete ALL " (strcase *qto_active_cat*) " elements?")))
      (if confirm
        (progn
          (foreach item active_records
            (if (and (nth 10 item) (entget (nth 10 item))) (entdel (nth 10 item)))
            (if (and (nth 11 item) (entget (nth 11 item))) (entdel (nth 11 item)))
          )
          (setq *qto_data* (vl-remove-if '(lambda (x) (equal (nth 0 x) *qto_active_cat*)) *qto_data*))
          (setq cur_sel nil)
          (set_tile "eb_tag" "")
          (set_tile "eb_vert_dim" "")
          (update_active_qto_list_ui)
          (alert (strcat "All elements in category [" *qto_active_cat* "] deleted."))
        )
      )
    )
    (alert "No elements found in active category to clear.")
  )
)

(defun locate_and_flash_qto (idx / records item ent_poly ent_txt minpt maxpt)
  (setq records (get_active_category_records))
  (if (and idx (< idx (length records)))
    (progn
      (setq item (nth idx records))
      (setq ent_poly (nth 10 item))
      (setq ent_txt  (nth 11 item))
      (if (and ent_poly (entget ent_poly))
        (progn
          (redraw ent_poly 3)
          (if (and ent_txt (entget ent_txt)) (redraw ent_txt 3))
          (vla-getboundingbox (vlax-ename->vla-object ent_poly) 'minpt 'maxpt)
          (vla-zoomwindow (vlax-get-acad-object) minpt maxpt)
          (command "_ZOOM" "0.7x")
          (alert (strcat "Locating " (nth 1 item) " on Layer QTO_" (strcase *qto_active_cat*)))
          (redraw ent_poly 4)
          (if (and ent_txt (entget ent_txt)) (redraw ent_txt 4))
        )
      )
    )
  )
)

(defun export_all_qto_csv (/ def_path csv_path f)
  (qto_sync_data_from_dwg)
  (if (null *qto_data*)
    (alert "Export failed: No QTO elements found in drawing to export!")
    (progn
      (if (and *qto_csv_saved_path* (vl-filename-directory *qto_csv_saved_path*))
        (setq def_path *qto_csv_saved_path*)
        (setq def_path "QTO_Master_Takeoff.csv")
      )
      (setq csv_path (getfiled "Export / Sync Master Takeoff CSV" def_path "csv" 1))
      (if csv_path
        (progn
          (setq *qto_csv_saved_path* csv_path)
          (setq f (open csv_path "w"))
          (if f
            (progn
              (write-line "CATEGORY,LAYER,TAG,SHAPE,NOS,LENGTH(m),WIDTH(m),AREA(m2),HEIGHT_DEPTH(m),VOLUME(m3),WORK" f)
              (foreach item *qto_data*
                (write-line 
                  (strcat
                    (nth 0 item) ","
                    "QTO_" (strcase (nth 0 item)) ","
                    (nth 1 item) ","
                    (nth 7 item) ","
                    (itoa (if (nth 8 item) (nth 8 item) 1)) ","
                    (rtos (nth 2 item) 2 3) ","
                    (rtos (nth 3 item) 2 3) ","
                    (rtos (nth 4 item) 2 3) ","
                    (rtos (nth 5 item) 2 3) ","
                    (rtos (nth 6 item) 2 3) ","
                    (if (> (length item) 9) (nth 9 item) "Regular")
                  ) 
                  f
                )
              )
              (close f)
              (alert (strcat "Full Sync Complete!\n\nAll entities synced cleanly to:\n" csv_path))
            )
            (alert "Error: Cannot write to CSV file!\nPlease close the CSV file if it is currently open in Microsoft Excel.")
          )
        )
      )
    )
  )
)

(defun subst_nth (n new_elem lst / i res)
  (setq i 0 res nil)
  (foreach x lst
    (if (= i n) (setq res (cons new_elem res)) (setq res (cons x res)))
    (setq i (1+ i))
  )
  (reverse res)
)

;; =========================================================================
;; DEDICATED SHUTTERING ENGINE & DIALOG (FULLY INTEGRATED)
;; =========================================================================
(defun qto_get_poly_segments (ent / pts n i s_list p1 p2 d)
  (setq pts (get_clean_poly_vertices ent))
  (setq n (length pts))
  (setq s_list nil)
  (if (>= n 2)
    (progn
      (setq i 0)
      (while (< i (1- n))
        (setq p1 (nth i pts) p2 (nth (1+ i) pts))
        (setq d (distance p1 p2))
        (setq s_list (append s_list (list d)))
        (setq i (1+ i))
      )
      (if (> n 2)
        (progn
          (setq d (distance (nth (1- n) pts) (nth 0 pts)))
          (setq s_list (append s_list (list d)))
        )
      )
    )
  )
  s_list
)

;; ==========================================================================
;; AUTO BEAM-TO-COLUMN JUNCTION INTERSECTION & DEDUCTION ENGINE
;; ==========================================================================
(defun qto_get_column_beam_deduction_details (col_ent / col_obj minpt maxpt ss i bm_ent bm_obj
                                                       int_pts n_pts pt1 pt2 contact_w bm_xdata
                                                       bm_depth ded_area bm_tag details_list
                                                       ins_unit to_m pts n_v j p_start p_end
                                                       mid_pt d1 d2 d_seg hit_side)
  (vl-load-com)
  (setq details_list nil)
  (setq ins_unit (getvar "INSUNITS"))
  (setq to_m (if (= ins_unit 4) 0.001 1.0))

  (if (and col_ent (entget col_ent))
    (progn
      (setq col_obj (vlax-ename->vla-object col_ent))
      (vla-getboundingbox col_obj 'minpt 'maxpt)
      (setq minpt (vlax-safearray->list minpt))
      (setq maxpt (vlax-safearray->list maxpt))

      (setq pts (get_clean_poly_vertices col_ent))
      (setq n_v (length pts))

      (setq ss (ssget "C" minpt maxpt '((0 . "LWPOLYLINE,POLYLINE") (8 . "QTO_BEAMS"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq bm_ent (ssname ss i))
            (if (not (equal bm_ent col_ent))
              (progn
                (setq bm_obj (vlax-ename->vla-object bm_ent))
                (setq int_pts (vlax-variant-value (vla-IntersectWith col_obj bm_obj acExtendNone)))
                (if (and int_pts (> (vlax-safearray-get-u-bound int_pts 1) 0))
                  (progn
                    (setq int_pts (vlax-safearray->list int_pts))
                    (setq n_pts (/ (length int_pts) 3))
                    (if (>= n_pts 2)
                      (progn
                        (setq pt1 (list (nth 0 int_pts) (nth 1 int_pts) 0.0))
                        (setq pt2 (list (nth 3 int_pts) (nth 4 int_pts) 0.0))
                        (setq contact_w (distance pt1 pt2))
                        
                        (setq mid_pt (list (/ (+ (car pt1) (car pt2)) 2.0)
                                           (/ (+ (cadr pt1) (cadr pt2)) 2.0)
                                           0.0))

                        (setq hit_side 1)
                        (setq j 0)
                        (while (< j n_v)
                          (setq p_start (nth j pts))
                          (setq p_end   (if (< (1+ j) n_v) (nth (1+ j) pts) (nth 0 pts)))
                          (setq d_seg (distance p_start p_end))
                          (setq d1    (distance p_start mid_pt))
                          (setq d2    (distance mid_pt p_end))
                          
                          (if (< (abs (- (+ d1 d2) d_seg)) 2.0)
                            (setq hit_side (1+ j))
                          )
                          (setq j (1+ j))
                        )

                        (setq bm_xdata (assoc "QTO_DATA_APP" (cdr (assoc -3 (entget bm_ent '("QTO_DATA_APP"))))))
                        (if bm_xdata
                          (progn
                            (setq bm_tag   (cdr (nth 2 bm_xdata)))
                            (setq bm_depth (cdr (nth 3 bm_xdata)))
                            (if (and bm_depth (> bm_depth 0.0) (> contact_w 0.0))
                              (progn
                                (setq ded_area (* (* contact_w to_m) (if (> bm_depth 10.0) (* bm_depth to_m) bm_depth)))
                                (setq details_list (append details_list (list (list bm_tag contact_w bm_depth ded_area hit_side))))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  details_list
)

(defun qto_calculate_shutter_item (item / cat ent h nos s_list perim s1 s2 s3 s4 s_area ded_list total_ded ded_val hnd)
  (setq cat (nth 0 item))
  (setq h   (nth 5 item))
  (setq nos (if (nth 8 item) (nth 8 item) 1))
  (setq ent (nth 10 item))
  
  (setq s_list (if (and ent (entget ent)) (qto_get_poly_segments ent) nil))
  
  (if (or (null s_list) (< (length s_list) 2))
    (setq s1 (nth 2 item) s2 (nth 3 item) s3 (nth 2 item) s4 (nth 3 item)
          s_list (list s1 s2 s3 s4))
    (setq s1 (if (nth 0 s_list) (nth 0 s_list) 0.0)
          s2 (if (nth 1 s_list) (nth 1 s_list) 0.0)
          s3 (if (nth 2 s_list) (nth 2 s_list) 0.0)
          s4 (if (nth 3 s_list) (nth 3 s_list) 0.0))
  )
  (setq perim (apply '+ s_list))
  
  (cond
    ((equal (strcase cat) "COPING")
     (setq s_area (* 2.0 (nth 2 item) h nos)))
    (t
     (setq s_area (* (if (> perim 50.0) (/ perim 1000.0) perim) 
                     (if (> h 50.0) (/ h 1000.0) h) 
                     nos)))
  )
  
  (if (and ent (equal (strcase cat) "COLUMNS"))
    (progn
      (setq ded_list (qto_get_column_beam_deduction_details ent))
      (setq total_ded 0.0)
      (foreach d_item ded_list (setq total_ded (+ total_ded (nth 3 d_item))))
      (if (> total_ded 0.0) (setq s_area (max 0.0 (- s_area total_ded))))
    )
  )

  (if (and ent (entget ent))
    (progn
      (setq hnd (cdr (assoc 5 (entget ent))))
      (setq ded_val (assoc hnd *qto_shutter_deductions*))
      (if ded_val (setq s_area (max 0.0 (- s_area (cdr ded_val)))))
    )
  )
  (list perim s1 s2 s3 s4 s_area)
)

(defun qto_dialog_shuttering_manager (/ dcl_file f dcl_id what_next cur_shutter_sel)
  (setq dcl_file (vl-filename-mktemp "qto_shutter.dcl"))
  (setq f (open dcl_file "w"))
  (write-line "shuttering_mgr : dialog { label = \"Dedicated Shuttering & Formwork Manager\"; width = 118;" f)
  (write-line "  : text { key = \"txt_shutter_cat\"; label = \"ACTIVE CATEGORY: COLUMNS | FORMWORK ANALYSIS\"; }" f)
  (write-line "  : boxed_column { label = \"Shuttering Elements (Sides Breakdown)\";" f)
  (write-line "    : list_box { key = \"shutter_list\"; width = 114; height = 12; fixed_width_font = true; }" f)
  (write-line "    : text { key = \"txt_total_shutter\"; label = \"Total Shuttering Area: 0.000 m2\"; alignment = right; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_row { label = \"Deductions & Adjustments\";" f)
  (write-line "    : text { label = \"Apply beam intersection / masonry adjustments to selected item:\"; }" f)
  (write-line "    : button { key = \"btn_beam_deduct\"; label = \"Beam Deduction (-m2)\"; width = 22; }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"btn_export_shutter_csv\"; label = \"Export Shuttering CSV\"; width = 24; }" f)
  (write-line "    : button { key = \"btn_back\"; label = \"Back to Main Manager\"; is_cancel = true; is_default = true; width = 20; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)

  (setq what_next 1)
  (setq cur_shutter_sel nil)
  (while (= what_next 1)
    (setq dcl_id (load_dialog dcl_file))
    (if (not (new_dialog "shuttering_mgr" dcl_id))
      (setq what_next 0)
      (progn
        (set_tile "txt_shutter_cat" (strcat "ACTIVE CATEGORY: " (strcase *qto_active_cat*) " | FORMWORK ANALYSIS"))
        (update_shutter_list_ui)
        (action_tile "shutter_list" "(setq cur_shutter_sel (- (atoi $value) 2))")
        (action_tile "btn_beam_deduct" "(done_dialog 2)")
        (action_tile "btn_export_shutter_csv" "(export_shuttering_csv)")
        (action_tile "btn_back" "(done_dialog 0)")
        (action_tile "cancel" "(done_dialog 0)")
        
        (setq what_next (start_dialog))
        (unload_dialog dcl_id)
        (if (= what_next 2)
          (progn
            (handle_shutter_beam_deduction cur_shutter_sel)
            (setq what_next 1)
          )
        )
      )
    )
  )
  (if (findfile dcl_file) (vl-file-delete dcl_file))
)

(defun update_shutter_list_ui (/ records total_shutter item res tag_str shape_str nos_val l_val w_val h_val p_val s1 s2 s3 s4 a_val)
  (setq records (get_active_category_records))
  (setq total_shutter 0.0)
  (start_list "shutter_list")
  (add_list "TAG      SHAPE    NOS  L(m)   W(m)   H(m)   PERIMETER(m)  S1(m)  S2(m)  S3(m)  S4(m)  SHUTTER(m2)")
  (add_list "------------------------------------------------------------------------------------------------------")
  (foreach item records
    (setq res (qto_calculate_shutter_item item))
    (setq tag_str   (nth 1 item))
    (setq shape_str (nth 7 item))
    (setq nos_val   (if (nth 8 item) (nth 8 item) 1))
    (setq l_val     (nth 2 item))
    (setq w_val     (nth 3 item))
    (setq h_val     (nth 5 item))
    (setq p_val     (nth 0 res))
    (setq s1        (nth 1 res))
    (setq s2        (nth 2 res))
    (setq s3        (nth 3 res))
    (setq s4        (nth 4 res))
    (setq a_val     (nth 5 res))
    (setq total_shutter (+ total_shutter a_val))
    (add_list
      (strcat
        (format_col tag_str 9)
        (format_col shape_str 9)
        (format_col (itoa nos_val) 5)
        (format_col (rtos l_val 2 2) 7)
        (format_col (rtos w_val 2 2) 7)
        (format_col (rtos h_val 2 2) 7)
        (format_col (rtos p_val 2 2) 14)
        (format_col (rtos s1 2 2) 7)
        (format_col (rtos s2 2 2) 7)
        (format_col (rtos s3 2 2) 7)
        (format_col (rtos s4 2 2) 7)
        (format_col (rtos a_val 2 3) 12)
      )
    )
  )
  (end_list)
  (set_tile "txt_total_shutter" (strcat "Total Shuttering Area: " (rtos total_shutter 2 3) " m2"))
)

(defun handle_shutter_beam_deduction (sel_idx / records item ent hnd dcl_file f dcl_id bw bd bn ded_area cur_ded)
  (setq records (get_active_category_records))
  (if (and sel_idx (>= sel_idx 0) (< sel_idx (length records)))
    (progn
      (setq item (nth sel_idx records))
      (setq ent (nth 10 item))
      (if (and ent (entget ent))
        (progn
          (setq hnd (cdr (assoc 5 (entget ent))))
          (setq dcl_file (vl-filename-mktemp "qto_ded.dcl"))
          (setq f (open dcl_file "w"))
          (write-line "shutter_deduct_dialog : dialog { label = \"Beam Junction Deduction\"; width = 40;" f)
          (write-line "  : boxed_column { label = \"Beam Dimensions\";" f)
          (write-line "    : edit_box { label = \"Beam Width (m):\"; key = \"eb_bm_w\"; edit_width = 10; }" f)
          (write-line "    : edit_box { label = \"Beam Depth (m):\"; key = \"eb_bm_d\"; edit_width = 10; }" f)
          (write-line "    : edit_box { label = \"Nos of Beams:\"; key = \"eb_bm_n\"; edit_width = 10; }" f)
          (write-line "  }" f)
          (write-line "  : row { : ok_button { label = \"Apply Deduction\"; is_default = true; } : cancel_button { label = \"Cancel\"; } }" f)
          (write-line "}" f)
          (close f)
          (setq dcl_id (load_dialog dcl_file))
          (if (new_dialog "shutter_deduct_dialog" dcl_id)
            (progn
              (set_tile "eb_bm_w" "0.23")
              (set_tile "eb_bm_d" "0.45")
              (set_tile "eb_bm_n" "1")
              (action_tile "accept"
                "(setq bw (atof (get_tile \"eb_bm_w\"))
                       bd (atof (get_tile \"eb_bm_d\"))
                       bn (atoi (get_tile \"eb_bm_n\")))
                 (done_dialog 1)")
              (if (= (start_dialog) 1)
                (progn
                  (if (<= bn 0) (setq bn 1))
                  (setq ded_area (* bw bd bn))
                  (setq cur_ded (assoc hnd *qto_shutter_deductions*))
                  (if cur_ded
                    (setq *qto_shutter_deductions* (subst (cons hnd (+ (cdr cur_ded) ded_area)) cur_ded *qto_shutter_deductions*))
                    (setq *qto_shutter_deductions* (cons (cons hnd ded_area) *qto_shutter_deductions*))
                  )
                  (alert (strcat "Deducted " (rtos ded_area 2 3) " m2 from " (nth 1 item)))
                )
              )
              (unload_dialog dcl_id)
            )
          )
          (if (findfile dcl_file) (vl-file-delete dcl_file))
        )
      )
    )
    (alert "Please select an item row from the list first!")
  )
)

(defun export_shuttering_csv (/ csv_path f records item ent tag_str shape_str nos_val
                                l_val w_val h_val p_val gross_shutter res bm_ded_list bm_item)
  (setq records (get_active_category_records))
  (if (null records)
    (alert "No elements found to export!")
    (progn
      (setq csv_path (getfiled "Export Shuttering Takeoff CSV" "Shuttering_Takeoff.csv" "csv" 1))
      (if csv_path
        (progn
          (setq f (open csv_path "w"))
          (if f
            (progn
              (write-line "PARENT_TAG,ITEM_TAG,ROW_TYPE,CATEGORY,SHAPE,NOS,L(m),W(m),H(m),PERIMETER(m),SHUTTER_AREA(m2)" f)
              (foreach item records
                (setq ent (nth 10 item))
                (setq res (qto_calculate_shutter_item item))
                (setq tag_str   (nth 1 item))
                (setq shape_str (nth 7 item))
                (setq nos_val   (if (nth 8 item) (nth 8 item) 1))
                (setq l_val     (nth 2 item))
                (setq w_val     (nth 3 item))
                (setq h_val     (nth 5 item))
                (setq p_val     (nth 0 res))
                
                (setq gross_shutter (* (if (> p_val 50.0) (/ p_val 1000.0) p_val) 
                                       (if (> h_val 50.0) (/ h_val 1000.0) h_val) 
                                       nos_val))

                (write-line
                  (strcat
                    tag_str ","
                    tag_str ","
                    "Gross" ","
                    (nth 0 item) ","
                    shape_str ","
                    (itoa nos_val) ","
                    (rtos l_val 2 3) ","
                    (rtos w_val 2 3) ","
                    (rtos h_val 2 3) ","
                    (rtos p_val 2 3) ","
                    (rtos gross_shutter 2 3)
                  )
                  f
                )

                (setq bm_ded_list (if ent (qto_get_column_beam_deduction_details ent) nil))
                (if bm_ded_list
                  (foreach bm_item bm_ded_list
                    (write-line
                      (strcat
                        tag_str ","
                        (strcat "Deduct: " (nth 0 bm_item)) ","
                        "Deduction" ","
                        (nth 0 item) ","
                        "Junction" ","
                        "1" ","
                        "-" ","
                        (rtos (nth 1 bm_item) 2 1) ","
                        (rtos (nth 2 bm_item) 2 1) ","
                        "-" ","
                        (strcat "-" (rtos (nth 3 bm_item) 2 3))
                      )
                      f
                    )
                  )
                )
              )
              (close f)
              (alert (strcat "Hierarchical CSV Exported Successfully!\nPath: " csv_path))
            )
            (alert "Cannot write to CSV file. Please close it in Excel if open.")
          )
        )
      )
    )
  )
)

;; ==========================================================================
;; ULTRA-FAST & SYNTAX-CLEAN DYNAMIC TABLE GENERATOR
;; ==========================================================================
(defun c:QTO_DRAW_SHUTTER_TABLE (/ records max_sides item ent s_list n col_count
                                   p1 p2 dx dy dist default_w scale_factor
                                   acad_obj doc ms tbl row col tag_str
                                   shape_str nos_val l_val w_val h_val p_val
                                   gross_shutter res s_val existing_tbl ss i
                                   ent_tbl tbl_ename bm_ded_list bm_item total_rows
                                   beam_side_idx cur_side_num old_echo old_regen
                                   cached_records r_data)
  (vl-load-com)
  (setq records (get_active_category_records))
  (if (null records)
    (progn (alert "No records found in active category to create table!") (exit))
  )

  (setq old_echo  (getvar "CMDECHO"))
  (setq old_regen (getvar "REGENMODE"))
  (setvar "CMDECHO" 0)
  (setvar "REGENMODE" 0)

  ;; 1. Single-Pass Pre-Scan
  (setq max_sides 4)
  (setq total_rows 2)
  (setq cached_records nil)

  (foreach item records
    (setq ent (nth 10 item))
    (setq s_list (if (and ent (entget ent)) (qto_get_poly_segments ent) nil))
    (if (or (null s_list) (< (length s_list) 2))
      (setq s_list (list (nth 2 item) (nth 3 item) (nth 2 item) (nth 3 item)))
    )
    (if (> (length s_list) max_sides) (setq max_sides (length s_list)))
    
    (setq bm_ded_list (if (and ent (equal (strcase (nth 0 item)) "COLUMNS"))
                        (qto_get_column_beam_deduction_details ent)
                        nil))
    
    (setq total_rows (1+ total_rows))
    (if bm_ded_list (setq total_rows (+ total_rows (length bm_ded_list))))
    (setq cached_records (append cached_records (list (list item s_list bm_ded_list))))
  )

  ;; 2. 2-Point Selection
  (setq p1 (getpoint "\nSpecify First Corner (Table Insertion Point): "))
  (if (null p1) 
    (progn (setvar "CMDECHO" old_echo) (setvar "REGENMODE" old_regen) (exit))
  )
  (setq p2 (getcorner p1 "\nSpecify Opposite Corner (Define Table Size & Scale): "))
  (if (null p2) (setq p2 (list (+ (car p1) 5000.0) (- (cadr p1) 3000.0) 0.0)))

  (setq dx (abs (- (car p2) (car p1))))
  (setq dy (abs (- (cadr p2) (cadr p1))))
  (setq dist (max dx dy))

  (setq acad_obj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acad_obj))
  (setq ms (vla-get-ModelSpace doc))

  ;; Delete Juni Table
  (setq ss (ssget "X" (list '(0 . "ACAD_TABLE") (cons 1 (strcat "SHUTTERING SCHEDULE - " (strcase *qto_active_cat*))))))
  (if ss
    (repeat (sslength ss)
      (setq ent_tbl (ssname ss 0))
      (if ent_tbl (entdel ent_tbl))
    )
  )

  (setq col_count (+ 10 max_sides))
  (setq tbl (vla-AddTable ms (vlax-3d-point p1) total_rows col_count 220.0 650.0))

  (vl-catch-all-apply 'vla-SetRowHeight (list tbl 0 350.0))
  (vl-catch-all-apply 'vla-SetRowHeight (list tbl 1 280.0))
  (vl-catch-all-apply 'vla-SetTextHeight (list tbl 1 180.0))
  (vl-catch-all-apply 'vla-SetTextHeight (list tbl 2 130.0))
  (vl-catch-all-apply 'vla-SetTextHeight (list tbl 4 110.0))

  ;; 3. Headers
  (vla-SetText tbl 0 0 (strcat "SHUTTERING SCHEDULE - " (strcase *qto_active_cat*)))
  (vla-SetText tbl 1 0 "PARENT_TAG")
  (vla-SetText tbl 1 1 "ITEM_TAG")
  (vla-SetText tbl 1 2 "TYPE")
  (vla-SetText tbl 1 3 "SHAPE")
  (vla-SetText tbl 1 4 "NOS")
  (vla-SetText tbl 1 5 "L (m)")
  (vla-SetText tbl 1 6 "W (m)")
  (vla-SetText tbl 1 7 "H (m)")
  (vla-SetText tbl 1 8 "PERIMETER (m)")

  (setq col 9)
  (repeat max_sides
    (vla-SetText tbl 1 col (strcat "S" (itoa (- col 8)) " (m)"))
    (setq col (1+ col))
  )
  (vla-SetText tbl 1 col "SHUTTER (m2)")

  ;; 4. Data Rows
  (setq row 2)
  (foreach r_data cached_records
    (setq item        (nth 0 r_data))
    (setq s_list      (nth 1 r_data))
    (setq bm_ded_list (nth 2 r_data))

    (setq tag_str   (nth 1 item))
    (setq shape_str (nth 7 item))
    (setq nos_val   (if (nth 8 item) (nth 8 item) 1))
    (setq l_val     (nth 2 item))
    (setq w_val     (nth 3 item))
    (setq h_val     (nth 5 item))
    (setq p_val     (apply '+ s_list))
    (setq gross_shutter (* (if (> p_val 50.0) (/ p_val 1000.0) p_val) 
                           (if (> h_val 50.0) (/ h_val 1000.0) h_val) 
                           nos_val))

    ;; Main Row
    (vla-SetText tbl row 0 tag_str)
    (vla-SetText tbl row 1 tag_str)
    (vla-SetText tbl row 2 "Gross")
    (vla-SetText tbl row 3 shape_str)
    (vla-SetText tbl row 4 (itoa nos_val))
    (vla-SetText tbl row 5 (rtos l_val 2 2))
    (vla-SetText tbl row 6 (rtos w_val 2 2))
    (vla-SetText tbl row 7 (rtos h_val 2 2))
    (vla-SetText tbl row 8 (rtos p_val 2 2))

    (setq col 9)
    (setq i 0)
    (repeat max_sides
      (if (< i (length s_list))
        (setq s_val (rtos (nth i s_list) 2 2))
        (setq s_val "-")
      )
      (vla-SetText tbl row col s_val)
      (setq col (1+ col))
      (setq i (1+ i))
    )
    (vla-SetText tbl row col (rtos gross_shutter 2 3))
    (setq row (1+ row))

    ;; Deduction Sub-Rows
    (if bm_ded_list
      (foreach bm_item bm_ded_list
        (setq beam_side_idx (nth 4 bm_item))
        (vla-SetText tbl row 0 tag_str)
        (vla-SetText tbl row 1 (strcat "Deduct: " (nth 0 bm_item)))
        (vla-SetText tbl row 2 "Deduction")
        (vla-SetText tbl row 3 "Junction")
        (vla-SetText tbl row 4 "1")
        (vla-SetText tbl row 5 "-")
        (vla-SetText tbl row 6 (rtos (nth 1 bm_item) 2 1))
        (vla-SetText tbl row 7 (rtos (nth 2 bm_item) 2 1))
        (vla-SetText tbl row 8 "-")

        (setq col 9)
        (setq cur_side_num 1)
        (repeat max_sides
          (if (= cur_side_num beam_side_idx)
            (vla-SetText tbl row col (strcat "-" (rtos (nth 1 bm_item) 2 1)))
            (vla-SetText tbl row col "-")
          )
          (setq col (1+ col))
          (setq cur_side_num (1+ cur_side_num))
        )
        (vla-SetText tbl row col (strcat "-" (rtos (nth 3 bm_item) 2 3)))
        (setq row (1+ row))
      )
    )
  )

  ;; 5. Auto-Scaling
  (setq default_w (* col_count 650.0))
  (if (> dist 100.0)
    (progn
      (setq scale_factor (/ dist default_w))
      (if (< scale_factor 0.1) (setq scale_factor 1.0))
      (setq tbl_ename (vlax-vla-object->ename tbl))
      (command "._SCALE" tbl_ename "" "_non" p1 scale_factor)
    )
  )

  (setvar "CMDECHO" old_echo)
  (setvar "REGENMODE" old_regen)
  (vla-update tbl)

  (princ "\nDynamic Shuttering Table generated cleanly without error!\n")
  (princ)
)