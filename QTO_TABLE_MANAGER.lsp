;; Register Application Name for XData
(regapp "QTO_DATA_APP")

;; Global category list setup - Default List
(setq *qto_default_categories* 
  '(("Footings" "FTG" 1 "Depth") 
    ("Columns"  "COL" 4 "Height") 
    ("Beams"    "BM"  2 "Depth") 
    ("Slabs"    "SLAB" 3 "Thickness") 
    ("Coping"   "COP" 6 "Thickness"))
)

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
      ;; Ensure default categories always exist
      (foreach item *qto_default_categories*
        (if (not (assoc (car item) *qto_categories*))
          (setq *qto_categories* (append *qto_categories* (list item)))
        )
      )
    )
    (setq *qto_categories* *qto_default_categories*)
  )
)

;; Initialize Categories from DWG Database on Load
(qto_load_categories_from_dwg)
(if (not *qto_active_cat*) (setq *qto_active_cat* "Footings"))

;; Main Command: Scan DWG for saved XData first, then launch dialog
(defun c:QTOMANAGER (/ result)
  (vl-load-com)
  (regapp "QTO_DATA_APP")
  
  ;; Sync memory and categories with current DWG file
  (qto_load_categories_from_dwg)
  (qto_sync_data_from_dwg)
  
  (setq result (qto_dialog_category_select))
  (if (= result 1)
    (qto_dialog_main_manager)
  )
  (princ)
)

;; =========================================================================
;; XDATA PERSISTENCE ENGINE (READ / WRITE TO DWG OBJECTS)
;; =========================================================================

;; Attach Data directly to Polyline entity inside DWG
(defun qto_write_xdata (ent cat_name tag_name vert_dim shape_label / exdata)
  (regapp "QTO_DATA_APP")
  (setq exdata
    (list 
      (list -3
        (list "QTO_DATA_APP"
          (cons 1000 cat_name)
          (cons 1000 tag_name)
          (cons 1040 (float vert_dim))
          (cons 1000 shape_label)
        )
      )
    )
  )
  (entmod (append (entget ent) exdata))
)

;; Read DWG database and recover all saved objects into memory
(defun qto_sync_data_from_dwg (/ ss i ent obj xdata xlist cat_name tag_name vert_dim shape_label area dims len wid minpt maxpt text_pt txt_h ent_txt)
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
            (setq area (vla-get-area obj))

            (setq dims (get_accurate_len_wid ent))
            (setq len (car dims))
            (setq wid (cadr dims))

            ;; Find text entity matching key coordinates/layer
            (vla-getboundingbox obj 'minpt 'maxpt)
            (setq minpt (vlax-safearray->list minpt))
            (setq maxpt (vlax-safearray->list maxpt))
            (setq text_pt (list (/ (+ (car minpt) (car maxpt)) 2.0) (/ (+ (cadr minpt) (cadr maxpt)) 2.0) 0.0))

            (setq ent_txt (qto_find_associated_text text_pt tag_name (strcat "QTO_" (strcase cat_name))))
            
            ;; Reconstruct memory item
            (setq *qto_data* 
              (append *qto_data* 
                (list (list cat_name tag_name len wid area vert_dim (* area vert_dim) shape_label ent ent_txt))
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
)

(defun qto_find_associated_text (pt tag_str layer_name / ss i text_ent elist found)
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
      
      ;; Save Category changes into DWG file persistently
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
       ))
    )
  )

  (if (findfile dcl_file) (vl-file-delete dcl_file))
)

(defun update_active_qto_list_ui (/ total_vol display_list item vol shape_str tag_str filtered_data vert_lbl)
  (setq total_vol 0.0)
  (setq vert_lbl (strcase (substr (nth 3 (assoc *qto_active_cat* *qto_categories*)) 1 1)))
  (setq display_list (list (format_col "TAG" 10) (format_col "SHAPE" 14) (format_col "L(m)" 8) (format_col "W(m)" 8) (format_col "AREA(m2)" 10) (format_col (strcat vert_lbl "(m)") 8) (format_col "VOL(m3)" 10)))
  
  (start_list "data_list")
  (add_list (strcat (nth 0 display_list) (nth 1 display_list) (nth 2 display_list) (nth 3 display_list) (nth 4 display_list) (nth 5 display_list) (nth 6 display_list)))
  (add_list "----------------------------------------------------------------------------------")
  
  (setq filtered_data (vl-remove-if-not '(lambda (x) (equal (nth 0 x) *qto_active_cat*)) *qto_data*))

  (foreach item filtered_data
    (setq vol (nth 6 item))
    (setq total_vol (+ total_vol vol))
    (setq shape_str (nth 7 item))
    (setq tag_str (nth 1 item))
    (add_list (strcat 
      (format_col tag_str 10)
      (format_col shape_str 14)
      (format_col (rtos (nth 2 item) 2 2) 8)
      (format_col (rtos (nth 3 item) 2 2) 8)
      (format_col (rtos (nth 4 item) 2 2) 10)
      (format_col (rtos (nth 5 item) 2 2) 8)
      (format_col (rtos vol 2 3) 10)
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

(defun register_qto_entity (ent shape_label / cat_info prefix layer_name count area tag vert_dim vol minpt maxpt dims len wid text_pt txt_h ent_txt obj)
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
      (setq txt_h (max 0.2 (/ (- (car maxpt) (car minpt)) 6.0)))

      (command "_TEXT" "_J" "_MC" "_non" text_pt txt_h "0" tag)
      (setq ent_txt (entlast))
      (vla-put-layer (vlax-ename->vla-object ent_txt) layer_name)

      (setq vert_dim 0.50)
      (setq vol (* area vert_dim))

      ;; Store XData persistently inside DWG polyline entity
      (qto_write_xdata ent *qto_active_cat* tag vert_dim shape_label)

      (setq *qto_data* (append *qto_data* (list (list *qto_active_cat* tag len wid area vert_dim vol shape_label ent ent_txt))))
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

(defun pick_and_calc_rect_qto (/ p1 p2)
  (setq p1 (getpoint (strcat "\nSpecify first corner of " *qto_active_cat* ": ")))
  (if p1 (setq p2 (getcorner p1 "\nSpecify opposite corner: ")))
  (if (and p1 p2)
    (progn
      (command "_RECTANG" "_non" p1 "_non" p2)
      (register_qto_entity (entlast) "Rectangular")
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
    )
    (progn
      (setq cur_sel nil)
      (set_tile "eb_tag" "")
      (set_tile "eb_vert_dim" "")
    )
  )
)

(defun handle_apply_changes (/ records item global_idx new_tag new_v area new_vol ent_poly ent_txt txt_obj)
  (setq records (get_active_category_records))
  (if (and cur_sel (< cur_sel (length records)))
    (progn
      (setq item (nth cur_sel records))
      (setq global_idx (vl-position item *qto_data*))
      (setq new_tag (get_tile "eb_tag"))
      (setq new_v (atof (get_tile "eb_vert_dim")))
      
      (if (and (> (strlen new_tag) 0) (> new_v 0.0))
        (progn
          (setq area (nth 4 item))
          (setq new_vol (* area new_v))
          (setq ent_poly (nth 8 item))
          (setq ent_txt (nth 9 item))

          (if (and ent_txt (entget ent_txt))
            (progn
              (setq txt_obj (vlax-ename->vla-object ent_txt))
              (vla-put-textstring txt_obj new_tag)
              (vla-update txt_obj)
            )
          )

          ;; Update DWG entity XData
          (qto_write_xdata ent_poly *qto_active_cat* new_tag new_v (nth 7 item))

          (setq item (list *qto_active_cat* new_tag (nth 2 item) (nth 3 item) area new_v new_vol (nth 7 item) ent_poly ent_txt))
          (setq *qto_data* (subst_nth global_idx item *qto_data*))

          (update_active_qto_list_ui)
          (alert (strcat "Record Updated!\nTag: " new_tag "\nVolume: " (rtos new_vol 2 3) " m3"))
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
          (setq ent_poly (nth 8 item))
          (setq ent_txt  (nth 9 item))

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
            (if (and (nth 8 item) (entget (nth 8 item))) (entdel (nth 8 item)))
            (if (and (nth 9 item) (entget (nth 9 item))) (entdel (nth 9 item)))
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
      (setq ent_poly (nth 8 item))
      (setq ent_txt  (nth 9 item))

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

(defun export_all_qto_csv (/ csv_file f item)
  (if *qto_data*
    (progn
      (setq csv_file (getfiled "Export Complete QTO Schedule" "Multi_Category_Takeoff.csv" "csv" 1))
      (if csv_file
        (progn
          (setq f (open csv_file "w"))
          (write-line "CATEGORY,LAYER,TAG,SHAPE,LENGTH(m),WIDTH(m),AREA(m2),HEIGHT_DEPTH(m),VOLUME(m3)" f)
          (foreach item *qto_data*
            (write-line (strcat (nth 0 item) ","
                                "QTO_" (strcase (nth 0 item)) ","
                                (nth 1 item) "," 
                                (nth 7 item) "," 
                                (rtos (nth 2 item) 2 3) "," 
                                (rtos (nth 3 item) 2 3) "," 
                                (rtos (nth 4 item) 2 3) "," 
                                (rtos (nth 5 item) 2 3) "," 
                                (rtos (nth 6 item) 2 3)) f)
          )
          (close f)
          (alert (strcat "Takeoff Schedule Exported Successfully:\n" csv_file))
        )
      )
    )
    (alert "No takeoff data available to export.")
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