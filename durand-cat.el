;;; durand-cat --- Raise a cute cat with good habits
;;; ~/elisp_packages/durand-cat/durand-cat.el -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; This is for raising a cute cat with positive habits. In the end one would
;;; have a cute cat which marks that one has cultivated some good habits.
;;;
;;; This could be considered an extension of the org-habit package.

;;; Code:

;; for common lisp things
(require 'cl-lib)
(require 'subr-x)
(require 'dash)

;; for calculating the day of week
(require 'calendar)

;; to make compiler happy

;; (declare-function with-current-file
;;                   (expand-file-name
;;                    "modules/lang/durand-org/autoload.el"
;;                    doom-private-dir)
;;                   (FILE-NAME &optional BUFFER-NAME &rest FORM) t)

(declare-function durand-collect-shop-infos
                  (expand-file-name
                   "modules/lang/durand-org/autoload.el"
                   doom-private-dir)
                  nil t)

;;; Profile system

;; A profile is identified by its name. Its total point marks the growth of the
;; cat.
;;
;; positive-list (and negative-list) indicates the activities of this cat. Each
;; list should consist of
;; - a tag, to be printed if non-nil.
;; - a file / a list of files: each file name is relative to `org-directory'.
;; - a regexp for matching tags: this can be either a string to match or a
;;   function to determine if an entry should be included
;; - an engine for calculating points.
;;
;; We call such a list an activity

;;;###autoload
(cl-defstruct durand-cat-activity
  tag
  file
  regex
  ; HACK: This is not used. Its purpose is to distinguish positive activities
  ; from negative ones, so that we can use an associative list to look up
  ; activity information.
  positivep
  (engine 'durand-cat-plus-one-engine))

;; NOTE: We need to collect activity informations. An activity information
;; consists of
;; - file name
;; - position
;; - computed point

;;;###autoload
(cl-defstruct durand-cat-activity-information
  file position point)

;;;###autoload
(cl-defstruct durand-cat-profile
  (name "cute cat")
  (total-point 0)
  avatar
  positive-list
  negative-list)

;; alignment macro

;;;###autoload
(defmacro durand-cat-warn-profile (profile)
  "Error out if PROFILE is not a profile."
  `(unless (durand-cat-profile-p ,profile)
    (user-error "This is an invalid profile: %s, which is of type %s"
                (pp-to-string ,profile)
                (type-of ,profile))))

;;;###autoload
(defun durand-cat-center (len s)
  "Center string S in a region of length LEN."
  (concat
   (make-string
    (ceiling (max 0 (- len (length s))) 2)
    32)
   s))

;;;###autoload
(defun durand-cat-lalign (len list-of-strs)
  "Left align strings in LIST-OF-STRS in a region of length LEN."
  (let ((max-len (apply 'max (cl-loop for str in list-of-strs
                                      collect (cond
                                               ((stringp str)
                                                (length str))
                                               (t 0))))))
    (cl-loop for str in list-of-strs
             collect (concat
                      (make-string
                       (ceiling (max 0 (- len max-len)) 2)
                       32)
                      str))))

;;;###autoload
(defun durand-cat-align-first-line (len list-of-strs)
  "Align the strings in LIST-OF-STRS in a region of length LEN.
The end result is that the first line is at the center."
  (let ((first-length (cond
                       ((stringp (car list-of-strs))
                        (length (car list-of-strs)))
                       (t 0))))
    (cl-loop for str in list-of-strs
             collect (concat
                      (make-string
                       (ceiling (max 0 (- len first-length)) 2)
                       32)
                      str))))

;;;###autoload
(defun durand-cat-ralign (len list-of-strs)
  "Right align strings in LIST-OF-STRS in a region of length LEN."
  (let ((max-len (apply 'max (mapcar 'safe-length
                                     (mapcar 'string-to-list list-of-strs)))))
    (cl-loop for str in list-of-strs
             collect (concat
                      (make-string (- len
                                      (length str)
                                      (ceiling (max 0 (- len max-len)) 2))
                                   32)
                      str))))

;; Saving

;;;###autoload
(defvar durand-cat-profile-save-directory
  "/Users/durand/.emacs.d/.local/etc/durand-cat/"
  "The directory for saving profiles.")

;;;###autoload
;; (defun durand-cat-save-profile (profile)
;;   "Save PROFILE in a file.
;; The directory to save is `durand-cat-profile-save-directory'."
;;   (durand-cat-warn-profile profile)
;;   (let* ((name (durand-cat-profile-name profile))
;;          (file-name (expand-file-name name durand-cat-profile-save-directory))
;;          (data (list
;;                 :name (durand-cat-profile-name profile)
;;                 :total-point (durand-cat-profile-total-point profile)
;;                 :avatar (durand-cat-profile-avatar profile)
;;                 :positive-list (durand-cat-profile-positive-list profile)
;;                 :negative-list (durand-cat-profile-negative-list profile))))
;;     (when (file-exists-p file-name)
;;       (with-current-file file-name nil
;;         (erase-buffer)
;;         (ignore-errors (save-buffer 0))))
;;     (write-region (pp-to-string data) nil
;;                   (expand-file-name name durand-cat-profile-save-directory)
;;                   0)))

;;; Display

;;;###autoload
(defvar durand-cat-current-profile nil
  "The current profile for durand-cat.")

;; TODO: Move the picture to the same folder.
;;;###autoload
(defvar durand-cat-default-avatar "/Users/durand/.doom.d/banners/default.png"
  "The default image to use as the avatar of the profile.")

;;;###autoload
(defvar durand-cat-avatar-padding (cons 0 2)
  "Padding before and after the avatar.")

;;;###autoload
(defvar durand-cat-width 30
  "Width of the centered avatar.")

(setf durand-cat-width 30)

;;;###autoload
(defvar durand-cat-buffer-name "*durand-cat*"
  "The name of the buffer to display the profiles.")

;;;###autoload
(defvar durand-cat-center-gap 3
  "The gap between the left and the right half.")

(setf durand-cat-center-gap 5)

;; for integration with org-mode
(require 'org)
(require 'org-element)
;; for displaying the avatr
(require 'image)

;; notes regexp

;;;###autoload
(defvar org-state-regexp
  (concat
   "^\\(\\s-+\\)- State \""
   "\\(\\sw\\|\\s_\\)+"
   "\"\\s-+from\\s-+\""
   "\\(\\sw\\|\\s_\\)+"
   "\"\\s-+\\("
      "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^
>]*?\\)\\]"
   "\\).*$")
  "The regexp for state changes in an org heading.")

;;;###autoload
(defvar org-close-note-regexp
  (concat
   "^\\s-+- CLOSING NOTE "
   "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^
>]*?\\)\\]"
   " ..\n\\s-+\\([0-9]*\\)")
  "The regexp for logging notes in an org heading.")

;;;###autoload
(defun durand-cat-format-file (file)
  "Format FILE in a pretty way.
FILE can be either a string or a list of strings."
  (mapcar
   (lambda (x)
     (propertize x
                 'face '(:foreground "SkyBlue1")))
   (cond
    ((stringp file)
     (list file))
    ((and (listp file)
          (cl-every 'stringp file))
     (cond ((> (length file) 1)
            (append
             (cl-loop for f in (reverse (cddr (reverse file)))
                      collect (concat f ","))
             (list (concat (nth (- (length file) 2) file) ", and"))
             (list (-last-item file))))
           (t
            file)))
    (t
     (user-error "Wrong type argument: %s, of type: %s"
                 (pp-to-string file)
                 (type-of file))))))

;;;###autoload
(defun durand-cat-format-activity (activity &optional point)
  "Format ACTIVITY with score POINT in a pretty way."
  (let* ((tag (or (durand-cat-activity-tag activity)
                  (durand-cat-activity-regex activity)))
         (file (durand-cat-activity-file activity))
         (regex (durand-cat-activity-regex activity))
         (engine (durand-cat-activity-engine activity))
         (positivep (durand-cat-activity-positivep activity))
         (formated-files (durand-cat-format-file file))
         (file-pluralp (cond ((and (listp file)
                                   (> (length file) 1))
                              "s")
                             (t " ")))
         (file-desc-text (format "  in file%s " file-pluralp)))
    (append
     (list
      (propertize
       (concat
        "  Activity "
        (propertize (format "«%s»" tag)
                    'face '(:foreground "orange"))
        (when point
          (propertize
           (format ": %d" point)
           'face 'success)))
       'tag tag
       'positivep positivep
       'file file
       'regex regex
       'engine engine)
      (propertize
       (concat
        file-desc-text
        (car formated-files))
       'tag tag
       'positivep positivep
       'file file
       'regex regex
       'engine engine))
     (when (> (length file) 1)
       (cl-loop for f in (cdr formated-files)
                collect (propertize
                         (concat (make-string (length file-desc-text) 32) f)
                         'tag tag
                         'positivep positivep
                         'file file
                         'regex regex
                         'engine engine)))
     (list
      (propertize
       (concat
        "  macthing :"
        (propertize (cond
                     ((stringp regex) regex)
                     ((functionp regex) (pp-to-string regex)))
                    'face '(:foreground "IndianRed1"))
        ":")
       'tag tag
       'positivep positivep
       'file file
       'regex regex
       'engine engine)))))

;;;###autoload
(defun durand-cat-strip-new-line (str)
  "Return a copy of STR without newlines at the end."
  (cond
   ((string-match "\n+$" str) (replace-match "" nil nil str))
   (t str)))

;;;###autoload
(defvar durand-cat-profiles nil
  "The list of profiles to use.")

;;;###autoload
(defvar durand-cat-header-line-format nil
  "The header format that should be used by `durand-cat-mode'.")

;;;###autoload
(defvar durand-cat-total-information nil
  "An associative list for looking up activity information.")

;;;###autoload
(defun durand-cat-associative-listification (a-list &optional key value)
  "Return an associative list version of A-LIST by KEY and VALUE.
KEY defaults to `car', and VALUE defaults to `cdr'."
  (let ((key (or key 'car))
        (value (or value 'cdr)))
    (cl-loop for element in a-list
             collect (cons (funcall key element)
                           (funcall value element)))))

;;;###autoload
(defun durand-cat-gather-data-together (list-of-activities)
  "Gather all needed information for LIST-OF-ACTIVITIES in one go.
Returns an alist whose keys are the activitites and whose values
are lists of activity informations."
  (let (file-combined information)
    (cl-loop for activity in list-of-activities
             for file = (durand-cat-activity-file activity)
             do (cond
                 ((stringp file)
                  (setf (alist-get file file-combined nil nil 'string=)
                        (append (alist-get file file-combined nil nil 'string=)
                                (list activity))))
                 ((listp file)
                  (cl-loop for file-name in file
                           do (setf (alist-get file-name file-combined nil nil 'string=)
                                    (append
                                     (alist-get file-name file-combined nil nil 'string=)
                                     (list activity)))))
                 (t
                  (user-error "FILE should be either a string or a list of strings, but got %s, of type %s"
                              (pp-to-string file)
                              (type-of file)))))
    (cl-loop for spec in file-combined
             for file = (car spec)
             for activities = (cdr spec)
             do (with-temp-buffer
                  (insert-file-contents (expand-file-name file org-directory))
                  (org-mode)
                  (save-excursion
                    (goto-char (point-min))
                    (while (re-search-forward org-heading-regexp nil t)
                      (cl-loop for act in activities
                               for regex = (durand-cat-activity-regex act)
                               for engine = (durand-cat-activity-engine act)
                               do (when (cond
                                         ((stringp regex)
                                          (cl-member regex (org-get-tags) :test 'string=))
                                         ((functionp regex)
                                          (funcall regex))
                                         (t (user-error "Regex %s is not a string nor a function, but a %s"
                                                        (pp-to-string regex)
                                                        (type-of regex))))
                                    (setf (alist-get act information nil nil 'equal)
                                          (append
                                           (alist-get act information nil nil 'equal)
                                           (list
                                            (make-durand-cat-activity-information
                                             :file file
                                             :position (point)
                                             :point (funcall engine)))))))))))
    information))

;;;###autoload
(defun durand-cat-profile-display-dashboard (profile)
  "Insert the information associated with PROFILE into `durand-cat-buffer-name'."
  (interactive (list (eval (car durand-cat-profiles))))
  (durand-cat-warn-profile profile)
  (unless (get-buffer durand-cat-buffer-name)
    (get-buffer-create durand-cat-buffer-name))
  (let ((inhibit-read-only t))
    (with-current-buffer durand-cat-buffer-name
      ;; first erase buffer
      (erase-buffer)
      (insert (make-string (car durand-cat-avatar-padding) 10))
      ;; avatar
      (let ((original-point (point)))
        (insert (durand-cat-profile-name profile))
        (add-text-properties original-point (point)
                             (list 'display
                                   (create-image
                                    (or (durand-cat-profile-avatar profile)
                                        durand-cat-default-avatar))
                                   'rear-nonsticky '(display)))
        (save-excursion
          (goto-char original-point)
          (insert (make-string (/ (- (window-body-width) durand-cat-width) 2) 32)))
        (insert (make-string (cdr durand-cat-avatar-padding) 10)))
      (let* ((pos-ls (durand-cat-profile-positive-list profile))
             (neg-ls (durand-cat-profile-negative-list profile))
             (pos-information (durand-cat-gather-data-together pos-ls))
             (neg-information (durand-cat-gather-data-together neg-ls))
             (total-information (append pos-information neg-information))
             (pos-total (cl-loop for information in pos-information
                                 sum (cl-loop for act-info in (cdr information)
                                              sum (durand-cat-activity-information-point act-info))))
             (neg-total (cl-loop for information in neg-information
                                 sum (cl-loop for act-info in (cdr information)
                                              sum (durand-cat-activity-information-point act-info))))
             (pos-activity-point-list
              (cl-loop for spec in pos-information
                       collect
                       (cons (car spec)
                             (cl-loop for inf in (cdr spec)
                                      sum (durand-cat-activity-information-point inf)))))
             (neg-activity-point-list
              (cl-loop for spec in neg-information
                       collect
                       (cons (car spec)
                             (cl-loop for inf in (cdr spec)
                                      sum (durand-cat-activity-information-point inf))))))
        ;; set up information look up alist.
        (setf durand-cat-total-information total-information)
        ;; insert pos and neg
        (let (display-list pos-display-list neg-display-list)
          (push (format "Total Points: %s\n"
                        (propertize
                         (number-to-string (- pos-total neg-total))
                         'face 'success))
                display-list)
          (push (list
                 (format "Positive: %s"
                         (propertize
                          (number-to-string pos-total)
                          'face 'success)))
                pos-display-list)
          (push (list
                 (format "Negative: %s"
                         (propertize
                          (number-to-string neg-total)
                          'face 'success)))
                neg-display-list)
          (setf pos-display-list
                (append
                 pos-display-list
                 (cl-loop for pos in pos-ls
                          collect (durand-cat-format-activity
                                   pos
                                   (alist-get pos pos-activity-point-list nil nil 'equal))))
                neg-display-list
                (append
                 neg-display-list
                 (cl-loop for neg in neg-ls
                          collect (durand-cat-format-activity
                                   neg
                                   (alist-get neg neg-activity-point-list nil nil 'equal)))))
          ;; NOTE: Zip the two lists. First make sure they have the same lengths.
          (let ((max-length (max (length pos-display-list)
                                 (length neg-display-list))))
            (cond ((< (length pos-display-list) max-length)
                   (setf pos-display-list
                         (append pos-display-list
                                 (make-list (- max-length (length pos-display-list))
                                            (list "")))))
                  ((< (length neg-display-list) max-length)
                   (setf neg-display-list
                         (append neg-display-list
                                 (make-list (- max-length (length neg-display-list))
                                            (list "")))))))
          ;; Then zip
          (let ((max-line-length (cl-loop
                                  for ls-str in pos-display-list
                                  maximize (cl-loop for str in ls-str
                                                    maximize (length str)))))
            (cl-loop
             for i from 0 to (- (length pos-display-list) 1)
             for ith-pos = (nth i pos-display-list)
             for ith-neg = (nth i neg-display-list)
             for max-block-len = (max (length ith-pos) (length ith-neg))
             ;; NOTE: Make each block have the same length
             do (cond ((< (length ith-pos) max-block-len)
                       (setf ith-pos
                             (append ith-pos
                                     (make-list (- max-block-len (length ith-pos)) ""))))
                      ((< (length ith-neg) max-block-len)
                       (setf ith-neg
                             (append ith-neg
                                     (make-list (- max-block-len (length ith-neg)) "")))))
             do (setf display-list
                      (append
                       display-list
                       (cl-loop for j from 0 to (- max-block-len 1)
                                for to-add = (concat
                                              (nth j ith-pos)
                                              (make-string
                                               (+ durand-cat-center-gap
                                                  (- max-line-length (length (nth j ith-pos))))
                                               32)
                                              (nth j ith-neg)
                                              "\n")
                                collect (propertize to-add
                                                    'block-num i))
                       (list "\n")))))
          (cl-loop for str in (append
                               (list (durand-cat-center (window-body-width) (car display-list)))
                               (durand-cat-align-first-line
                                (window-body-width)
                                (cdr display-list)))
                   do (insert str))))
      (goto-char (point-min))))
  ;; switch to it instead of displaying it.
  (switch-to-buffer durand-cat-buffer-name)
  ;; set header format
  (setf durand-cat-header-line-format (propertize
                                       (durand-cat-center
                                        (ceiling (window-body-width) 1.5)
                                        (durand-cat-profile-name profile))
                                       'face '(:foreground "gold" :height 300)))
  ;; a dedicated major mode
  (durand-cat-mode))

;;;###autoload
(defun durand-cat-toggle-details ()
  "Hide or show the details about files and regexps."
  (interactive)
  (let ((inhibit-read-only t)
        (current-block-num 1)
        at-the-end block-start block-end)
    (save-excursion
      (goto-char (point-min))
      (while (null at-the-end)
        (while (and
                (or (null (get-char-property (point) 'block-num))
                    (< (get-char-property (point) 'block-num) current-block-num))
                (/= (point) (point-max)))
          (forward-char 1))
        (when (= (point) (point-max))
          (setf at-the-end t))
        (cl-incf current-block-num)
        (setf block-start (line-beginning-position 2)
              block-end
              (progn
                (while (and
                        (or (null (get-char-property (point) 'block-num))
                            (< (get-char-property (point) 'block-num) current-block-num))
                        (/= (point) (point-max)))
                  (forward-char))
                (- (line-beginning-position) 1)))
        (cond ((null (get-char-property block-start 'display))
               (add-text-properties block-start block-end
                                    '(display "")))
              (t
               (add-text-properties block-start block-end
                                    '(display nil))))))))

;;;###autoload
(defun durand-cat-show-every-item ()
  "Show every heading associated to the activity."
  (interactive)
  (unless (get-char-property (point) 'tag)
    (user-error "No activity associated to the point"))
  (let* ((tag (get-char-property (point) 'tag))
         (file (get-char-property (point) 'file))
         (regex (get-char-property (point) 'regex))
         (engine (get-char-property (point) 'engine))
         (positivep (get-char-property (point) 'positivep))
         (associated-act (make-durand-cat-activity
                          :tag tag
                          :file file
                          :regex regex
                          :positivep positivep
                          :engine engine))
         (associated-info (alist-get associated-act durand-cat-total-information
                                     nil nil 'equal))
         file-combined)
    (cl-loop
     for info in associated-info
     for file = (durand-cat-activity-information-file info)
     do (setf (alist-get file file-combined nil nil 'string=)
              (append (alist-get file file-combined nil nil 'string=)
                      (list (list (durand-cat-activity-information-position info)
                                  (durand-cat-activity-information-point info))))))
    (with-current-buffer-window
     "*durand-cat-every-item*" nil nil
     (erase-buffer)
     (goto-char (point-min))
     (insert (format "Below are all headings for the activity «%s»\n"
                     (propertize tag 'face '(:foreground "orange"))))
     (cl-loop for spec in file-combined
              for headings = (with-temp-buffer
                               (insert-file-contents (expand-file-name (car spec) org-directory))
                               (cl-loop for pos-and-point in (cdr spec)
                                        collect (progn
                                                  (goto-char (car pos-and-point))
                                                  (list
                                                   (buffer-substring
                                                    (line-beginning-position)
                                                    (car pos-and-point))
                                                   (line-beginning-position)
                                                   (cadr pos-and-point)))))
              do (progn
                   (insert (propertize
                            (format "File: %s\n" (car spec))
                            'file (expand-file-name (car spec) org-directory)))
                   (cl-loop for heading in headings
                            do (insert (format "%d points:\n" (caddr heading)))
                            do (insert (propertize
                                        (car heading)
                                        'file (expand-file-name (car spec) org-directory)
                                        'pos (cadr heading))
                                       "\n"))
                   (insert "\n")))
     (durand-cat-every-item-mode))))

;;; major mode for every item

;;;###autoload
(define-derived-mode durand-cat-every-item-mode org-mode "Every heading"
  "For jumping directly to the found heading.")

;;;###autoload
(defun durand-cat-every-item-jump ()
  "Jump to the corresponding heading or file."
  (interactive)
  (unless (and (get-char-property (point) 'file)
               (stringp (get-char-property (point) 'file)))
    (user-error "No file or heading here"))
  (let ((pos (get-char-property (point) 'pos)))
    (find-file (get-char-property (point) 'file))
    (when (and pos (numberp pos) (> pos 0))
      (goto-char pos)
      (org-show-entry)
      (recenter 0))))

;;;###autoload
(defun durand-cat-every-item-quit ()
  "Kill the every-item buffer and go to durand-cat buffer."
  (interactive)
  (kill-current-buffer)
  (cond
   ((get-buffer durand-cat-buffer-name)
    (switch-to-buffer durand-cat-buffer-name))
   (t (user-error "No durand-cat buffer available!"))))

;;;###autoload
(defun durand-cat-back-to-group-begin ()
  "Go back to the begin of an activity block."
  (when (get-char-property (point) 'tag)
    (goto-char (previous-single-property-change (point) 'tag))
    (skip-syntax-forward " " (line-end-position))))

;;;###autoload
(defun durand-cat-down-activity ()
  "Go to the activity group downward in the same positive or negative category."
  (interactive)
  (when (looking-at "Activity")
    (forward-word))
  (cond
   ((get-char-property (point) 'block-num)
    (let ((current-polarity (get-char-property (point) 'positivep)))
      (unless (re-search-forward "Activity" nil t)
        (goto-char (point-min))
        (re-search-forward "Activity" nil t))
      (unless (eq current-polarity (get-char-property (point) 'positivep))
        (unless (re-search-forward "Activity" nil t)
          (goto-char (point-min))
          (re-search-forward "Activity" nil t))
        (unless (eq current-polarity (get-char-property (point) 'positivep))
          (re-search-forward "Activity" nil t)))))
   (t
    (unless (re-search-forward "Activity" nil t)
      (goto-char (point-min))
      (re-search-forward "Activity" nil t))))
  (durand-cat-back-to-group-begin))

;;;###autoload
(defun durand-cat-up-activity ()
  "Go to the activity group upward in the same positive or negative category."
  (interactive)
  (when (looking-back "Activity" (- (point) 8))
    (forward-word -1))
  (cond
   ((get-char-property (point) 'block-num)
    (let ((current-polarity (get-char-property (point) 'positivep)))
      (unless (re-search-backward "Activity" nil t)
        (goto-char (point-max))
        (re-search-backward "Activity" nil t))
      (unless (eq current-polarity (get-char-property (point) 'positivep))
        (unless (re-search-backward "Activity" nil t)
          (goto-char (point-max))
          (re-search-backward "Activity" nil t))
        (unless (eq current-polarity (get-char-property (point) 'positivep))
          (re-search-backward "Activity" nil t)))))
   (t
    (unless (re-search-backward "Activity" nil t)
      (goto-char (point-max))
      (re-search-backward "Activity" nil t))))
  (durand-cat-back-to-group-begin))

;;;###autoload
(defun durand-cat-next-activity ()
  "Go to the next activity group."
  (interactive)
  (when (looking-at "Activity")
    (forward-word))
  (unless (re-search-forward "Activity" nil t)
    (goto-char (point-min))
    (re-search-forward "Activity" nil t))
  (durand-cat-back-to-group-begin))

;;;###autoload
(defun durand-cat-previous-activity ()
  "Go to the previous activity group."
  (interactive)
  (when (looking-back "Activity" (- (point) 8))
    (forward-word -1))
  (unless (re-search-backward "Activity" nil t)
    (goto-char (point-max))
    (re-search-backward "Activity" nil t))
  (durand-cat-back-to-group-begin))

;;;###autoload
(defun durand-cat-left-activity ()
  "Move to the activity on the left side.
Since there are only two columns, this is the same as
`durand-cat-right-activity'."
  (interactive)
  (when (looking-back "Activity" (- (point) 8))
    (forward-word -1))
  (unless (re-search-backward "Activity" (line-beginning-position) t)
    (goto-char (line-end-position))
    (re-search-backward "Activity" (line-beginning-position) t))
  (durand-cat-back-to-group-begin))

;;;###autoload
(defun durand-cat-right-activity ()
  "Move to the activity on the left side.
Since there are only two columns, this is the same as
`durand-cat-left-activity'."
  (interactive)
  (when (looking-at "Activity")
    (forward-word))
  (unless (re-search-forward "Activity" (line-end-position) t)
    (goto-char (line-beginning-position))
    (re-search-forward "Activity" (line-end-position) t))
  (durand-cat-back-to-group-begin))

;;; Engines

;;;###autoload
(defun durand-cat-plus-one-engine ()
  "Count each state change as having point 1.
It is expected to be executed with point at a heading."
  (let ((limit (save-excursion (outline-next-heading) (point))))
    (+ (save-excursion
         (cl-loop while (re-search-forward org-state-regexp limit t)
                  sum 1))
       (save-excursion
         (cl-loop while (re-search-forward org-close-note-regexp limit t)
                  sum 1)))))

;;;###autoload
(defvar durand-cat-default-running-rounds 6
  "The average number of rounds that I run without specific note.")

;;;###autoload
(defun durand-cat-running-engine ()
  "Count each state change as point 1 or the number in the note."
  (let ((limit (save-excursion (outline-next-heading) (point))))
    (+ (save-excursion
         (cl-loop while (re-search-forward org-close-note-regexp limit t)
                  sum (cond
                       ((/= (string-to-number (match-string-no-properties 2)) 0)
                        (string-to-number (match-string-no-properties 2)))
                       (t durand-cat-default-running-rounds))))
       (save-excursion
         (cl-loop while (re-search-forward org-state-regexp limit t)
                  sum durand-cat-default-running-rounds)))))

;;;###autoload
(defvar org-clock-info-regexp
  (concat
   "^\\s-*CLOCK: "
   "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^
>]*?\\)\\]"
   "--"
   "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^
>]*?\\)\\]"
   "\\s-+=>\\s-+\\([0-9]+:[0-9]+\\)")
  "Regexp matching a clocking information.
The time spent is matched by the third sub-exp.
The first sub-exp matches the starting time, and the second sub-exp the ending time.")

;;;###autoload
(defun durand-cat-time-less-or-equal (t1 t2)
  "Return non-nil if T1 is earlier than or equal to T2.
A nil value for either argument stands for the current time.
See `current-time-string' for the various forms of a time value."
  (or (time-less-p t1 t2)
      (= (float-time (time-subtract t2 t1)) 0.0)))

;;;###autoload
(defun durand-cat-time-span-engine (&optional unit)
  "Count the time spent on the entry with unit UNIT.
This requires to be executed at an org heading.
Default unit is 'min.
Supported UNITs are 'min, 'sec, 'hour, 'day, 'week, 'month, and 'year.

This returns the largest integer such that after so many UNITs the time passes the spent time."
  (let ((unit (or unit 'min)))
    (unless (save-excursion
              (goto-char (line-beginning-position))
              (looking-at org-heading-regexp))
      (user-error "Executed not at the beginning of an org heading"))
    (unless (cl-member unit (list 'sec 'min 'hour 'day 'week 'month 'year)
                       :test 'eq)
      (user-error "Unsupported unit: %s" (pp-to-string unit)))
    (save-excursion
      (let* ((limit (save-excursion (outline-next-heading) (point)))
             (spent-time-list
              (cl-loop
               while (re-search-forward org-clock-info-regexp limit t)
               collect (list
                        (save-match-data
                          (apply 'encode-time
                                 (-take 6 (org-parse-time-string (match-string-no-properties 1)))))
                        (save-match-data
                          (apply 'encode-time
                                 (-take 6 (org-parse-time-string (match-string-no-properties 2))))))))
             (counter-time (caar spent-time-list))
             (total-time (caar spent-time-list))
             (counter 0)
             (starting t))
        (cl-loop for time-range in spent-time-list
                 for delta = (time-subtract (cadr time-range)
                                            (car time-range))
                 do (setf total-time (time-add total-time
                                               delta)))
        (cl-loop while (durand-cat-time-less-or-equal counter-time total-time)
                 do (cl-destructuring-bind (sec min hour day month year _ _ _) (decode-time counter-time)
                      (pcase unit
                        ('sec (cl-incf sec))
                        ('min (cl-incf min))
                        ('hour (cl-incf hour))
                        ('day (cl-incf day))
                        ('week (let ((day-of-week (if starting
                                                      (calendar-day-of-week (list month day year))
                                                    0)))
                                 (cl-incf day (- 7 day-of-week))))
                        ('month (cl-incf month))
                        ('year (cl-incf year)))
                      (setf starting nil)
                      (setf counter-time
                            (encode-time sec min hour day month year))
                      (when (durand-cat-time-less-or-equal counter-time total-time)
                        (cl-incf counter)))
                 finally return counter)))))

;;;###autoload
(defun durand-cat-schedule-delay-engine ()
  "Count the days between the scheduled day and today.
If it starts today, then it is counted as one day."
  (unless (save-excursion
            (goto-char (line-beginning-position))
            (looking-at org-heading-regexp))
    (user-error "Executed not at the beginning of an org heading"))
  (let* ((scheduled-time (org-entry-get (point) "SCHEDULED"))
         (subtracted-days (-
                           (time-to-days (current-time))
                           (time-to-days
                            (apply 'encode-time
                                   (-take 6 (org-parse-time-string scheduled-time)))))))
    (max 0 (+ subtracted-days 1))))

;;;###autoload
(defun durand-cat-account-predicate ()
  "Determine if this entry is a valid account.
In fact this just examines if the level is 4."
  (= (car (org-heading-components)) 4))

;;;###autoload
(define-derived-mode durand-cat-mode special-mode "Durand Cat"
  "The major mode for viewing the cute cats.")

;;; add a hook to set up the header format

;;;###autoload
(defun durand-cat-set-header ()
  "Set up the header line format according to `durand-cat-header-line-format'."
  (setf header-line-format durand-cat-header-line-format))

;;;###autoload
(defun durand-cat-set-cursor ()
  "Set up the cursor type."
  (setq cursor-type nil))

(add-hook 'durand-cat-mode-hook 'durand-cat-set-header)
;; (add-hook 'durand-cat-mode-hook 'durand-cat-set-cursor t)
(add-hook 'durand-cat-mode-hook 'durand-cat-toggle-details)
(add-hook 'durand-cat-mode-hook 'durand-cat-next-activity)
(add-hook 'durand-cat-mode-hook 'doom-modeline-set-minimal-modeline)

;; define profiles

;;;###autoload
(cl-defun durand-cat-def-profile (name
                                  &key
                                  positive-list
                                  negative-list
                                  total-point
                                  avatar)
  "Define a profile for durand-cat, called NAME.
The parameters POSITIVE-LIST and NEGATIVE-LIST are the
constituents of the profile.
They are lists of keys of the activity, not actual activities,
for convenience.
The parameters TOTAL-POINT and AVATAR are not necessary, and are
not recommended to set unless you know what you are doing."
  (let ((profile (intern (concat "durand-cat-" name "-profile"))))
    (set profile (make-durand-cat-profile
                  :name name
                  :total-point total-point
                  :avatar avatar
                  :positive-list (cl-loop
                                  for raw in positive-list
                                  collect (apply 'make-durand-cat-activity
                                                 (append
                                                  raw
                                                  (list :positivep t))))
                  :negative-list (cl-loop
                                  for raw in negative-list
                                  collect (apply 'make-durand-cat-activity
                                                 raw))))
    (cond
     ((memq profile durand-cat-profiles)
      (message "The profile %s is overwritten." name))
     (t (setf durand-cat-profiles
              (append durand-cat-profiles
                      (list profile)))))))

;;; main profile
(durand-cat-def-profile "main"
                        :positive-list
                        '((
                           :tag "running"
                           :file ("aujourdhui.org"
                                  "aujourdhui.org_archive")
                           :regex "run")
                          (
                           :tag "clean the house"
                           :file "agenda.org"
                           :regex "clean_house")
                          (
                           :tag "reading"
                           :file "notes.org"
                           :regex "lire"
                           :engine (lambda () (durand-cat-time-span-engine 'hour))))
                        :negative-list
                        '((
                           :tag "YouTube"
                           :file ("notes.org"
                                  "notes.org_archive")
                           :regex "youtube"
                           :engine durand-cat-time-span-engine)
                          (
                           :tag "No job"
                           :file "aujourdhui.org"
                           :regex "no_job"
                           :engine durand-cat-schedule-delay-engine)))

;;* account section

;;; engine

;;;###autoload
(defun durand-cat-account-engine-pos ()
  "Count the incomes."
  0)

;;;###autoload
(defvar durand-cat-account-cost-sources '("Cash")
  "The sources for cost.")

;;;###autoload
(defun durand-cat-account-engine-neg ()
  "Count the cost."
  (let* ((info (-last-item (durand-collect-shop-infos)))
         (cost (cl-loop
                for item in info
                when (cl-member (car item) durand-cat-account-cost-sources
                                :test 'string=)
                sum (- (cdr item)))))
    (or cost
        0)))

;;; account profile
(durand-cat-def-profile "account"
                        :positive-list nil
                        :negative-list
                        '((
                           :tag "costs"
                           :file "account/account.org"
                           :regex durand-cat-account-predicate
                           :engine durand-cat-account-engine-neg)))

;;; switch profiles

;;;###autoload
(defun durand-cat-switch-profile ()
  "Switch to a different profile in `durand-cat-profiles'."
  (interactive)
  (durand-cat-profile-display-dashboard
   (eval (intern (completing-read "Choose a profile:" durand-cat-profiles nil t)))))

(provide 'durand-cat)
;;; durand-cat.el ends here
