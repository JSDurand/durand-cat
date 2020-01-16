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

(declare-function with-current-file
                  (expand-file-name
                   "modules/lang/durand-org/autoload.el"
                   doom-private-dir)
                  (FILE-NAME &optional BUFFER-NAME &rest FORM) t)

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
  (engine 'durand-cat-plus-one-engine))

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
(defun durand-cat-save-profile (profile)
  "Save PROFILE in a file.
The directory to save is `durand-cat-profile-save-directory'."
  (durand-cat-warn-profile profile)
  (let* ((name (durand-cat-profile-name profile))
         (file-name (expand-file-name name durand-cat-profile-save-directory))
         (data (list
                :name (durand-cat-profile-name profile)
                :total-point (durand-cat-profile-total-point profile)
                :avatar (durand-cat-profile-avatar profile)
                :positive-list (durand-cat-profile-positive-list profile)
                :negative-list (durand-cat-profile-negative-list profile))))
    (when (file-exists-p file-name)
      (with-current-file file-name nil
        (erase-buffer)
        (ignore-errors (save-buffer 0))))
    (write-region (pp-to-string data) nil
                  (expand-file-name name durand-cat-profile-save-directory)
                  0)))

;;; Display

;;;###autoload
(defvar durand-cat-current-profile nil
  "The current profile for durand-cat.")

;; TODO: Move the picture to the same folder.
;;;###autoload
(defvar durand-cat-default-avatar "/Users/durand/.doom.d/banners/default.png"
  "The default image to use as the avatar of the profile.")

;;;###autoload
(defvar durand-cat-avatar-padding (cons 3 1)
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
   org-element--timestamp-regexp
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
           'face 'success))
        "\n")
       'tag tag
       'file file
       'regex regex
       'engine engine)
      (propertize
       (concat
        file-desc-text
        (car formated-files)
        "\n")
       'tag tag
       'file file
       'regex regex
       'engine engine))
     (when (> (length file) 1)
       (cl-loop for f in (cdr formated-files)
                collect (propertize
                         (concat
                          (make-string (length file-desc-text) 32)
                          f)
                         'tag tag
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
        ":\n\n")
       'tag tag
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
      (let ((pos-ls (durand-cat-profile-positive-list profile))
            (neg-ls (durand-cat-profile-negative-list profile))
            (pos-total 0)
            (neg-total 0)
            temp
            pos-activity-point-list neg-activity-point-list)
        ;; positive list
        (cl-loop
         for pos in pos-ls
         for pos-file = (durand-cat-activity-file pos)
         for pos-engine = (durand-cat-activity-engine pos)
         for pos-regex = (durand-cat-activity-regex pos)
         do (cond
             ((stringp pos-file)
              (setf temp
                    (with-current-file (expand-file-name pos-file org-directory) nil
                      (cond
                       ((stringp pos-regex)
                        (apply '+ (org-map-entries pos-engine pos-regex)))
                       ((functionp pos-regex)
                        (apply '+ (cl-remove-if
                                   'null
                                   (org-map-entries
                                    (lambda ()
                                      (when (funcall pos-regex)
                                        (funcall pos-engine)))))))))))
             ((listp pos-file)
              (setf temp
                    (cl-loop for file in pos-file
                             sum (with-current-file (expand-file-name file org-directory) nil
                                   (cond
                                    ((stringp pos-regex)
                                     (apply '+ (org-map-entries pos-engine pos-regex)))
                                    ((functionp pos-regex)
                                     (apply '+ (cl-remove-if
                                                'null
                                                (org-map-entries
                                                 (lambda ()
                                                   (when (funcall pos-regex)
                                                     (funcall pos-engine))))))))))))
             (t
              (user-error "Unknown file type: %s, which is of type: %s"
                          (pp-to-string pos-file)
                          (type-of pos-file))))
         do (cl-incf pos-total temp)
         do (setf pos-activity-point-list
                  (append
                   pos-activity-point-list
                   (list temp))))
        ;; negative list
        (cl-loop
         for neg in neg-ls
         for neg-file = (durand-cat-activity-file neg)
         for neg-engine = (durand-cat-activity-engine neg)
         for neg-regex = (durand-cat-activity-regex neg)
         do (cond
             ((stringp neg-file)
              (setf temp
                    (with-current-file (expand-file-name neg-file org-directory) nil
                      (cond
                       ((stringp neg-regex)
                        (apply '+ (org-map-entries neg-engine neg-regex)))
                       ((functionp neg-regex)
                        (apply '+ (cl-remove-if
                                   'null
                                   (org-map-entries
                                    (lambda ()
                                      (when (funcall neg-regex)
                                        (funcall neg-engine)))))))))))
             ((listp neg-file)
              (setf temp
                    (cl-loop for file in neg-file
                             sum (with-current-file (expand-file-name file org-directory) nil
                                   (cond
                                    ((stringp neg-regex)
                                     (apply '+ (org-map-entries neg-engine neg-regex)))
                                    ((functionp neg-regex)
                                     (apply '+ (cl-remove-if
                                                'null
                                                (org-map-entries
                                                 (lambda ()
                                                   (when (funcall neg-regex)
                                                     (funcall neg-engine))))))))))))
             (t
              (user-error "Unknown file type: %s, which is of type: %s"
                          (pp-to-string neg-file)
                          (type-of neg-file))))
         do (cl-incf neg-total temp)
         do (setf neg-activity-point-list
                  (append
                   neg-activity-point-list
                   (list temp))))
        ;; insert pos and neg
        (let (display-list pos-display-list neg-display-list)
          (push (format "Positive: %s\n"
                        (propertize
                         (number-to-string pos-total)
                         'face 'success))
                pos-display-list)
          (push (format "Negative: %s\n"
                        (propertize
                         (number-to-string neg-total)
                         'face 'success))
                neg-display-list)
          (cl-loop for pos in pos-ls
                   do (setf pos-display-list
                            (append
                             pos-display-list
                             (durand-cat-format-activity pos (pop pos-activity-point-list))
                             (list "\n"))))
          (cl-loop for neg in neg-ls
                   do (setf neg-display-list
                            (append
                             neg-display-list
                             (durand-cat-format-activity neg (pop neg-activity-point-list))
                             (list "\n"))))
          ;; Zip the two lists. First make sure they have the same lengths.
          (let ((max-length (max (length pos-display-list)
                                 (length neg-display-list))))
            (cond ((< (length pos-display-list) max-length)
                   (setf pos-display-list
                         (append pos-display-list
                                 (make-list (- max-length (length pos-display-list))
                                            ""))))
                  ((< (length neg-display-list) max-length)
                   (setf neg-display-list
                         (append neg-display-list
                                 (make-list (- max-length (length neg-display-list))
                                            "\n"))))))
          ;; Then zip
          (let ((max-line-length (apply 'max
                                        (cl-loop
                                         for str in pos-display-list
                                         collect (length str)))))
            (cl-loop
             for i from 0 to (- (length pos-display-list) 1)
             do (setf display-list
                      (append display-list
                              (list
                               (concat (durand-cat-strip-new-line
                                        (nth i pos-display-list))
                                       (make-string
                                        (+ durand-cat-center-gap
                                           (- max-line-length (length (nth i pos-display-list))))
                                        32)
                                       (nth i neg-display-list)))))))
          (cl-loop for str in (durand-cat-align-first-line
                               (window-body-width)
                               display-list)
                   do (insert str))))))
  ;; switch to it instead of displaying it.
  (switch-to-buffer durand-cat-buffer-name)
  ;; set header format
  (setf durand-cat-header-line-format (propertize
                                       (durand-cat-center
                                        (ceiling (window-body-width) 1.5)
                                        (durand-cat-profile-name profile))
                                       'face '(:foreground "gold" :height 300)))
  ;; a dedicated major mode
  (durand-cat-mode)
  (durand-cat-set-cursor))

;;;###autoload
(defun durand-cat-toggle-details ()
  "Hide or show the details about files and regexps."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "in file" nil t)
        (cond ((null (get-char-property (point) 'display))
               (add-text-properties (line-beginning-position)
                                    (progn
                                      (re-search-forward "\n\\s-*\n" nil t)
                                      (- (point) 1))
                                    '(display "")))
              (t
               (add-text-properties (line-beginning-position)
                                    (progn
                                      (re-search-forward "\n\\s-*\n" nil t)
                                      (- (point) 1))
                                    '(display nil))))))))

;;; Engines

;;;###autoload
(defun durand-cat-plus-one-engine ()
  "Count each state change as having point 1.
It is expected to be executed with point at a heading."
  (let ((limit (save-excursion (outline-next-heading) (point))))
    (save-excursion
      (cl-loop while (re-search-forward org-state-regexp limit t)
               sum 1))))

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
  (setf header-line-format durand-cat-header-line-format)
  (setq cursor-type nil))

;;;###autoload
(defun durand-cat-set-cursor ()
  "Set up the cursor type."
  (setq cursor-type nil))

(add-hook 'durand-cat-mode-hook 'durand-cat-set-header)
(add-hook 'durand-cat-mode-hook 'durand-cat-set-cursor t)
(add-hook 'durand-cat-mode-hook 'durand-cat-toggle-details)

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
                                                 raw))
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
                                  "aujourdhui.org_archive"
                                  "agenda.org")
                           :regex "run")
                          (
                           :tag "clean the house"
                           :file "agenda.org"
                           :regex "clean_house")
                          (
                           :tag "reading"
                           :file "notes.org"
                           :regex "lire"
                           :engine durand-cat-time-span-engine)))

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
