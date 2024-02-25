;;; xsort-tab.el --- Smarter tab solution for Emacs, it sort tab with using frequency.   -*- lexical-binding: t; -*-

;; Filename: xsort-tab.el
;; Description: Provide an out of box configuration to use xsort-tab in Emacs.
;; Author: xlshiz
;; Version: 1.0

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Smarter tab solution for Emacs, it sort tab with using frequency.
;;

(require 'cl-lib)

(defgroup xsort-tab nil
  "Display xsort-tab in top of Emacs."
  :group 'convenience)

(defcustom xsort-tab-buffer-name "*xsort-tab*"
  "The buffer name of xsort-tab."
  :type 'string)

(defcustom xsort-tab-height 30
  "The height of xsort-tab."
  :type 'integer)

(defcustom xsort-tab-name-max-length 50
  "Max length of tab name."
  :type 'int)

(defcustom xsort-tab-separator "|"
  "The separator between tabs."
  :type 'string)

(defcustom xsort-tab-align 'left
  "The align of xsort-tab."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)))

(defcustom xsort-tab-ace-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Key used for ace tab."
  :group 'xsort-tab
  :set #'(lambda (symbol value)
           (set-default symbol value)
           (let ((1k-seqs nil)
                 (2k-seqs nil))
             (dolist (a value)
               (dolist (b value)
                 (push (list a b) 2k-seqs))
               (push (list a) 1k-seqs))
             (setq xsort-tab-ace-2-key-seqs (nreverse 2k-seqs))
             (setq xsort-tab-ace-1-key-seqs (nreverse 1k-seqs))))
  :type '(repeat :tag "Keys" character))

(defcustom xsort-tab-ace-quit-keys '(?\C-g ?q ?\s)
  "Key used for ace tab."
  :group 'xsort-tab
  :type '(repeat :tag "Keys" character))

(defface xsort-tab-current-tab-face
  '((((background light))
     :background "#d5c9c0" :foreground "#282828" :bold t)
    (t
     :background "#504945" :foreground "#fbf1c7" :bold t))
  "Face for current tab.")

(defface xsort-tab-other-tab-face
  '((((background light))
     :foreground "#665c54" :bold nil)
    (t
     :foreground "#bdae93" :bold nil))
  "Face for inactive tabs.")

(defface xsort-tab-separator-face
  '((((background light))
     :foreground "#bdae93" :bold t)
    (t
     :foreground "#665c54" :bold t))
  "Face for separator.")

(defface xsort-tab-ace-keys-face
  '((t
     :inherit transient-red :bold t :height 1.0))
  "Face for ace keys.")

(defconst xsort-tab-propertized-separator
  (propertize xsort-tab-separator 'face 'xsort-tab-separator-face))

(defvar xsort-tab-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap to use in xsort-tab mode.")

(defvar xsort-tab-window nil)

(defvar xsort-tab-buffer-freq-count-timer nil
  "Timer used for count buffer used frequency.")

(defvar-local xsort-tab-buffer-freq 0
  "Used frequency of current buffer.")

(defvar xsort-tab-visible-buffer-limit 4
  "Max number of visible buffers in a group.")

(defvar xsort-tab-ace-state nil
  "Whether current buffer is doing ace or not.")

(defvar xsort-tab-count-freq-idle-time 1
  "Add used frequency after being idle for this much secs.")

(defvar xsort-tab-visible-buffers nil)

(defvar xsort-tab-last-active-buffer nil)

(defvar xsort-tab-pin-keys '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨")
  "Key used for pin tab.")

(defvar xsort-tab-ace-1-key-seqs nil
  "List of 1-key sequences.")

(defvar xsort-tab-ace-2-key-seqs nil
  "List of 2-key sequences.")

(defvar xsort-tab-ace-strs nil)

(defvar xsort-tab-hide-buffers '("*" " *")
  "Do not sort these buffers")

(defvar xsort-tab-hide-modes '(magit-status-mode magit-process-mode magit-diff-mode
                               magit-log-mode magit-status-mode)
  "Do not sort these modes")

(defvar xsort-tab-hide-whitelist '("*scratch" "*Messages"))

(defvar xsort-tab-hide-tabs '(" *eldoc" "*Help" "*helpful" "*Flycheck")
  "Do not show these buffer on xsort-tab")

(defvar xsort-tab-project-root-function
  (lambda ()
    (when-let ((project (project-current nil))) (cdr project)))
  "A function that returns project root for current buffer.")


(defvar-local toki-tabs/buffer-group nil)
(defun xsort-tab-buffer-group (&optional buffer)
  "Return the group name of BUFFER.
When BUFFER is nil, use current buffer."
  (let* ((buffer (or buffer (window-buffer)))
         (name (buffer-name buffer))
         group)
    (cond
     ;; These should be hidden.
     ;; ((eq (aref name 0) ?\s) nil)
     ((cl-some (lambda (common-name-prefix)
                 (string-prefix-p common-name-prefix name))
               '("*Backtrace" "*scratch" "*Messages"))
      "*Common*")
     ;; ((setq group
     ;;        (or (buffer-local-value 'toki-tabs/buffer-group buffer)
     ;;            (with-current-buffer buffer
     ;;              (setq toki-tabs/buffer-group
     ;;                    (when-let ((project
     ;;                                (funcall xsort-tab-project-root-function)))
     ;;                      (expand-file-name project))))))
     ;;  group)
     ;; ((eq (aref name 0) ?*) "*Common*")
     (t "*Others*"))))

(defun xsort-tab-self-buffer ()
  (get-buffer-create xsort-tab-buffer-name))

(defun xsort-tab-turn-on ()
  (interactive)
  ;; Create xsort-tab buffer.
  (with-current-buffer (xsort-tab-self-buffer)
    ;; Disable line numbers mode.
    (when display-line-numbers
      (setq-local display-line-numbers nil))
    ;; Disable tab-line.
    (when (version< "27.0" emacs-version)
      (setq-local tab-line-format nil))
    ;; Disable hl-line, header-line and mode-line in input buffer.
    (setq-local header-line-format nil)
    (setq-local mode-line-format nil)
    ;; Disable cursor type if option `disable-cursor' is non-nil.
    (setq-local cursor-type nil)
    ;; Set Mini height to make sure xsort-tab window always 1-line height.
    (setq-local window-min-height 1)
    ;; Disable wrap line.
    (setq-local truncate-lines t)
    ;; Disable window resize.
    (setq-local window-size-fixed 'height))

  ;; Create xsort-tab window.
  (xsort-tab-create-window)

  ;; Start count buffer frequency.
  (xsort-tab-start-count-freq)

  ;; Update xsort-tab buffer list.
  (xsort-tab-update-list)

  ;; Add update hook.
  (add-hook 'buffer-list-update-hook #'xsort-tab-update-list))

(defun xsort-tab-create-window ()
  ;; Split top window.
  (ignore-errors
    (dotimes (i 50)
      (windmove-up)))
  (split-window-vertically 1)

  ;; Record xsort-tab window.
  (setq xsort-tab-window (selected-window))
  (switch-to-buffer (xsort-tab-self-buffer))
  (other-window 1)

  ;; Set window dedicated to make sure pop buffer won't use xsort-tab window.
  (set-window-dedicated-p xsort-tab-window t)

  ;; Make sure xsort-tab window can skip `delete-other-windows' and 'other-window'.
  (set-window-parameter xsort-tab-window 'no-delete-other-windows t)
  (set-window-parameter xsort-tab-window 'window-side 'top)
  (set-window-parameter xsort-tab-window 'window-slot 0)
  (set-window-parameter xsort-tab-window 'no-other-window t))

(defun xsort-tab-turn-off ()
  (interactive)
  (setq xsort-tab-mode nil)

  (when (xsort-tab-live-p)
    ;; Reset window parameter.
    (set-window-parameter xsort-tab-window 'no-delete-other-windows nil)
    (set-window-parameter xsort-tab-window 'window-side nil)
    (set-window-parameter xsort-tab-window 'window-slot nil)
    (set-window-parameter xsort-tab-window 'no-other-window nil)

    ;; Kill xsort-tab window.
    (delete-window (get-buffer-window (xsort-tab-self-buffer))))

  ;; Reset xsort-tab window.
  (setq xsort-tab-window nil)

  ;; Stop count.
  (xsort-tab-stop-count-freq)

  ;; Remove update hook.
  (remove-hook 'buffer-list-update-hook #'xsort-tab-update-list))

(defun xsort-tab-live-p ()
  (and (buffer-live-p (get-buffer xsort-tab-buffer-name))
       xsort-tab-window
       (window-live-p xsort-tab-window)))

(defun xsort-tab-increase-buffer-freq ()
  "Increase the used frequency of current buffer by 1."
  (cl-incf xsort-tab-buffer-freq))

(defun xsort-tab-start-count-freq ()
  "Start counting buffer used frequency."
  (setq xsort-tab-buffer-freq-count-timer
        (run-with-idle-timer xsort-tab-count-freq-idle-time
                             t #'xsort-tab-increase-buffer-freq)))

(defun xsort-tab-stop-count-freq ()
  "Stop counting buffer used frequency."
  (cancel-timer xsort-tab-buffer-freq-count-timer)
  (setq xsort-tab-buffer-freq-count-timer nil))

(defun xsort-tab-buffer-freq (buf)
  "Return the used frequency of buffer BUF."
  (or (buffer-local-value 'xsort-tab-buffer-freq buf)
      0))

(defun xsort-tab-is-eaf-browser-buffer-p (buf)
  (with-current-buffer buf
    (and (eq major-mode 'eaf-mode)
         (equal eaf--buffer-app-name "browser"))))

(defun xsort-tab-is-eaf-file-manager-buffer-p (buf)
  (with-current-buffer buf
    (and (eq major-mode 'eaf-mode)
         (equal eaf--buffer-app-name "file-manager"))))

(defun xsort-tab-buffer-freq-higher-p (buf1 buf2)
  "Return t if the used frequency of BUF1 is higher than BUF2."
  (cond
   ;; EAF Browser tab xsorted first.
   ((and (xsort-tab-is-eaf-browser-buffer-p buf1)
         (not (xsort-tab-is-eaf-browser-buffer-p buf2)))
    t)
   ((and (xsort-tab-is-eaf-browser-buffer-p buf2)
         (not (xsort-tab-is-eaf-browser-buffer-p buf1)))
    nil)
   ;; EAF File manager tab xsorted last.
   ((and (xsort-tab-is-eaf-file-manager-buffer-p buf1)
         (not (xsort-tab-is-eaf-file-manager-buffer-p buf2)))
    nil)
   ((and (xsort-tab-is-eaf-file-manager-buffer-p buf2)
         (not (xsort-tab-is-eaf-file-manager-buffer-p buf1)))
    t)
   (t
    (let ((buf1-index (or (cl-position buf1 xsort-tab-visible-buffers :test #'eq) -1))
          (buf2-index (or (cl-position buf2 xsort-tab-visible-buffers :test #'eq) -1)))
      ;; Do not swapped tab if two tags are adjacent and current command is next or prev tab.
      (if (and (<= (abs (- buf1-index buf2-index)) 1)
               (member this-command '(xsort-tab-select-next-tab xsort-tab-select-prev-tab xsort-tab-ace-jump)))
          (< buf1-index buf2-index)

        ;; Otherwise, sort by frequency of tab.
        (> (xsort-tab-buffer-freq buf1) (xsort-tab-buffer-freq buf2) )
        )))))

(defun xsort-tab-buffer-need-hide-p (buf)
  "If non-nil, `BUF' need sort."
  (let* ((name (buffer-name buf)))
    (if (cl-some (lambda (prefix) (string-prefix-p prefix name)) xsort-tab-hide-whitelist)
        nil
      (or
       (cl-some (lambda (prefix) (string-prefix-p prefix name)) xsort-tab-hide-buffers)
       (eq (aref name 0) ?\s)
       (cl-some (lambda (mode) (with-current-buffer buf
                                 (derived-mode-p mode)))
                xsort-tab-hide-modes)))))

(defun xsort-tab-if-show-tab-p (buf)
  "If non-nil, `BUF' need hidden."
  (let* ((name (buffer-name buf)))
    (and
     (not (window-minibuffer-p))
     (not (cl-some (lambda (prefix) (string-prefix-p prefix name)) xsort-tab-hide-tabs))
     (not (string-equal xsort-tab-buffer-name name))
     )))

(defun xsort-tab-get-buffer-list ()
  (sort (cl-remove-if
         #'xsort-tab-buffer-need-hide-p
         (buffer-list))
        #'xsort-tab-buffer-freq-higher-p))

(defun xsort-tab-visible-tabs-and-remain-num ()
  "Return the visible tabs and number of remaining tabs in a cons cell.
When the current buffer is a hidden buffer, return nil."
  (let* ((buf (window-buffer))
         (group (xsort-tab-buffer-group buf))
         (counter 0)
         found-current-tab
         push-current-tab
         tabs)
    (when group
      (dolist (b xsort-tab-visible-buffers)
        (when (equal (xsort-tab-buffer-group b) group)
          (if (< (length tabs) xsort-tab-visible-buffer-limit)
              (progn (push b tabs)
                     (if (eq b buf)
                         (setq push-current-tab t)))
            (cl-incf counter))
          (if (eq b buf)
              (setq found-current-tab t))))
      (when (and found-current-tab (not push-current-tab))
        (pop tabs)
        (push buf tabs))
      (cons (nreverse tabs)
            counter))))

(defun xsort-tab-visible-tabs ()
  "Return the visible tabs."
  (let* ((buf (window-buffer))
         (group (xsort-tab-buffer-group buf))
         found-current-tab
         push-current-tab
         tabs)
    (when group
      (dolist (b xsort-tab-visible-buffers)
        (when (equal (xsort-tab-buffer-group b) group)
          (if (< (length tabs) xsort-tab-visible-buffer-limit)
              (progn (push b tabs)
                     (if (eq b buf)
                         (setq push-current-tab t))))
          (if (eq b buf)
              (setq found-current-tab t))))
      (when (and found-current-tab (not push-current-tab))
        (pop tabs)
        (push buf tabs))
      (nreverse tabs))))

(cl-defmacro xsort-tab-update-tabs (&rest body)
  `(with-current-buffer (xsort-tab-self-buffer)
     ;; Clean buffer.
     (erase-buffer)

     ;; Update tabs.
     ,@body

     (when (eq xsort-tab-align 'center)
       (goto-char (point-min))
       (insert xsort-tab-propertized-separator)
       (let* ((width (window-width (get-buffer-window)))
              (content-length (length (buffer-string)))
              (padding (max 0 (/ (- width content-length) 2))))
         (goto-char (point-min))
         (insert (make-string padding ?\s))))

     ;; Record last active buffer.
     (setq xsort-tab-last-active-buffer (current-buffer))
     ))

(defun xsort-tab-update-list ()
  (let ((current-buffer (window-buffer)))
    (cond
     ;; Erase xsort-tab content if current buffer is xsort-tab buffer.
     ((string-equal xsort-tab-buffer-name (buffer-name current-buffer))
      (xsort-tab-update-tabs))
     ;; Display tabs if current-buffer is normal buffer.
     (t
      ;; Debug usage.
      ;; (with-current-buffer (get-buffer-create "xsort-tab-debug")
      ;;   (goto-char (point-max))
      ;;   (insert (format "**** %s %s\n"
      ;;                   last-command
      ;;                   (buffer-name current-buffer))))

      (let* ((current-tab-start-column 0)
             (current-tab-end-column 0)
             (tab-window (get-buffer-window (xsort-tab-self-buffer)))
             tabs-and-remain
             tabs
             num
             found-current-tab
             tab
             (buffer-index -1))
        (xsort-tab-update-tabs
         ;; Don't sort tabs if using xsort-tab commands.
         (unless (string-prefix-p "xsort-tab-" (prin1-to-string last-command))
           (setq xsort-tab-visible-buffers (xsort-tab-get-buffer-list)))

         (setq tabs-and-remain (xsort-tab-visible-tabs-and-remain-num))
         (setq tabs (car tabs-and-remain))
         (setq num (cdr tabs-and-remain))
         (dolist (buf tabs)
           ;; Insert tab.
           (setq buffer-index (+ buffer-index 1))
           (setq tab (xsort-tab-get-tab-name buf current-buffer buffer-index))
           (if (or (not xsort-tab-ace-state) (> buffer-index (length xsort-tab-ace-strs)))
               (insert "  ")
             (let ((show-numbers xsort-tab-ace-strs))
               (insert (propertize (format "%-2s" (nth buffer-index show-numbers)) 'face 'xsort-tab-ace-keys-face))))
           (insert tab)
           (insert xsort-tab-propertized-separator)

           ;; Calculate the current tab column.
           (unless found-current-tab
             (when (eq buf current-buffer)
               (setq found-current-tab t)
               (setq current-tab-start-column current-tab-end-column))
             (setq current-tab-end-column (+ current-tab-end-column (length tab) (length xsort-tab-separator)))))

         ;; Show hide buffer at left when current buffer is match hidden rule.
         (when (and (not found-current-tab) (xsort-tab-if-show-tab-p current-buffer))
           (insert " ")
           (insert (xsort-tab-get-tab-name current-buffer current-buffer))
           (insert " ")
           (insert xsort-tab-propertized-separator))

         (when (and num (> num 0))
           (insert (propertize (format " +%s.. " num) 'face 'xsort-tab-ace-keys-face))
           (insert xsort-tab-propertized-separator))
         ;; Make tab always visible.
         (when tab-window
           (with-selected-window tab-window
             (cond ((> current-tab-end-column (+ (window-hscroll) (window-width)))
                    (scroll-left (+ (- current-tab-end-column (window-hscroll) (window-width)) (/ (window-width) 2))))
                   ((< current-tab-start-column (window-hscroll))
                    (set-window-hscroll tab-window current-tab-start-column))
                   )))))))))

(defun xsort-tab-get-tab-name (buf current-buffer &optional buffer-index)
  (propertize
   (format "%s"
           (let* ((bufname (buffer-name buf))
                  (ellipsis "..."))
             ;; We need remove space in web page title.
             (when (xsort-tab-is-eaf-browser-buffer-p buf)
               (setq bufname (replace-regexp-in-string "\\s-" "" bufname)))

             (if (> (length bufname) xsort-tab-name-max-length)
                 (format "%s%s" (substring bufname 0 (- xsort-tab-name-max-length (length ellipsis))) ellipsis)
               (format "%s " bufname))))
   'face
   (if (eq buf current-buffer)
       'xsort-tab-current-tab-face
     'xsort-tab-other-tab-face)))

(defun xsort-tab-get-index ()
  (cl-position (window-buffer) (xsort-tab-visible-tabs) :test #'eq))

(defun xsort-tab-get-next-buffer ()
  (let ((index (xsort-tab-get-index)))
    (cond
     ((or (null index) (eq index (1- (length (xsort-tab-visible-tabs)))))
      (car (xsort-tab-visible-tabs)))
     (t
      (nth (1+ index) (xsort-tab-visible-tabs))))))

(defun xsort-tab-get-prev-buffer ()
  (let ((index (xsort-tab-get-index)))
    (cond
     ((or (null index) (eq index 0))
      (car (last (xsort-tab-visible-tabs))))
     (t
      (nth (1- index) (xsort-tab-visible-tabs))))))

(defun xsort-tab-get-first-buffer ()
  (cl-first (xsort-tab-visible-tabs)))

(defun xsort-tab-get-last-buffer ()
  (car (last (xsort-tab-visible-tabs))))

(defun xsort-tab-select-prev-tab ()
  (interactive)
  (switch-to-buffer (xsort-tab-get-prev-buffer)))

(defun xsort-tab-select-next-tab ()
  (interactive)
  (switch-to-buffer (xsort-tab-get-next-buffer)))

(defun xsort-tab-select-first-tab ()
  (interactive)
  (switch-to-buffer (xsort-tab-get-first-buffer)))

(defun xsort-tab-select-last-tab ()
  (interactive)
  (switch-to-buffer (xsort-tab-get-last-buffer)))

(defun xsort-tab-build-ace-strs (len key-number seqs)
  "Build strings for `xsort-tab-ace-jump'.
LEN is the number of strings, should be the number of current visible
tabs. NKEYS should be 1 or 2."
  (let ((i 0)
        (str nil))
    (when (>= key-number 3)
      (error "NKEYS should be 1 or 2"))
    (while (< i len)
      (push (apply #'string (elt seqs i)) str)
      (setq i (1+ i)))
    (nreverse str)))

(defun xsort-tab-ace-jump ()
  "Jump to a visible tab."
  (interactive)
  (catch 'quit
    (let* ((visible-tabs-length (length (xsort-tab-visible-tabs)))
           done-flag
           (lower-bound 0)
           (upper-bound visible-tabs-length)
           (ace-keys (length xsort-tab-ace-keys))
           (key-number (cond
                        ((<= visible-tabs-length ace-keys) 1)
                        ((<= visible-tabs-length (* ace-keys ace-keys)) 2)
                        (t (error "Too many visible tabs. Put more keys into `xsort-tab-ace-keys'"))))
           (visible-seqs
            (cl-subseq
             (symbol-value
              (intern
               (concat "xsort-tab-ace-" (number-to-string key-number) "-key-seqs")))
             0 visible-tabs-length))
           (ace-strs (xsort-tab-build-ace-strs visible-tabs-length key-number visible-seqs)))
      (setq xsort-tab-ace-strs ace-strs)
      (setq xsort-tab-ace-state t)
      (xsort-tab-update-list)
      (dotimes (i key-number)
        (while (not done-flag)
          (let ((char (with-local-quit (read-key (format "Tab Ace Jump (%d):" (1+ i))))))
            (if (not (member char xsort-tab-ace-quit-keys))
                (let ((current-chars (mapcar #'car visible-seqs)))
                  (when (member char current-chars)
                    (setq done-flag t)
                    (setq lower-bound (cl-position char current-chars))
                    (setq upper-bound (1- (- visible-tabs-length (cl-position char (nreverse current-chars)))))
                    (dotimes (lower-index lower-bound)
                      (setcar (nthcdr lower-index visible-seqs) nil))
                    (setq upper-index (1+ upper-bound))
                    (while (< upper-index visible-tabs-length)
                      (setcar (nthcdr upper-index visible-seqs) nil)
                      (setq upper-index (1+ upper-index)))
                    (setq upper-index 0)
                    ))
              ;; Quit when user press Ctrl + g.
              (setq xsort-tab-ace-state nil)
              (xsort-tab-update-list)
              (throw 'quit nil))))
        (setq done-flag nil)
        (setq visible-seqs (mapcar #'cdr visible-seqs))
        (setq xsort-tab-ace-strs (xsort-tab-build-ace-strs visible-tabs-length key-number visible-seqs))
        (xsort-tab-update-list))
      (setq xsort-tab-ace-state nil)
      (xsort-tab-update-list)
      (xsort-tab-select-visible-nth-tab (1+ lower-bound)))))

(defun xsort-tab-close-current-tab-and-select-previous ()
  (interactive)
  (let* ((buf (window-buffer)))
    (xsort-tab-kill-buffer buf)))

(defun xsort-tab-close-current-tab ()
  (interactive)
  (let* ((buf (window-buffer))
         (prev-buffer (xsort-tab-get-prev-buffer))
         (next-buffer (xsort-tab-get-next-buffer))
         (last-buffer (xsort-tab-get-last-buffer))
         (is-last-buffer (eq buf last-buffer)))
    ;; Then kill current buffer.
    (xsort-tab-kill-buffer buf)
    ;; Switch to previous buffer if current buffer is last buffer,
    ;; otherwise switch to next buffer.
    (if is-last-buffer
        (when (buffer-live-p prev-buffer)
          (switch-to-buffer prev-buffer))
      (when (buffer-live-p next-buffer)
        (switch-to-buffer next-buffer))
      )))

(defun xsort-tab-close-all-tabs ()
  (interactive)
  (let ((visible-buffers xsort-tab-visible-buffers))
    (setq xsort-tab-visible-buffers nil)
    (dolist (buf visible-buffers)
      (kill-buffer buf))))

(defun xsort-tab-close-other-tabs ()
  (interactive)
  (let* ((current-buf (current-buffer))
         (visible-buffers xsort-tab-visible-buffers))
    (dolist (buf visible-buffers)
      (unless (eq buf current-buf)
        (kill-buffer buf)))
    (setq xsort-tab-visible-buffers (xsort-tab-get-buffer-list))))

(defun xsort-tab-kill-buffer (buffer)
  ;; Update `xsort-tab-visible-buffers' first.
  (setq xsort-tab-visible-buffers (delete buffer xsort-tab-visible-buffers))
  (kill-buffer buffer))

(defun xsort-tab-select-visible-nth-tab (&optional tab-index)
  (interactive "p")
  (switch-to-buffer (nth (1- tab-index) xsort-tab-visible-buffers)))

(defun xsort-tab-select-visible-tab ()
  (interactive)
  (let* ((event last-command-event)
         (key (make-vector 1 event))
         (key-desc (key-description key)))
    (xsort-tab-select-visible-nth-tab
     (string-to-number (car (last (split-string key-desc "-")))))))

(defun xsort-tab-kill-buffer-advisor (orig-fun &optional arg &rest args)
  (if (equal (buffer-name) xsort-tab-buffer-name)
      (message "xsort-tab buffer can't be kill, please use `xsort-tab-turn-off' command to quit xsort-tab.")
    (apply orig-fun arg args)))

(defun xsort-tab-bury-buffer-advisor (&optional arg)
  (xsort-tab-update-list))

(advice-add #'kill-buffer :around #'xsort-tab-kill-buffer-advisor)
(advice-add #'bury-buffer :after #'xsort-tab-bury-buffer-advisor)
(advice-add #'unbury-buffer :after #'xsort-tab-update-list)

(defun initialize-xsort-tab-delay (&optional frame)
  (run-with-idle-timer 0 nil 'xsort-tab-turn-on))

;;;###autoload
(define-minor-mode xsort-tab-mode
  "Toggle display of a xsort-tab.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{xsort-tab-mode-map}"
  :group 'xsort-tab
  :require 'xsort-tab
  :global t
  :keymap xsort-tab-mode-map
  (if xsort-tab-mode
      (progn
        (xsort-tab-turn-on)

        ;; Add hook for emacs daemon.
        (when (and (fboundp 'daemonp) (daemonp))
          (add-hook 'after-make-frame-functions #'initialize-xsort-tab-delay t)))
    (xsort-tab-turn-off)

    ;; Remove hook for emacs daemon.
    (when (and (fboundp 'daemonp) (daemonp))
      (remove-hook 'after-make-frame-functions #'initialize-xsort-tab-delay)
      )))

(provide 'xsort-tab)

;;; xsort-tab.el ends here
