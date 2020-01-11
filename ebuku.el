
;;; ebuku.el --- Interface to the Buku Web bookmark manager -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Alexis <flexibeast@gmail.com>

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2019-11-07
;; URL: https://github.com/flexibeast/ebuku
;; Keywords: bookmarks,buku,data,web,www
;; Version: 0
;; Package-Requires: ((emacs "24.3"))

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; EBuku provides a basic interface to the
;; [Buku](https://github.com/jarun/Buku) Web bookmark manager.

;; ## Table of Contents

;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [Customisation](#customisation)
;; - [TODO](#todo)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Installation

;; Install [EBuku from MELPA](https://melpa.org/#/ebuku), or put the
;; `ebuku' folder in your load-path and do a `(load "ebuku")'.

;; ## Usage

;; Create an EBuku buffer with `M-x ebuku'.

;; In the `*EBuku*' buffer, the following bindings are available:

;; * `s' - Search for a bookmark (`ebuku-search').

;; * `r' - Show recently-added bookmarks (`ebuku-search-on-recent').

;; * `*' - Show all bookmarks (`ebuku-show-all').

;; * `-' - Toggle results limit (`ebuku-toggle-results-limit').

;; * `g' - Refresh the search results, based on last search (`ebuku-refresh').

;; * `RET' - Open the bookmark at point in a browser (`ebuku-open-url').

;; * `n' - Move point to the next bookmark URL (`ebuku-next-bookmark').

;; * `p' - Move point to the previous bookmark URL (`ebuku-previous-bookmark').

;; * `a' - Add a new bookmark (`ebuku-add-bookmark').

;; * `d' - Delete a bookmark (`ebuku-delete-bookmark').  If point is on
;;   a bookmark, offer to delete that bookmark; otherwise, ask for the
;;   index of the bookmark to delete.

;; * `e' - Edit a bookmark (`ebuku-edit-bookmark').  If point is on a
;;   bookmark, edit that bookmark; otherwise, ask for the index of the
;;   bookmark to edit.

;; * `q' - Quit EBuku.

;; ## Customisation

;; The `ebuku' customize-group can be used to customise:

;; * the path to the `buku' executable;

;; * the number of recently-added bookmarks to show;

;; * which bookmarks to show on startup;

;; * the maximum number of bookmarks to show; and

;; * the faces used by EBuku.

;; ## TODO

;; * One should be able to edit bookmarks directly in the `*EBuku*'
;;   buffer, à la `wdired'.  Much of the infrastructure to support this
;;   is already in place, but there are still important details yet to
;;   be implemented.

;; <a name="issues"></a>

;; ## Issues / bugs

;; If you discover an issue or bug in EBuku not already
;; noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on
;;   GitHub](https://github.com/flexibeast/ebuku/issues),

;; please create a new issue with as much detail as possible,
;; including:

;; * which version of Emacs you're running on which operating system,
;;   and

;; * how you installed EBuku.

;; ## License

;; [GNU General Public License version
;; 3](https://www.gnu.org/licenses/gpl.html), or (at your option) any
;; later version.

;;; Code:

;;
;; User-customisable settings.
;;

(defgroup ebuku nil
  "Emacs interface to the Buku bookmark manager."
  :group 'external)

(defcustom ebuku-buku-path (executable-find "buku")
  "Absolute path of the `buku' executable."
  :type '(file :must-match t)
  :group 'ebuku)

(defcustom ebuku-display-on-startup 'all
  "What to display in the search results area on startup.

Specify `\\='all' for all bookmarks; `\\='recent' for recent additions; or
`nil' for no bookmarks."
  :type '(radio (const :tag "All bookmarks" all)
                (const :tag "Recent additions" recent)
                (const :tag "No bookmarks" nil))
  :group 'ebuku)

(defcustom ebuku-mode-hook nil
  "Functions to run when starting `ebuku-mode'."
  :type '(repeat function)
  :group 'ebuku)

(defcustom ebuku-recent-count 3
  "Number of recently-added bookmarks to show."
  :type 'integer
  :group 'ebuku)

(defcustom ebuku-results-limit 1000
  "Maximum number of bookmarks to show.

Set this variable to 0 for no maximum."
  :type 'integer
  :group 'ebuku)


(defgroup ebuku-faces nil
  "Faces for `ebuku-mode'."
  :group 'ebuku)

(defface ebuku-comment-face
  '((t
     :inherit default))
  "Face for *EBuku* bookmark comments."
  :group 'ebuku-faces)

(defface ebuku-heading-face
  '((t
     :height 2.0
     :weight bold))
  "Face for *EBuku* headings."
  :group 'ebuku-faces)

(defface ebuku-help-face
  '((t
     :inherit default))
  "Face for *EBuku* help text."
  :group 'ebuku-faces)

(defface ebuku-separator-face
  '((t
     :inherit default))
  "Face for *EBuku* separators."
  :group 'ebuku-faces)

(defface ebuku-tags-face
  '((t
     :inherit default))
  "Face for *EBuku* bookmark tags."
  :group 'ebuku-faces)

(defface ebuku-title-face
  '((t
     :inherit default))
  "Face for *EBuku* bookmark titles."
  :group 'ebuku-faces)

(defface ebuku-url-face
  '((t
     :inherit link))
  "Face for *EBuku* bookmark URLs."
  :group 'ebuku-faces)

(defface ebuku-url-highlight-face
  '((t
     :inherit highlight))
  "Face for highlighting *EBuku* bookmark URLs."
  :group 'ebuku-faces)


;;
;; Keymaps.
;;

(defvar ebuku-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "a") #'ebuku-add-bookmark)
    (define-key km (kbd "d") #'ebuku-delete-bookmark)
    (define-key km (kbd "e") #'ebuku-edit-bookmark)
    (define-key km (kbd "g") #'ebuku-refresh)
    (define-key km (kbd "n") #'ebuku-next-bookmark)
    (define-key km (kbd "p") #'ebuku-previous-bookmark)
    (define-key km (kbd "r") #'ebuku-search-on-recent)
    (define-key km (kbd "s") #'ebuku-search)
    (define-key km (kbd "*") #'ebuku-show-all)
    (define-key km (kbd "-") #'ebuku-toggle-results-limit)
    (define-key km (kbd "<RET>") #'ebuku-open-url)
    (define-key km [mouse-1] #'ebuku-open-url)
    (define-key km [mouse-2] #'ebuku-open-url)
    km))


;;
;; Internal variables.
;;

(defvar ebuku--last-search nil
  "Internal variable containing the parameters of the last search.")

(defvar ebuku--last-results-limit 0
  "Internal variable for use by `ebuku-toggle-results-limit'.")

(defvar ebuku--results-start nil
  "Internal variable containing buffer location of start of search results.")


;;
;; Internal functions.
;;

(defun ebuku--call-buku (args)
  "Internal function for calling `buku' with list ARGS."
  (unless ebuku-buku-path
    (error "Couldn't find buku: check 'ebuku-buku-path'"))
  (apply #'call-process
         `(,ebuku-buku-path nil t nil
                            "--np" "--nc"
                            ,@args)))

(defun ebuku--create-mode-menu ()
  "Internal function to create the `ebuku-mode' menu."
  (easy-menu-define ebuku--menu ebuku-mode-map "Menu bar entry for EBuku"
    '("EBuku"
      ["Search" (ebuku-search) :keys "s"]
      ["Recent" (ebuku-search-on-recent) :keys "r"]
      ["Show all" (ebuku-show-all) :keys "*"]
      ["Toggle results limit" (ebuku-toggle-results-limit) :keys "-"]
      ["Refresh" (ebuku-refresh) :keys "g"]
      "---"
      ["Open bookmark" (ebuku-open-url) :keys "RET"]
      ["Next bookmark" (ebuku-next-bookmark) :keys "n"]
      ["Previous bookmark" (ebuku-previous-bookmark) :keys "p"]
      "---"
      ["Add bookmark" (ebuku-add-bookmark) :keys "a"]
      ["Delete bookmark" (ebuku-delete-bookmark) :keys "d"]
      ["Edit bookmark" (ebuku-edit-bookmark) :keys "e"]
      "---"
      ["Customize" (customize-group 'ebuku) :keys "c"]
      ["Quit" (kill-buffer) :keys "q"])))

(defun ebuku--delete-bookmark-helper (index)
  "Internal function to delete the Buku bookmark at INDEX.

Buku doesn't provide a `--force' option for `--delete'; instead,
it always prompts the user for confirmation.  This function starts
an asychrononous Buku process to delete the bookmark, to which we
can then send 'y' to confirm.  (The user will have already been
prompted for confirmation by the \\[ebuku-delete-bookmark] command.)"
  (unless ebuku-buku-path
    (error "Couldn't find buku: check 'ebuku-buku-path'"))
  (let ((proc (start-process
               "ebuku-delete"
               nil
               ebuku-buku-path "--delete" index)))
    (set-process-sentinel proc 'ebuku--delete-bookmark-sentinel)
    (process-send-string proc "y\n")))

(defun ebuku--delete-bookmark-sentinel (_proc event)
  "Internal function to wait for Buku to complete bookmark deletion.

Argument PROC is the Buku process started by `ebuku--delete-bookmark-helper'.

Argument EVENT is the event received from that process."
  (if (string= "finished\n" event)
      (progn
        (ebuku-refresh)
        (message "Bookmark deleted."))
    (error "Failed to delete bookmark")))

(defun ebuku--get-index-at-point ()
  "Internal function to get the Buku index of the bookmark at point."
  (get-char-property (point) 'buku-index))

(defun ebuku--get-bookmark-at-index (index)
  "Internal function to get bookmark data for INDEX."
  (let ((title "")
        (url "")
        (comment "")
        (tags ""))
    (with-temp-buffer
      (if (ebuku--call-buku `("--print" ,index))
          (progn
            (goto-char (point-min))
            (re-search-forward "^[[:digit:]]+\\.\\s-+\\(.+\\)$" nil t)
            (setq title (match-string 1))
            (re-search-forward "^\\s-+> \\(.+\\)$" nil t)
            (setq url (match-string 1))
            (if (re-search-forward "^\\s-+\\+ \\(.+\\)$" nil t)
                (setq comment (match-string 1)))
            (if (re-search-forward "^\\s-+# \\(.+\\)$" nil t)
                (setq tags (match-string 1))))
        (error (concat "Failed to get bookmark data for index " index))))
    `((title . ,title) (url . ,url) (comment . ,comment) (tags ,tags))))

(defun ebuku--bookmarks (type &optional term exclude)
  "Get result data from buku with appropriate search arguments:

  - TYPE (string): the type of Buku search (example \"--print\").
  - TERM (string): what to search for.
  - EXCLUDE (string): keywords to exclude from the search results.

Result data is a list of alists with keys 'title, 'url, 'index, 'tags, and 'comment."
  (let ((results)
        (title-line-re
         (concat
          ;; Result number, or index number when using '--print'.
          "^\\([[:digit:]]+\\)\\. "
          ;; Title.
          "\\(.+?\\)"
          ;; Index number when not using '--print'.
          "\\(?: \\[\\([[:digit:]]+\\)\\]\\)?\n")))
    (with-temp-buffer
      (ebuku--call-buku
       (remq nil `(,type ,term ,@(when (and exclude (not (string-empty-p exclude)))
                                   `("--exclude" ,exclude)))))
      (goto-char (point-min))
      (while (re-search-forward title-line-re nil t)
        (let ((data))
          (if (or (string= "--print" type)
                  (string= "-p" type))
              (progn
                (map-put data 'index (match-string 1))
                (map-put data 'title (match-string 2)))
            (map-put data 'title (match-string 2))
            (map-put data 'index (match-string 3)))
          (re-search-forward "^\\s-+> \\([^\n]+\\)") ; URL
          (map-put data 'url (match-string 1))
          (forward-line)
          (when (looking-at "^\\s-+[+] \\(.+\\)$")
            (map-put data 'comment (match-string 1))
            (forward-line))
          (when (looking-at "^\\s-+[#] \\(.+\\)$")
            (map-put data 'tags (split-string (match-string 1) "," t)))
          (push data results)))
      results)))

(defun ebuku--insert-bookmark-string (bookmark)
  "Insert BOOKMARK, as returned from `ebuku--bookmarks', as a string."
  (let ((index (alist-get 'index bookmark))
        (url (alist-get 'url bookmark))
        (comment (alist-get 'comment bookmark))
        (tags (string-join (alist-get 'tags bookmark) ","))
        (title (alist-get 'title bookmark)))
    (insert (propertize "  --  "
                        'buku-index index)
            (propertize title
                        'buku-index index
                        'data title
                        'face 'ebuku-title-face)
            (propertize "\n"
                        'buku-index index)
            (propertize "      "
                        'buku-index index)
            (propertize url
                        'buku-index index
                        'data url
                        'face 'ebuku-url-face
                        'mouse-face 'ebuku-url-highlight-face
                        'help-echo "mouse-1: open link in browser")
            (propertize "\n"
                        'buku-index index))
    (when comment
      (insert
       (propertize "      "
                   'buku-index index)
       (propertize comment
                   'buku-index index
                   'data comment
                   'face 'ebuku-comment-face)
       (propertize "\n"
                   'buku-index index)))
    (unless (string-empty-p tags)
      (insert
       (propertize "      "
                   'buku-index index)
       (propertize tags
                   'buku-index index
                   'data tags
                   'face 'ebuku-tags-face)
       (propertize "\n"
                   'buku-index index)))
    (insert "\n")))

(defun ebuku--search-helper (type prompt &optional term exclude)
  "Internal function to call `buku' with appropriate search arguments.

Argument TYPE is a string: the type of Buku search.

Argument PROMPT is a string: the minibuffer prompt for that search.

Argument TERM is a string: the search term.

Argument EXCLUDE is a string: keywords to exclude from search results."
  (let* ((term (or term (read-from-minibuffer prompt)))
         (exclude (or exclude (read-from-minibuffer "Exclude keywords? ")))
         (search (string-join
                  (remq nil `(,type ,term ,@(when (and exclude (not (string-empty-p exclude)))
                                              `("--exclude" ,exclude)))) " "))
         (bookmarks (ebuku--bookmarks type term exclude))
         (count (if (and (string= "--print" type)
                         (string= "[recent]" prompt))
                    ebuku-recent-count
                  (length bookmarks))))
    (setq ebuku--last-search `(,type ,prompt ,term ,exclude))
    (with-current-buffer "*EBuku*"
      (let ((inhibit-read-only t))
        (goto-char ebuku--results-start)
        (kill-region (point) (point-max))
        (cond
         ((equal 0 count)
          (insert (concat "  No results found for '" search "'.\n\n")))
         ((equal 1 count)
          (insert (concat "  Found 1 result for '" search "'.\n\n")))
         (t
          (if (or (< count ebuku-results-limit)
                  (= 0 ebuku-results-limit))
              (insert (format "  Found %s results for '%s'.\n\n" count search))
            (if (> count ebuku-results-limit)
                (progn
                  (insert (format "  Found %s results for '%s'.\n\n" ebuku-results-limit search))
                  (insert (concat "  (Omitted "
                                  (number-to-string
                                   (- count
                                      ebuku-results-limit))
                                  " results due to non-zero value\n"
                                  "   of 'ebuku-results-limit'.)\n\n")))))))
        (mapc #'ebuku--insert-bookmark-string bookmarks)))))


;;
;; User-facing functions.
;;

(defun ebuku-add-bookmark ()
  "Add a bookmark to the Buku database."
  (interactive)
  (let ((title (read-from-minibuffer "Bookmark title? "))
        (url (read-from-minibuffer "Bookmark URL? "))
        (tags (read-from-minibuffer "Bookmark tag(s)? "))
        (comment (read-from-minibuffer "Bookmark comment? ")))
    (with-temp-buffer
      (if (ebuku--call-buku `("--add" ,url
                              "--title" ,title
                              "--tag" ,tags
                              "--comment" ,comment))
          (progn
            (ebuku-refresh)
            (message "Bookmark added."))
        (error "Failed to add bookmark")))))

(defun ebuku-delete-bookmark ()
  "Delete a bookmark from the Buku database.

If point is on a bookmark, offer to delete that bookmark;
otherwise, ask for the index of the bookmark to delete."
  (interactive)
  (let ((index (ebuku--get-index-at-point)))
    (if (not index)
        (setq index (read-from-minibuffer "Bookmark index to delete? ")))
    (let ((bookmark (ebuku--get-bookmark-at-index index)))
      (if bookmark
          (let ((title (cdr (assoc 'title bookmark))))
            (if (y-or-n-p (concat "Delete bookmark \"" title "\"? "))
                (ebuku--delete-bookmark-helper index))))
      (error (concat "Failed to get bookmark data for index " index)))))

(defun ebuku-edit-bookmark ()
  "Edit a bookmark in the Buku database.

If point is on a bookmark, offer to edit that bookmark;
otherwise, ask for the index of the bookmark to edit."
  (interactive)
  (let ((index (ebuku--get-index-at-point)))
    (if (not index)
        (setq index (read-from-minibuffer "Bookmark index to edit? "))
      (let ((bookmark (ebuku--get-bookmark-at-index index)))
        (if bookmark
            (let ((title (read-from-minibuffer
                          "Title? "
                          (cdr (assoc 'title bookmark))))
                  (url (read-from-minibuffer
                        "URL? "
                        (cdr (assoc 'url bookmark))))
                  (comment (read-from-minibuffer
                            "Comment? "
                            (cdr (assoc 'comment bookmark))))
                  (tags (read-from-minibuffer
                         "Tags? "
                         (cdr (assoc 'tags bookmark)))))
              (with-temp-buffer
                (if (ebuku--call-buku `("--update" ,index
                                        "--title" ,title
                                        "--url" ,url
                                        "--comment" ,comment
                                        "--tag" ,tags))
                    (progn
                      (ebuku-refresh)
                      (message "Bookmark updated."))
                  (error "Failed to update bookmark")))))
        (error (concat "Failed to get bookmark data for index " index))))))

(defun ebuku-next-bookmark ()
  "Move point to the next bookmark URL."
  (interactive)
  (re-search-forward "^\\s-+http" nil t 1)
  (beginning-of-line)
  (re-search-forward "^\\s-+" nil t 1))

(defun ebuku-open-url ()
  "Open the URL for the bookmark at point."
  (interactive)
  (let ((index (get-char-property (point) 'buku-index)))
    (if index
        (save-excursion
          (goto-char (1+ (previous-single-property-change (point) 'buku-index)))
          (forward-line)
          (goto-char (next-single-property-change (point) 'data))
          (browse-url-at-point))
      (user-error "No bookmark at point"))))

(defun ebuku-previous-bookmark ()
  "Move point to the previous bookmark URL."
  (interactive)
  (re-search-forward "^\\s-+http" nil t -1)
  (beginning-of-line)
  (re-search-forward "^\\s-+" nil t 1))

(defun ebuku-refresh ()
  "Refresh the list of search results, based on last search."
  (interactive)
  (if ebuku--last-search
      (let ((term (nth 2 ebuku--last-search)))
        (if (and (not (string= "[recent]" (nth 1 ebuku--last-search)))
                 (string-match "^-\\([[:digit:]]+\\)$" term))
            (let ((count (string-to-number (match-string 1 term))))
              (if (/= count ebuku-results-limit)
                  (setf (nth 2 ebuku--last-search)
                        (concat "-"
                                (number-to-string ebuku-results-limit))))
              (apply #'ebuku--search-helper ebuku--last-search))
          (apply #'ebuku--search-helper ebuku--last-search)))))

(defun ebuku-search (char)
  "Search the Buku database for bookmarks.

Argument CHAR is the character selected by the user to specify
the type of search to be performed."
  (interactive "cSearch on a[n]y, a[l]l, [t]ag or [r]egex?")
  (cond
   ((char-equal char ?n) (ebuku-search-on-any))
   ((char-equal char ?l) (ebuku-search-on-all))
   ((char-equal char ?t) (ebuku-search-on-tag))
   ((char-equal char ?r) (ebuku-search-on-reg))))

(defun ebuku-search-on-all ()
  "Do a `buku' search using '--sall'."
  (interactive)
  (ebuku--search-helper "--sall" "Keyword? "))

(defun ebuku-search-on-any ()
  "Do a `buku' search using '--sany'."
  (interactive)
  (ebuku--search-helper "--sany" "Keyword? "))

(defun ebuku-search-on-recent ()
  "Do a `buku' search for recently-added bookmarks."
  (interactive)
  (ebuku--search-helper "--print"
                        "[recent]" ; Dummy prompt to indicate prefab 'search'
                        (concat "-"
                                (number-to-string ebuku-recent-count))
                        ""))

(defun ebuku-search-on-reg ()
  "Do a `buku' search using '--sreg'."
  (interactive)
  (ebuku--search-helper "--sreg" "Regex? "))

(defun ebuku-search-on-tag ()
  "Do a `buku' search using '--stag'."
  (interactive)
  (ebuku--search-helper "--stag" "Tag? "))

(defun ebuku-show-all ()
  "Do a `buku' search for as many bookmarks as possible.

The maximum number of bookmarks to show is specified by
`ebuku-results-limit'."
  (interactive)
  (ebuku--search-helper "--print"
                        "[all]" ; Dummy prompt to indicate prefab 'search'
                        (concat "-"
                                (number-to-string ebuku-results-limit))
                        ""))

(defun ebuku-toggle-results-limit ()
  "Toggle whether to limit results to `ebuku-results-limit'."
  (interactive)
  (if (> ebuku-results-limit 0)
      (progn
        (setq ebuku--last-results-limit ebuku-results-limit)
        (setq ebuku-results-limit 0))
    (setq ebuku-results-limit ebuku--last-results-limit))
  (ebuku-refresh))


;;;###autoload
(define-derived-mode ebuku-mode special-mode "EBuku"
  "Major mode for interacting with the Buku bookmark manager.

\\{ebuku-mode-map}"
  :group 'ebuku)


;;;###autoload
(defun ebuku ()
  "Start EBuku, an interface to the Buku bookmark manager."
  (interactive)
  (if (get-buffer "*EBuku*")
      (switch-to-buffer "*EBuku*")
    (progn
      (setq ebuku--last-search nil)
      (with-current-buffer (generate-new-buffer "*EBuku*")
        (goto-char (point-min))
        (insert "\n")
        (insert (propertize
                 " EBuku\n"
                 'face 'ebuku-heading-face))
        (insert (propertize
                 "  ----------\n\n"
                 'face 'ebuku-separator-face))
        (setq ebuku--results-start (point))
        (cond
         ((eq 'all ebuku-display-on-startup)
          (ebuku-show-all))
         ((eq 'recent ebuku-display-on-startup)
          (ebuku-search-on-recent))
         ((eq nil ebuku-display-on-startup)
          (insert "  [ Please specify a search, or press 'r' for recent additions. ]")))
        (goto-char ebuku--results-start)
        (add-text-properties (point-min) (point)
                             '(read-only t intangible t))
        (ebuku--create-mode-menu)
        (setq header-line-format nil)
        (ebuku-mode))
      (switch-to-buffer "*EBuku*"))))


;; --

(provide 'ebuku)

;;; ebuku.el ends here
