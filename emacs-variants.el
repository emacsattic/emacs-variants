;;; emacs-variants.el --- identify emacs versions, variants, and platforms

;; Author: Noah Friedman <friedman@splode.com>
;; Public domain

;; $Id: emacs-variants.el,v 1.2 2016/11/24 20:24:52 friedman Exp $

;;; Commentary:
;;; Code:

;;;###autoload
(defun emacs-variant ()
  "Returns a symbol indicating emacs variant.
This can be one of `emacs', `xemacs', `lucid', `epoch', `mule', etc."
  (let ((data (match-data))
        (version (cond
                  ((fboundp 'nemacs-version)
                   (nemacs-version))
                  (t
                   (emacs-version))))
        (alist '(("\\bXEmacs\\b" . xemacs)
                 ("\\bLucid\\b"  . lucid)
                 ("^Nemacs\\b"   . nemacs)
                 ("^GNU Emacs"   . emacs)))
        result)
    (while alist
      (cond
       ((string-match (car (car alist)) version)
        (setq result (cdr (car alist)))
        (setq alist nil))
       (t
        (setq alist (cdr alist)))))
    (store-match-data data)
    result))

;;;###autoload
(defun version-components (version-string)
  "Return the version components of VERSION-STRING as a list.
Any character other than `.' or the digits 0-9 terminate the search.

Examples:

    \(version-components \"19.30.1\"\)      ; emacs-version
    => \(\"19\" \"30\" \"1\"\)

    \(version-components \"5.72 \(beta\)\"\)  ; vm-version
    => \(\"5\" \"72\"\)

    \(version-components \"1.14/1.52\"\)    ; efs-version
    => \(\"1\" \"14\"\)"
  (let ((l nil)
        (len (length version-string))
        (pos 0)
        (match-data (match-data)))
    (while (string-match "^[^0-9]+" version-string pos)
      (setq pos (match-end 0)))
    (while (< pos len)
      (while (string-match "[0-9]+" version-string pos)
        (setq l (cons (matching-substring 0 version-string) l))
        (if (and (< (match-end 0) len)
                 (= ?. (aref version-string (match-end 0))))
            (setq pos (1+ (match-end 0)))
          (setq pos len))))
    (store-match-data match-data)
    (nreverse l)))

;;;###autoload
(defun emacs-version-get-component (component)
  (let ((old-match-data (match-data))
	(version 0)
	(regexp (cond
		 ((eq 'major component) "^\\([0-9]+\\)")
		 ((eq 'minor component) "^[0-9]+\\.\\([0-9]+\\)")
		 ((eq 'build component) "^[0-9]+\\.[0-9]+\\.\\([0-9]+\\)"))))
    (unwind-protect
	(and (string-match regexp emacs-version)
	     (setq version
		   (string-to-number (substring emacs-version
                                                (match-beginning 1)
                                                (match-end 1)))))
      (store-match-data old-match-data))
    version))

;;;###autoload
(defun emacs-version-major ()
  "Returns (as an integer) the major version number."
  (interactive)
  (emacs-version-get-component 'major))

;;;###autoload
(defun emacs-version-minor ()
  "Returns (as an integer) the minor version number."
  (interactive)
  (emacs-version-get-component 'minor))

;;;###autoload
(defun emacs-version-build ()
  "Returns (as an integer) the build version number."
  (interactive)
  (emacs-version-get-component 'build))

;;;###autoload
(defun system-configuration-matches-p (&rest regexps)
  "Return true if any of REGEXPs match the current system configuration.
The actual return value is the regexp successfully matching the current
value of the variable `system-configuration'.

Note: system-configuration is not defined prior to Emacs 19.23."
  (and (boundp 'system-configuration)
       (stringp system-configuration)
       (let ((result nil)
             (case-fold-search t)
             (match-data (match-data)))
         (while regexps
           (cond ((string-match (car regexps) system-configuration)
                  (setq result (car regexps))
                  (setq regexps nil))
                 (t
                  (setq regexps (cdr regexps)))))
         (store-match-data match-data)
         result)))

(provide 'emacs-variants)

;;; emacs-variants.el ends here.
