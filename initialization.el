 ;; first it is necessary to ensure that Org-mode loads support for the
  ;; languages used by code blocks in this article
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (ditaa      . t)     
     (dot        . t)
     (emacs-lisp . t)
     (haskell    . t)
     (org        . t)
     (perl       . t)
     (python     . t)
     (R          . t)
     (ruby       . t)
     (sh         . t)
     (sqlite     . t)))
  ;; then we'll remove the need to confirm evaluation of each code
  ;; block, NOTE: if you are concerned about execution of malicious code
  ;; through code blocks, then comment out the following line
  (setq org-confirm-babel-evaluate nil)
  ;; finally we'll customize the default behavior of Org-mode code blocks
  ;; so that they can be used to display examples of Org-mode syntax
  (setf org-babel-default-header-args:org '((:exports . "code")))
