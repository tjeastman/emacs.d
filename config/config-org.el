(use-package ob-async)

(use-package org
  :custom
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (C . t))))

(use-package org-ref
  :custom
  (org-ref-bibliography-notes "~/refs/notes.org")
  (org-ref-default-bibliography '("~/refs/references.bib"))
  (org-ref-pdf-directory "~/refs/docs/"))

(provide 'config-org)
