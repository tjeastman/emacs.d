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
  (org-ref-bibliography-notes "~/projects/refs/notes.org")
  (org-ref-default-bibliography '("~/projects/refs/references.bib"))
  (org-ref-pdf-directory "~/projects/refs/papers/"))

(provide 'config-org)
