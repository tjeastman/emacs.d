(use-package org
  :custom
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t))))

(use-package org-ref
  :custom
  (org-ref-bibliography-notes "~/refs/notes.org")
  (org-ref-default-bibliography '("~/refs/references.bib"))
  (org-ref-pdf-directory "~/refs/docs/"))

(provide 'config-org)
