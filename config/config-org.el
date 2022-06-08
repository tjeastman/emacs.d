(use-package ob-async)

(use-package org
  :custom
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages
   '((clojure . t)
     (emacs-lisp . t)
     (python . t)
     (shell . t)
     (C . t))))

(use-package jupyter)

(provide 'config-org)
