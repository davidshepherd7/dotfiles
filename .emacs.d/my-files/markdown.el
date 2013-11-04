
;; run markdown-mode on files ending in .md
(set 'auto-mode-alist
      (append auto-mode-alist '((".md" . markdown-mode)
                                (".markdown" . markdown-mode))))
