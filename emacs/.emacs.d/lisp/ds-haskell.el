
;; Load the necessary stuff
;;(load "haskell-mode-autoloads")

(use-package haskell-mode)
;; Full indentation fancyness
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
