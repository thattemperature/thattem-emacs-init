;;; Thattem-emacs-init --- my emacs settings  -*- lexical-binding: t; -*-

;; Author: That Temperature <2719023332@qq.com>
;; Package-Requires: (agenix colorful-mode company-posframe company-prescient compile-multi-nerd-icons consult-company consult-compile-multi consult-eglot consult-flyspell consult-org-roam consult-projectile consult-yasnippet envrc fennel-mode fish-completion forge gptel-agent haskell-ts-mode kotlin-ts-mode marginalia modus-themes nerd-icons-dired nix-ts-mode nixfmt rainbow-delimiters rime sdcv thattem-mode-line thattem-tab-bar tramp treesit-auto undo-tree verilog-ts-mode vertico-prescient vhdl-ts-mode yasnippet-snippets)
;; URL: https://github.com/thattemperature/thattem-emacs-init

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(use-package agenix
  :mode
  ("\\.age\\'" . agenix-mode-if-with-secrets-nix))


(use-package auth-source
  :custom
  (auth-sources
   '(;;PLACEHOLDER:AUTH-SOURCE;;
     "~/.authinfo")))


(use-package autorevert
  :hook
  (after-init . global-auto-revert-mode))


(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-string
   (concat
    (nerd-icons-faicon "nf-fae-palette_color")
    "."))
  :hook
  (after-init . global-colorful-mode))


(use-package company
  :bind
  (("C-M-<tab>" . completion-at-point)
   ("M-[" . completion-at-point)
   ("C-c c" . completion-at-point)
   ("C-c C-c" . completion-at-point)
   ([remap completion-at-point] . company-complete)
   ([remap indent-for-tab-command] . company-indent-or-complete-common)
   :map company-active-map
   ("M-s" . consult-company))
  :custom
  (company-idle-delay nil)
  (tab-always-indent 'complete)
  :hook
  (after-init . global-company-mode))


(use-package company-posframe
  :custom
  (company-posframe-quickhelp-delay nil)
  :hook
  (after-init . company-posframe-mode))


(use-package company-prescient
  :hook
  (after-init . company-prescient-mode))


(use-package compile-multi
  :bind
  (("M-p" . compile)
   ("M-]" . compile)
   ("C-c p" . compile)
   ("C-c C-p" . compile)
   ([remap compile] . compile-multi))
  :custom
  (compile-multi-default-directory #'projectile-project-root)
  (compile-multi-config
   `(((projectile-file-exists-p
       (concat (projectile-project-root) "CMakeLists.txt"))
      ,(lambda ()
         (list
          (cons "CMake generate"
                (concat "cmake -S "
                        (projectile-project-root)
                        " -B "
                        (projectile-project-root)
                        "build/"))
          (cons "CMake build"
                (concat "cmake --build "
                        (projectile-project-root)
                        "build/")))))
     ((or (projectile-file-exists-p
           (concat (projectile-project-root) "Makefile"))
          (projectile-file-exists-p
           (concat (projectile-project-root) "makefile")))
      ,(lambda ()
         (list
          (cons "Make"
                (concat "make -k "
                        (when (projectile-project-root)
                          " -C ")
                        (projectile-project-root))))))
     ((derived-mode-p 'c++-ts-mode 'c++-mode)
      ,(lambda ()
         (list
          (cons "G++"
                (concat "g++ "
                        (buffer-file-name)
                        " -o "
                        (file-name-sans-extension
                         (buffer-file-name))
                        (car exec-suffixes)))
          (cons "G++ and run"
                (concat "g++ "
                        (buffer-file-name)
                        " -o "
                        (file-name-sans-extension
                         (buffer-file-name))
                        (car exec-suffixes)
                        " ; "
                        (file-name-sans-extension
                         (buffer-file-name))
                        (car exec-suffixes))))))
     ((derived-mode-p 'c-ts-mode 'c-mode)
      ,(lambda ()
         (list
          (cons "Gcc"
                (concat "gcc "
                        (buffer-file-name)
                        " -o "
                        (file-name-sans-extension
                         (buffer-file-name))
                        (car exec-suffixes)))
          (cons "Gcc and run"
                (concat "gcc "
                        (buffer-file-name)
                        " -o "
                        (file-name-sans-extension
                         (buffer-file-name))
                        (car exec-suffixes)
                        " ; "
                        (file-name-sans-extension
                         (buffer-file-name))
                        (car exec-suffixes))))))))
  :config
  (use-package compile-multi-nerd-icons)
  (use-package consult-compile-multi
    :functions
    consult-compile-multi-mode
    :config
    (consult-compile-multi-mode)))


(use-package consult
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ([remap yank] . consult-yank-from-kill-ring)
   ([remap yank-pop] . consult-yank-replace)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ("M-g M-i" . consult-imenu-multi)
   ("M-g m" . consult-mark)
   ("M-g M-m" . consult-global-mark)
   ("M-g d" . consult-flymake)
   ("M-g M-d" . consult-flymake)
   ("M-s s" . consult-line)
   ("M-s M-s" . consult-line-multi)
   ("M-s g" . consult-ripgrep)
   ("M-s M-g" . consult-ripgrep)))


(use-package consult-company
  :bind
  (:map company-active-map
        ("M-s" . consult-company)))


(use-package consult-eglot
  :bind
  (:map eglot-mode-map
        ("M-s e" . consult-eglot-symbols)))


(use-package consult-flyspell
  :bind
  (("M-g s" . consult-flyspell)
   ("M-g M-s" . consult-flyspell))
  :custom
  (consult-flyspell-select-function
   #'flyspell-correct-word))


(use-package consult-org-roam
  :bind
  (("C-c n f" . consult-org-roam-file-find)
   ("C-c n s" . consult-org-roam-search)
   :map org-mode-map
   ("C-c b" . consult-org-roam-backlinks)
   ("C-c f" . consult-org-roam-forward-links))
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :functions
  consult-org-roam-mode
  :config
  (consult-org-roam-mode))


(use-package consult-projectile
  :bind
  (("C-x P" . consult-projectile)))


(use-package consult-yasnippet
  :bind
  (("C-c y" . consult-yasnippet)))


(use-package delsel
  :hook
  (after-init . delete-selection-mode))


(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t))


(use-package display-line-numbers
  :custom
  (display-line-numbers-major-tick 16)
  (display-line-numbers-minor-tick 4)
  (display-line-numbers-width 4)
  :custom-face
  (line-number
   ((t :weight thin)))
  (line-number-major-tick
   ((((class color) (background dark))
     :box (:line-width (-6 . -6) :color "#9a609a")
     :foreground "#c04028"
     :weight bold)
    (((class color) (background light))
     :box (:line-width (-6 . -6) :color "#e880e8")
     :foreground "#a00000"
     :weight bold)
    (t :inverse-video t)))
  (line-number-minor-tick
   ((((class color) (background dark))
     :box (:line-width (-6 . -6) :color "#308d8a")
     :foreground "#33b420"
     :weight normal)
    (((class color) (background light))
     :box (:line-width (-6 . -6) :color "#6cd9d9")
     :foreground "#0000a0"
     :weight normal)
    (t :inverse-video t)))
  :hook
  (after-init . global-display-line-numbers-mode))


(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c r" . eglot-rename)
        ("C-c f" . eglot-format)
        ("C-c a" . eglot-code-actions)
        ("M-s e" . consult-eglot-symbols))
  :custom
  (eglot-server-programs
   `(((c++-mode c-mode c++-ts-mode c-ts-mode) .
      ("clangd" "--header-insertion=never"))
     ((rust-mode rust-ts-mode) .
      ("rust-analyzer" :initializationOptions
       (:check (:command "clippy"))))
     ((python-mode python-ts-mode) "pylsp")
     ((cmake-mode cmake-ts-mode) "cmake-language-server")
     ((nix-mode nix-ts-mode) "nixd")
     ((sh-mode bash-ts-mode) .
      ("bash-language-server" "start"))
     ((tex-mode latex-mode) "texlab")
     ((fennel-mode) "fennel-ls")))
  :hook
  ((c-mode c++-mode rust-mode
           python-mode cmake-mode nix-mode
           sh-mode tex-mode latex-mode
           fennel-mode)
   .
   eglot-ensure)
  ((c-ts-mode c++-ts-mode rust-ts-mode
              python-ts-mode cmake-ts-mode nix-ts-mode
              bash-ts-mode)
   .
   eglot-ensure))


(use-package emacs
  :bind
  (("C-c s e" . eshell)
   ("C-c s s" . shell))
  :custom
  (enable-recursive-minibuffers t)
  (inhibit-startup-screen t)
  (tab-width 4)
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (truncate-partial-width-windows nil))


(use-package envrc
  :hook
  (after-init . envrc-global-mode))


(use-package faces
  :custom-face
  (default
   ((t
     :weight normal
     :slant normal
     :height 200))))


(use-package fennel-mode
  :mode
  ("\\.fnlm\\'" . fennel-mode)
  ("\\.fnl\\'" . fennel-mode))


(use-package ffap
  :hook
  (after-init . ffap-bindings))


(use-package files
  :custom
  (make-backup-files nil))


(use-package find-file
  :bind
  (("M-g o" . ff-find-other-file)
   ("M-g M-o" . ff-find-other-file)))


(use-package find-func
  :bind
  (("C-h C-f" . find-function)
   ("C-h C-v" . find-variable)))


(use-package fish-completion
  :hook
  (after-init . global-fish-completion-mode))


(use-package flymake
  :custom-face
  (flymake-error
   ((((class color) (background dark))
     :background "#883d71"
     :underline nil)
    (((class color) (background light))
     :background "#ffb8f4"
     :underline nil)
    (t :inverse-video t)))
  (flymake-warning
   ((((class color) (background dark))
     :background "#40522a"
     :underline nil)
    (((class color) (background light))
     :background "#efffd0"
     :underline nil)
    (t :inverse-video t)))
  (flymake-note
   ((((class color) (background dark))
     :background "#202a63"
     :underline nil)
    (((class color) (background light))
     :background "#dbe7ff"
     :underline nil)
    (t :inverse-video t)))
  :hook
  (emacs-lisp-mode . flymake-mode))


(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))


(use-package fontset
  :preface
  (defun thattem-emacs-init--fonts ()
    (let ((font-families (font-family-list))
          (basic-candidates
           '("Iosevka Nerd Font"
             "Sarasa Mono SC"))
          (other-candidates
           '("Symbols Nerd Font Mono")))
      (let ((filter
             (lambda (family) (member family font-families))))
        (let ((basic-families
               (seq-filter filter basic-candidates))
              (other-families
               (seq-filter filter other-candidates)))
          (when basic-families
            (custom-set-faces
             `(default
               ((t :family ,(car basic-families)))))
            (dolist (family basic-families)
              (set-fontset-font t nil (font-spec :family family)
                                nil 'append)))
          (dolist (family other-families)
            (set-fontset-font t nil (font-spec :family family)
                              nil 'append))))))
  :hook
  (after-init . thattem-emacs-init--fonts))


(use-package frame
  :hook
  (after-init . toggle-frame-maximized)
  (after-init . toggle-frame-fullscreen))


(use-package gptel)
(use-package gptel
  :bind
  (("C-c g g" . gptel)
   ("C-c g s" . gptel-send)
   ("C-c g m" . gptel-menu)
   ("C-c g r" . gptel-rewrite)
   ("C-c g a" . gptel-add)
   ("C-c g k" . gptel-context-remove-all))
  :custom
  (gptel-model 'deepseek-reasoner)
  (gptel-backend
   (prog1
       (gptel-make-deepseek "DeepSeek"
         :stream t
         :key #'gptel-api-key-from-auth-source)
     (gptel-make-anthropic "Claude"
       :stream t
       :key #'gptel-api-key-from-auth-source)))
  :custom
  (gptel-highlight-methods '(face))
  :hook
  (gptel-post-stream . gptel-auto-scroll)
  (gptel-post-response . gptel-end-of-response))


(use-package gptel-agent
  :bind
  (("C-c g t" . gptel-agent))
  :hook
  (after-init . gptel-agent-update))


(use-package haskell-ts-mode
  :mode
  ("\\.hs\\'" . haskell-ts-mode))


(use-package hideshow
  :functions
  nerd-icons-octicon
  :preface
  (defun thattem-emacs-init--hideshow-overlay (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((face '(:weight bold :box (:line-width (0 . -2))))
             (nlines (count-lines (overlay-start ov)
                                  (overlay-end ov)))
             (info (concat (nerd-icons-octicon "nf-oct-fold"
                                               :face face)
                           (propertize(format " %d ..." nlines)
                                      'face face))))
        (overlay-put ov 'display info))))
  :custom
  (hs-set-up-overlay #'thattem-emacs-init--hideshow-overlay)
  :hook
  (prog-mode . hs-minor-mode))


(use-package hl-line
  :hook
  (after-init . global-hl-line-mode))


(use-package isearch
  :custom
  (isearch-lazy-count t))


(use-package kotlin-ts-mode
  :mode
  ("\\.kts?\\'" . kotlin-ts-mode))


(use-package magit
  :bind
  (("C-x g" . magit-status))
  :config
  (use-package forge))


(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :hook
  (after-init . marginalia-mode))


(use-package mb-depth
  :hook
  (after-init . minibuffer-depth-indicate-mode))


(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  :preface
  (defun thattem-emacs-init--themes ()
    (load-theme 'modus-operandi-tinted t))
  :hook
  (after-init . thattem-emacs-init--themes))


(use-package nerd-icons-completion
  :hook
  (after-init . nerd-icons-completion-mode)
  (marginalia-mode . nerd-icons-completion-marginalia-setup))


(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


(use-package nix-ts-mode
  :mode
  ("\\.nix\\'" . nix-ts-mode))


(use-package nixfmt
  :hook
  ((nix-mode nix-ts-mode) . nixfmt-on-save-mode))


(use-package orderless
  :custom
  (completion-styles '(partial-completion orderless basic))
  (completion-category-overrides
   '((command (styles partial-completion orderless basic))
     (symbol (styles partial-completion orderless basic))
     (function (styles partial-completion orderless basic))
     (variable (styles partial-completion orderless basic)))))


(use-package org
  :custom
  (org-pretty-entities t)
  (org-use-sub-superscripts '{})
  (org-export-with-sub-superscripts '{})
  (org-latex-packages-alist
   '(("" "xeCJK" t ("xelatex"))))
  (org-latex-compiler "xelatex"))


(use-package org-roam
  :bind
  (:map org-mode-map
        ("C-c l" . org-roam-buffer-toggle)
        ("C-c i" . org-roam-node-insert))
  :hook
  (after-init . org-roam-db-autosync-mode))


(use-package prescient
  :hook
  (after-init . prescient-persist-mode))


(use-package projectile
  :bind
  (([remap project-switch-to-buffer] . projectile-switch-to-buffer)
   ([remap project-find-dir] . projectile-find-dir)
   ([remap project-find-file] . projectile-find-file)
   ([remap project-kill-buffers] . projectile-kill-buffers)
   ([remap project-switch-project] . projectile-switch-project)
   ("C-x p C-M-%" . projectile-replace-regexp)
   ("C-x p M-%" . projectile-replace))
  :custom
  (projectile-auto-cleanup-known-projects t)
  :hook
  (after-init . projectile-mode))


(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package recentf
  :hook
  (after-init . recentf-mode))


(use-package rime)
(use-package rime
  :custom
  (rime-translate-keybindings
   '("C-p" "C-n" "C-b" "C-f" "C-a" "C-e"
     "C-d" "C-g" "M-v" "C-v" "M-b" "M-f"
     "C-x"))
  (rime-show-candidate 'posframe)
  (rime-posframe-style 'vertical)
  (rime-deactivate-when-exit-minibuffer nil)
  (default-input-method "rime"))


(use-package sdcv
  :bind
  (("M-s i" . sdcv-search-input)
   ("M-s M-i" . sdcv-search-input)
   ("M-s p" . sdcv-search-pointer)
   ("M-s M-p" . sdcv-search-pointer)))


(use-package simple
  :custom
  (save-interprogram-paste-before-kill t)
  (indent-tabs-mode nil)
  :hook
  (org-mode . auto-fill-mode))


(use-package subword
  :hook
  (after-init . global-subword-mode))


(use-package tab-bar
  :bind
  (("M-g b" . tab-bar-history-back)
   ("M-g M-b" . tab-bar-history-back)
   ("<mouse-8>" . tab-bar-history-back)
   ("M-g f" . tab-bar-history-forward)
   ("M-g M-f" . tab-bar-history-forward)
   ("<mouse-9>" . tab-bar-history-forward)
   ("C-S-t" . tab-new-to)
   ("C-S-k" . tab-close)
   ("C-S-f" . tab-next)
   ("C-S-b" . tab-previous))
  :custom
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-new-tab-choice #'get-scratch-buffer-create)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-tab-hints t)
  (tab-bar-history-limit 1024)
  :hook
  (after-init . tab-bar-mode)
  (after-init . tab-bar-history-mode))


(use-package thattem-mode-line
  :bind
  (("C-c l" . thattem-shell-window-dwim)
   ("C-c h" . thattem-help-window-dwim))
  :hook
  (after-init . thattem-mode-line-mode))


(use-package thattem-tab-bar
  :hook
  (after-init . thattem-tab-bar-mode))


(use-package time
  :custom
  (display-time-interval 1)
  (display-time-format "%Y%B%d%A %H:%M:%S")
  (display-time-default-load-average nil)
  :hook
  (after-init . display-time-mode))


(use-package tramp)


(use-package treesit-auto
  :custom
  (treesit-font-lock-level 4)
  :hook
  (after-init . treesit-auto-add-to-auto-mode-alist)
  (after-init . global-treesit-auto-mode))


(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
  :hook
  (after-init . global-undo-tree-mode))


(use-package verilog-ts-mode
  :mode
  ("\\.s?vh?\\'" . verilog-ts-mode))


(use-package vertico
  :bind
  (:map vertico-map
        ("TAB" . minibuffer-complete)
        ("C-M-<tab>" . vertico-insert))
  :hook
  (after-init . vertico-mode))


(use-package vertico-prescient
  :hook
  (after-init . vertico-prescient-mode))


(use-package vhdl-ts-mode
  :mode
  ("\\.vhdl?\\'" . vhdl-ts-mode))


(use-package whitespace
  :custom
  (whitespace-line-column nil)
  (whitespace-style
   '(face ;show different face
     trailing ;highlight the spaces at the end of a line
     tabs ;show tab face
     spaces ;show space face
     lines-tail ;highlight the tail of too long lines
     newline ;show new line face
     missing-newline-at-eof ;highlight last character
     empty ;highlight empty lines at the begging and end
     newline-mark ;show newline mark
     ))
  (whitespace-display-mappings ;set newline mark
   '((newline-mark ?\n [? ?\n] [?$ ?\n])))
  :custom-face
  (whitespace-space
   ((((class color) (background dark))
     :box (:line-width (-2 . -2) :color "#331111")
     :background unspecified)
    (((class color) (background light))
     :box (:line-width (-2 . -2) :color "#d1eeee")
     :background unspecified)
    (t :inverse-video t)))
  (whitespace-tab
   ((((class color) (background dark))
     :box (:line-width (-2 . -2) :color "#333311")
     :background unspecified)
    (((class color) (background light))
     :box (:line-width (-2 . -2) :color "#eeee00")
     :background unspecified)
    (t :inverse-video t)))
  (whitespace-trailing
   ((((class color) (background dark))
     :box (:line-width (-2 . -2) :color "#800000")
     :background "#594400")
    (((class color) (background light))
     :box (:line-width (-2 . -2) :color "#ee0000")
     :background "#ffdab9")
    (t :inverse-video t)))
  (whitespace-line
   ((((class color) (background dark))
     :background "#594400"
     :foreground unspecified)
    (((class color) (background light))
     :background "#ffdab9"
     :foreground unspecified)
    (t :inverse-video t)))
  (whitespace-newline
   ((((class color) (background dark))
     :background unspecified
     :foreground "#866440")
    (((class color) (background light))
     :background unspecified
     :foreground "#866440")
    (t :inverse-video t)))
  (whitespace-empty
   ((((class color) (background dark))
     :background "#233120"
     :foreground unspecified)
    (((class color) (background light))
     :background "#dfffd0"
     :foreground unspecified)
    (t :inverse-video t)))
  (whitespace-missing-newline-at-eof
   ((((class color) (background dark))
     :background "#842111"
     :foreground unspecified)
    (((class color) (background light))
     :background "#ff9aaa"
     :foreground unspecified)
    (t :inverse-video t)))
  :hook
  (after-init . global-whitespace-mode))


(use-package window
  :custom
  (split-width-threshold 96))


(use-package xref
  :custom
  (xref-history-storage #'xref-window-local-history))


(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
        ("TAB" . nil)
        ("<backtab>" . yas-expand))
  :hook
  (after-init . yas-global-mode))


(use-package yasnippet-snippets
  :hook
  (after-init . yasnippet-snippets-initialize))


(provide 'thattem-emacs-init)
;;; thattem-emacs-init.el ends here
