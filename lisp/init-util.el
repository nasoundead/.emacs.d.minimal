;;; init-ivy.el -*- lexical-binding: t; -*-
;; -*- lexical-binding: t -*-
;; Copyright (C) 2019 Bruce Wong

;; Author: Bruce Wong <nasoundead@163.com>
;; URL: https://github.com/nasoundead/.emacs.d.minimal

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


; (use-package rg
  ; :hook (after-init . (lambda () (rg-enable-default-bindings "\M-s")))
  ; :config
  ; (setq rg-group-result t)
  ; (setq rg-show-columns t)
  ; (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)
  ; (with-eval-after-load 'projectile
    ; (defalias 'projectile-ripgrep 'rg-project))
  ; (with-eval-after-load 'counsel
    ; (bind-keys :map rg-global-map
               ; ("c r" . counsel-rg))))  
;; Youdao Dictionay
(use-package youdao-dictionary
  :bind (("C-c y" . youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point-tooltip))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))


(provide 'init-util)
