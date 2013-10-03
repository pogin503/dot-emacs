;;; 50_init-tex --- 50_init-tex
;;; Commentary:
;;; Code:

;;
;; TeX mode
;;
(setq auto-mode-alist
      (append '(("\\.tex$" . latex-mode)) auto-mode-alist))
(setq tex-default-mode 'latex-mode)
(setq tex-start-commands "\\nonstopmode\\input")
(setq tex-run-command "/usr/texbin/ptex2pdf -e -ot '-synctex=1 -interaction=nonstopmode'")
;(setq tex-run-command "/usr/texbin/ptex2pdf -e -u -ot '-synctex=1 -interaction=nonstopmode'")
;(setq tex-run-command "/usr/texbin/pdftex -synctex=1 -interaction=nonstopmode")
;(setq tex-run-command "/usr/texbin/luatex -synctex=1 -interaction=nonstopmode")
;(setq tex-run-command "/usr/texbin/luajittex -synctex=1 -interaction=nonstopmode")
;(setq tex-run-command "/usr/texbin/xetex -synctex=1 -interaction=nonstopmode")
(setq latex-run-command "/usr/texbin/ptex2pdf -l -ot '-synctex=1 -interaction=nonstopmode'")
;(setq latex-run-command "/usr/texbin/ptex2pdf -l -u -ot '-synctex=1 -interaction=nonstopmode'")
;(setq latex-run-command "/usr/texbin/pdflatex -synctex=1 -interaction=nonstopmode")
;(setq latex-run-command "/usr/texbin/lualatex -synctex=1 -interaction=nonstopmode")
;(setq latex-run-command "/usr/texbin/luajitlatex -synctex=1 -interaction=nonstopmode")
;(setq latex-run-command "/usr/texbin/xelatex -synctex=1 -interaction=nonstopmode")
(setq tex-bibtex-command "/usr/texbin/pbibtex")
;(setq tex-bibtex-command "/usr/texbin/upbibtex")
;(setq tex-bibtex-command "/usr/texbin/bibtex")
;(setq tex-bibtex-command "/usr/texbin/bibtexu")
(require 'tex-mode)
(defun tex-view ()
  "Tex view setting."
  (interactive)
  (tex-send-command "/usr/bin/open -a Preview.app" (tex-append tex-print-file ".pdf")))

(defun tex-print (&optional alt)
  "Tex print setting.
`ALT' is nantoka."
  (interactive "P")
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (tex-send-command "/usr/bin/open -a \"Adobe Reader.app\"" (tex-append tex-print-file ".pdf")))
(setq tex-compile-commands
      '(("/usr/texbin/platex -synctex=1 -interaction=nonstopmode %f && /usr/texbin/dvipdfmx %r" "%f" "%r.pdf")
        ("/usr/texbin/platex -synctex=1 -interaction=nonstopmode %f && /usr/texbin/dvips -Ppdf -z -f %r.dvi | /usr/texbin/convbkmk -g > %r.ps && /usr/local/bin/ps2pdf %r.ps" "%f" "%r.pdf")
        ("/usr/texbin/uplatex -synctex=1 -interaction=nonstopmode %f && /usr/texbin/dvipdfmx %r" "%f" "%r.pdf")
        ("/usr/texbin/uplatex -synctex=1 -interaction=nonstopmode %f && /usr/texbin/dvips -Ppdf -z -f %r.dvi | /usr/texbin/convbkmk -u > %r.ps && /usr/local/bin/ps2pdf %r.ps" "%f" "%r.pdf")
        ("/usr/texbin/pdflatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
        ("/usr/texbin/lualatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
        ("/usr/texbin/luajitlatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
        ("/usr/texbin/xelatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
        ("/usr/texbin/latexmk %f" "%f" "%r.pdf")
        ("/usr/texbin/latexmk -e '$latex=q/platex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/pbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %f" "%f" "%r.pdf")
        ("/usr/texbin/latexmk -e '$latex=q/platex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/pbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvips=q/dvips %%O -z -f %%S | convbkmk -g > %%D/' -e '$ps2pdf=q/ps2pdf %%O %%S %%D/' -norc -gg -pdfps %f" "%f" "%r.pdf")
        ("/usr/texbin/latexmk -e '$latex=q/uplatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %f" "%f" "%r.pdf")
        ("/usr/texbin/latexmk -e '$latex=q/uplatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvips=q/dvips %%O -z -f %%S | convbkmk -u > %%D/' -e '$ps2pdf=q/ps2pdf %%O %%S %%D/' -norc -gg -pdfps %f" "%f" "%r.pdf")
        ("/usr/texbin/latexmk -e '$pdflatex=q/pdflatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtex %%O %%B/' -e '$makeindex=q/makeindex %%O -o %%D %%S/' -norc -gg -pdf %f" "%f" "%r.pdf")
        ("/usr/texbin/latexmk -e '$pdflatex=q/lualatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -lualatex %f" "%f" "%r.pdf")
        ("/usr/texbin/latexmk -e '$pdflatex=q/luajitlatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -lualatex %f" "%f" "%r.pdf")
        ("/usr/texbin/latexmk -e '$pdflatex=q/xelatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -xelatex %f" "%f" "%r.pdf")
        ("/usr/texbin/pbibtex %r" "%r.aux" "%r.bbl")
        ("/usr/texbin/upbibtex %r" "%r.aux" "%r.bbl")
        ("/usr/texbin/bibtex %r" "%r.aux" "%r.bbl")
        ("/usr/texbin/bibtexu %r" "%r.aux" "%r.bbl")
        ("/usr/texbin/biber %r" "%r.bcf" "%r.bbl")
        ("/usr/texbin/mendex %r" "%r.idx" "%r.ind")
        ("/usr/texbin/makeindex %r" "%r.idx" "%r.ind")
        ("/usr/texbin/texindy %r" "%r.idx" "%r.ind")
        ((concat "\\doc-view" " \"" (car (split-string (format "%s" (tex-main-file)) "\\.")) ".pdf\"") "%r.pdf")
        ("/usr/bin/open -a Preview.app %r.pdf" "%r.pdf")
        ("/usr/bin/open -a Skim.app %r.pdf" "%r.pdf")
        ("/usr/bin/open -a TeXShop.app %r.pdf" "%r.pdf")
        ("/usr/bin/open -a TeXworks.app %r.pdf" "%r.pdf")
        ("/usr/bin/open -a Firefox.app %r.pdf" "%r.pdf")
        ("/usr/bin/open -a \"Adobe Reader.app\" %r.pdf" "%r.pdf")))

(defun skim-forward-search ()
  "For skim setting."
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf (tex-main-file))
         (pf (concat (car (split-string mtf "\\.")) ".pdf"))
         (ln (format "%d" (line-number-at-pos)))
         (cmd "/Applications/Skim.app/Contents/SharedSupport/displayline")
         (args (concat ln " " pf " " ctf)))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "displayline" nil cmd args))))

(add-hook 'latex-mode-hook
          '(lambda ()
             (define-key latex-mode-map (kbd "C-c s") 'skim-forward-search)))

;;
;; RefTeX with TeX mode
;;
(add-hook 'latex-mode-hook 'turn-on-reftex)

(provide '50_init-tex)
;;; 50_init-tex ends here
