;;; 01_init-keybind.el --- keybind conf
;;; Commentary:
;;; Code:

(require 'mylib)

(global-set-key (kbd "C-M-k")
  (lambda ()
    (interactive)
    (kill-buffer (current-buffer))))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "M-S-z") 'redo)

(global-set-key (kbd "s-<right>") 'right-word)
(global-set-key (kbd "s-<left>") 'left-word)
(global-set-key (kbd "s-<up>") 'backward-paragraph)
(global-set-key (kbd "s-<down>") 'forward-paragraph)
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)
(global-set-key (kbd "s-w") 'kill-ring-save)

;; ¥の代わりにバックスラッシュを入力する
(define-key global-map [?¥] [?\\])

(keyboard-translate ?\C-h ?\C-?)

(global-set-key (kbd "C-m") 'newline-and-indent)

(global-set-key (kbd "C-<tab>") 'indent-for-tab-command)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "M-O") 'my-edit-previous-line)
(global-set-key (kbd "S-<return>") 'my-edit-next-line)
(global-set-key (kbd "C-S-<return>") 'my-edit-previous-line)

(global-set-key (kbd "C-x C-z") 'nil)

(global-set-key (kbd "C-x C-o") 'other-window)

;; window操作
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "C-t") 'my-other-window-or-split)

;; for oyayubi shift
(global-set-key (kbd "M-,") #'(lambda () (interactive) (insert "、")))

;; | C-t           | 別のウィンドウに移動する。ウィンドウが一つの       |
;; |               | 場合はウィンドウを分割する                         |
;; | C-x <right>   | 右のウィンドウに移動                               |
;; | C-x <left>    | 左のウィンドウに移動                               |
;; | C-x <down>    | 下のウィンドウに移動                               |
;; | C-x <up>      | 上のウィンドウに移動                               |
;; | C-x C-o       | 間違ってC-x C-o としてしまったとき用のキーバインド |
;; | C-x C-z       | なにか忘れたけどnilにする                          |
;; | M-O           | 次の行を挿入                                       |
;; | S-<return>    | 次の行に挿入                                       |
;; | C-S-<return>  | 前の行に挿入                                       |
;; | C-+           | バッファの文字を拡大                               |
;; | C--           | バッファの文字を縮小                               |
;; | C-<tab>       | タブによるインデント                               |
;; | C-m           | 改行 + インデントをする                            |
;; | s-w           | Mac用 コピー                                       |
;; | s-<backspace> | Mac用 1単語削除                                    |
;; | s-<down>      | Mac用 次のパラグラフ (空行?)に飛ぶ                 |
;; | s-<up>        | Mac用 前のパラグラフ (空行?)に飛ぶ                 |
;; | s-<left>      | Mac用 前の単語に移動                               |
;; | s-<right>     | Mac用 次の単語に移動                               |
;; | M-S-z         | リドゥ                                             |
;; | C-S-z         | リドゥ                                             |
;; | C-z           | アンドゥ                                           |
;; | C-M-k         | 確認なしでパッファをキルする                       |

;;; 01_init-keybind.el ends here
