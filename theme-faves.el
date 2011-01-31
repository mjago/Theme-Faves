;; theme-faves.el
;; Martyn Jago
;; * Theme Faves Emacs
;; 
;; ** Setup
;; 
;; *** I have used the F7 key for theme cycling
;;  - Favourites cycling on F7
;;  - Favourites reverse cycling on Shift-F7
;;  - Audition cycling on Control-F7
;;  - Audition reverse cycling on Shift-Control-F7
;; 
;; #+BEGIN_SRC emacs-lisp
;; 
;; (add-to-path 'load-path (concat dotfile-dir "theme-faves"))
;; (require 'theme-faves)
;; (add-hook 'window-setup-hook 'theme-faves-init)
;; 
;; ;; to add to F7 key...
;; (global-set-key [f7] 'theme-faves-cycle-up)
;; (global-set-key [S-f7] 'theme-faves-cycle-down)
;; (global-set-key [C-f7] 'theme-faves-audition-cycle-up)
;; (global-set-key [S-C-f7] 'theme-faves-audition-cycle-down)
;; 
;; #+END_SRC
;; 
;; 
;; ** What its for?
;; 
;;  - Theme Faves works with [[http://www.emacswiki.org/emacs/ColorTheme][color-theme]] to provide a rapid way of
;;    switching between and selecting favourite themes
;;    - (themes provided by [[http://www.emacswiki.org/emacs/ColorTheme][color-theme]] - add to these at will)
;; 
;; ** Features
;; 
;;  - Single key or chord stroke switching between unlimited favourite
;;    themes
;;  - Easy selection of new favourites from over 150 themes
;;  - Easy deletion of favourites
;;  - Easy generation of new theme templates from favourites
;;  - To reverse favourites or audition cycle direction hold down shift
;;  - theme-faves-select(theme) function available for buffer switchers
;;    to switch theme
;; 
;; ** Other functions
;; 
;; The following functions  are not currently bound to keys since they
;; aren't used very often...
;; 
;;  - M-x theme-faves-insert
;;    - Insert current auditioned theme into favourites
;;  - M-x theme-faves-delete
;;    - Delete currently displayed theme from favourites 
;; 
;; ** Other theme switchers
;; 
;;    Other theme switchers exist and may be checked out [[http://www.emacswiki.org/emacs/ColorTheme][here]]

(defun theme-faves-init ()
  (interactive)
  (setq theme-faves-active-idx 0)
  (setq theme-faves-has-focus-p t)
  (setq theme-faves-saved-idx 0)
  (theme-faves-activate) t)

(defun theme-faves-convert-fave-idx(idx)
  (nth idx theme-faves-list))

(defun theme-faves-get-fave-theme(idx)
  (let ((temp ( theme-faves-convert-fave-idx idx)))
    (nth temp theme-faves-audition-list)))

(defun theme-faves--insert (elem org-list pos)
  (if (or (equal pos 0)
          (equal org-list nil))
      (cons elem org-list)
    (cons (car org-list) (theme-faves--insert elem (cdr org-list) (- pos 1)))))

(defun theme-faves-insert ()
  "Insert current auditioned theme into favourites"
  (interactive)
  (theme-faves-startup-check)
  (when theme-faves-active-idx
    (progn
      (when (not theme-faves-has-focus-p)
          (theme-faves-cycle-up))
      (customize-save-variable
       'theme-faves-list
       (theme-faves--insert
        theme-faves-saved-idx
        theme-faves-list
        theme-faves-active-idx))
      (theme-faves-activate)
      (theme-faves-insert-message)
      )))

(defun theme-faves-insert-message ()
  (message "%S inserted into favourites"
          (theme-faves-get-fave-theme theme-faves-active-idx)))

(defun theme-faves-remove (org-list pos &optional ini)
  (unless ini
    (setq ini 1)) 
  (if (equal pos ini)
      (cdr org-list)
    (cons (car org-list) (theme-faves-remove (cdr org-list) pos (+ ini 1)))))
 
(defun theme-faves-delete ()
  "Delete current favourite from favourites. You can add back to favourites by re-auditioning again later if you need to."
  (interactive)
  (theme-faves-startup-check)
  (when (not theme-faves-has-focus-p)
    (theme-faves-cycle-up))

  (if (and theme-faves-active-idx
             (> (length theme-faves-list) 1))
    (progn
      (customize-save-variable
       'theme-faves-list
       (theme-faves-remove theme-faves-list
                           (+ 1 theme-faves-active-idx)))
      (theme-faves-wrap-themes 'up)
      (theme-faves-activate)
      )
    (message "You need at least one favourite theme!")))

;;TODO
(defun theme-faves-delete-audition ()
  "Delete theme from audition candidate themes"
  (interactive)
  (theme-faves-startup-check))

(defun theme-faves-select()
  "Switches theme to theme referenced by THEME-IDX if it exists THEME-IDX must be greater or equal to 1"
  (interactive)
  (let (( theme-idx
          (cond ((equal major-mode 'org-mode)
                 2)
          ((equal major-mode 'emacs-lisp-mode)
                 3)
                (t 1))))
    (when (and (<= theme-idx (length theme-faves-list))
               (> theme-idx 0))
      (progn
        (setq theme-idx (- theme-idx 1))
        (if theme-faves-has-focus-p
            (setq theme-faves-active-idx theme-idx)
          (progn
            (setq theme-faves-saved-idx theme-idx)
            (theme-faves-swap-cycle-mode)))
        (theme-faves-activate)))))
;;TODO
(defun theme-faves-create ()
  "Create new named theme template for modification and further customization."
  (interactive)
  (theme-faves-startup-check))

(defun theme-faves-cycle-up ()
  "Bind to a key or key-chord to enable cycling favourite themes"
  (interactive)
  (if (theme-faves-startup-check)
      (theme-faves--cycle t 'up)))

(defun theme-faves-cycle-down ()
  "Bind to a key or key-chord to enable cycling favourite themes"
  (interactive)
  (if (theme-faves-startup-check)
      (theme-faves--cycle t 'down)))

(defun theme-faves--cycle (do-faves-p direction)
  (interactive)
  (if (not (equal do-faves-p theme-faves-has-focus-p))
      (theme-faves-swap-cycle-mode do-faves-p)
    (theme-faves-next-theme direction))
  (theme-faves-activate))

(defun theme-faves-audition-cycle-up ()
  "Bind to a key or key-chord to enable auditioning of all themes"
  (interactive)
  (theme-faves-startup-check)
  (theme-faves--cycle nil 'up))

(defun theme-faves-audition-cycle-down ()
  "Bind to a key or key-chord to enable auditioning of all themes"
  (interactive)
  (theme-faves-startup-check)
  (theme-faves--cycle nil 'down))

(defun theme-faves-startup-check ()
  (if(not (boundp 'theme-faves-saved-idx))
      (progn
        (theme-faves-init)
        nil) t))

(defun theme-faves-swap-cycle-mode (do-faves-p)
  (let ((temp theme-faves-active-idx))
    (setq theme-faves-active-idx theme-faves-saved-idx)
    (setq theme-faves-saved-idx temp)
    (setq theme-faves-has-focus-p do-faves-p)))

(defun theme-faves-next-theme (direction)
  (if (equal direction 'up)
      (setq theme-faves-active-idx (+ theme-faves-active-idx 1))
    (setq theme-faves-active-idx (- theme-faves-active-idx 1)))
  (theme-faves-ensure-within-range direction))

(defun theme-faves-ensure-within-range (direction)
  (if theme-faves-has-focus-p
      (when (or (>= theme-faves-active-idx (length theme-faves-list))
                (< theme-faves-active-idx 0))
        (theme-faves-wrap-themes direction))
    (when (or (>= theme-faves-active-idx (length theme-faves-audition-list))
              (< theme-faves-active-idx 0))
      (theme-faves-wrap-themes direction))))

(defun theme-faves-wrap-themes (direction)

  (if theme-faves-has-focus-p
      (if (equal direction 'up)
          (setq theme-faves-active-idx 0)
        (setq theme-faves-active-idx (- (length theme-faves-list) 1)))
    (if (equal direction 'up)
        (setq theme-faves-active-idx 0)
      (setq theme-faves-active-idx (- (length theme-faves-audition-list) 1)))))

(defun theme-faves-activate ()
  (let ((theme
         (if theme-faves-has-focus-p
             (progn
               (nth (theme-faves-convert-fave-idx
                     theme-faves-active-idx)
                    theme-faves-audition-list))
           (nth theme-faves-active-idx
                theme-faves-audition-list))))

    (if (theme-faves-ensure-exists-p theme)
        (progn
          (funcall theme)
          (theme-faves-activated-message theme))
      (theme-faves-doesnt-exist-message theme))))

(defun theme-faves-doesnt-exist-message (theme)
      (message "Can't find theme: %S, %d of %d"
             theme
             (+ theme-faves-active-idx 1)
             (if theme-faves-has-focus-p
                 (length theme-faves-list)
               (length theme-faves-audition-list))))

(defun theme-faves-ensure-exists-p (theme)
  (if (and theme
           (fboundp theme)) t  nil))

(defun theme-faves-activated-message (theme)
  (message "Theme changed to: %S, %d of %d"
           theme
           (+ theme-faves-active-idx 1)
           (if theme-faves-has-focus-p
               (length theme-faves-list)
             (length theme-faves-audition-list))))

(defun theme-faves-debug-message ()
  (message "theme-faves-active-idx: %d - theme-faves-saved-idx: %d - theme-faves-has-focus-p: %S"
           theme-faves-active-idx
           theme-faves-saved-idx
           theme-faves-has-focus-p))

(defcustom theme-faves-list '(13 14 3)
  "Favourite Themes List"
  :type 'list
  :group 'theme-faves)

(defcustom theme-faves-audition-list (list
   'color-theme-aalto-dark
   'color-theme-aalto-light
   'color-theme-aliceblue
   'color-theme-andreas
   'color-theme-arjen
   'color-theme-bharadwaj
   'color-theme-bharadwaj-slate
   'color-theme-billw
   'color-theme-black-on-gray
   'color-theme-blippblopp
   'color-theme-blue-mood
   'color-theme-blue-sea
   'color-theme-calm-forest
   'color-theme-charcoal-black
   'color-theme-clarity
   'color-theme-classic
   'color-theme-comidia
   'color-theme-dark-blue
   'color-theme-dark-blue2
   'color-theme-dark-laptop
   'color-theme-deep-blue
   'color-theme-digital-ofs1
   'color-theme-emacs-21
   'color-theme-emacs-nw
   'color-theme-euphoria
   'color-theme-feng-shui
   'color-theme-fischmeister
   'color-theme-gnome
   'color-theme-gnome2
   'color-theme-goldenrod
   'color-theme-gray1
   'color-theme-gray30
   'color-theme-greiner
   'color-theme-gtk-ide
   'color-theme-high-contrast
   'color-theme-hober
   'color-theme-infodoc
   'color-theme-jb-simple
   'color-theme-jedit-grey
   'color-theme-jonadabian
   'color-theme-jonadabian-slate
   'color-theme-jsc-dark
   'color-theme-jsc-light
   'color-theme-jsc-light2
   'color-theme-katester
   'color-theme-kingsajz
   'color-theme-late-night
   'color-theme-lawrence
   'color-theme-ld-dark
   'color-theme-lethe
   'color-theme-marine
   'color-theme-marquardt
   'color-theme-matrix
   'color-theme-midnight
   'color-theme-mistyday
   'color-theme-montz
   'color-theme-oswald
   'color-theme-parus
   'color-theme-pierson
   'color-theme-pok-wob
   'color-theme-pok-wog
   'color-theme-ramangalahy
   'color-theme-raspopovic
   'color-theme-renegade
   'color-theme-resolve
   'color-theme-retro-green
   'color-theme-retro-orange
   'color-theme-robin-hood
   'color-theme-rotor
   'color-theme-ryerson
   'color-theme-scintilla
   'color-theme-shaman
   'color-theme-simple-1
   'color-theme-sitaramv-nt
   'color-theme-sitaramv-solaris
   'color-theme-snow
   'color-theme-snowish
   'color-theme-standard
   'color-theme-subtle-blue
   'color-theme-subtle-hacker
   'color-theme-taming-mr-arneson
   'color-theme-taylor
   'color-theme-tty-dark
   'color-theme-vim-colors
   'color-theme-whateveryouwant
   'color-theme-wheat
   'color-theme-word-perfect
   'color-theme-xemacs
   'color-theme-xp)
  "Audition Themes List"
  :type 'list
  :group 'theme-faves)

(provide 'theme-faves)

