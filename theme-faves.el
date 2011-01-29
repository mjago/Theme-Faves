;; theme-faves.el
;; Martyn Jago

(defun theme-faves-init ()
  (interactive)
  (setq theme-faves-active-idx
        theme-faves-idx)
  (setq theme-faves-has-focus-p t)
  (setq theme-faves-saved-idx 0)
  (theme-faves-activate) t)

(defun theme-get-fave-full-idx(idx)
  (nth idx theme-faves-list))

(defun theme-get-fave-theme(idx)
  (let ((temp (theme-get-fave-full-idx idx)))
    (when temp (nth temp theme-faves-audition-list)
          nil)))

(defun theme-faves-insert (elem org-list pos)
  (if (or (equal pos 0)
          (equal org-list nil))
      (cons elem org-list)
    (cons (car org-list) (theme-faves-insert elem (cdr org-list) (- pos 1)))))

(defun theme-faves-save ()
  "Save current auditioned theme to favourites"
  (interactive)
  (theme-faves-startup-check)
  (when theme-faves-active-idx
    (progn
      (customize-save-variable
       'theme-faves-list
       (theme-faves-insert
        theme-faves-active-idx
        theme-faves-list
        (if theme-faves-has-focus-p
            theme-faves-active-idx
          theme-faves-saved-idx)))
      (theme-faves-saved-message))))

(defun theme-faves-saved-message ()
  (message "%S added to favourite themes"
           (nth theme-faves-active-idx  theme-faves-audition-list)))

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
  (when (and theme-faves-has-focus-p
             theme-faves-active-idx
             (> (length theme-faves-list) 1))
    (progn
      (customize-save-variable
       'theme-faves-list
       (theme-faves-remove theme-faves-list
                           (+ 1 theme-faves-active-idx)))
      (theme-faves--cycle t))))

;;TODO
(defun theme-faves-delete-audition ()
  "Delete theme from audition candidate themes"
  (interactive)
  (theme-faves-startup-check))

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
      (theme-faves-swap-cycle-mode do-faves-p direction)
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
        nil)
    t))

(defun theme-faves-swap-cycle-mode (do-faves-p direction)
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
      (when (or (not (nth theme-faves-active-idx theme-faves-list))
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
               (nth (nth (customize-save-variable
                          'theme-faves-idx theme-faves-active-idx)
                         theme-faves-list)
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
  (fboundp theme))

(defun theme-faves-activated-message (theme)
  (message "Theme changed to: %S, %d of %d"
           theme
           (+ theme-faves-active-idx 1)
           (if theme-faves-has-focus-p
               (length theme-faves-list)
             (length theme-faves-audition-list))))

(defcustom theme-faves-idx 0
  "Index of favourite in use"
  :type 'integer
  :group 'theme-faves)

(defcustom theme-faves-list '(0 125 104 150 54 7)
  "Favourite Themes List"
  :type 'list
  :group 'theme-faves)

(defcustom theme-faves-audition-list
  (list
   'color-theme-renegade
   'color-theme-feng-shui
   'color-theme-matrix
   'color-theme-lawrence
   'color-theme-calm-forest
   'color-theme-vim-colors
   'color-theme-charcoal-black
   'color-theme-andreas
   'color-theme-clarity
   'color-theme-late-night
   'color-theme-emacs-nw
   'color-theme-shaman
   'color-theme-lethe
   'color-theme-bharadwaj-slate
   'color-theme-whateveryouwant
   'color-theme-dark-green
   'color-theme-gray30
   'color-theme-xp
   'color-theme-resolve
   'color-theme-euphoria
   'color-theme-blue-mood
   'color-theme-dark-blue2
   'color-theme-black-on-gray
   'color-theme-aliceblue
   'color-theme-tty-dark
   'color-theme-arjen
   'color-theme-katester
   'color-theme-comidia
   'color-theme-kingsajz
   'color-theme-deep-blue
   'color-theme-ld-dark
   'color-theme-jsc-light2
   'color-theme-emacs-21
   'color-theme-word-perfect
   'color-theme-gray1
   'color-theme-jonadabian-slate
   'color-theme-dark-blue
   'color-theme-subtle-blue
   'color-theme-dark-erc
   'color-theme-blue-erc
   'color-theme-marine
   'color-theme-mistyday
   'color-theme-digital-ofs1
   'color-theme-taming-mr-arneson
   'color-theme-dark-laptop
   'color-theme-snowish
   'color-theme-robin-hood
   'color-theme-salmon-diff
   'color-theme-oswald
   'color-theme-bharadwaj
   'color-theme-hober
   'color-theme-blippblopp
   'color-theme-aalto-dark
   'color-theme-aalto-light
   'color-theme-montz
   'color-theme-snow
   'color-theme-jedit-grey
   'color-theme-midnight
   'color-theme-gtk-ide
   'color-theme-scintilla
   'color-theme-classic
   'color-theme-infodoc
   'color-theme-high-contrast
   'color-theme-parus
   'color-theme-marquardt
   'color-theme-taylor
   'color-theme-raspopovic
   'color-theme-ramangalahy
   'color-theme-goldenrod
   'color-theme-beige-eshell
   'color-theme-standard-ediff
   'color-theme-beige-diff
   'color-theme-jb-simple
   'color-theme-greiner
   'color-theme-jsc-dark
   'color-theme-jsc-light
   'color-theme-xemacs
   'color-theme-pierson
   'color-theme-rotor
   'color-theme-blue-sea
   'color-theme-pok-wob
   'color-theme-pok-wog
   'color-theme-subtle-hacker
   'color-theme-retro-orange
   'color-theme-retro-green
   'color-theme-billw
   'color-theme-sitaramv-nt
   'color-theme-sitaramv-solaris
   'color-theme-fischmeister
   'color-theme-standard
   'color-theme-wheat
   'color-theme-ryerson
   'color-theme-jonadabian
   'color-theme-simple-1
   'color-theme-gnome2
   'color-theme-dark-info
   'color-theme-dark-font-lock
   'color-theme-salmon-font-lock
   'color-theme-blue-eshell
   'color-theme-dark-gnus
   'color-theme-blue-gnus
   'color-theme-gnome
   'color-theme-hober
   'color-theme-emacs-21
   'color-theme-arjen
   'color-theme-bharadwaj-slate
   'color-theme-billw
   'color-theme-blue-gnus
   'color-theme-dark-gnus
   'color-theme-blue-eshell
   'color-theme-retro-green
   'color-theme-retro-orange
   'color-theme-subtle-hacker
   'color-theme-salmon-font-lock
   'color-theme-dark-font-lock
   'color-theme-dark-info
   'color-theme-simple-1
   'color-theme-jonadabian
   'color-theme-ryerson
   'color-theme-standard
   'color-theme-blue-mood
   'color-theme-sitaramv-solaris
   'color-theme-sitaramv-nt
   'color-theme-blue-sea
   'color-theme-calm-forest
   'color-theme-charcoal-black
   'color-theme-clarity
   'color-theme-classic
   'color-theme-comidia
   'color-theme-dark-blue
   'color-theme-dark-blue2
   'color-theme-dark-erc
   'color-theme-dark-laptop
   'color-theme-deep-blue
   'color-theme-digital-ofs1
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
   'color-theme-infodoc
   'color-theme-lawrence
   'color-theme-ld-dark
   'color-theme-montz
   'color-theme-oswald
   'color-theme-pok-wob
   'color-theme-pok-wog
   'color-theme-tty-dark
   'color-theme-xp)
  "Audition Themes List"
  :type 'list
  :set 'set-default
  :group 'theme-faves)

(provide 'theme-faves)

( customize-save-variable
  'theme-faves-audition-list
  (list
   'color-theme-renegade
   'color-theme-feng-shui
   'color-theme-matrix
   'color-theme-lawrence
   'color-theme-calm-forest
   'color-theme-vim-colors
   'color-theme-charcoal-black
   'color-theme-andreas
   'color-theme-clarity
   'color-theme-late-night
   'color-theme-emacs-nw
   'color-theme-shaman
   'color-theme-lethe
   'color-theme-bharadwaj-slat
   'color-theme-whateveryouwant
   'color-theme-dark-green
   'color-theme-gray30
   'color-theme-xp
   'color-theme-resolve
   'color-theme-euphoria
   'color-theme-blue-mood
   'color-theme-dark-blue2
   'color-theme-black-on-gray
   'color-theme-aliceblue
   'color-theme-tty-dark
   'color-theme-arjen
   'color-theme-katester
   'color-theme-comidia
   'color-theme-kingsajz
   'color-theme-deep-blue
   'color-theme-ld-dark
   'color-theme-jsc-light2
   'color-theme-emacs-21
   'color-theme-word-perfect
   'color-theme-gray1
   'color-theme-jonadabian-slate
   'color-theme-dark-blue
   'color-theme-subtle-blue
   'color-theme-dark-erc
   'color-theme-blue-erc
   'color-theme-marine
   'color-theme-mistyday
   'color-theme-digital-ofs1
   'color-theme-taming-mr-arneson
   'color-theme-dark-laptop
   'color-theme-snowish
   'color-theme-robin-hood
   'color-theme-salmon-diff
   'color-theme-oswald
   'color-theme-bharadwaj
   'color-theme-hober
   'color-theme-blippblopp
   'color-theme-aalto-dark
   'color-theme-aalto-light
   'color-theme-montz
   'color-theme-snow
   'color-theme-jedit-grey
   'color-theme-midnight
   'color-theme-gtk-ide
   'color-theme-scintilla
   'color-theme-classic
   'color-theme-infodoc
   'color-theme-high-contrast
   'color-theme-parus
   'color-theme-marquardt
   'color-theme-taylor
   'color-theme-raspopovic
   'color-theme-ramangalahy
   'color-theme-goldenrod
   'color-theme-beige-eshell
   'color-theme-standard-ediff
   'color-theme-beige-diff
   'color-theme-jb-simple
   'color-theme-greiner
   'color-theme-jsc-dark
   'color-theme-jsc-light
   'color-theme-xemacs
   'color-theme-pierson
   'color-theme-rotor
   'color-theme-blue-sea
   'color-theme-pok-wob
   'color-theme-pok-wog
   'color-theme-subtle-hacker
   'color-theme-retro-orange
   'color-theme-retro-green
   'color-theme-billw
   'color-theme-sitaramv-nt
   'color-theme-sitaramv-solaris
   'color-theme-fischmeister
   'color-theme-standard
   'color-theme-wheat
   'color-theme-ryerson
   'color-theme-jonadabian
   'color-theme-simple-1
   'color-theme-gnome2
   'color-theme-dark-info
   'color-theme-dark-font-lock
   'color-theme-salmon-font-lock
   'color-theme-blue-eshell
   'color-theme-dark-gnus
   'color-theme-blue-gnus
   'color-theme-gnome
   'color-theme-hober
   'color-theme-emacs-21
   'color-theme-arjen
   'color-theme-bharadwaj-slate
   'color-theme-billw
   'color-theme-blue-gnus
   'color-theme-dark-gnus
   'color-theme-blue-eshell
   'color-theme-retro-green
   'color-theme-retro-orange
   'color-theme-subtle-hacker
   'color-theme-salmon-font-lock
   'color-theme-dark-font-lock
   'color-theme-dark-info
   'color-theme-simple-1
   'color-theme-jonadabian
   'color-theme-ryerson
   'color-theme-standard
   'color-theme-blue-mood
   'color-theme-sitaramv-solaris
   'color-theme-sitaramv-nt
   'color-theme-blue-sea
   'color-theme-calm-forest
   'color-theme-charcoal-black
   'color-theme-clarity
   'color-theme-classic
   'color-theme-comidia
   'color-theme-dark-blue
   'color-theme-dark-blue2
   'color-theme-dark-erc
   'color-theme-dark-laptop
   'color-theme-deep-blue
   'color-theme-digital-ofs1
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
   'color-theme-infodoc
   'color-theme-lawrence
   'color-theme-ld-dark
   'color-theme-montz
   'color-theme-oswald
   'color-theme-pok-wob
   'color-theme-pok-wog
   'color-theme-tty-dark
   'color-theme-xp))

