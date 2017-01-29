(require 'dash)
(require 'cl)
(require 'async)

(defconst marine-root-path (file-name-directory (or load-file-name buffer-file-name)))
(defconst marine-image-padding 3)

(defun marine-image-path (file-name)
  (concat marine-root-path file-name))

(defun marine-image-height (height padding)
  (- height padding))

(defun marine-load-image (file-name)
  (create-image (marine-image-path file-name) 'imagemagick nil
                :height (when (not noninteractive)
                          (marine-image-height powerline-height marine-image-padding))
                :ascent 'center))

(defconst marine-image-straight-5          (marine-load-image "straight-5.xpm"))
(defconst marine-image-glance-left-5       (marine-load-image "glance-left-5.xpm"))
(defconst marine-image-glance-right-5      (marine-load-image "glance-right-5.xpm"))
(defconst marine-image-attacked-left-5     (marine-load-image "attacked-left-5.xpm"))
(defconst marine-image-attacked-right-5    (marine-load-image "attacked-right-5.xpm"))
(defconst marine-image-attacked-straight-5 (marine-load-image "attacked-straight-5.xpm"))

(defconst marine-image-straight-2          (marine-load-image "straight-2.xpm"))
(defconst marine-image-glance-left-2       (marine-load-image "glance-left-2.xpm"))
(defconst marine-image-glance-right-2      (marine-load-image "glance-right-2.xpm"))
(defconst marine-image-attacked-left-2     (marine-load-image "attacked-left-2.xpm"))
(defconst marine-image-attacked-right-2    (marine-load-image "attacked-right-2.xpm"))
(defconst marine-image-attacked-straight-2 (marine-load-image "attacked-straight-2.xpm"))
(defconst marine-image-ouch-2              (marine-load-image "ouch-2.xpm"))

(defconst marine-list-glance-5   (list marine-image-straight-5          marine-image-glance-left-5   marine-image-glance-right-5))
(defconst marine-list-glance-2   (list marine-image-straight-2          marine-image-glance-left-2   marine-image-glance-right-2))
(defconst marine-list-attacked-5 (list marine-image-attacked-straight-5 marine-image-attacked-left-5 marine-image-attacked-right-5))
(defconst marine-list-attacked-2 (list marine-image-attacked-straight-2 marine-image-attacked-left-2 marine-image-attacked-right-2))
(defconst marine-list-ouch-2     (list marine-image-ouch-2))

(cl-defstruct marine-scene frames current-frame on-next-frame health duration next-scene)

(defun marine-get-next-not-current-frame (current-frame frames idx-gen)
  (let* ((not-current-list (marine-filter-not-current current-frame frames))
         (new-length       (length not-current-list))
         (random-idx       (funcall idx-gen new-length)))
    (nth random-idx not-current-list)))

(defun marine-get-next-random-frame (current-frame frames)
  (marine-get-next-not-current-frame current-frame frames 'random))

(defun marine-get-next-same-frame (current-frame _frames)
  current-frame)

(defun marine-make-scene-0 (image-list health loops on-next-frame &optional next-scene)
  (make-marine-scene
   :frames image-list
   :current-frame (car image-list) ;; always use first image by default
   :on-next-frame on-next-frame
   :health health
   :duration loops
   :next-scene next-scene))

(defun marine-make-scene (type health &optional next-scene)
  (let ((tuple (list type health)))
    (pcase tuple
      (`,'(glance   2) (marine-make-scene-0 marine-list-glance-2   2 'infinite 'marine-get-next-random-frame))
      (`,'(glance   5) (marine-make-scene-0 marine-list-glance-5   5 'infinite 'marine-get-next-random-frame))
      (`,'(attacked 2) (marine-make-scene-0 marine-list-attacked-2 2 'infinite 'marine-get-next-random-frame))
      (`,'(attacked 5) (marine-make-scene-0 marine-list-attacked-5 5 'infinite 'marine-get-next-random-frame))
      (`,'(ouch     2) (marine-make-scene-0 marine-list-ouch-2     2 1         'marine-get-next-same-frame next-scene))
      (_ nil))
    ))

(defun marine-get-next-frame (scene)
  (let* ((on-next-frame (marine-scene-on-next-frame scene))
         (frames        (marine-scene-frames scene))
         (current-frame (marine-scene-current-frame scene))
         (next-frame    (funcall on-next-frame current-frame frames)))
    (setf (marine-scene-current-frame scene) next-frame)))

(defun marine-filter-not-current (current-frame frames)
  (-filter
   (lambda (frame) (not (eq frame current-frame)))
   frames))

(defun marine-decrement-duration (scene)
  (let ((loops (marine-scene-duration scene)))
    (pcase loops
      (`infinite nil)
      (_         (cl-decf (marine-scene-duration scene))))
    scene))

(defun marine-next-scene (scene)
  (let ((loops (marine-scene-duration scene)))
    (pcase loops
      (`infinite scene)
      (0         (marine-scene-next-scene scene))
      (_         scene))
    )
  )

;;;; Tests
(ert-deftest test-marine-on-next-frame ()
  (let* ((frames (list 1 2 3 4))
         (scene (make-marine-scene
                 :frames        frames
                 :current-frame 3
                 :on-next-frame (lambda (_current frames) (nth 0 frames)))))
    (should (equal
             (marine-get-next-frame scene)
             1))))

(ert-deftest test-marine-get-random-frame ()
  (let* ((current-frame 1)
         (frames        (list 1 2 3))
         (idx-gen       (lambda (limit) 1))
         (result        (marine-get-next-not-current-frame current-frame frames idx-gen)))
    (should (equal result 3))))

(ert-deftest test-marine-next-same-frame ()
  (let* ((frames (list 2 3))
         (scene (make-marine-scene
                 :frames        frames
                 :current-frame 1
                 :on-next-frame 'marine-get-next-same-frame)))
    (should (equal
             (marine-get-next-frame scene)
             1))))

(ert-deftest test-marine-make-scene ()
  (let ((scene-1 (marine-make-scene 'glance 2))
        (scene-2 (marine-make-scene 'glance 5)))
    (should
     (and
      (equal scene-1 (marine-make-scene-0 marine-list-glance-2 2 'infinite 'marine-get-next-random-frame))
      (equal scene-2 (marine-make-scene-0 marine-list-glance-5 5 'infinite 'marine-get-next-random-frame))))))

(ert-deftest test-marine-next-scene ()
  (let* ((inf-scene (marine-make-scene 'glance 2))
         (scene-0   (marine-make-scene 'ouch   2 inf-scene)))
    (marine-decrement-duration scene-0)
    (should (and
             (should (equal (marine-next-scene scene-0) inf-scene))
             (should (equal (marine-next-scene inf-scene) inf-scene))))))
;;;; Main
(setq marine-current-scene (marine-make-scene 'glance 5))
(setq marine-current-image (marine-scene-current-frame marine-current-scene))

(defun marine-init-spaceline-segment ()
  (when (not noninteractive)
    (spaceline-define-segment nyan-cat
      "I'M TOO YOUNG TO DIE."
      marine-current-image)
    (spaceline-compile)))

(defun marine-async-render-loop ()
  (async-start
   (lambda ()
     (sleep-for 1))
   (lambda (res)
     (marine-render)
     (marine-async-render-loop))))

(defun marine-render ()
  (setq marine-current-scene (marine-next-scene marine-current-scene))
  (setq marine-current-image (marine-get-next-frame marine-current-scene))
  (marine-decrement-duration marine-current-scene))

(defun marine-start-async-render-loop ()
  (when (not (boundp 'marine-async-loop-is-running))
    (setq marine-async-loop-is-running t)
    (marine-async-render-loop)))

(defun marine-reset-current-scene (new-scene)
  (setq marine-current-scene new-scene)
  (setq marine-current-image (marine-scene-current-frame new-scene)))

(defun marine-add-compile-hooks ()
  (add-hook 'compilation-start-hook (lambda (_)
                                      (let* ((health (marine-scene-health marine-current-scene))
                                             (new-scene (marine-make-scene 'attacked health)))
                                        (marine-reset-current-scene new-scene))))

  (add-hook 'compilation-finish-functions (lambda (buf msg)
                                            (if (string-match "exited abnormally" msg)
                                                (let* ((next-scene (marine-make-scene 'glance 2))
                                                       (ouch       (marine-make-scene 'ouch   2 next-scene)))
                                                  (marine-reset-current-scene ouch))
                                              (marine-reset-current-scene (marine-make-scene 'glance 5))))))

(defun marine-main ()
  (interactive)
  (marine-init-spaceline-segment)
  (marine-start-async-render-loop)
  (marine-add-compile-hooks))
