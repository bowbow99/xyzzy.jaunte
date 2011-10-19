;;; -*- mode: lisp; package: jaunte -*-
;;;
;;; jaunte.l --- ��ʓ��Ƀq���g���U�炩���Ă����փW�����v
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; Version:    0.00.xx
;;; Category:   Utilities
;;; License:    MIT (see COPYING.mit)
;;; Commentary: (see README.md)
;;; ChangeLog:  (see ChangeLog)

;;; Usage:
;;
;; 0. `*load-path*` �̂ǂ����ɂ��̃t�@�C����u��
;; 1. .xyzzy �Ȃǂ�
#|
(require "jaunte")
(use-package :jaunte)
(global-set-key '(#\C-c #\C-j) 'jaunte)
|#
;; 2. �����ނ�� M-x jaunte (or C-c C-j)
;;

;;; TODOs:
;; - �q���g�̕�����Ȃ��ĕ������ŏ㏑�����Ă�̂ŁA�S�p����������ƃY����
;; - window-flag ���肪���r���[
;;   - line-number �����Č����Ă�
;;   - eof, tab, newline,,, �������ĂȂ��i��
;; - �����c���Ă��Ԃ� C-n/C-p �őI��->Enter �ŃW�����v�A�Ƃ�
;; - �I�������コ��Ɍ����ŁA�I������Ă����̂ɋ߂��z��I��
;; - �x��
;; - �q���g���s���͂ݏo�����ɁA����ڍs�̃^�[�Q�b�g�փW�����v����ƃ|�C���g���Y����
;;   - �R���Z�v�g�I�ɂ������[�v���I
;;   - �����������̂߂�ǂ�
;; - ���[�h�s
;; - �W�����v����Ɠ����Ƀe�L�g�[�ɑI�����Ăǂ������A�Ƃ��ł�����֗�����

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansi-loop")
  (require "ansify"))

(defpackage :jaunte
  (:use :lisp :editor)
  (:shadowing-import-from :ansi-loop
    #:loop #:loop-finish)
  (:shadowing-import-from :ansify
    #:assert #:check-type
    #:symbol-macrolet #:etypecase))

(in-package :jaunte)

(export '(jaunte
          *jaunte-active-hint-attribute*
          *jaunte-inactive-hint-attribute*
          *jaunte-hint-target-keys*
          *jaunte-target-min-distance*
          -jaunte-find-target-function-
          *jaunte-keymap*
          ))


;;;; �J�X�^�}�C�Y�p�̕ϐ�

(defvar *jaunte-active-hint-attribute* '(:foreground 2 :background 4)
  "*�q�b�g���Ă�q���g�̕����F�Ȃ�")

(defvar *jaunte-inactive-hint-attribute* '(:foreground 3 :background 8)
  "*�q�b�g���ĂȂ��q���g�̕����F�Ȃ�")

(defvar *jaunte-hint-target-keys* "awsedrftghukijolp;@:"
  "*�q���g�Ɏg�������Q�B
���̕�����Ɋ܂܂�镶�������ԂɃq���g�Ƃ��Ďg���Ă����B

�������̎�ނ����Ȃ��Ƃ����q���g�����񂪒����Ȃ��Ă��܂��̂ŁA�q���g��
�\������X�y�[�X������Ȃ��Ȃ��ăo�O��B")

(defvar *jaunte-target-min-distance* 6
  "*�W�����v��̊Ԋu��������L������B

���W�����v�悩�琔�������Ƀq���g��\������̂Ɏ��̃W���E���g��܂ł�
�\���Ȍ��Ԃ������ꍇ�̏������e�L�g�[�Ȃ̂ŁA���܂菬���Ȓl��ݒ肷���
�o�O��B")

(defvar *jaunte-jump-if-unique* t
  "*�i�荞��Ŏc��1�ɂȂ����瑦�W�����v����B")

(defvar-local -jaunte-find-target-function- 'jaunte-find-next-word
  "*�W�����v���T���֐��B
���W���[���[�h���Ɠ��ɃW���E���g����w�肵�����ꍇ�Ɏg����B

�����Ȃ��ŌJ��Ԃ��Ăяo�����̂ŁA�Ăяo���ꂽ���̃|�C���g�Ȍ�ōŏ�
�̃W�����v��Ƃ���|�C���g�� integer �ŕԂ����ƁB����ȏ�W�����v�悪
�����ꍇ�� nil ��Ԃ��Ƃ����ŏI������B")

(defvar *jaunte-keymap* nil
  "*jaunte ���Ɏg�p����L�[�}�b�v�B")


;;;; Utilities

;; TODO: remove unused utilities...

(defmacro with-current-window (window &body body)
  "Temporarily switch to WINDOW and execute BODY."
  `(let ((#1=#:owin (selected-window))
         (#2=#:nwin ,window))
     (if (eql #1# #2#)
       (progn ,@body)
       (unwind-protect
           (progn (set-window #2#) ,@body)
         (ignore-errors (set-window #1#))))))

(defmacro with-current-buffer-1 (buffer &body body)
  "Temoporarily switch to BUFFER and execute BODY."
  `(let ((#1=#:obuf (selected-buffer))
         (#2=#:nbuf ,buffer))
     (if (eql #1# #2#)
       (progn ,@body)
       (unwind-protect
           (with-set-buffer (set-buffer #2#) ,@body)
         (ignore-errors (set-buffer #1#))))))

(defmacro with-current-buffer ((buffer &key (window nil win-sp))
                               &body body)
  "Temporarily switch to BUFFER and WINDOW and execute BODY."
  `(with-current-buffer-1 ,buffer
     ,(if win-sp
        `(with-current-window ,window)
        `(progn ,@body))))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun keyword (name)
    (intern (string name) :keyword)))

(defmacro with-struct ((struct &rest slots) object &body body)
  (let ((package (symbol-package struct)))
    (labels ((acc (slot)
               (let ((name (format nil "~A-~A" struct slot)))
                 (or (find-symbol name package)
                     (error "Accessor `~A` not found in package ~S"
                            name package)))))
      `(let ((,struct ,object))
         (symbol-macrolet (,@(loop for slot in slots
                               collect `(,slot (,(acc slot) ,struct))))
           ,@body)))))
(setf (get 'with-struct 'ed::lisp-indent-hook) 2)

(defmacro awhen (test-form &body body)
  `(let ((it ,test-form)) (when it ,@body)))

(defmacro awhile (test-form &body body)
  `(let (it) (while (setq it ,test-form) ,@body)))

(defmacro when-let (var test-form &body body)
  `(let ((,var ,test-form)) (when ,var ,@body)))

(defmacro while-let (var test-form &body body)
  `(let (,var) (while (setq ,var ,test-form) ,@body)))

(defmacro multiple-value-awhile ((&rest vars) mv-test-form &body body)
  (assert (every #'symbolp vars))
  `(prog (,@vars)
     #1=#:multiple-value-awhile
     (multiple-value-setq (,@vars) ,mv-test-form)
     (if ,(car vars)
       (progn ,@body (go #1#))
       (return nil))))
(setf (get 'multiple-value-awhile 'ed::lisp-indent-hook) 2)

(defun copy-hash-table (table)
  (let ((ntable (make-hash-table :test (hash-table-test tabel))))
    (maphash (lambda (k v)
               (setf (gethash k ntable) v))
             table)
    ntable))

(defun point-virtual-bol ()
  (save-excursion (goto-virtual-bol) (point)))

(defun point-virtual-eol ()
  (save-excursion (goto-virtual-eol) (point)))

(defun screen-start/end ()
  "Return points beginning and end of screen (visible part of current buffer)."
  (let* ((top-line (get-window-start-line))
         (bottom-line (+ top-line (window-height))))
    (save-excursion
      (goto-virtual-line top-line)
      (values (point-virtual-bol)
              (progn
                (goto-virtual-line bottom-line)
                (point-virtual-eol))))))

;; modified from http://gist.github.com/608651 @miyamuko
(defun window-list ()
  (let* ((owin (selected-window))
         (list (list owin)))
    (do ((w (next-window owin) (next-window w)))
        ((eql w owin) (nreverse list))
      (push w list))))

(defun effective-window-flag (flag)
  (labels ((flagged-p (x) (plusp (logand flag x))))
    (multiple-value-bind (buffer-on buffer-off)
        (get-local-window-flags (selected-buffer))
      (cond ((flagged-p buffer-on) t)
            ((flagged-p buffer-off) nil)
            (t (multiple-value-bind (window-on window-off)
                   (get-local-window-flags (selected-window))
                 (cond ((flagged-p window-on) t)
                       ((flagged-p window-off) nil)
                       (t (flagged-p (get-window-flags))))))))))

(defun line-number-displayed-p ()
  (effective-window-flag *window-flag-line-number*))

(defun string-columns (string)
  (loop :for c :across string
    :sum (char-columns c)))

(deftype callable ()
  `(or function (and symbol (satisfies fboundp))))

;;;;; development support

(defmacro @debug (datum &rest args)
  (if (stringp datum)
    `(format t ,datum ,@args)
    `(format t "~&~:{* ~S => ~S~%~}"
           (list ,@(loop for form in (cons datum args)
                     collect `(list ',form ,form))))))

(defmacro @msgbox (datum &rest args)
  (if (stringp datum)
    `(msgbox ,datum ,@args)
    `(msgbox "@msgbox:~2%~:{* ~S => ~S~%~}"
             (list ,@(loop for form in (cons datum args)
                       collect `(list ',form ,form))))))

(defmacro @obsoleted (thing)
  `(error "OBSOLETED: ~S" thing))

(defun @pause (n)
  (refresh-screen) (sit-for n))


;;;; internal variable

(defparameter *cleanup-hook* nil
  "Hook to cleanup everything at end of jaunte.")

(defparameter *targets* nil
  "List of targets.")

(defparameter *hint-index* nil
  "Index number of hint.")

(defparameter *window-restore-info* nil)


;;;; mirroring

(defun create-mirror-buffer ()
  (let ((buffer (create-new-buffer " Jaunte Mirror")))
    (setup-temp-buffer buffer)
    buffer))

(defstruct (screen-info
            (:type vector))
  "Info about *ORIGINAL* screen."
  buffer
  contents
  point-offset
  preceding-lines
  find-target-function
  syntax-table)

(defun valid-screen-info-p (info)
  (and (vectorp info)
       (= (length info) 6)
       (with-struct (screen-info buffer contents preceding-lines
                                 point-offset find-target-function
                                 syntax-table)
           info
         (and (bufferp buffer)
              (stringp contents)
              (or (and (integerp preceding-lines)
                       (>= preceding-lines 0))
                  (null preceding-lines))
              (and (integerp point-offset)
                   (<= 0 point-offset)
                   (< point-offset (length contents)))
              (or (functionp find-target-function)
                  (and (symbolp find-target-function)
                       (fboundp find-target-function)))
              (syntax-table-p syntax-table)))))

(deftype screen-info ()
  `(and vector (satisfies valid-screen-info-p)))

(defun save-screen-info (window info)
  #+jaunte/safety (check-type window window)
  #+jaunte/safety (check-type info screen-info)
  (push (cons window info) *window-restore-info*))

(defun restore-window-1 (window)
  #+jaunte/safety (check-type window window)
  (let ((info (cdr (assoc window *window-restore-info*))))
    #+jaunte/safety (check-type info screen-info)
    (with-current-window window
      (with-struct (screen-info buffer) info
        (set-buffer buffer)))))

(defun delete-all-mirror-buffers ()
  (loop for buff in (buffer-list)
    when (string-match "^ Jaunte Mirror" (buffer-name buff))
    do (delete-buffer buff)))

(defun restore-screen ()
  (loop for win in (window-list)
    do (restore-window-1 win))
  (delete-all-mirror-buffers))

(add-hook '*cleanup-hook* 'restore-screen)

(defun examine-screen ()
  (multiple-value-bind (start end) (screen-start/end)
    #+jaunte/safety (assert (<= start end))
    (save-excursion
      (let* ((opoint (point))
             (plines (when (line-number-displayed-p)
                       (goto-char start)
                       (1- (current-line-number)))))
        (make-screen-info
         :buffer (selected-buffer)
         :contents (buffer-substring start end)
         :preceding-lines plines
         :point-offset (- opoint start)
         :find-target-function -jaunte-find-target-function-
         :syntax-table (syntax-table))))))

(defun replicate-in-mirror-buffer (info)
  (with-struct (screen-info buffer contents preceding-lines point-offset
                            find-target-function syntax-table)
      info
    (let ((mirror (create-new-buffer " Jaunte Mirror")))
      (setup-temp-buffer mirror)
      (set-buffer mirror)
      (goto-char (point-min))
      (cond ((integerp preceding-lines)
             (insert #\LFD preceding-lines)
             (set-local-window-flags mirror *window-flag-line-number* t))
            (t
             (set-local-window-flags mirror *window-flag-line-number* nil)))
      (recenter 0)
      (save-excursion (insert contents))
      (forward-char point-offset)
      (macrolet ((setq-local (var value &rest more)
                   `(progn
                      (make-local-variable ',var)
                      (setq ,var ,value)
                      ,(when more `(setq-local ,@more)))))
        (setq-local
         -jaunte-find-target-function- find-target-function
         -source-buffer- buffer
         -original-syntax-table- syntax-table)
        #+jaunte/safety (setq-local -mirror-buffer-of- (selected-window))))))

(defun mirror-screen (window)
  (with-current-window window
    (let ((info (examine-screen)))
      (save-screen-info window info)
      (replicate-in-mirror-buffer info))))


;;;; targets

(defstruct (target)
  window buffer mirror point otext hint offset)

(defun valid-target-p (target)
  (and (typep target 'target)
       (with-struct (target window buffer mirror point otext hint) target
         (and (windowp window)
              (bufferp buffer)
              (bufferp mirror)
              (and (integerp point)
                   (>= point 0))
              (stringp otext)
              (stringp hint)))))

(deftype valid-target () `(and target (satisfies valid-target-p)))

(defun target-set-text-attribute (target attribute)
  (with-struct (target window hint point mirror) target
    (with-current-window window
      #+jaunte/safety (assert (eql (selected-buffer) mirror))
      (delete-text-attributes target)
      (apply #'set-text-attribute point (+ point (length hint))
             target attribute))))

(defun target-activate (target)
  (target-set-text-attribute target *jaunte-active-hint-attribute*))

(defun target-deactivate (target)
  (target-set-text-attribute target *jaunte-inactive-hint-attribute*))

;;;;; initialization

(defun gen-hint-string (&optional (index *hint-index*))
  (let ((len (length *jaunte-hint-target-keys*)))
    (multiple-value-bind (excess n) (floor index len)
      (prog1
          (concat (cond ((zerop excess) "")
                        (t  (gen-hint-string (1- excess))))
                  (princ-to-string (char *jaunte-hint-target-keys* n)))
        (incf *hint-index*)))))

(defun create-new-target (pos offset)
  #+jaunte/safety (check-type pos (integer 0 *))
  #+jaunte/safety (assert (and (local-variable-p '-mirror-buffer-of-)
                               (windowp -mirror-buffer-of-)))
  (let* ((hint-string (gen-hint-string))
         (text (buffer-substring pos (+ pos (length hint-string)))))
    (make-target :window (selected-window) :buffer -source-buffer-
                 :mirror (selected-buffer) :point pos :offset offset
                 :hint hint-string :otext text)))

(defun target-setup-hint (target)
  (with-struct (target window mirror point hint) target
    #+jaunte/safety (assert (eql (selected-window) window))
    #+jaunte/safety (assert (eql (selected-buffer) mirror))
    (save-excursion
      (goto-char point)
      (insert hint)
      (let ((hint-end (point))
            (cline (current-virtual-line-number)))
        (goto-char (+ hint-end (length hint)))
        (delete-region hint-end
                       (cond ((/= (current-virtual-line-number) cline)
                              (goto-char hint-end)
                              (point-virtual-eol))
                             (t  (point)))))
      (target-activate target)
      )))

(defun jaunte-find-next-word ()
  (let ((opoint (point)))
    (when (and (prog1 (forward-word 1)
                 (backward-word 1))
               (or (> (point) opoint)
                   (and (prog1 (forward-word 2)
                          (backward-word 1))
                        (> (point) opoint))))
      (point))))

(defun find-targets-in-window ()
  #+jaunte/safety (assert (eql -mirror-buffer-of- (selected-window)))
  #+jaunte/safety (check-type -jaunte-find-target-function- callable)
  (let ((targets nil))
    (save-excursion
      (multiple-value-bind (start limit) (screen-start/end)
        (goto-char start)
        (loop for pos = (funcall -jaunte-find-target-function-)
          while (and (integerp pos) (< pos limit)) do
          (push (create-new-target pos (- pos start)) targets)
          (goto-char (+ pos *jaunte-target-min-distance*)))
        (nreverse targets)))))

(defun setup-jaunte-targets ()
  (loop for win in (window-list) do
    (mirror-screen win)
    (with-current-window win
      (let ((targets (find-targets-in-window)))
        (setf *targets* (append targets *targets*))
        (loop for target in targets
          do (target-setup-hint target))))))

;;;;; update

(defun target-match-p (target keys)
  #+jaunte/safety (check-type target valid-target)
  #+jaunte/safety (check-type keys string)
  (let ((len (length keys))
        (hint (target-hint target)))
    (and (>= (length hint) len)
         (string= hint keys :end1 len))))

(defun find-goal-target (keys)
  (or (find keys *targets* :key 'target-hint :test 'string=)
      (find keys *targets* :test (lambda (keys target)
                                   (target-match-p target keys)))))

(defun update-jaunte-targets (keys)
  (let ((matches nil))
    (loop for target in *targets* do
      (cond ((target-match-p target keys)
             (push target matches)
             (target-activate target))
            (t
             (target-deactivate target))))
    (nreverse matches)))

;; @*post-command-hook* @not-used
(defun jaunte-update-screen ()
  (let* ((keys (buffer-substring (point-min) (point-max)))
         (matches (update-jaunte-targets keys)))
    (when (and (car matches) (null (cdr matches)))
      (call-interactively 'quit))))


(defun jaunte-self-insert-command (&optional (arg 1))
  (interactive "*p")
  (call-interactively 'self-insert-command)
  (let* ((keys (buffer-substring (point-min) (point-max)))
         (matches (update-jaunte-targets keys)))
    (when (and (car matches) (null (cdr matches)))
      (exit-recursive-edit (car matches)))
    (message "jaunte: ~D targes remaining..." (length matches))
    t))

#+jaunte/reset
(setf *jaunte-keymap* nil)

(unless *jaunte-keymap*
  (let ((keymap (make-sparse-keymap)))
    (substitute-key-definition 'self-insert-command 'jaunte-self-insert-command
                               keymap *global-keymap*)
    (define-key keymap #\C-g 'quit-recursive-edit)
    (define-key keymap #\RET 'minibuffer-exit)
    (setf *jaunte-keymap* keymap)))

(defun jaunte-setup-minibuffer (buffer history)
  (use-keymap *jaunte-keymap* buffer))


;;;; Command

(defun jaunte ()
  (interactive)
  (let ((*hint-index* 0)
        (*targets* nil)
        (*window-restore-info* nil)
        (goal))
    (unwind-protect
        (let ((keys ""))
          (setup-jaunte-targets)
          (let ((*enter-minibuffer-hook*
                  (list 'jaunte-setup-minibuffer 'ed::prologue-minibuffer)))
            (setf goal (read-string "jaunte: "))))
      (run-hooks '*cleanup-hook*))
    (when goal
      (when (stringp goal)
        (setf goal (find-goal-target goal)))
      (with-struct (target window offset) goal
        (set-window window)
        (goto-char (+ (screen-start/end) offset)))
      )))

#+jaunte/quickrun
(jaunte)

(provide "jaunte")
;;; jaunte.l ends here.