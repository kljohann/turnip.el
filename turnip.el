;;; turnip.el --- Interacting with tmux from Emacs

;; Copyright (C) 2014 Johann Klähn

;; Author: Johann Klähn <kljohann@gmail.com>

;; Keywords: tmux
;; Package-Requires: ((dash "2.6.0") (s "1.9.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Turnip is a package to ease interacting with a tmux session from Emacs.
;; Its scope and philosophy is inspired by Tim Pope's vim-tbone.

;;; Code:

(require 'dash)
(require 's)

(defvar turnip:attached-session nil
  "Name of the tmux session turnip is currently attached to.
Can be changed using `turnip-attach'.")
(defvar turnip:output-buffer-name "*turnip-output*"
  "Name of buffer where output of tmux commands is put.")

(defun turnip:session (&optional session silent)
  (or session
      turnip:attached-session
      (unless silent
        (user-error "No session specified or attached to"))))

(defun turnip:call-command (command &rest args)
  (with-temp-buffer
    (let ((status (apply #'call-process "tmux" nil t nil command args)))
      (unless (and (numberp status) (zerop status))
        (error "Failed: %s (%s)" command status))
      (s-chomp (buffer-string)))))

(defun turnip:call-command-split (command &rest args)
  (s-lines (apply #'turnip:call-command command args)))

(defun turnip:list-sessions ()
  (turnip:call-command-split "list-sessions" "-F" "#S"))

(defun turnip:list-windows (&optional session)
  (let ((maybe-session (turnip:session session 'silent)))
    (append (when maybe-session
              (turnip:call-command-split "list-windows" "-F" "#W" "-t" maybe-session))
            (turnip:call-command-split "list-windows" "-F" "#S:#W" "-a"))))

(defun turnip:list-panes (&optional session)
  (let ((maybe-session (turnip:session session 'silent)))
    (append (when maybe-session
              (turnip:call-command-split "list-panes" "-F" "#W.#P" "-s" "-t" maybe-session))
          (turnip:call-command-split "list-panes" "-F" "#S:#W.#P" "-s" "-a")
          '("last" "top" "bottom" "left" "right"
            "top-left" "top-right" "bottom-left" "bottom-right"))))

(defun turnip:list-clients ()
  (turnip:call-command-split "list-clients" "-F" "#{client_tty}"))

(defun turnip:list-buffers ()
  (-map #'number-to-string
        (number-sequence
         0 (1- (length (turnip:call-command-split "list-buffers"))))))

(defun turnip:list-executables ()
  (let* ((path (turnip:call-command "show-environment" "-g" "PATH"))
         (dirs (parse-colon-path (s-chop-prefix "PATH=" path))))
    (->> dirs
      (-map-when #'file-directory-p
                 (lambda (dir)
                   (--filter (file-executable-p (expand-file-name it dir))
                             (directory-files dir nil nil 'nosort))))
      -flatten
      -uniq)))

(defun turnip:parse-command-options (line)
  (let
      ((option-names
        (->> line
          (s-match-strings-all "[[|]-\\(\\w+\\)")
          (-map #'cadr)
          (apply #'concat)))
       (option-arguments
        (-when-let*
            ((matches (s-match-strings-all
                       "[[|]\\(-\\w\\)\\s-+\\([^]|]+\\)[]|]" line)))
          (--map (cons (cadr it) (-last-item it)) matches))))
    (cons option-names option-arguments)))

(defun turnip:list-commands ()
  (let ((lines (turnip:call-command-split "list-commands")))
    (apply
     #'append
     (-map (lambda (line)
             (-when-let*
                 ((match (s-match "^\\(\\S-+\\)\\(?:\\s-+\\(.*\\)\\)?$" line))
                  (cmd (cadr match))
                  (rest (-last-item match)))
               (let ((options (turnip:parse-command-options rest))
                     (alias (cadr (s-match "(\\([^)]+\\))" rest))))
                 (append
                  (list (cons cmd options))
                  (when alias
                    (list (cons alias options))))))) lines))))

(defun turnip:completions-for-argument (arg &optional session)
  (cond
   ((s-suffix? "-pane" arg) (turnip:list-panes session))
   ((s-suffix? "-window" arg) (turnip:list-windows session))
   ((s-suffix? "-session" arg) (turnip:list-sessions))
   ((s-suffix? "-client" arg) (turnip:list-clients))
   ((s-equals? "buffer-index" arg) (turnip:list-buffers))))

(defun turnip:normalize-argument-type (arguments current)
  (when current
    (let ((cmd (car arguments)))
      (cond
       ((and (-contains? '("list-panes" "lsp") cmd)
             (s-prefix? "target" current))
        (if (-contains? arguments "-s")
            "target-session"
          "targe-window"))
       (t current)))))

;;;###autoload
(defun turnip-attach ()
  "Prompt for and attach to a particular tmux session.
If only one session is available, it will be used without displaying a prompt."
  (interactive)
  (let* ((sessions (turnip:list-sessions))
         (choice (if (= (length sessions) 1)
                     (car sessions)
                   (completing-read "Session: " sessions nil t))))
    (when (s-equals? choice "")
      (user-error "No session name provided"))
    (setq turnip:attached-session choice)))

(defun turnip:prompt-for-command (&optional session)
  "Interactively prompts for a tmux command to execute.
See `turnip-command'."
  (-when-let* ((commands (turnip:list-commands))
               (command-names (-map #'car commands))
               (choice (completing-read "tmux " command-names nil t)))
    (let* ((options (cdr (assoc choice commands)))
           (option-names (--map (concat "-" (list it)) (car options)))
           (option-arguments (cdr options))
           (arguments))

      (while (not (s-equals? choice ""))
        (setq arguments (-snoc arguments choice))
        (setq option-names (--remove (s-equals? choice it) option-names))
        (let ((prompt (format "tmux %s " (s-join " " arguments)))
              (takes-argument
               (turnip:normalize-argument-type
                arguments (cdr (assoc choice option-arguments)))))
          (if (not takes-argument)
              (setq choice (completing-read prompt option-names))
            (setq choice "")
            (while (s-equals? choice "")
              (setq choice
                    (completing-read
                     (format "%s[%s] " prompt takes-argument)
                     (turnip:completions-for-argument takes-argument session)))))))
      arguments)))

;;;###autoload
(defun turnip-command ()
  "Interactively prompts for a tmux command to execute.
The command will be built in several steps.  First the user can choose
the tmux command to run.  In the next prompts completion for the options
of this command is provided.  The command will be executed once the user
gives an empty answer."
  (interactive)
  (let* ((arguments (turnip:prompt-for-command))
         (cmd (car arguments))
         (argument-types (cddr (assoc cmd (turnip:list-commands)))))
    (when turnip:attached-session
      (when (-contains? '("list-panes" "lsp") cmd)
        (unless (or (-contains? arguments "-a")
                    (-contains? arguments "-t"))
          (setq arguments (-snoc arguments "-s" "-t" turnip:attached-session))))
      (--when-let (rassoc "target-session" argument-types)
        (unless (or (-contains? arguments (car it))
                    (-contains? arguments "-a"))
          (setq arguments (-snoc arguments (car it) turnip:attached-session)))))
    (with-current-buffer (get-buffer-create turnip:output-buffer-name)
      (barf-if-buffer-read-only)
      (end-of-buffer)
      (insert (format "\n## tmux %s =>\n" arguments)
              (apply #'turnip:call-command arguments)
              "\n"))))

(provide 'turnip)

;;; turnip.el ends here
