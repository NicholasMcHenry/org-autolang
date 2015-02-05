;;; org-autolang.el --- Auto translation + Auto text into flashcards.
     
;; Copyright (C) 2015 Nicholas Allen McHenry

;; Author: Nicholas Allen McHenry <nick at futilityquest dot com>
;; Created: 22 Jan 2015
;; Version: "0.1.1"
;; Keywords: languages, wp, outlines, calendar
;; Homepage: https://github.com/clibralus/org-autolang.el
;; Package-Requires: ((google-translate "1.0.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An emacs module to take foreign language words or phrases under the 
;; cursor point and:
;; 1) automatically show the definition in another window
;; 2) upon "C-c s" store the word in a vocabulary list
;; 3) also upon "C-c s" automatically generate a flashcard for that word
;; or phrase by generating the right text to append to a flashcard file.
;; Flashcards are formatted to be read by org-drill.
;;
;; Dependent on google-translate.el and requires an internet connection
;; for translation and definitions.
;;
;; Documentation
;; -------------
;; https://github.com/clibralus/org-autolang.el
;;
;; Commands
;; --------
;; M-x org-autolang-save-definition
;;
;; Customization
;; -------------
;; Use M-x customize, Search for "Org-Autolang"
;; The following options are provided:
;;   org-autolang-vocab-file
;;   org-autolang-flashcard-file
;;   org-autolang-default-flashcard-type
;; See the detailed doc strings below or look at them in the customization menu.
;;
;; You may also change these variables by putting something like the following
;; into your .emacs file:
;;   (setq org-autolang-default-flashcard-type 'simple-translation-first)
;;
;; IMPORTANT!!!
;; It highly recommended that you also set certain google-translate.el
;; variables so that you are not prompted upon every movement.
;; Add the following to your .emacs file changing the languages to the ones
;; you wish to use:
;;  (setq google-translate-default-target-language "en")
;;  (setq google-translate-default-source-language "ru")
;;
;; Recommended Settings:
;; 
;;  (setq google-translate-default-target-language "en")
;;  (setq google-translate-default-source-language "ru")
;;  (global-set-key "\C-ct" 'google-translate-at-point)
;;  (global-set-key "\C-cT" 'google-translate-query-translate)
;;  (global-set-key (kbd "C-c r") 'google-translate-at-point-reverse) 
;;  (global-set-key (kbd "C-c R") 'google-translate-query-translate-reverse)
;;  (global-set-key (kbd "C-c s") 'org-autolang-save-translation)
;;  (global-set-key (kbd "C-c S") 'org-autolang-save-translation-reverse)
;;  (defalias 'org-autolang-undo 'org-autolang-undo-save-translation)
;;  (setq org-autolang-vocab-file "~/org/all-vocab.org")
;;  (setq org-autolang-flashcard-file "~/org/all-flashcards.org")

;;; Code:

(eval-when-compile (require 'cl))
(require 'google-translate)
;Org not required. Only outputs files for use in org, doesn't need it to create the output.

(defgroup org-autolang nil
   "Options concerning automatic translation and definition storage in Org mode (org-autolang)."
  :tag "Org-Autolang"
  :group 'org)

(defcustom org-autolang-vocab-file 
  "~/all-vocab.org"
  "Path to the file where words are stored."
  :type '(file)
  :group 'org-autolang)

(defcustom org-autolang-flashcard-file 
  "~/all-flashcards.org"
  "Path to the file where flashcards are stored."
  :type '(file)
  :group 'org-autolang)

(defcustom org-autolang-default-flashcard-type 
  'two-sided
  "Allows the user to choose what type of flashcards get made. Options are:
a) 'two-sided - the default, makes a 2 sided flashcard
b) 'simple-translation-first - for a simple flashcard with the translation on the front
c) 'simple-original-text-first - for a simple flashcard with the translation on the back"
  :type '(choice (const two-sided) (const simple-translation-first) (const simple-original-text-first))
  :group 'org-autolang)

(defun org-autolang-insert-line (&rest strings)
  "elisp 'insert followed by the elisp 'newline command"
  (apply 'insert strings)
  (newline))

(defun org-autolang-insert-lines (&rest lines)
  "Assumes each input string goes on its own line and inserts into the current buffer as such."
  (mapcar 'org-autolang-insert-line lines))

(defun org-autolang-sconcat (&rest strings)
  "String Concatenate. Curries 'concatenate with 'string."
  (apply 'concatenate (cons 'string strings)))

(defun org-autolang-repeat-string (n string)
  "Generates a list of length 'n' with every element being 'string'."
  (loop for i below n
	collect string))

(defun org-autolang-concat-string-with-empty-lines (&rest strings)
  "Joins the provided strings together with two newlines between each. First prepends 2 newlines to each string. Then concatenates everything. Then removes the 1st 2 characters which are 2 extraneous newlines."
  (substring
    (apply 'org-autolang-sconcat
      (mapcar* 'org-autolang-sconcat 
	      (org-autolang-repeat-string (length strings) "\n\n")
	      strings))
    2))

(defun org-autolang-pad-list (n list)
  "Adds n empty strings to the end of list."
  (append list (org-autolang-repeat-string n "")))

(defun org-autolang-merge-extra-data (data)
  "Return a list of length 4 with all elements after 4 from the original list concatenated onto the element at index 3."
  (list (first data) 
	(second data) 
	(third data) 
	(apply 'org-autolang-concat-string-with-empty-lines (cdddr data))))

(defun org-autolang-correct-parse-grouping (parsed-data)
  "Data received may have too many sections or lack some. This pads the list with empty strings or combines extra data into the 4th section if extra data exists."
  (let ((len (length parsed-data))
	(proper-length 4))
    (cond 
     ((< len proper-length) 
        (org-autolang-pad-list (- proper-length len) parsed-data))
     ((> len proper-length) 
        (org-autolang-merge-extra-data parsed-data))
     (t parsed-data))))

(defun org-autolang-parse-google-translate-buffer ()
  "Splits the *Google Translate* buffer on empty lines and assumes the 1st section says what languages are involved, that the 2nd holds the original text, that the 3rd holds the translation, and that the 4th suggest spelling corrections or list other translation possibilities. Example Output: "
  (with-current-buffer (get-buffer "*Google Translate*")
    (org-autolang-correct-parse-grouping
      (split-string (buffer-string) "\n\n"))))

(defun org-autolang-get-google-translate-data ()
  "Gets the google-translate.el output and parses it further into a 4 element list."
  (save-current-buffer ;'google-translate-at-point doesn't reset the buffer
    (google-translate-at-point)
    (org-autolang-parse-google-translate-buffer)))

(defun org-autolang-get-google-translate-data-reverse ()
  "Gets the google-translate.el output and parses it further into a 4 element list."
  (save-current-buffer ;'google-translate-at-point doesn't reset the buffer
    (google-translate-at-point-reverse)
    (org-autolang-parse-google-translate-buffer)))

(defun org-autolang-add-vocab-to-master-list (data)
  "Appends only the word/phrase and translation to the file specified by the 'vocab-file' variable. Creates the file if it does not exist."
  (destructuring-bind (_ original-text translation _1) data
    (with-temp-buffer 
      (org-autolang-insert-line "* " original-text)
      (org-autolang-insert-line "** " translation)
      (org-autolang-insert-line "")
      (append-to-file nil nil org-autolang-vocab-file))))

(defun org-autolang-add-simple-flashcard (data &optional put-translation-first-p)
  "Generates and appends a simple flash card to the file specified by the flashcard-file variable. Creates the file if it does not exist."
  (with-temp-buffer 
    (destructuring-bind (languages-involved original-text translation alternatives) data
      (org-autolang-insert-lines 
        (org-autolang-sconcat "* " languages-involved " :drill:")
  ""
        (if put-translation-first-p translation  original-text)
  ""
        "** Answer"
  ""
        (if put-translation-first-p original-text translation)
  ""
  "** Alternatives"
  alternatives
  "")
      (append-to-file nil nil org-autolang-flashcard-file))))

(defun org-autolang-add-two-sided-flashcard (data)
  "Generates and appends a 2-sided flash card to the file specified by the flashcard-file variable. Creates the file if it does not exist."
  (with-temp-buffer
    (destructuring-bind (languages-involved original-text translation alternatives) data
      (org-autolang-insert-lines 
        (org-autolang-sconcat "* " languages-involved "     :drill:")
        "    :PROPERTIES:"
        "    :DRILL_CARD_TYPE: twosided"
        "    :END:"
              ""
        "Translate this word."
        ""
              "** Original Text"
              original-text
        ""
              "** Translation"
              translation
        ""
        "** Alternatives"
        alternatives
        "")
      (append-to-file nil nil org-autolang-flashcard-file))))


(defun org-autolang-append-to-vocab-and-add-flashcard (translation-function)
  "Uses the parsed data received from 'translation-function and both adds it to the master vocabulary list and creates a flash card for the translated text. Parsed data is expected to be of the format '(languages-involved orig-text translation extra) where all elements are strings."
  (interactive)
  (let ((data (funcall translation-function)))
    (org-autolang-add-vocab-to-master-list data)
    (case org-autolang-default-flashcard-type
      ('two-sided (org-autolang-add-two-sided-flashcard data)) 
      ('simple-translation-first  (org-autolang-add-simple-flashcard data t))
      ('simple-original-text-first (org-autolang-add-simple-flashcard data nil)))))

(defun org-autolang-save-translation ()
  (interactive)
  (org-autolang-append-to-vocab-and-add-flashcard
   'org-autolang-get-google-translate-data))

(defun org-autolang-save-translation-reverse ()
  (interactive)
  (org-autolang-append-to-vocab-and-add-flashcard
   'org-autolang-get-google-translate-data-reverse))

(defmacro org-autolang-with-file (filepath &rest body)
  "Open the file into the buffer, move the point to the end, do stuff, save."
 `(with-current-buffer (find-file-noselect ,filepath)
    (goto-char (point-max))
    ,@body
    (save-buffer)))

(defun org-autolang-delete-final-tree ()
  "Cut the level 1 parent containing the point. Else silently continue (so that the surrounding code gets executed.)"
  (ignore-errors
    (outline-up-heading 101 t)
    (org-cut-subtree)))

(defun org-autolang-undo-save-translation ()
  "Dumb undo - Removes everything starting from the last top level header in both the vocabulary list file and the flashcard file. Thus if you've added something to the end manually, this function may not work as expected. Assumes headings are less than ~100 levels deep. Side-effect - saves file despite the modified buffer possibly being open."
  (interactive)
  (org-autolang-with-file org-autolang-vocab-file
    (org-autolang-delete-final-tree))
  (org-autolang-with-file org-autolang-flashcard-file
    (org-autolang-delete-final-tree)))

(provide 'org-autolang)
;;; org-autolang.el ends here
