;;; highlight-doxygen.el --- Highlight Doxygen comments

;; Copyright (C) 2016-2018 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces
;; Created: 2016-02-12
;; Version: 0.0.2
;; URL: https://github.com/Lindydancer/highlight-doxygen

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

;; Advanced highlighting package for Doxygen comments.
;;
;; In addition to highlighting Doxygen commands and their arguments,
;; entire Doxygen comment are highlighted, making them stand out
;; compared to other comments.  In addition, and code blocks are
;; highlighted according to the language they are written in.

;; Usage:
;;
;; This package provide two minor modes, `highlight-doxygen-mode' and
;; `highlight-doxygen-global-mode'.
;;
;; Can enable `highlight-doxygen-mode' from the hook of a mode.
;;
;; Alternatively, you can enable the minor mode for all major modes
;; specified in `highlight-doxygen-modes'.  Typically, this is done by
;; placing the following in a suitable init file:
;;
;;     (highlight-doxygen-global-mode 1)

;; What is highlighted:
;;
;; * The full Doxygen comment is highlighted with a different
;;   background color, to make them stand out compared to normal code
;;   other comments.
;;
;; * Doxygen commands and their arguments are highlighted.  The
;;   arguments are highlighted according to the signature of the
;;   commands.  For example, the argument to the `\a' command is
;;   highlighted as a variable.
;;
;; * Code blocks are highlighted using the Emacs highlighting rules
;;   for the language they are written in.  In addition, the
;;   background is changed to make the code block stand out.
;;
;; * Customization friendly.  This package define a number of custom
;;   faces that can be customized to fine tune the appearance if this
;;   package.  The default value of all defined faces inherit from
;;   standard Emacs faces, which mean that customizations done by the
;;   user or themes are automatically used.

;; Code blocks:
;;
;; A code block is specified using a pair of Doxygen commands like
;; `\code' and `\endcode' or `\dot' and `\enddot'.
;;
;; Code blocks are syntax highlighted using the major mode they are
;; written in.  The major mode is selected as follows:
;;
;; * If the `\code{.ext}' construct is used, the major mode associated
;;   with extension `.ext' is used.
;;
;; * For `\dot', `\msc', and `\startuml' is used, the extensions
;;   `.dot', `.msc', and `.plantuml' are used, respectively.
;;
;; * For `\code' blocks that does not specify an extension, the major
;;   mode of the buffer is used.

;;; Code:

;; Internal comments starts here.
;;
;; (Nothing below the "Code:" comment is included in the generated
;; README.md file.)

;; Known problems:
;;
;; * When there is space between two C-style Doxygen comments, it
;;   should not be highlighted.
;;
;;   > /*!
;;   >  * A comment
;;   >  */
;;   >                           <-- Don't highlight here.
;;   > /*!
;;   >  * A comment
;;   >  */
;;
;; * A \code without a matching \endcode should not pick up someone
;;   elses \endcode, that would highlight too much.
;;
;; * Empty lines in Doxygen comments disrupt the "block" highlighting.
;;
;;   >     /* Title              <-- The block starts on the "/".
;;   >                           <-- Starts at the beginning of the line.
;;   >      */                   <-- Starts at the space before the "*".
;;
;; * Empty lines in code disrupt the "code block" highlighting.
;;
;;   >     //! \code
;;   >     //!     if (true)
;;   >     //!     {
;;   >     //!                   <-- This line is not highlighted as code
;;   >     //!     }
;;   >     //! \endcode
;;
;; * Verbatim blocks
;;
;; In Doxygen, everything between \verbatim and \endverbatim is
;; included, exactly as written.  This include things like comment
;; start characters.  Currently, verbatim blocks are highlighted like
;; code blocks.
;;
;; Also, when the \verbatim and \endverbatim is placed in different
;; comments, any code between them is included in the comment.  This
;; package highlight one Doxygen comment block at a time; with this
;; system in place in it not possible to highlight blocks across
;; Doxygen comments.

;; Notes on individual commands:
;;
;; The Doxygen documentation says that "a", "e", and "em" are the
;; same.  However, it also says that "a" should be used to highlights
;; parameters.  This package highlight the argument of "a" as a
;; variable (to match the parameter list) and the argument to "e" and
;; "em" in a different way, by default in italics.
;;
;; The commands "sa" and "see" start a new paragraph that could
;; contains references and links.  There are example code where the
;; paragraphs only contain one word, a reference, but in the general
;; case it's impossible to know that this really is the case.

;; Future ideas:
;;
;; This section contains "wild ideas" that aren't in the current
;; implementation plan.
;;
;; ## Hide "\a" in "\a var"?
;;
;; This would make the code more readable. However, it would also make
;; it easy to accidentally write badly formatted or too long lines.
;;
;; ## Highlight some commands differently
;;
;; For example, "\attention" "\warning" "\todo" "\bug" could be
;; highlighted in (a face inheriting from) the warning face.
;;
;; ## Support more commands
;;
;; - "\def foo" -- Highlight "foo" as a variable. "\def foo(x,y)" --
;;   Highlight "foo" as a function and the parameters as variables.
;;   Note: I failed to include \def:ed macros in the generated Doxygen
;;   output, so it's hard to know if the commands are used as
;;   intended.
;;
;; ## Highlight across lines
;;
;; Today, the argument to commands like "\a" must be place on the same
;; line as the command.  However, Doxygen supports placing the
;; argument on the next line.  Technically, one way to implement this
;; is to only match "\a" using the main regexp and replace the face
;; expression with a call to a function that highlights the next word
;; (possibly skipping comment start characters, stars etc, and then
;; return nil (so that font-lock doesn't add anther face on top of the
;; one we just used ourselves).
;;
;; ## Highlight HTML constructs
;;
;; Highlight HTML constructs like <tt>Multiple words</tt>.  Tricky to
;; handle constructs that spam multiple lines and nested constructs.
;;
;; ## Clickable links
;;
;; Make it possible to click on "http"-like links.  (Check if there
;; exist a package for this already.)
;;
;; ## Support for formats like JavaDoc
;;
;; Currently, this package only support Doxygen.  However, a number of
;; similar formats exists, like JavaDoc.  Maybe they should be
;; supported as well.
;;
;; ## Edit code block
;;
;; Add support to edit code blocks in a separate buffer, like in org
;; mode.

;; Implementation:
;;
;; Extra properties:
;;
;; In addition to faces, this package sets a number of text properties
;; on Doxygen comments.
;;
;; - `highlight-doxygen-code' -- Set on all lines in an actual code
;;   block.  This is used to prevent Doxygen font-lock rules from
;;   being applied inside code blocks.
;;
;; - `highlight-doxygen-ignore' -- Set on all lines in a code block
;;   from the code/verbatim Doxygen command to the endcode/endverbatim
;;   command. This is used to prevent the markdown code rule from
;;   inspecting the surrounding Doxygen commands, as they often are
;;   heavily indented

;; Doxygen information:
;;
;; https://www.stack.nl/~dimitri/Doxygen/manual/commands.html
;;
;; A MarkDown-style code block needs four spaces more than the
;; surrounding text. When the indentation gets lets than four spaces,
;; the code block ends, even when in the middle of consecutive lines.
;;
;; From the Doxygen manual:
;;
;; > Some commands have one or more arguments. Each argument has a
;; > certain range:
;; >
;; > If <sharp> braces are used the argument is a single word.
;; >
;; > If (round) braces are used the argument extends until the end of
;; > the line on which the command was found.
;; >
;; > If {curly} braces are used the argument extends until the next
;; > paragraph. Paragraphs are delimited by a blank line or by a section
;; > indicator.

;; -------------------------------------------------------------------
;; Dependencies
;;

;; For the faces.
(require 'outline)

;; Don't warn for using these dynamically bound variables, see
;; `font-lock-extend-region-functions'.
(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))


;; -------------------------------------------------------------------
;; Variables, faces, and customization support
;;

(defgroup highlight-doxygen nil
  "Highlight Doxygen comments."
  :group 'faces)


(defface highlight-doxygen-comment
  '((((background light)) :inherit font-lock-doc-face :background "grey95")
    (((background dark))  :inherit font-lock-doc-face :background "grey30"))
  "The face used for Doxygen comment blocks."
  :group 'highlight-doxygen)


(defface highlight-doxygen-code-block
  '((((background light)) :background "grey85")
    (((background dark))  :background "grey40"))
  "The face used to mark a code block."
  :group 'highlight-doxygen)


(defface highlight-doxygen-command
  '((t :inherit font-lock-constant-face))
  "The face used to mark Doxygen commands."
  :group 'highlight-doxygen)


(defcustom highlight-doxygen-modes
  '(c-mode
    c++-mode
    objc-mode)
  "List of major modes where Highlight Doxygen Global mode should be enabled.

The mode is enabled for buffers whose major mode is a member of
this list, or is derived from a member in the list."
  :group 'highlight-doxygen
  :type '(repeat symbol))


(defcustom highlight-doxygen-commend-start-regexp
  "\\(/\\*\\(!\\|\\*[^*]\\)\\|//\\(!\\|/[^/\n]\\)\\)"
  "Regexp matching the beginning of a Doxygen comment."
  :group 'highlight-doxygen
  :type 'regexp)


(defcustom highlight-doxygen-triple-slash-comment-regexp "///"
  "Regexp to match triple slash comments."
  :group 'highlight-doxygen
  :type 'regexp)


(defcustom highlight-doxygen-ignore-triple-slash-comments nil
  "When non-nil, triple slash comments are ignored."
  :group 'highlight-doxygen
  :type 'boolean)


;; -------------------------------------------------------------------
;; Helper functions.
;;

(defun highlight-doxygen-replace-in-sexp (new old sexp)
  "Return a copy of SEXP where OLD has been replaced by NEW.

If OLD does not occur in SEXP, SEXP is returned."
  (cond ((eq old sexp) new)
        ((consp sexp)
         (let* ((lhs0 (car sexp))
                (rhs0 (cdr sexp))
                (lhs (highlight-doxygen-replace-in-sexp new old lhs0))
                (rhs (highlight-doxygen-replace-in-sexp new old rhs0)))
           (if (and (eq lhs lhs0)
                    (eq rhs rhs0))
               sexp
             (cons lhs rhs))))
        (t sexp)))


;; -------------------------------------------------------------------
;; Code block highlighter.
;;

(defun highlight-doxygen-next-property-change (pos property)
  "Next position after POS where PROPERTY change.

If POS is nil, also include `point-min' in the search.
If last character contains the property, return `point-max'."
  (if (equal pos (point-max))
      ;; Last search returned `point-max'. There is no more to search
      ;; for.
      nil
    (if (and (null pos)
             (get-text-property (point-min) property))
        ;; `pos' is `nil' and the character at `point-min' contains
        ;; the property, return `point-min'.
        (point-min)
      (unless pos
        ;; Start from the beginning.
        (setq pos (point-min)))
      ;; Do a normal search. Compensate for that
      ;; `next-single-property-change' does not include the end of the
      ;; buffer, even when the property reach it.
      (let ((res (next-single-property-change pos property)))
        (if (and (not res)              ; No more found.
                 (not (equal pos (point-max))) ; Not already at the end.
                 (not (equal (point-min) (point-max))) ; Not an empty buffer.
                 (get-text-property (- (point-max) 1) property))
            ;; If the property goes all the way to the end of the
            ;; buffer, return `point-max'.
            (point-max)
          res)))))


;; TODO: Check if face properties on newline are handled properly.
(defun highlight-doxygen-code-block (start end &optional mode)
  "Highlight block between START and END as code.

MODE determined which major mode should be used to highlight the
block.

When MODE is a string, it should be on the form of a file
extension, and the major mode associated with the file extension
is used.  When a function, that function is called to set the
major mode.  When nil, the block is not highlighted."
  ;; ----------
  ;; Mark the code block so that the other rules doesn't overwrite it.

  ;; TODO: As other rules use this, move it to the function that
  ;; handle code rule.
  (add-text-properties start end '(highlight-doxygen-code t))
  ;; ----------
  ;; Find start column
  (let (column)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((indentation (highlight-doxygen-current-indentation)))
          (when indentation
            (when (or (null column)
                      (< indentation column))
              (setq column indentation))))
        (forward-line)))
    ;; ----------
    (save-excursion
      (goto-char start)
      (let ((buf (get-buffer-create " highlight-doxygen"))
            (src-buf (current-buffer)))
        ;; ----------
        ;; Copy code block to temp buffer, ignoring anything to the left
        ;; of the starting column of the code block.
        (with-current-buffer buf
          (delete-region (point-min) (point-max))
          ;; Ensure temp buffers doesn't keep major mode over calls.
          (fundamental-mode))
        (while (< (point) end)
          ;; Go to column, include a tab if it spans the intended column.
          (move-to-column column)
          (when (> (current-column) column)
            (backward-char))
          (let ((line (buffer-substring-no-properties
                       (point)
                       (min end (line-end-position))))
                (origin (point)))
            (with-current-buffer buf
              (insert line)
              (set-text-properties
               (line-beginning-position)
               (line-end-position)
               (list 'highlight-doxygen origin))
              (insert "\n")))
          (forward-line))
        ;; ----------
        ;; Highlight the code block.
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (move-to-column column)
            (when (> (current-column) column)
              (backward-char))
            (unless (eolp)
              (font-lock-prepend-text-property
               (point)
               (min end (line-beginning-position 2))
               'face
               'highlight-doxygen-code-block))
            (forward-line)))
        ;; ----------
        ;; Copy syntax highlighting from temp to original buffer.
        (with-current-buffer buf
          (when (ignore-errors
                  (cond ((stringp mode)
                         (let ((buffer-file-name
                                (concat default-directory
                                        "dummy" mode)))
                           (set-auto-mode))
                         t)
                        ((functionp mode)
                         (funcall mode)
                         t)
                        ;; Don't highlight.
                        (t nil)))
	    (unless (eq major-mode 'fundamental-mode)
              (ignore-errors
		(font-lock-fontify-region (point-min) (point-max))))
            (goto-char (point-min))
            (while (not (eobp))
              (let ((origin (get-text-property (point) 'highlight-doxygen))
                    next)
                (while (and (not (eolp))
                            (setq next (next-single-property-change
                                        (point) 'face
                                        nil (line-end-position))))
                  (let ((face (get-text-property (point) 'face)))
                    (when face
                      ;; Without this, code block containing comments
                      ;; will look half done when rendered in a code
                      ;; block.
                      (dolist (old '(font-lock-comment-face
                                     font-lock-comment-delimiter-face
                                     font-lock-doc-face
                                     highlight-doxygen-comment
                                     default))
                        (setq face (highlight-doxygen-replace-in-sexp
                                    'highlight-doxygen-code-block
                                    old
                                    face)))
                      (font-lock-prepend-text-property
                       (+ origin (- (point) (line-beginning-position)))
                       (+ origin (- next (line-beginning-position)))
                       'face
                       face
                       src-buf)))
                  (goto-char next)))
              (forward-line))))))))


(defun highlight-doxygen-forward-to-indentation ()
  "Move point to first text after comment starter in current line.

Return non-nil if the line is not empty, it only contains
whitespace and comment start characters."
  (skip-syntax-forward " ")
  (cond ((looking-at "\\(/\\*\\(!\\|\\*[^*]\\)\\|//\\(!\\|/[^/]\\)\\)")
         (goto-char (+ (match-beginning 0) 3)))
        ((eq (following-char) ?*)
         (forward-char)))
  (skip-syntax-forward " ")
  (not (eolp)))


(defun highlight-doxygen-current-indentation ()
  "The current indentation, or nil if line is empty.

\"Empty\", in this context mean that it only contains whitespace
and comment start characters."
  (save-excursion
    (and (highlight-doxygen-forward-to-indentation)
         (current-column))))


(defun highlight-doxygen-forward-to-paragraph-start (limit)
  "Move forward to start of Doxygen comment paragraph.

Empty lines and highlighted code blocks are skipped.

Return indentation for first line in paragraph, or nil if LIMIT is reached."
  (let ((res nil))
    (while
        (if (>= (point) limit)
            nil
          (setq res (highlight-doxygen-current-indentation))
          (not res))
      (forward-line))
    res))


(defun highlight-doxygen-end-of-paragraph-position (limit)
  "Return the position of the end of the current paragraph, or nil.

LIMIT is the end of the Doxygen comment."
  (save-excursion
    ;; Skip empty lines.
    (while (and (< (point) limit)
                (not (highlight-doxygen-current-indentation)))
      (forward-line))
    (let ((res nil))
      (while (and (< (point) limit)
                  (highlight-doxygen-current-indentation))
        (setq res (line-end-position))
        (forward-line))
      res)))


;; Doxygen treats a block as a code block when it's indented four
;; steps more than the preceding paragraph.
;;
;; Note that text lines that step up the indentation doesn't qualify.
;;
;; Point is at the beginning of a Doxygen comment or after another
;; code block.
(defun highlight-doxygen-find-and-highlight-markdown-code-block (limit)
  "Skip to next paragraph and maybe highlight a MarkDown code block.

Move point to end of the current paragraph.  If the paragraph is
followed by a MarkDown code block (i.e. a block indented four
spaces more than the paragraph), highlight the code block and
move point to the paragraph after it.

Do not search past LIMIT.

Return non-nil if a paragraph was found.

This function is intended to be used in a font-lock keyword."
  (let ((paragraph-indentation
         (highlight-doxygen-forward-to-paragraph-start limit)))
    (when paragraph-indentation
      ;; Skip to end of paragraph.
      (forward-line)
      (while (and (< (point) limit)
                  (or
                   (get-text-property (point) 'highlight-doxygen-ignore)
                   (let ((indentation
                          (highlight-doxygen-current-indentation)))
                     (if indentation
                         (progn
                           ;; Record the last line of the paragraph.
                           (setq paragraph-indentation indentation)
                           t)
                       nil))))
        (forward-line))
      ;; Skip to next non-empty line.
      (while (and (< (point) limit)
                  (not (highlight-doxygen-current-indentation)))
        (forward-line))
      ;; Skip code block (if present).
      (let ((start (point))
            (least-indentation nil))
        (while (and
                (< (point) limit)
                ;; Stop at explicit code blocks (it will be skipped by
                ;; `highlight-doxygen-forward-to-paragraph-start' the
                ;; next time this function is called).
                (not (get-text-property (point) 'highlight-doxygen-ignore))
                (let ((indentation (highlight-doxygen-current-indentation)))
                  (if indentation
                      (if (< indentation (+ paragraph-indentation 4))
                          ;; Indentation no longer least four spaces, stop.
                          nil
                        ;; Continue skipping.
                        (setq least-indentation
                              (if least-indentation
                                  (min least-indentation
                                       indentation)
                                indentation))
                        t)
                    ;; Empty line in block.
                    t)))
          (forward-line))
        (if least-indentation
            (highlight-doxygen-code-block
             (+ start least-indentation)
             (point))
          (goto-char start))))
    ;; By returning a non-nil value (regardless if a code block is
    ;; present), this function is called again when used in a
    ;; font-lock rule.
    paragraph-indentation))


(defcustom highlight-doxygen-code-block-commands
  '(("code"     "endcode")
    ("dot"      "enddot")
    ("msc"      "endmsc")
    ("startuml" "enduml")
    ("verbatim" "endverbatim"))
  "List of Doxygen commands that start a code block.

Each entry in the list is a list on the form (START-COMMAND END-COMMAND)."
  :group 'highlight-doxygen
  :type 'sexp)


(defcustom highlight-doxygen-block-major-mode-alist
  '(("code"     . t)
    ("dot"      . ".dot")
    ("msc"      . ".msc")
    ("startuml" . ".plantuml")
    ("verbatim" . nil))
  "Alist used to determine major mode for Doxygen commands.

The key (car part) is a Doxygen command.  The value (cdr part)
can be one of the following:

- t        -- the buffer is checked for the {.ext} construct
- nil      -- No special major mode is used.
- A string -- An file extension
- A symbol -- A major mode."
  :group 'highlight-doxygen
  :type 'sexp)


(defun highlight-doxygen-block-major-mode (command)
  "Return the major mode or extension that should be used for block.

When a major mode is returned, it is returned as a symbol.  When
it's an extension, it is returned as a string.

Return nil when no suitable major mode is found.

COMMAND is the Doxygen command of the block.  The variable
`highlight-doxygen-block-major-mode-alist' is used to determine
the Emacs major mode should be used for highlighting.

Point is initially placed after the command.  If the Doxygen
command supports the `{.ext}' construct, the point is moved to
the end of the construct."
  (let ((entry (assoc command highlight-doxygen-block-major-mode-alist)))
    (and entry
         (let ((value (cdr entry)))
           (cond ((null value)
                  nil)
                 ((eq value t)
                  (save-match-data
                    (if (looking-at "{\\(\\.\\sw+\\)}")
                        (progn
                          (goto-char (match-end 0))
                          (match-string 1))
                      major-mode)))
                 (t
                  value))))))


(defun highlight-doxygen-find-and-highlight-keywords-code-block (limit)
  "Highlight next code block within `code' or `verbatim' Doxygen commands.

Do not search past LIMIT."
  (let ((start-regexp
         (concat
          "[\\@]\\("
          (regexp-opt (mapcar (lambda (e)
                                (nth 0 e))
                              highlight-doxygen-code-block-commands))
          "\\)")))
    (if (re-search-forward start-regexp limit t)
        (let ((mode (highlight-doxygen-block-major-mode (match-string 1)))
              (keyword-start (match-beginning 0)))
          (skip-syntax-forward " ")
          (when (and (eolp)
                     (< (point) limit))
            (forward-line)
            (while (and (< (point) limit)
                        (not (highlight-doxygen-forward-to-indentation)))
              (forward-line)))
          (let ((start (point))
                (end-regexp
                 (concat
                  "[\\@]"
                  (regexp-opt (mapcar (lambda (e)
                                        (nth 1 e))
                                      highlight-doxygen-code-block-commands))
                  "\\_>")))
            (if (re-search-forward end-regexp limit t)
                (save-excursion
                  (let ((end (match-beginning 0))
                        (keyword-end (match-beginning 0)))
                    ;; Backup to last line containing anything
                    (beginning-of-line)
                    (highlight-doxygen-forward-to-indentation)
                    (when (eq (point) end)
                      (while
                          (progn
                            (forward-line -1)
                            (setq end (line-beginning-position 2))
                            (and
                             (< start (point))
                             (null
                              (highlight-doxygen-forward-to-indentation))))))
                    (highlight-doxygen-code-block start end mode)
                    ;; Mark the code block (including code/endcode) to
                    ;; ensure that the markdown code block highlighter
                    ;; ignores it.
                    (add-text-properties
                     (save-excursion
                       (goto-char keyword-start)
                       (line-beginning-position))
                     (save-excursion
                       (goto-char keyword-end)
                       (line-end-position))
                     '(highlight-doxygen-ignore t))
                    t))
              nil)))
      nil)))


(defun highlight-doxygen-highlight-link-object ()
  "Highlight reference or filename following point, if any."
  (skip-syntax-forward "-")
  (let ((start (point))
        (end (point))
        (limit (line-end-position)))
    ;; Match file names and references.
    ;;
    ;; Example:
    ;;
    ;;    myname.h
    ;;    myfunc()
    ;;    myfunc(unsigned long)       <- Note the space
    (while (and (not (looking-at "\\s-"))
                (condition-case nil
                    (progn
                      (forward-sexp)
                      (if (<= (point) limit)
                          (progn
                            (setq end (point))
                            t)
                        nil))
                  (error nil))))
    (when (< start end)
      ;; TODO: How do we tell file names apart from references?
      (if (string-match "\\." (buffer-substring start end))
          ;; File name
          (font-lock-prepend-text-property
           start end 'face 'font-lock-constant-face)
        ;; Reference
        (highlight-doxygen-code-block start end major-mode)))))

;; -------------------------------------------------------------------
;; Font lock keywords
;;

;; --------------------
;; bold
;;

(defcustom highlight-doxygen-bold-commands
  '("b")
  "List of Doxygen commands that make their argument bold."
  :group 'highlight-doxygen
  :type '(repeat string))

;; Note: `:inherit bold' is used over `:weight bold', to minimize the
;; number of faces a user or a theme would have to customize.
(defface highlight-doxygen-bold
  '((t :inherit bold))
  "The face used to make text bold."
  :group 'highlight-doxygen)


;; --------------------
;; code
;;

(defcustom highlight-doxygen-code-commands
  '("c" "retval")
  "List of Doxygen commands that make their argument code."
  :group 'highlight-doxygen
  :type '(repeat string))

(defface highlight-doxygen-code
  '((t :inherit font-lock-constant-face))
  "The face used to highlight things as code within Doxygen comments.

This is not used for code blocks."
  :group 'highlight-doxygen)


;; --------------------
;; emphasize
;;

(defcustom highlight-doxygen-emphasize-commands
  '("e" "em")
  "List of Doxygen commands that emphasize their argument."
  :group 'highlight-doxygen
  :type '(repeat string))

;; Note: `:inherit italic' is used over `:slant italic', to minimize
;; the number of faces a user or a theme would have to customize.
(defface highlight-doxygen-emphasize
  '((t :inherit italic))
  "The face used to make text emphasized."
  :group 'highlight-doxygen)


;; --------------------
;; exception
;;

(defcustom highlight-doxygen-exception-commands
  '("exception" "idlexcept" "throw" "throws")
  "List of Doxygen commands that take an exception argument."
  :group 'highlight-doxygen
  :type '(repeat string))

;; Note: This mimics C++ mode.
(defface highlight-doxygen-exception
  '((t :inherit font-lock-type-face))
  "The face for exceptions in Doxygen comments."
  :group 'highlight-doxygen)


;; --------------------
;; namespace
;;

(defcustom highlight-doxygen-namespace-commands
  '("namespace")
  "List of Doxygen commands that take a namespace argument."
  :group 'highlight-doxygen
  :type '(repeat string))

;; Note: This mimics C++ mode.
(defface highlight-doxygen-namespace
  '((t :inherit font-lock-constant-face))
  "The face for namespaces in Doxygen comments."
  :group 'highlight-doxygen)


;; --------------------
;; Class
;;

(defcustom highlight-doxygen-qualified-type-commands
  '("class" "enum" "extends" "implements" "interface" "memberof"
    "protocol" "relates" "related" "relatesalso" "relatedalso"
    "struct" "union")
  "List of Doxygen commands that take a \"class\" argument."
  :group 'highlight-doxygen
  :type '(repeat string))

(defface highlight-doxygen-type
  '((t :inherit font-lock-type-face))
  "The face used to highlight class arguments."
  :group 'highlight-doxygen)


;; --------------------
;; Group
;;

;; TODO: The `ingroup' commands can take multiple groups as
;; arguments. Since this is rule itself is included in a anchored
;; match, it's not possible to handle this using anchored matches, so
;; it must be done in elisp.
(defcustom highlight-doxygen-group-commands
  '("addtogroup" "defgroup" "ingroup" "weakgroup")
  "List of Doxygen commands that take a \"group\" argument."
  :group 'highlight-doxygen
  :type '(repeat string))

(defface highlight-doxygen-group
  '((t :inherit highlight))
  "The face used to highlight group arguments."
  :group 'highlight-doxygen)


;; --------------------
;; File names
;;

(defface highlight-doxygen-filename
  '((t :inherit highlight))
  "The face used to highlight filename arguments."
  :group 'highlight-doxygen)


(defcustom highlight-doxygen-filename-commands
  '("dir" "dontinclude" "example" "file" "htmlinclude" "include"
    "includedoc" "includelineno" "latexinclude" "verbinclude"
    ;; Note: also takes "( block_id )"
    "snippet" "snippetdoc" "snippetlineno"
    ;; Note: Also takes "["caption"] [<sizeindication>=<size>]"
    "diafile" "dotfile" "mscfile"
    ;; Note: Also takes "[<header-name>]":
    "headerfile")
  "List of Doxygen commands that take a filename argument."
  :group 'highlight-doxygen
  :type '(repeat string))


;; --------------------
;; Reference (named page or anchor)
;;

(defcustom highlight-doxygen-reference-commands
  '("ref" "refitem" "xrefitem")
  "List of Doxygen commands that take a reference argument."
  :group 'highlight-doxygen
  :type '(repeat string))


;; --------------------
;; Section label
;;

(defcustom highlight-doxygen-section-label-commands
  '("cond" "if" "ifnot" "elseif")
  "List of Doxygen commands that take a \"section-label\" argument."
  :group 'highlight-doxygen
  :type '(repeat string))

(defface highlight-doxygen-section-label
  '((t :inherit font-lock-type-face))
  "The face used to highlight section label arguments."
  :group 'highlight-doxygen)


;; --------------------
;; Variables
;;

(defcustom highlight-doxygen-variable-commands
  '("a" "p" "tparam")
  "List of Doxygen commands that take a variable argument."
  :group 'highlight-doxygen
  :type '(repeat string))

(defcustom highlight-doxygen-variable-with-dir-commands
  '("param")
  "List of Doxygen commands that take an optional dir and a variable argument."
  :group 'highlight-doxygen
  :type '(repeat string))

(defface highlight-doxygen-direction
  '((t :inherit font-lock-builtin-face))
  "The face used to highlight parameter direction."
  :group 'highlight-doxygen)

(defface highlight-doxygen-variable
  '((t :inherit font-lock-variable-name-face))
  "The face used to highlight variables in Doxygen comments."
  :group 'highlight-doxygen)


;; --------------------
;; Line of code
;;

(defcustom highlight-doxygen-code-line-commands
  '("fn" "var" "typedef" "property" "overload")
  "List of Doxygen commands that take a line of code as argument."
  :group 'highlight-doxygen
  :type '(repeat string))


;; --------------------
;; Link object (member, type, page, filename, etc.)
;;

(defcustom highlight-doxygen-link-object-commands
  '("copybrief" "copydetails" "copydoc")
  "List of Doxygen commands that take a line of code or file name as argument."
  :group 'highlight-doxygen
  :type '(repeat string))


;; --------------------
;; Links
;;

(defface highlight-doxygen-link
  '((t :inherit link))
  "The face used to highlight links (URL:s) in Doxygen comments."
  :group 'highlight-doxygen)


;; --------------------
;; Titles
;;

;; mainpage      -- heading-1
;; section       -- heading-2
;; subsection    -- heading-3
;; subsubsection -- heading-4
;; paragraph     -- heading-5

;; page          -- heading-2
;; subpage       -- heading-3

(defface highlight-doxygen-label
  '((t :inherit font-lock-type-face))
  "The face used to highlight level 1 headings."
  :group 'highlight-doxygen)

(defface highlight-doxygen-heading-1
  '((t :inherit outline-1))
  "The face used to highlight level 1 headings."
  :group 'highlight-doxygen)


(defface highlight-doxygen-heading-2
  '((t :inherit outline-2))
  "The face used to highlight level 2 headings."
  :group 'highlight-doxygen)


(defface highlight-doxygen-heading-3
  '((t :inherit outline-3))
  "The face used to highlight level 3 headings."
  :group 'highlight-doxygen)


(defface highlight-doxygen-heading-4
  '((t :inherit outline-4))
  "The face used to highlight level 4 headings."
  :group 'highlight-doxygen)


(defface highlight-doxygen-heading-5
  '((t :inherit outline-5))
  "The face used to highlight level 5 headings."
  :group 'highlight-doxygen)


(defcustom highlight-doxygen-title-commands-alist
  '((("mainpage") . highlight-doxygen-heading-1)
    ;; Note: Doxygen use the same font size for "par" as
    ;; subsubsections.
    (("par" "vhdlflow") . highlight-doxygen-heading-4))
  "Alist from list of Doxygen commands to faces.

The Doxygen commands should take one argument, a title."
  :group 'highlight-doxygen
  :type 'sexp)


;; Note: According to Doxygen, "subpage" requires that the title is
;; placed within quotes whereas the title of the other commands should
;; not use quotes. The highlighting rules generated from this
;; highlight the rest of the line as a title, regardless of quoting
;; style.
(defcustom highlight-doxygen-name-title-commands-alist
  '((("section" "page")        . highlight-doxygen-heading-2)
    (("subsection" "subpage")  . highlight-doxygen-heading-3)
    (("subsubsection")         . highlight-doxygen-heading-4)
    (("paragraph")             . highlight-doxygen-heading-5))
  "Alist from list of Doxygen commands to faces.

The Doxygen commands should take two arguments, a name and a
title."
  :group 'highlight-doxygen
  :type 'sexp)


;; --------------------
;; The font-lock rules
;;


(defvar highlight-doxygen-comment-start-position nil)
(make-variable-buffer-local 'highlight-doxygen-comment-start-position)

(defvar highlight-doxygen-comment-end-position nil)
(make-variable-buffer-local 'highlight-doxygen-comment-end-position)

(defvar highlight-doxygen-start-column nil)
(make-variable-buffer-local 'highlight-doxygen-start-column)


;; Note: In case a face has a corresponding variable (like
;; `font-lock-constant-face', the face expression doesn't quote the
;; name of the face.  Technically, the face expression refers to the
;; variable.  This allows users to customize the face be changing the
;; variable.  However, this technique is deprecated in favour of using
;; `face-remapping-alist'.

(defun highlight-doxygen-anchored-keywords-template ()
  "List of font-lock keywords that will be converted to anchored submatches.

The MATCHER will be wrapped in a call to
`highlight-doxygen-forward-search' and pre and post match forms
will be added.

Note that these rules can't contain anchored rules themselves."
  (let ((title-rules '()))
    (dolist (pair highlight-doxygen-title-commands-alist)
      (let ((commands (car pair))
            (face     (cdr pair)))
        (push `(,(concat "[\\@]\\_<"
                         (regexp-opt commands)
                         "\\s-+"
                         "\\(.*\\)")
                (1 (quote ,face) prepend))
              title-rules)))
    (dolist (pair highlight-doxygen-name-title-commands-alist)
      (let ((commands (car pair))
            (face     (cdr pair)))
        (push `(,(concat "[\\@]\\_<"
                         (regexp-opt commands)
                         "\\s-+"
                         "\\_<\\(\\sw+\\)"
                         "\\(\\s-+"
                         "\\(.*\\)\\)?")
                (1 'highlight-doxygen-label prepend)
                (2 (quote ,face) prepend t))
              title-rules)))
    (append
     `(
       ;; --------------------
       ;; Highlight every line in the Doxygen block.
       ;;
       ;; Unlike plain comment highlighting, make the highlighting
       ;; follow the indentation of the Doxygen comment.
       (highlight-doxygen-match-comment-line
        (0 'highlight-doxygen-comment prepend))
       ;; --------------------
       ;; Explicit code blocks
       (highlight-doxygen-find-and-highlight-keywords-code-block)
       ;; --------------------
       ;; Implicit (indented) code blocks
       (highlight-doxygen-find-and-highlight-markdown-code-block)
       ;; --------------------
       ;; Doxygen command.
       (,(concat "[\\@]"
                 "\\_<\\([a-z]+\\)\\_>")
        (1 'highlight-doxygen-command prepend))

       ;; ----------------------------------------
       ;; Inline constructs.

       ;; --------------------
       ;; Type name

       (highlight-doxygen-match-camel-case
        (1 font-lock-type-face prepend))

       ;; --------------------
       ;; Qualified class name

       ("\\_<\\(\\sw+\\)\\(::\\|#\\)"
        (1 font-lock-type-face prepend))

       ;; --------------------
       ;; Function name
       ("\\_<\\(\\(\\sw\\)+\\)()"
        (1 font-lock-function-name-face prepend))

       ;; --------------------
       ;; Links (URI:s). See RFC 3986, chapter 3.

       ("\\_<\\([a-zA-Z][-a-zA-Z0-9+.]*://[^ \t\n]*\\)"
        (1 'highlight-doxygen-link prepend)))
     title-rules
     `(
       ;; ------------------------------
       ;; Various command signatures.
       ;;

       ;; --------------------
       ;; bold
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-bold-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-bold prepend))

       ;; --------------------
       ;; code
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-code-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-code prepend))

       ;; --------------------
       ;; emphasize
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-emphasize-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-emphasize prepend))

       ;; --------------------
       ;; Type name

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-qualified-type-commands)
                 "\\s-+"
                 ;; Skip qualifiers.
                 "\\_<\\(?:\\sw+\\(?:::\\|#\\)\\)*"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-type prepend))

       ;; --------------------
       ;; exception

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-exception-commands)
                 "\\s-+"
                 ;; Skip qualifiers.
                 "\\_<\\(?:\\sw+\\(?:::\\|#\\)\\)*"
                 "\\(\\sw+\\)")
        (1 'highlight-doxygen-exception prepend))

       ;; --------------------
       ;; namespace

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-namespace-commands)
                 "\\s-+"
                 ;; Skip qualifiers.
                 "\\_<\\(?:\\sw+\\(?:::\\|#\\)\\)*"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-namespace prepend))

       ;; --------------------
       ;; Group name
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-group-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-group prepend))

       ;; --------------------
       ;; File name
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-filename-commands)
                 "\\s-+"
                 "\\_<\\([a-zA-Z0-9_:/\\.]+\\)")
        (1 'highlight-doxygen-filename prepend))

       ;; --------------------
       ;; Reference

       ;; Note: The Doxygen documentation doesn't specify the format
       ;; of a reference, this code use a combination of word
       ;; characters, symbol characters, and punctuation
       ;; characters. Another approach would be to match every
       ;; character except whitespace.  Unfortunately, "\\S-" might
       ;; match newlines, so the search must be restricted to the end
       ;; of the line that contains the Doxygen command.
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-reference-commands)
                 "\\s-+"
                 "\\(\\(\\sw\\|\\s_\\|\\s.\\)+\\)")
        (1 'highlight-doxygen-link prepend))

       ;; --------------------
       ;; section-label (`if' and `elseif' etc.)

       ;; TODO: The section label can be a complex expression like
       ;; "(TEST1 && !TEST2). Since this is rule itself is included in a
       ;; anchored match, it's not possible to handle this using anchored
       ;; matches, so it must be done in elisp.
       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-section-label-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-section-label prepend))

       ;; --------------------
       ;; Variable

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-variable-commands)
                 "\\s-+"
                 "\\_<\\(\\sw+\\)")
        (1 'highlight-doxygen-variable prepend))

       ;; --------------------
       ;; Variable with direction

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-variable-with-dir-commands)
                 "\\_>"
                 "\\s-*"
                 "\\(?:\\["
                 "\\(?:\\(in\\)\\|\\(out\\)\\|\\(in\\),\\(out\\)\\)"
                 "\\]\\)?"
                 "\\s-*"
                 "\\(\\_<\\sw+\\)?")
        (1 'highlight-doxygen-direction prepend t) ; in
        (2 'highlight-doxygen-direction prepend t) ; out
        (3 'highlight-doxygen-direction prepend t) ; in  (part of in,out)
        (4 'highlight-doxygen-direction prepend t) ; out (part of in,out)
        (5 'highlight-doxygen-variable prepend t))

       ;; --------------------
       ;; Line of code

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-code-line-commands)
                 "\\s-+\\(.*\\)$")
        (0 (progn
             (highlight-doxygen-code-block
              (match-beginning 1)
              (match-end 1)
              major-mode)
             nil)))

       ;; --------------------
       ;; Reference or file name

       (,(concat "[\\@]\\_<"
                 (regexp-opt highlight-doxygen-link-object-commands)
                 "\\_>")
        (0 (progn
             ;; This will apply suitable highlighting to whatever is
             ;; after the command.
             (highlight-doxygen-highlight-link-object)
             nil)))

       ;; --------------------
       ;; Highlight "`foo`". Note that in Doxygen a quote cancels a
       ;; backquote.
       ;;
       ;; TODO: Multi-line support.
       ("`\\([^\n`']+\\)`"
        (1 (progn
             (goto-char (match-end 0))
             font-lock-constant-face)
           prepend))))))


(defun highlight-doxygen-forward-search (matcher limit)
  "Search for MATCHER but skip Doxygen code blocks.

If MATCHER is a string `re-search-forward' is used, otherwise it
is called as a function.

Do not search past LIMIT."
  (let (res)
    (while (and
            (setq res (let ((old-point (point)))
                        (and
                         (< (point) limit)
                         (if (stringp matcher)
                             (re-search-forward matcher limit t)
                           (funcall matcher limit))
                         ;; If the matcher function succeeds but
                         ;; doesn't move the point, Emacs hangs.
                         (< old-point (point)))))
            (get-text-property (point) 'highlight-doxygen-code)))
    res))


(defun highlight-doxygen--pre-match-form ()
  "Prepare for submatches in an anchored font-lock rule.

Move point to start of current Doxygen comment and return the
end, making it the region the sub matcher will be applied to."
  ;; Font-lock moves point one position in an attempt to avoid
  ;; infinite searches after matching the main matcher. We need
  ;; to move the point back to the start of the comment to make
  ;; the block highlighting work properly.
  (goto-char highlight-doxygen-comment-start-position)
  ;; Search limit
  highlight-doxygen-comment-end-position)


(defun highlight-doxygen--post-match-form ()
  "Wind up an anchored font-lock rule."
  ;; TODO: If this is nil, font-lock work but font-lock-studio
  ;; hangs. Find out why!
  (goto-char highlight-doxygen-comment-end-position))


(defun highlight-doxygen-compose-font-lock-keywords ()
  "Construct the font-lock keywords for highlighting Doxygen comments."
  (let (subrules '())
    (dolist (template (highlight-doxygen-anchored-keywords-template))
      (let ((expr (nth 0 template)))
        (unless (stringp expr)
          (setq expr (list 'function expr)))  ; Same as #'
        (push `((lambda (limit)
                  (highlight-doxygen-forward-search ,expr limit))
                (highlight-doxygen--pre-match-form)
                (highlight-doxygen--post-match-form)
                ,@(cdr template))
              subrules)))
    `((highlight-doxygen-match-comment ,@(nreverse subrules)))))



;; -------------------------------------------------------------------
;; Comment matcher.
;;

(defun highlight-doxygen-next-comment (limit)
  "Search for next Doxygen comment.

Stop search at LIMIT.  If a Doxygen comment is found, move point
and return non-nil.  Otherwise nil is returned (point may be
moved)."
  (let (res)
    (while
        (and
         (setq res (re-search-forward
                    highlight-doxygen-commend-start-regexp
                    limit t))
         ;; Continue looping if:
         (or (and
              highlight-doxygen-ignore-triple-slash-comments
              (save-excursion
                (goto-char (match-beginning 0))
                (looking-at highlight-doxygen-triple-slash-comment-regexp)))
             (let ((state (syntax-ppss)))
               (or
                ;; Not in comment?
                (not (nth 4 state))
                ;; At the start of the comment?
                (not (eq (match-beginning 0) (nth 8 state))))))))
    (when res
      (goto-char (match-beginning 0)))
    res))


(defun highlight-doxygen-move-end-of-comment ()
  "Move point to end of Doxygen comment.

Treat consecutive Doxygen comments like one."
  (let ((end (point)))
    (while
        (if (forward-comment 1)
	    (progn
	      (setq end (point))
	      (skip-chars-forward " \t\n")
	      (looking-at highlight-doxygen-commend-start-regexp))
	  ;; Guard against being enabled in modes where the comment
	  ;; syntax doesn't match
	  ;; `highlight-doxygen-commend-start-regexp'.
	  (setq end (line-end-position))
	  nil))
    (goto-char end)
    (when (and (eolp)
               (not (bolp)))
      (forward-line))))


(defun highlight-doxygen-match-comment (limit)
  "Find next Doxygen comment.

Do not search past LIMIT.

Set `highlight-doxygen-comment-end-position' to end of Doxygen comment."
  (let ((res (highlight-doxygen-next-comment limit)))
    (when res
      (setq highlight-doxygen-comment-start-position (point))
      (setq highlight-doxygen-comment-end-position
            ;; Respect `limit'.
            ;;
            ;; In normal operation, the refontified region is always
            ;; extended to include the full comment.  However, when
            ;; using font-lock studio (a debugger for font-lock
            ;; keywords) the user may use a smaller region.
            (min
             (save-excursion
               (highlight-doxygen-move-end-of-comment)
               (point))
             limit))
      (setq highlight-doxygen-start-column (current-column)))
    res))


(defun highlight-doxygen-match-comment-line (limit)
  "Match a single Doxygen comment line.

Do not search past LIMIT."
  (while (and (< (current-column) highlight-doxygen-start-column)
              (not (eolp))
              (memq (following-char) '(?\s ?\t)))
    (forward-char))
  (if (or (and (eolp)
               (not (bolp)))
          (> (point) limit))
      nil
    (set-match-data (list (point) (min (save-excursion
                                         (forward-line)
                                         (point))
                                       limit)))
    (forward-line)
    t))


(defun highlight-doxygen-match-camel-case (limit)
  "Search for next type, which is a CamelCase word.

Do not search past LIMIT.

Constructs like `CamelCase(' are ignored, as they are assumed to
be function calls.

Constructs like `CamelCase.h' are ignores, as they look like file
names."
  (let (res)
    (while (and (setq res (re-search-forward
                           (concat "\\_<\\("
                                   ;; Match a word written using
                                   ;; CamelCase, starting with a
                                   ;; capital letter.
                                   ;;
                                   ;; TODO: Allow "_"?
                                   "[A-Z]+[a-zA-Z0-9]*[a-z]+[a-zA-Z0-9]*"
                                   "[A-Z][a-zA-Z0-9]+"
                                   "\\)\\_>")
                           limit t))
                (or (save-excursion
                      ;; This is a function call, ignore.
                      (skip-chars-forward " \t")
                      (eq (following-char) ?\( ))
                    (save-match-data
                      ;; This is a file extension, ignore.
                      (looking-at "\\.[a-zA-Z]+\\_>")))))
    res))


;; -------------------------------------------------------------------
;; Region extender.
;;

(defun highlight-doxygen-inside-special-comment ()
  "Return start of Doxygen-style comment, or nil.

Treat consecutive line comments like one block."
  (save-excursion
    (let ((res nil))
      (while
          (progn
            (skip-chars-backward " \t")
            (when (and (not (bobp))
                       (bolp))
              (backward-char))
            (let ((state (syntax-ppss)))
              (if (nth 4 state)         ; Comment
                  (progn
                    (goto-char (nth 8 state))
                    (if (looking-at highlight-doxygen-commend-start-regexp)
                        (progn
                          (setq res (point))
                          t)
                      nil))
                nil))))
      res)))


(defun highlight-doxygen-extend-region-full-comment ()
  "Extend font-lock region to include the full Doxygen comment."
  (save-excursion
    (let ((res nil))
      (goto-char font-lock-beg)
      (let ((start (highlight-doxygen-inside-special-comment)))
        (when (and start
                   (< start font-lock-beg))
          (setq font-lock-beg start)
          (setq res t)))
      (goto-char font-lock-end)
      (let ((start (highlight-doxygen-inside-special-comment)))
        (when start
          (highlight-doxygen-move-end-of-comment)
          (when (< font-lock-end (point))
            (setq font-lock-end (point))
            (setq res t))))
      res)))


;; -------------------------------------------------------------------
;; The modes.
;;


(defvar highlight-doxygen--old-c-doc-rules nil)

;;;###autoload
(define-minor-mode highlight-doxygen-mode
  "Minor mode that highlights Doxygen comments."
  :group 'highlight-doxygen
  ;; TODO: Cache the keywords.
  (if highlight-doxygen-mode
      (progn
        (add-to-list 'font-lock-extend-region-functions
                     #'highlight-doxygen-extend-region-full-comment)
        (add-to-list 'font-lock-extra-managed-props 'highlight-doxygen-code)
        (add-to-list 'font-lock-extra-managed-props 'highlight-doxygen-ignore)
        ;; Remove built-in c-doc rules, to avoid it interfering with
        ;; our rules.
        (when (fboundp 'c-compose-keywords-list)
          (let ((c-doc-keywords (c-compose-keywords-list '())))
            (set (make-local-variable 'highlight-doxygen--old-c-doc-rules)
                 c-doc-keywords)
            (font-lock-remove-keywords nil c-doc-keywords)))
        (font-lock-add-keywords nil
                                (highlight-doxygen-compose-font-lock-keywords))
        (setq font-lock-multiline t))
    ;; Note: `font-lock-multiline' is not restored. It may have gotten
    ;; set by some other minor mode. Besides, it doesn't hurt keeping
    ;; it set to t.
    (setq font-lock-extend-region-functions
          (delq 'highlight-doxygen-extend-region-full-comment
                font-lock-extend-region-functions))
    (setq font-lock-extra-managed-props
          (delq 'highlight-doxygen-code font-lock-extra-managed-props))
    (setq font-lock-extra-managed-props
          (delq 'highlight-doxygen-ignore font-lock-extra-managed-props))
    (font-lock-remove-keywords nil
                               (highlight-doxygen-compose-font-lock-keywords))
    (font-lock-add-keywords nil highlight-doxygen--old-c-doc-rules))
  ;; As of Emacs 25, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))



;;;###autoload
(define-global-minor-mode highlight-doxygen-global-mode highlight-doxygen-mode
  (lambda ()
    (when (apply #'derived-mode-p highlight-doxygen-modes)
      (highlight-doxygen-mode 1)))
  :group 'highlight-doxygen
  :require 'highlight-doxygen)


(provide 'highlight-doxygen)

;;; highlight-doxygen.el ends here
