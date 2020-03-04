;; ****************************************
;; ***      Gokhan Has - 161044067      ***
;; ****************************************

;; This is the G++ Language Lexer implemented in Common Lisp ....
;; DFA cizimi aynı klasorde 161044067_dfa.pdf dosyasi icerisinde ....

;; Assign the keywords and operators according to G++ Lexical Syntax PDF ...
(setq KEYWORDS_LIST '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "while"))
(setq KEYWORDS_TOKEN_LIST '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT"
                            "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE" "KW_WHILE"))

(setq OPERATORS_LIST '("+" "-" "/" "*" "(" ")" "**" """" ","))
(setq OPERATORS_TOKEN_LIST '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA"))

(setq TOKENS_LIST '())

;; Read as file by string ...
(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          	collect line
    )
  )
)

;; Return T if the ch is between the 'A' and 'z' according the ASCII code ...
(defun is-character(ch)
    (and (<= (char-code ch) 122) (>= (char-code ch) 65))
)

;; Return T if the nb is between the '0' and '9' according the ASCII code ...
(defun is-number(nb)
    (and (<= (char-code nb) 57) (>= (char-code nb) 48)) 
)

;; Return T if the ch is space according the ASCII code ...
(defun is-space (ch)
    (= (char-code ch) 32)
)

;; Searching list and if the str is in liste, return its index ...
(defun searchList (str liste)
    (dotimes (index (length liste))
        (if (equal (nth index liste) str)
            (return-from searchList index)
        )
    )
    nil
)

;; Trimmed the list ...
(defun trim-list (list)
	(setq newList '())
	(dolist (line list)
		(setq resultString "")
		(loop for ch across line
			do 
				(cond
					((equal (string ch) "(") (setq resultString (concatenate 'string resultString (string "( "))))
					((equal (string ch) ")") (setq resultString (concatenate 'string resultString (string " )"))))
					(t (setq resultString (concatenate 'string resultString (string ch))))
				)
		)
		(setq newList (append newList (list resultString)))
	)
	newList
)

;; Returns error message as a string ...
(defun return-error-message (strORch)
	(setq error "SYNTAX__ERROR ")
	(setq error2 " cannot be tokenized")
	(setq error (concatenate 'string error (string strORch)))
	(setq error (concatenate 'string error error2))
	error
)

;; Determined the token is number, iden or syntax_error
(defun is-identifier-or-number (str)
	(setq ch-number 0)
	(setq nb-number 0)
	(setq ifisFirst 0)
	(setq syntax_control nil)

	(loop for c across str do
		(if (equal ifisFirst 0)
			(if (is-number c) ;; such as 123AD is not identifier, chechs its ...
				(progn 
					(setq syntax_control T)
					(setq ifisFirst 1)
				)
				(setq ifisFirst 1)
			)
		)
		(if (is-character c)
			(setq ch-number (+ ch-number 1))
		)
		(if (is-number c)
			(setq nb-number (+ nb-number 1))
		)
	)
	(if (equal (length str) ch-number)
		(return-from is-identifier-or-number "IDENTIFIER")
		;; token is IDENTIFIER, because consists of characters ...
	)
	(if (equal (length str) nb-number)
		(return-from is-identifier-or-number "VALUE")
		;; token is VALUE, because consists of numbers ...
	)
	(if (equal syntax_control T)
		(progn 
			(return-from is-identifier-or-number (return-error-message str))
			;; concreate syntax_error message ...
		)
		(return-from is-identifier-or-number "IDENTIFIER")
	)
)

;; Determine the tokens names ...
(defun determine-tokens-name (line) 
	(setq controlComma 0)
	(setq controlSpace 0)
	(setq tokens '())
	(setq word "")
	(setq integers "")
	(loop for ch across line do ;; ch is character ...
		
		(cond 
			( (equal ch #\;) ;; Control comment using controlComma variable ...
				(if (equal controlComma 0)
					(setf controlComma 1)
					(progn 
						(setf controlComma 0)
						(setq tokens (append tokens (list '("COMMENT"))))
						(return-from determine-tokens-name tokens)
					)
				)
			)

			((equal ch #\*) ;; Control DBL_MLT operator .... like using controlcomma ....	
				(if (equal controlComma 0)
					(setf controlComma 1)
					(progn 
						(setf controlComma 0)
						(setq tokens (append tokens (list '("OP_DBLMULT" "**"))))
					)
				)

			)

			( (not (eq (searchList (string ch) OPERATORS_LIST) nil))
				;; Control operators ...
				(setq index (searchList (string ch) OPERATORS_LIST))
				(setq tokens (append tokens (list (list (nth index OPERATORS_TOKEN_LIST) (string ch)))))
			)

			( (not (equal (is-character ch) nil))
				;; WORD STRING'I GUNCELLENECEK ...
				(setf word (concatenate 'string word (string ch)))
			)

			( (not (equal (is-number ch) nil))
				;; INTEGER STRINGI GUNCELLENECEK
				(setf word (concatenate 'string word (string ch)))
				(setf integers (concatenate 'string integers (string ch)))
			)

			( (equal (is-space ch) t)
				;; If the character is space, control the  word string and determined its token ...
				(setq controlSpace 1)
				(if (not (equal word ""))
					(progn 
						(if (not (equal (searchList word KEYWORDS_LIST) nil))
							(progn 
								(setq index (searchList word KEYWORDS_LIST)) ;; If is keyword ...
								(setq tokens (append tokens (list (list (nth index KEYWORDS_TOKEN_LIST) word))))
								(setq word "")	
							)
							(progn ;; Determine if the word string is identifier, value or sytanx_error using helper function such as is-identifier-or-number ...
								;(setq tokens (append tokens (list (is-identifier-or-number word))))
								(setq tokens (append tokens (list (list (is-identifier-or-number word) word))))
								(setq word "")
							)
						)
					)
				)							
			)
			;; An error occured, because the program does not syntax analysis correctly ...
			(t (setq tokens (append tokens (list (list (return-error-message ch) (string ch))))))
		)
	)
	(return-from determine-tokens-name tokens)
)

;; Assign the global TOKENS_LIST their tokens names ...
(defun helper-lexer (liste)
	(dolist (line liste)
		(setq TOKENS_LIST (append TOKENS_LIST (determine-tokens-name line)))
	)
	TOKENS_LIST
)

;; Printing tokens name ...
(defun print-tokens (liste)
	(terpri)
	(dolist (line liste)
		(format t "~a~%" line)
	)
	(terpri)
)

;; This is the main function, lexer ...
(defun gpp_lexer (file_string)
	(setq TOKENS_LIST '())
    (setq string-list '())
    (setq new-string-list '())

    ;(setq file_string (read-file filename))
	(setq file-list '())
	(setq file-list (append file-list (list file_string)))
	
	(setf new-string-list (trim-list file-list))
	(helper-lexer new-string-list)
	(return-from gpp_lexer TOKENS_LIST)
)

