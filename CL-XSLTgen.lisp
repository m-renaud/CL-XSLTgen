(defgeneric stringify (value))

(defmethod stringify (value)
  value)

(defmethod stringify ((value number))
  (write-to-string value))


(defun keywords->attribute (key value)
  (format nil "~a=~s" (string-downcase (symbol-name key)) (stringify value)))


(defun generate-attribute-list (attribute-plist)
  (format nil "~{~a~^ ~}"
          (loop
           :while attribute-plist
           :collecting (keywords->attribute
                        (pop attribute-plist) 
                        (pop attribute-plist)))))
  

(defun gen-open-tag (name attribute-plist &key (close-tag nil))
  (format nil
          (concatenate 'string "~%<" name " ~a" (if close-tag "/" "") ">")
          (generate-attribute-list attribute-plist)))


(defun gen-close-tag (name)
  (format nil (concatenate 'string "</" name ">")))


(defmacro def-xml-element (name mapping &key empty-element)
  `(defun ,name
     (attribute-plist ,@(when (not empty-element) '(&rest rest)))
     (format nil ,(if empty-element "~a" "~a~%~{~a~}~%~a")
             (gen-open-tag ,mapping attribute-plist :close-tag ,empty-element)
             ,@(when (not empty-element)
                 `((loop :for form :in rest :collecting form)
                   (gen-close-tag ,mapping))))))


(def-xml-element xsl-apply-imports "xsl:apply-imports" :empty-element t)
(def-xml-element xsl-apply-templates "xsl:apply-templates" :empty-element t)
(def-xml-element xsl-attribute "xsl:attribute")
(def-xml-element xsl-attribute-set "xsl:attribute-set")
(def-xml-element xsl-call-template "xsl:call-template")
(def-xml-element xsl-choose "xsl:choose")
(def-xml-element xsl-comment "xsl:comment")
(def-xml-element xsl-copy "xsl:copy")
(def-xml-element xsl-copy-of "xsl:copy-of" :empty-element t)
(def-xml-element xsl-decimal-format "xsl:decimal-format" :empty-element t)
(def-xml-element xsl-element "xsl:element")
(def-xml-element xsl-fallback "xsl:fallback")
(def-xml-element xsl-for-each "xsl:for-each")
(def-xml-element xsl-if "xsl:if")
(def-xml-element xsl-import "xsl:import" :empty-element t)
(def-xml-element xsl-include "xsl:include" :empty-element t)
(def-xml-element xsl-key "xsl:key" :empty-element t)
(def-xml-element xsl-message "xsl:message")
(def-xml-element xsl-namespace-alias "xsl:namespace-alias" :empty-element t)
(def-xml-element xsl-number "xsl:number" :empty-element t)
(def-xml-element xsl-otherwise "xsl:otherwise")
(def-xml-element xsl-output "xsl:output" :empty-element t)
(def-xml-element xsl-param "xsl:param")
(def-xml-element xsl-preserve-space "xsl:preserve-space" :empty-element t)
(def-xml-element xsl-processing-instruction "xsl:processing-instruction")
(def-xml-element xsl-sort "xsl:sort" :empty-element t)
(def-xml-element xsl-strip-space "xsl:strip-space" :empty-element t)
(def-xml-element xsl-stylesheet "xsl:stylesheet")
(def-xml-element xsl-template "xsl:template")
(def-xml-element xsl-text "xsl:text")
(def-xml-element xsl-transform "xsl:transform")
(def-xml-element xsl-value-of "xsl:value-of" :empty-element t)
(def-xml-element xsl-variable "xsl:variable")
(def-xml-element xsl-when "xsl:when")
(def-xml-element xsl-with-param "xsl:with-param")
