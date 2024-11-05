" Keywords
syntax keyword rflxKeyword with
syntax keyword rflxKeyword package
syntax keyword rflxKeyword is
syntax keyword rflxKeyword type
syntax keyword rflxKeyword range
syntax keyword rflxKeyword unsigned
syntax keyword rflxKeyword all
syntax keyword rflxKeyword some
syntax keyword rflxKeyword Head
syntax match rflxKeyword "'"
syntax keyword rflxKeyword null
syntax keyword rflxKeyword message
syntax keyword rflxKeyword case
syntax keyword rflxKeyword when
syntax keyword rflxKeyword First
syntax keyword rflxKeyword Size
syntax keyword rflxKeyword Last
syntax match rflxKeyword "'Opaque"
syntax match rflxKeyword "[.]\\{2,\\}"
syntax match rflxKeyword "Always_Valid"
syntax keyword rflxKeyword end
syntax keyword rflxKeyword Checksum
syntax match rflxKeyword "Byte_Order"
syntax match rflxKeyword "High_Order_First"
syntax match rflxKeyword "Low_Order_First"
syntax keyword rflxKeyword use
syntax keyword rflxKeyword new
syntax keyword rflxKeyword sequence
syntax keyword rflxKeyword of
syntax keyword rflxKeyword generic
syntax keyword rflxKeyword function
syntax keyword rflxKeyword return
syntax keyword rflxKeyword Readable
syntax keyword rflxKeyword Writable
syntax keyword rflxKeyword machine
syntax keyword rflxKeyword renames
syntax keyword rflxKeyword begin
syntax keyword rflxKeyword state
syntax keyword rflxKeyword Desc
syntax keyword rflxKeyword Append
syntax keyword rflxKeyword Extend
syntax keyword rflxKeyword Reset
syntax keyword rflxKeyword Read
syntax keyword rflxKeyword Write
syntax keyword rflxKeyword transition
syntax keyword rflxKeyword goto
syntax keyword rflxKeyword exception
hi link rflxKeyword Keyword

" Conditional keywords
syntax keyword rflxConditional if
syntax keyword rflxConditional then
hi link rflxConditional Conditional

" Punctuation (neovim specific). See `:h treesitter-highlight`
if has('nvim-0.9')
   syntax match rflxPunctuation ";"
   syntax match rflxPunctuation "("
   syntax match rflxPunctuation ")"
   syntax match rflxPunctuation "\["
   syntax match rflxPunctuation "]"
   syntax match rflxPunctuation ","
   syntax match rflxPunctuation "[.]"
   syntax match rflxPunctuation ":"
   hi link rflxPunctuation @punctuation
endif

" Operators
syntax match rflxOperator "=>"
syntax match rflxOperator ":="
syntax match rflxOperator "+"
syntax match rflxOperator "-"
syntax match rflxOperator "[*]"
syntax match rflxOperator "[.]\{2,}"
syntax match rflxOperator "/"
syntax keyword rflxOperator mod
syntax match rflxOperator ":\{2,}"
syntax keyword rflxOperator and
syntax keyword rflxOperator or
syntax keyword rflxOperator not
syntax keyword rflxOperator Valid
syntax match rflxOperator "Has_Data"
syntax keyword rflxOperator Present
syntax match rflxOperator "="
syntax match rflxOperator "/="
syntax match rflxOperator "<="
syntax match rflxOperator "<"
syntax match rflxOperator ">="
syntax match rflxOperator ">"
syntax keyword rflxOperator for
syntax keyword rflxOperator in
syntax match rflxOperator "\""
syntax match rflxOperator "not in"
syntax match rflxOperator "|"
syntax match rflxOperator "[*]\{2,}"
syntax match rflxOperator "Valid_Checksum"
hi link rflxOperator Operator

" Boolean constants
syntax keyword rflxBoolean True
syntax keyword rflxBoolean False
hi link rflxBoolean Boolean

" Builtin types
syntax keyword rflxType Channel
syntax keyword rflxType Opaque
hi link rflxType Type

" Match number literal such as `1` or in an explicit base `#16#FF#`
syntax match rflxNumber "\<\d[0-9_]*\(\.\d[0-9_]*\)\=\([Ee][+-]\=\d[0-9_]*\)\=\>"
syntax match rflxNumber
    \ "\<\d\d\=#\x[0-9A-Fa-f_]*\(\.\x[0-9A-Fa-f_]*\)\=#\([Ee][+-]\=\d[0-9_]*\)\="
hi link rflxNumber Number

" TODO, FIXME, NOTE and XXX
syntax keyword rflxTodo contained TODO FIXME XXX NOTE
hi link rflxTodo Todo

" Comments
syntax region rflxComment
    \ oneline
    \ contains=rflxTodo
    \ start="--"
    \ end="$"
hi link rflxComment Comment

" String literals (e.g. "some text")
syntax region rflxString
    \ oneline
    \ contains=rflxTodo
    \ start="\""
    \ end="\""
hi link rflxString String
