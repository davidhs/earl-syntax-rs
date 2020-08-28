# Earl Syntax

Earl syntax is an s-expression-like language.  It supports lists but not pairs like s-expression.

The main difference between S-expressions and Earl syntax are the multi-line strings and nesting comments.

You should be able to comment out any text given sufficently many semicolons on both sides of the text, e.g. `;;;(...);;;`, and you should be able to put whatever inside a string given sufficently many double quotation marks, e.g. `"""".text here.""""`.

## Description

## Strings

A string starts with a double quotation mark `"`.

A string starts with n (one or more) `"`, followed by the flags which are any number of characters excluding `.` and maybe `,`, then the body of the string, and finally ends with `.` or `,` and n `"`.

Examples,

```text
"Hello world!"

".Hello world!."

".Hello
world."

""""",
This is a multi-line string,
."""
the continues on here.
,"""""

; A raw string could look like this.
"r.C\User\Name\My Documents\script.js"

```

### Comments

A single-line comments starts with `;` and spans until the end of the line (or end of file)

A (nesting) multi-line starts with n `;`, followed by `(`, then some text, and then by `)` followed by n `;`.

Examples,

```text
; This is a single-line comment

;( This is a
  ;( nesting );
  multi-line comment );

;;( this is also a ;( nesting ); multi-line comment );;
```
